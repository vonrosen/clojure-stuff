(ns c-finance.core2
  (:require [clj-time.core :as tc] 
            [clj-time.periodic :as tp] 
            [clj-time.coerce :as tco]
            [clojure.math.numeric-tower :as math])
  
  (:import [org.apache.commons.math3. distribution BetaDistribution]))

(defn random-in-range-2 [lower upper] 
  (let [r (rand upper)] 
    (if (>= r lower) r 
      (+ (rand (- upper lower)) lower))))

(defn stochastic-k-2 [last-price low-price high-price]
  ;(prn (str last-price " ** " low-price " ** " high-price))
  (let [hlrange (+ high-price low-price) ;10
        hlmidpoint (/ hlrange 2) ;5
        numerator (if (> last-price hlmidpoint) ; 28.385138634503797 5 
                    (- last-price hlmidpoint) 
                    (- hlmidpoint low-price))] 
    (/ numerator hlrange)))

(defn break-local-minima-maxima-2 [k] 
  (as-> k k 
    (if (<= (int (+ 0.95 k)) 0) 
      (+ 0.15 k) 
      k) 
    (if (>= k 1)
      (- k 0.15) 
      k)))

(defn generate-prices-2 
  ([low high] 
    (generate-prices-2 (random-in-range-2 low high)))
  ([last-price] 
    (iterate (fn [{:keys [last]}]
               ;(prn last)
               (let [low (- last 5) 
                     high (+ last 5)
                     high (if (> high 100) 100 high) ;this needs to be added, otherwise numbers get too high and we get cast exception when int cast happens in break-local-minima-maxima-2 function
                     low (if (< low -100) -100 low) ;this needs to be added, otherwise numbers get too low and we get cast exception when int cast happens in break-local-minima-maxima-2 function
                     k (stochastic-k-2 last low high)                     
                     plus-OR-minus (rand-nth [- +]) 
                     kPM (if (= plus-OR-minus +) (+ 1 (break-local-minima-maxima-2 k)) (- 1 (break-local-minima-maxima-2 k))) 
                     newprice (* kPM last) 
                     newlow (if (< newprice low) 
                              newprice 
                              low) 
                     newhigh (if (> newprice high) 
                               newprice 
                               high)]
                 ;(prn (str "k=" k))                 
                 {:last newprice})) {:last last-price})))

(defn generate-timeseries-2 
  ([pricelist] 
    (generate-timeseries-2 pricelist (tc/now))) 
  ([pricelist datetime]
    (->> (map (fn [x y] [x y]) 
              (map (fn [x] {:time x}) 
                   (iterate #(tc/plus % (tc/seconds (rand 4))) datetime)) 
              (map (fn [x] {:price x}) pricelist)) 
      (map (fn [x] (merge (first x) (second x)))))))

(defn moving-average [tick-seq tick-window]
  (partition tick-window 1 tick-seq))

(defn simple-moving-average [options tick-window tick-list]  
  (let [start-index tick-window
        {input-key :input
         output-key :output
         etal-keys :etal
         :or {input-key :last-trade-price
              output-key :last-trade-price-average
              etal-keys [:last-trade-entry]}} options]    
    (reduce 
      (fn [rslt ech] ; ech=(0 1 2 3 4 5 6), (1 2 3 4 5 6 7), etc.
        (let [tsum (reduce 
                     (fn [rr ee] ; ee=0, 2, 3, etc
                       (let [ltprice (:last (:price ee))]
                         (+ ltprice rr))) 0 ech) 
              taverage (/ tsum (count ech))]  ; rslt=the average
          ;(prn (last ech))
          (lazy-cat rslt [(merge {:last-trade-entry (last ech)} {output-key taverage :population ech})]))) ;first rslt = '() 
      '() 
      (take (* 2 tick-window) (partition tick-window 1 tick-list)))))

#_(take 2 (simple-moving-average {} 20 timeseries))
#_(map :last (take 40 (generate-prices 5 15)))

#_(defn extract-price-only [pricelist] (map :last pricelist))

(defn exponential-moving-average 
  "From a tick-list, generates an accompanying exponential moving average list. 
EMA = price( today) * k + EMA( yesterday) * (1 - k) k = 2 / N + 1 N = number of days Returns a list, equal in length to the tick-list, but only with slots filled,
where preceding tick-list allows. Options are: :input - input key function will look for (defaults to :last-trade-price) :output - output key function will emit (defaults to :last-trade-price-exponential) :etal - other keys to emit in each result map ** This function assumes the latest tick is on the left**" 
  ([options tick-window tick-list] 
    (exponential-moving-average options tick-window tick-list (simple-moving-average {} tick-window tick-list))) 
  ([options tick-window tick-list sma-list] 
    ;; 1. calculate 'k' ;; k = 2 / N + 1 ;; N = number of days 
    (let [k (/ 2 (+ tick-window 1)) 
          {input-key :input 
           output-key :output 
           etal-keys :etal 
           :or {input-key :last-trade-price               
                output-key :last-trade-price-exponential 
                etal-keys [:last-trade-entry]}} options] 
      ;; 2. get the simple-moving-average for a given tick - 1      
      (reduce (fn [rslt ech] ;; 3. calculate the EMA (for the first tick, EMA( yesterday) = MA( yesterday) )                
                (let [;; price( today) 
                      ;;ltprice (:last (:price ech))
                      ;ltprice (input-key ech)
                      ltprice (:last (:price (:last-trade-entry ech))) 
                      ;; EMA( yesterday) 
                      ema-last (if (output-key (last rslt)) (output-key (last rslt)) (:last-trade-price-average ech)) 
                      ;; ** EMA now = price( today) * k + EMA( yesterday) * (1 - k) 
                      ema-now (+ (* k ltprice) (* ema-last (- 1 k)))]
                  ;(prn ltprice)                  
                  (lazy-cat rslt 
                            [(merge {:last-trade-entry (:last-trade-entry ech)} {output-key ema-now :population (:population ech)})]))) 
              '() 
              sma-list))))

#_(take 2 (exponential-moving-average {} 5 timeseries))

(defn bollinger-band 
 "From a tick-list, generates an accompanying list with upper-band and lower-band Upper Band: K times an N-period standard deviation above the moving average (MA + Kσ) Lower Band: K times an N-period standard deviation below the moving average (MA − Kσ) K: number of standard deviations N: period, or tick-window we are looking at Returns a list, equal in length to the tick-list, but only with slots filled, where preceding tick-list allows. ** This function assumes the latest tick is on the left**" 
 ([tick-window tick-list] 
   (bollinger-band tick-window tick-list (simple-moving-average {} tick-window tick-list))) 
 ([tick-window tick-list sma-list]
   ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean) 
   (reduce 
     (fn [rslt ech] 
       (let [;; get the Moving Average 
             ma (:last-trade-price-average ech) ;; work out the mean 
             mean (/ (reduce 
                       (fn [rslt ech] (+ (:last (:price ech)) rslt)) 
                       0 (:population ech)) (count (:population ech))) 
             ;; Then for each number: subtract the mean and square the result (the squared difference)
             sq-diff-list (map
                            (fn [ech] (let [diff (- mean (:last (:price ech)))] (* diff diff))) (:population ech)) 
             variance (/ (reduce + sq-diff-list) (count (:population ech))) 
             standard-deviation (. Math sqrt variance)] 
         (lazy-cat rslt 
                   [{:last-trade-entry (:last-trade-entry ech)                     
                     :upper-band (+ ma (* 2 standard-deviation)) 
                     :lower-band (- ma (* 2 standard-deviation))}]))) 
     '()
     sma-list)))

(defn polynomial [x] 
  (-> (+ (* 2
            (Math/pow x 3)) 
         (* 2 (Math/pow x 2))) (- (* 3 x))))

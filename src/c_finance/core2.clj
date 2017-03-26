(ns c-finance.core2
  (:require [clj-time.core :as tc]
            [clj-time.periodic :as tp] 
            [clj-time.coerce :as tco]
            [clojure.math.numeric-tower :as math])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

(defn rand-double-in-range 
  "Returns a random double between min and max." 
  [min max] 
  ;{:pre [(<= min max)]} 
  (+ min (* (- max min) (Math/random))))

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
  ([]
    (generate-prices-2 (rand-double-in-range 1 5) (rand-double-in-range 10 15)))
  ([last-price] 
    (iterate (fn [last #_{:keys [last]}]
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
                 #_{:last newprice} newprice)) #_{:last last-price} last-price)))

(defn generate-timeseries-2 
  ([pricelist] 
    (generate-timeseries-2 pricelist (tc/now))) 
  ([pricelist datetime]
    (->> (map (fn [x y] [x y]) 
              (map (fn [x] {:time (.toDate x)}) 
                   (iterate #(tc/plus % (tc/seconds (rand 4))) datetime)) 
              (map (fn [x] {:price x}) pricelist)) 
      (map (fn [x] (merge (first x) (second x)))))))

(defn moving-average [tick-seq tick-window]
  (partition tick-window 1 tick-seq))

(def ticks-to-sma-key-map {:last-trade-price :price :last-trade-time :time})

(defn simple-moving-average [options tick-window tick-list] 
  (let [start-index tick-window 
        {input-key :input output-key :output etal-keys :etal 
         :or 
         {input-key :last-trade-price 
          output-key :last-trade-price-average 
          etal-keys [:last-trade-price :last-trade-time]}} options] 
    (map 
      (fn [ech] 
        ;(prn (:price (first ech)))
        (let [tsum (reduce 
                     (fn [rr ee] (let [ltprice (:price ee)]
                                   (+ ltprice rr))) 0 ech)
              taverage (/ tsum (count ech))]          
          (merge (zipmap etal-keys (map #((% ticks-to-sma-key-map) (last ech)) etal-keys)) 
                 {output-key taverage :population ech}))) 
      (partition tick-window 1 tick-list))))

#_(defn simple-moving-average [options tick-window tick-list]  
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
                        (let [ltprice (:price ee)]
                          (+ ltprice rr))
                        #_(let [ltprice (:last (:price ee))]
                           (+ ltprice rr))) 0 ech) 
               taverage (/ tsum (count ech))]  ; rslt=the average
           ;(prn (last ech))
           (lazy-cat rslt [(merge {:last-trade-entry (last ech)} {output-key taverage :population ech})]))) ;first rslt = '() 
       '() 
       (take (* 2 tick-window) (partition tick-window 1 tick-list)))))

#_(take 2 (simple-moving-average {} 20 timeseries))
#_(map :last (take 40 (generate-prices 5 15)))

#_(defn extract-price-only [pricelist] (map :last pricelist))

;why is this printing (())) !!!!!
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
                etal-keys [:last-trade-price :last-trade-time]}} options] 
      ;; 2. get the simple-moving-average for a given tick - 1
      ;(prn (first sma-list))      
      (reductions (fn [rslt ech] ;; 3. calculate the EMA (for the first tick, EMA( yesterday) = MA( yesterday) )                
                                           (let [;; price( today) 
                                                 ;;ltprice (:last (:price ech))
                                                 ;ltprice (input-key ech)
                                                 ;ltprice (:last (:price (:last-trade-entry ech))) 
                                                 ltprice (:last-trade-price ech) 
                                                 ;; EMA( yesterday) 
                                                 ema-last (if (output-key (last rslt)) (output-key (last rslt)) (:last-trade-price ech)) 
                                                 ;; ** EMA now = price( today) * k + EMA( yesterday) * (1 - k) 
                                                 ema-now (+ (* k ltprice) (* ema-last (- 1 k)))]
                                             (prn rslt)
                                             #_(lazy-cat rslt [(merge                                                
                                                               (zipmap 
                                                                 etal-keys 
                                                                 (map #(% (last (:population ech))) etal-keys)) 
                                                               {output-key ema-now})]))) 
                                     '()
                                     sma-list)
      #_(last ))))

#_(take 2 (exponential-moving-average {} 5 timeseries))

(defn bollinger-band ([tick-window tick-list] 
                       (bollinger-band tick-window tick-list 
                                       (simple-moving-average nil tick-window tick-list))) 
  ([tick-window tick-list sma-list] ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)
                                    (map (fn [ech] (let [;; get the Moving Average 
                                                         ma (:last-trade-price-average ech) ;; work out the mean 
                                                         mean (/ (reduce (fn [rlt ech] (+ (:last-trade-price ech) rlt)) 
                                                                         0 (:population ech)) 
                                                                 (count (:population ech))) 
                                                         ;; Then for each number: subtract the mean and square the result (the squared difference) 
                                                         sq-diff-list (map (fn [ech] 
                                                                             (let [diff (- mean (:last-trade-price ech))] 
                                                                               (* diff diff))) (:population ech)) 
                                                         variance (/ (reduce + sq-diff-list) 
                                                                     (count (:population ech))) 
                                                         standard-deviation (. Math sqrt variance)] 
                                                     {:last-trade-price (:last-trade-price ech) 
                                                      :last-trade-time (:time ech) 
                                                      :upper-band (+ ma (* 2 standard-deviation)) 
                                                      :lower-band (- ma (* 2 standard-deviation))})) sma-list)))

#_(defn bollinger-band 
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
                        (fn [rslt ech] (+ (:price ech) rslt)) 
                        0 (:population ech)) (count (:population ech))) 
              ;; Then for each number: subtract the mean and square the result (the squared difference)
              sq-diff-list (map
                             (fn [ech] (let [diff (- mean (:price ech))] (* diff diff))) (:population ech)) 
              variance (/ (reduce + sq-diff-list) (count (:population ech))) 
              standard-deviation (. Math sqrt variance)] 
          (lazy-cat rslt 
                    [{:last-trade-entry (:last-trade-entry ech)                     
                      :upper-band (+ ma (* 2 standard-deviation)) 
                      :lower-band (- ma (* 2 standard-deviation))}]))) 
      '()
      sma-list)))



(defn polynomial [a b c x] 
  (-> (+ (* a (Math/pow x 3)) (* b (Math/pow x 2))) (- (* c x)))) 

(defn sine [a b d x] 
  (- (* a (Math/sin (* b (- x (/ Math/PI 2))))) d))

;(map polynomial '( 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
;(map polynomial '(-5 -4 -3 -2 -1 0 1 2 3 4 5))

(defn polynomial-xintercept [x] 
  (polynomial 2 2 3 x)) 

(defn sine-xintercept [x]
  (sine 2 2 0 x)) 

(defn ydirection [ypos] 
  (if (pos? ypos) :positive :negative))

(defn direction-changed? [ypos dirn] 
  (not (= (ydirection ypos) dirn))) 

(defn get-opposite-direction-key [ydir] 
  (if (= ydir :positive) :negative :positive)) 

(defn get-opposite-direction-fn [dirn] 
  (if (= dirn +) - +))

(defn effectively-zero? [xval] 
  (= 0.0 (Double. (format "%.7f" xval)))) 

(defn find-xintercept [direction mfn] 
  (loop [start-point 0.0 
         distance 1.0 
         ydir (ydirection (mfn (direction 0 0.1))) ; :positive or :negative of y of poly or sine 
         dirn direction] 
    (let [next-point (dirn start-point distance)] 
      (if (effectively-zero? (mfn next-point)) 
        next-point 
        (let [dc? (direction-changed? (mfn next-point) ydir)] 
          (recur next-point 
                 (if dc? (/ distance 2) distance) 
                 (if dc? 
                   (get-opposite-direction-key ydir) 
                   ydir) 
                 (if dc? (get-opposite-direction-fn dirn) dirn)))))))

(defn randomize-vertical-dilation 
  [mathfn min max] 
  (let [a (rand-double-in-range min max)] 
    (partial mathfn a))) 

(defn randomize-horizontal-dilation 
  [mathfn-curried min max] 
  (let [b (rand-double-in-range min max)] 
    (partial mathfn-curried b)))

(defn generate-polynomial-sequence [] 
  (let [one (randomize-vertical-dilation polynomial 0.5 2) 
        two (randomize-horizontal-dilation one 0.5 2) 
        polyn-partial (partial two 3) 
        xinterc-polyn-left (find-xintercept - polynomial-xintercept) 
        xinterc-polyn-right (find-xintercept + polynomial-xintercept) 
        granularityP (rand-double-in-range 0.1 1) 
        xsequenceP (iterate (partial + granularityP) xinterc-polyn-left)]
    ;(prn xinterc-polyn-left)
    (map polyn-partial xsequenceP)))

(defn generate-sine-sequence []
  (let [ein (randomize-vertical-dilation sine 0.5 2.7) 
        zwei (randomize-horizontal-dilation ein 0.3 2.7) 
        sine-partial (partial zwei 0) 
        xinterc-sine-left (find-xintercept - sine-xintercept) 
        xinterc-sine-right (find-xintercept + sine-xintercept) 
        granularityS (rand-double-in-range 0.1 1) 
        xsequenceS (iterate (partial + granularityS) xinterc-sine-left)]
    ;(prn sine-xintercept)
    (map sine-partial xsequenceS)))

(defn test-beta 
  [beta-distribution] 
  (let [sample-val (.sample beta-distribution)] 
    (cond (< sample-val 0.50) :a :else :b))) 

(def beta-distribution (org.apache.commons.math3.distribution.BetaDistribution. 2.0 4.1)) 
(def result (repeatedly #(test-beta beta-distribution))) 

;(sort (take 100 result))

(defn sample-dispatcher [sample-type sample-length sample-fn] 
  (take sample-length (sample-fn))) 

(defn sample-prices 
  [beta-distribution] 
  (let [sample-val (.sample beta-distribution)] 
    (cond (< sample-val 0.50) (sample-dispatcher :sine (rand-double-in-range 10 15) generate-sine-sequence) 
          :else (sample-dispatcher :polynomial (rand-double-in-range 4 6) generate-polynomial-sequence))))

; Why do we need to normalize between different samples? So what if they have different y starting points?:
;user=> (sample-prices beta-distribution)
;(-1.6472123564659837 -1.760281907047639 -1.861212363316189 -1.9493076979348756 -2.0239603959532806 -2.0846556442960975 -2.1309748819697494 -2.1625986865042433 -2.179308976725055 -2.18099051666448 -2.1676317102413276 -2.1393246812287443 -2.0962646379587206 -2.0387485271443313 -1.9671729861030993)
;user=> (sample-prices beta-distribution)
;(-2.351066471596478 -2.582827616986438 -2.4952513344475213 -2.099165440993 -1.4435414783637253 -0.6094399356676975 0.3000119634568149 1.1723707758696509 1.8997791991496604 2.3923014073763107 2.5890425990001056 2.465677950448765)
; why should code take -2.351066471596478 and add abs(-2.351066471596478 - -1.9671729861030993) ??? then we have dupe don't we? 
;(-1.6472123564659837 -1.760281907047639 -1.861212363316189 -1.9493076979348756 -2.0239603959532806 -2.0846556442960975 -2.1309748819697494 -2.1625986865042433 -2.179308976725055 -2.18099051666448 -2.1676317102413276 -2.1393246812287443 -2.0962646379587206 -2.0387485271443313 
; -1.9671729861030993 -1.9671729861030993) ***** 

; BAD FUNCTION!!!
(defn generate-prices-bad [beta-distribution] 
  (reduce (fn [^clojure.lang.LazySeq rslt ^clojure.lang.LazySeq each-sample-seq] 
            (let [beginning-price (if (empty? rslt) (rand-double-in-range 5 15) (last rslt))
                  sample-seq-head (first each-sample-seq) 
                  price-difference (Math/abs (- sample-seq-head beginning-price))] 
              (if (< sample-seq-head beginning-price) 
                (concat rslt (map #(+ % price-difference) each-sample-seq)) 
                (concat rslt (map #(- % price-difference) each-sample-seq) each-sample-seq)))) 
          '() 
          (repeatedly #(sample-prices beta-distribution))))
                  
; BETTER FUNCTION OF ABOVE
(defn generate-prices-iterate [beta-distribution] 
  (let [sample-seq (repeatedly #(sample-prices beta-distribution)) 
        iterfn 
        (fn [[^clojure.lang.LazySeq rslt ^clojure.lang.LazySeq remaining-sample-seq]] 
          (let [each-sample-seq (first remaining-sample-seq) beginning-price (if (empty? rslt)
                                                                               (rand-double-in-range 5 15) 
                                                                               (last rslt)) 
                sample-seq-head (first each-sample-seq) price-difference (Math/abs 
                                                                           (- sample-seq-head beginning-price))] 
            ;; only raise the price if below the beginning price 
            (if (< sample-seq-head beginning-price) [(concat rslt (map #(+ % price-difference) each-sample-seq))                                                     
                                                     (rest remaining-sample-seq)] 
              [(concat rslt (map #(- % price-difference) each-sample-seq)) 
               (rest remaining-sample-seq)])))] (map first (iterate iterfn ['() sample-seq]))))

; BEST
(defn generate-prices-reductions 
  [beta-distribution] 
  (reductions (fn [^clojure.lang.LazySeq rslt ^clojure.lang.LazySeq each-sample-seq] 
                (let [beginning-price (if (empty? rslt) (rand-double-in-range 5 15)
                                        (last rslt)) 
                      sample-seq-head (first each-sample-seq) 
                      price-difference (Math/abs (- sample-seq-head beginning-price))] 
                  ;; only raise the price if below the beginning price 
                  (if (< sample-seq-head beginning-price) 
                    (concat rslt (map #( + % price-difference) each-sample-seq)) 
                    (concat rslt (map #(- % price-difference) each-sample-seq) each-sample-seq))))
              '() 
              (repeatedly #(sample-prices beta-distribution))))
              
(defn generate-prices 
  ([] 
    (generate-prices (BetaDistribution. 2.0 4.1))) 
  ([beta-distribution] 
    (map (fn [x] (if (neg? x) (* -1 x) x)) (distinct (apply concat (generate-prices-reductions beta-distribution))))))

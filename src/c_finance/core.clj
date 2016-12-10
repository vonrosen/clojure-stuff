(ns c-finance.core
  (:require [clj-time.core :as tc] 
            [clj-time.periodic :as tp] 
            [clj-time.coerce :as tco]))

#_(require '[clj-time.core :as tc] 
            '[clj-time.periodic :as tp] 
            '[clj-time.coerce :as tco])

#_(defn generate-prices [lower-bound upper-bound] 
   (filter (fn [x] (>= x lower-bound)) (repeatedly (fn [] (rand upper-bound)))))

#_(def pricelist (generate-prices 1 10))

#_(take 25 (map (fn [x] {:price x}) pricelist))

#_(take 25 
      (map (fn [x y] [x y]) (map (fn [x] {:time x}) (iterate inc 0)) 
           (map (fn [x] {:price x}) pricelist)))

#_(defn generate-timeseries [pricelist] (map (fn [x y] {:time x :price y}) (iterate inc 0) pricelist))



#_(defn random-in-range [lower upper] 
   (let [r (rand upper)] 
     (if (>= r lower) (Double/parseDouble (format "%.2f" r)) 
       (Double/parseDouble (format "%.2f" (+ (rand (- upper lower)) lower))))))

(defn random-in-range [lower upper] 
  (let [r (rand upper)] 
    (if (>= r lower) r 
      (+ (rand (- upper lower)) lower))))

(defn stochastic-k [last-price low-price high-price]
  ;(prn (str high-price " ** " low-price))
  (let [hlrange (- high-price low-price) 
        hlmidpoint (/ hlrange 2) 
        numerator (if (> last-price hlmidpoint) 
                    (- last-price hlmidpoint) 
                    (- hlmidpoint low-price))] 
    (/ numerator hlrange)))

(defn break-local-minima-maxima [k] 
  (as-> k k 
    (if (<= (int (+ 0.95 k)) 0) 
      (+ 0.15 k) 
      k) 
    (if (>= k 1)
      (- k 0.15) 
      k)))

(defn generate-prices 
  ([low high] 
    (generate-prices (random-in-range low high)))
  ([last-price] 
    (iterate (fn [{:keys [last]}]
               ;(prn last)
               (let [low (- last 5) 
                     high (+ last 5) 
                     k (stochastic-k last low high) 
                     plus-OR-minus (rand-nth [- +]) 
                     kPM (if (= plus-OR-minus +) (+ 1 (break-local-minima-maxima k)) (- 1 (break-local-minima-maxima k))) 
                     newprice (* kPM last) 
                     newlow (if (< newprice low) 
                              newprice 
                              low) 
                     newhigh (if (> newprice high) 
                               newprice 
                               high)]
                 ;(prn (str "k=" k))                 
                 {:last newprice})) {:last last-price})))

(defn generate-timeseries 
  ([pricelist] 
    (generate-timeseries pricelist (tc/now))) 
  ([pricelist datetime]
    (->> (map (fn [x y] [x y]) 
              (map (fn [x] {:time x}) 
                   (iterate #(tc/plus % (tc/seconds (rand 4))) datetime)) 
              (map (fn [x] {:price x}) pricelist)) 
      (map (fn [x] (merge (first x) (second x)))))))

#_(map :last (take 40 (generate-prices 5 15)))
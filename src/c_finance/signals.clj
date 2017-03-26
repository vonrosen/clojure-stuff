(ns c-finance.signals
  (:require 
    [c-finance.core2 :as core]))

;(def price-list (generate-prices-2))
;(def time-series (generate-timeseries-2 price-list))

(defn join-averages 
  ([tick-window tick-list] 
    (let [sma-list (core/simple-moving-average nil tick-window tick-list) 
          ema-list (core/exponential-moving-average nil tick-window tick-list sma-list)] 
      (join-averages tick-list sma-list ema-list))) 
  ([tick-list sma-list ema-list]
    ;(prn ema-list)
    (let [trimmed-ticks (drop-while #(not (= (:time %) (:time (first sma-list)))) tick-list)] 
      (map (fn [titem sitem eitem] (if (and (and (not (nil? (:time sitem))) 
                                                 (not (nil? (:time eitem)))) 
                                            (= (:time titem) (:time sitem) 
                                               (:time eitem)))
                                     {:time (:time titem) 
                                      :last-trade-price (if (string? (:last-trade-price titem)) 
                                                          (read-string (:last-trade-price titem)) (:last-trade-price titem)) 
                                      :last-trade-price-average (:last-trade-price-average sitem) 
                                      :last-trade-price-exponential (:last-trade-price-exponential eitem)} nil)) 
           trimmed-ticks
           sma-list ema-list))))


(defn moving-averages-signals 
  "Takes baseline time series, along with 2 other moving averages. Produces a list of signals where the 2nd moving average overlaps (abouve or below) the first. By default, this function will produce a Simple Moving Average and an Exponential Moving Average." 
  ([tick-window tick-list] (let [sma-list (core/simple-moving-average nil tick-window tick-list) 
                                 ema-list (core/exponential-moving-average nil tick-window tick-list sma-list)] 
                             (moving-averages-signals tick-list sma-list ema-list))) 
  ([tick-list sma-list ema-list] ;; create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid 
                                 (let [joined-list (join-averages tick-list sma-list ema-list) 
                                       partitioned-join (partition 2 1 (remove nil? joined-list))] ;; find time points where ema-list (or second list) crosses over the sma-list (or 1st list) 
                                   (map (fn [[fst snd]] 
                                          (let [ ;; in the first element, has the ema crossed abouve the sma from the second element 
                                                signal-up 
                                                (and (< (:last-trade-price-exponential snd) (:last-trade-price-average snd))
                                                     (> (:last-trade-price-exponential fst) (:last-trade-price-average fst))) 
                                                ;; in the first element, has the ema crossed below the sma from the second element 
                                                signal-down 
                                                (and (> (:last-trade-price-exponential snd) 
                                                        (:last-trade-price-average snd)) 
                                                     (< (:last-trade-price-exponential fst) (:last-trade-price-average fst))) raw-data fst] ;; return either i) :up signal, ii) :down signal or iii) nothing, with just the raw data 
                                            (if signal-up 
                                              (assoc raw-data :signals 
                                                     [{:signal :up :why :moving-average-crossover :arguments [fst snd]}]) 
                                              (if signal-down 
                                                (assoc raw-data :signals 
                                                       [{:signal :down :why :moving-average-crossover :arguments [fst snd]}]) raw-data)))) 
                                        partitioned-join))))
(ns c-finance.stream
  (:require [c-finance.core2 :as core]
            [clojure.core.async :as async :refer [go go-loop chan close! <! >!]]
            [clojure.java.io :as io]))

(def price-list (core/generate-prices))
(def time-series (core/generate-timeseries-2 price-list))
(def prices (take 320 time-series))
(def remaining (drop 320 time-series))
(def sma (core/simple-moving-average {} 20 prices)) 
(def ema (core/exponential-moving-average {} 20 prices sma)) 
(def bol (core/bollinger-band 20 prices sma))

(defn generate-file-name [fname] (str "data/" fname)) 

(defn write-data [fname data] 
  (let [full-path (generate-file-name fname)] 
    (io/make-parents full-path) 
    (spit full-path (list (apply pr-str data)) :encoding "UTF-8"))) 

;(write-data "datafile.edn" '(: one :two :three))
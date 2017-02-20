(ns c-finance.component
  (:require [c-finance.core2 :as core]
            [c-finance.stream :as stream]
            [com.stuartsierra.component :as component]
            [clj-time.format :as fmt]
            [clojure.core.async :as async :refer [go go-loop chan close! <! >! <!!]]
            [clojure.java.io :as io])
  (:import [java.text.SimpleDateFormat]))

#_(defrecord Timeseries [] component/Lifecycle 
   (start [component] 
     (let [c (chan)] 
       (assoc component :channel c))) 
   (stop [component] 
     (let [c (:channel component)] 
       (close! c) 
       (dissoc component :channel)))) 

#_(defrecord Consumer [timeseries] component/Lifecycle 
   (start [component]
     (assoc component :channel (:channel timeseries))) 
   (stop [component] (dissoc component :channel)))

(defn send-data! [channel time-series] 
  (go-loop [prices (take 320 time-series) 
            remaining (drop 320 time-series)] 
           (let [sma (core/simple-moving-average {} 20 prices) 
                 ema (core/exponential-moving-average {} 20 prices sma) 
                 bol (core/bollinger-band 20 prices sma)] 
             (>! channel {:ticks prices :sma sma :ema ema :bol bol}) 
             (Thread/sleep 1000)) 
           (recur (take 320 remaining) 
                  (drop 320 remaining)))) 

#_(defn receive-data! [channel] 
   (go-loop [data (<! channel)] 
            (println data) 
            (if-not (nil? data) 
              (recur (<! channel)))))

(defn receive-data! [channel]
  (go-loop [data (<! channel)] 
           (if-not (nil? data) 
             (do
               #_(let [{ticks :ticks sma :sma ema :ema bol :bol} data]
                  (prn (last ticks))           
                  (prn (-> ticks last :time .toString))
                  )
               (let [{ticks :ticks sma :sma ema :ema bol :bol} data 
                     timestamp (-> ticks last :time .toString) 
                     generate-file-name-with-timestamp-fn (fn [fname] (str timestamp "-" fname))] 
                 (stream/write-data (generate-file-name-with-timestamp-fn "ticks.edn") ticks) 
                 (stream/write-data (generate-file-name-with-timestamp-fn "sma.edn") sma) 
                 (stream/write-data (generate-file-name-with-timestamp-fn "ema.edn") ema) 
                 (stream/write-data (generate-file-name-with-timestamp-fn "bol.edn") bol)) 
               (recur (<! channel))))))

(defrecord Timeseries [] component/Lifecycle 
  (start [component] (let [c (chan) price-list (core/generate-prices) 
                           time-series (core/generate-timeseries-2 price-list)] 
                       (send-data! c time-series) 
                       (assoc component :channel c)))
  (stop [component] (let [c (:channel component)] 
                      (close! c)
                      (dissoc component :channel)))) 

(defrecord Consumer [timeseries] component/Lifecycle 
  (start [component] (let [channel (:channel timeseries)] 
                       (receive-data! channel) 
                       (assoc component :channel channel)))
  (stop [component] (dissoc component :channel)))

(defn new-timeseries [] 
  (map->Timeseries {}))

(defn new-consumer [] 
  (map->Consumer {})) 

(defn build-system [] (component/system-map 
                        :tms (new-timeseries) 
                        :cns (component/using (new-consumer) 
                                         {:timeseries :tms})))

(def system (build-system))

;(alter-var-root #'system component/start)
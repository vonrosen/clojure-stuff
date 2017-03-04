(ns c-finance.files
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def many-files (file-seq (io/file "data/")))

(def second-file (slurp (second many-files)))

(def second-edn (edn/read-string second-file))

(defn load-directory [fname] (filter #(.isFile %) (file-seq (io/file fname))))

(defn lookup [& constraints] ;; ensure constraints are in pairs -> Preconditions 
  {:pre [( even? (count constraints))]}
  (let [files (if (some #{:source} constraints) 
                (let [source-source (comp (partial filter #(= :source (first %1))) 
                                          (partial partition 2)) 
                      source-value (comp second source-source) 
                      source-key (comp first source-source)]                  
                  (if (string? source-key)
                    (load-directory (source-key constraints)) source-value)) 
                (load-directory "data/")) 
        constraint-pairs (->> constraints (partition 2) (remove #( = :source (first %)))) 
        find-lookup-fn (fn [inp] (case inp 
                                   :time specific-time-pred 
                                   :time-after time-after-pred 
                                   :time-before time-before-pred 
                                   :price specific-price-pred 
                                   :price-abouve price-abouve-pred 
                                   :price-below price-below-pred)) 
        constraint-pairs-A (map (fn [x] [( find-lookup-fn (first x)) (second x)]) constraint-pairs)
        lookupfn-fns-A (map (fn [x] (fn [y] (lookupfn y (( first x) (second x))))) constraint-pairs-A)] 
    ;; apply all fns with args 
    (apply concat ((apply juxt lookupfn-fns-A) files))))

(defn lookupfn [flist pred-fn] 
  (flatten (map (fn [x] (let [read-fn (comp edn/read-string slurp) 
                              inp-edn (read-fn x)]
                          (filter pred-fn inp-edn))) flist)))

(defn specific-time-pred [inst] ;; -> functions returning functions 
  #(= inst (:time %))) 

(defn time-after-pred [time] 
  #(.after (:time %) time)) 

(defn time-before-pred [time] #(.before (:time %) time))

(defn time-range-pred [lower upper] 
  #(and (.after (:time %) lower) 
        (.before (:time %) upper))) 

(defn specific-price-pred [price] #(= price (:price %))) 

(defn price-abouve-pred [price] #(> (:price %) price))

(defn price-below-pred [price] 
  #(< (:price %) price)) 

(defn price-range-pred [lower upper] 
  #(and (> (:price %) lower) 
        (< (:price %) upper)))

(def files (filter #(.isFile %) many-files))

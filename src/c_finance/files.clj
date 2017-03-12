(ns c-finance.files
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def many-files (file-seq (io/file "data/")))

(def second-file (slurp (second many-files)))

(def second-edn (edn/read-string second-file))

(defn load-directory [fname] (filter #(.isFile %) (file-seq (io/file fname))))

(def files (filter #(.isFile %) many-files))

;(lookup :time-after #inst "2015-08-15T17:18:00.000-00:00" :time-before #inst "2015-08-15T17:19:00.000-00:00")

(defn lookupfn [flist pred-fn] 
  (flatten (map (fn [x]                                    
                  (let [read-fn (comp edn/read-string slurp)
                        ;- (prn (read-fn x))
                        inp-edn (read-fn x)]                    
                    (filter pred-fn inp-edn))) flist)))

(defn specific-time-pred [inst] ;; -> functions returning functions 
  #(if (not (nil? (:time %))) (= inst (:time %)) false))

(defn time-after-pred [time]  
  #(if (not (nil? (:time %))) (.after (:time %) time) false)) 

(defn time-before-pred [time] #(if (not (nil? (:time %))) (.before (:time %) time) false))

(defn time-range-pred [lower upper] 
  #(if (not (nil? (:time %))) (and (.after (:time %) lower) 
        (.before (:time %) upper)) false)) 

(defn specific-price-pred [price] #(if (not (nil? (:time %))) (= price (:price %)) false)) 

(defn price-abouve-pred [price] #(if (not (nil? (:time %))) (> (:price %) price) false))

(defn price-below-pred [price] 
  #(if (not (nil? (:time %))) (< (:price %) price) false)) 

(defn price-range-pred [lower upper] 
  #(if (not (nil? (:time %))) (and (> (:price %) lower) 
        (< (:price %) upper)) false))

(defn lookup [& constraints] ;; ensure constraints are in pairs -> Preconditions 
  {:pre [( even? (count constraints))]}
  ;(prn (partition 2 constraints))
  (let [files (if (some #{:source} constraints) 
                (let [source-source (comp (partial filter #(= :source (first %1))) 
                                          (partial partition 2)) 
                      source-value (comp second source-source) 
                      source-key (comp first source-source)]                
                  (if (string? source-key)
                    (load-directory (source-key constraints)) source-value)) 
                (load-directory "data/")) 
        constraint-pairs (->> constraints (partition 2) (remove #(= :source (first %)))) 
        find-lookup-fn (fn [inp] (case inp 
                                   :time specific-time-pred 
                                   :time-after time-after-pred 
                                   :time-before time-before-pred 
                                   :price specific-price-pred 
                                   :price-abouve price-abouve-pred 
                                   :price-below price-below-pred)) 
        constraint-pairs-A (map (fn [x] [(find-lookup-fn (first x)) (second x)]) constraint-pairs)
        lookupfn-fns-A (map
                         ;x=[pred-func timestamp-to-pass-to-pred-func]
                         (fn [x] (fn [y]                                      
                                   (lookupfn y ((first x) (second x))))) 
                         constraint-pairs-A)] ; [pred-func timestamp-to-pass-to-pred-func]
    #_(defn lookupfn [flist pred-fn]
        (flatten (map (fn [x] (let [read-fn (comp edn/read-string slurp) 
                                    inp-edn (read-fn x)]
                                (filter pred-fn inp-edn))) flist)))
    ;; apply all fns with args
    ;(prn constraint-pairs-A)
    
    
    ;(prn ((apply juxt lookupfn-fns-A) files))
    (apply concat ((apply juxt lookupfn-fns-A) files))))

(defn generate-input-list [constraints]
  (if (some #{:source} constraints) 
    (let [source-source (comp (partial filter #(= :source (first %1))) (partial partition 2)) 
          source-value (comp second source-source) 
          source-key (comp first source-source)] 
      (if (string? source-key) 
        (load-directory (source-key constraints)) 
        source-value)) 
    (load-directory "data/"))) 

(defn generate-constraint-pairs [constraints] 
  (->> constraints (partition 2) 
    (remove #(= :source (first %)))))

(defn find-lookup-fn [inp] 
  (case inp 
    :time specific-time-pred 
    :time-after time-after-pred 
    :time-before time-before-pred 
    :price specific-price-pred 
    :price-abouve price-abouve-pred 
    :price-below price-below-pred)) 

;; refactor some code to clean up 
(defn lookup-refactored [& constraints] ;; ensure constraints are in pairs -> Preconditions 
  {:pre [(even? (count constraints))]}
  ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg 
  (let [files (generate-input-list constraints) 
        constraint-pairs (generate-constraint-pairs constraints) 
        constraint-pairs-A (map (fn [x] [(find-lookup-fn (first x)) 
                                         (second x)]) 
                                constraint-pairs) 
        lookupfn-fns-A (map (fn [x] (fn [y] (lookupfn y ((first x) (second x))))) 
                            constraint-pairs-A)] ;; apply all fns with args 
    (apply concat ((apply juxt lookupfn-fns-A) files)))) 

(defn lookup-and [& constraints] ;; ensure constraints are in pairs -> Preconditions
  {:pre [(even? (count constraints))]} ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg 
  (let [files (generate-input-list constraints) 
        constraint-pairs (generate-constraint-pairs constraints) 
        constraint-pairs-B (map (fn [x] [(find-lookup-fn (first x)) 
                                         (second x)]) 
                                constraint-pairs)
        constraint-predicates (map (fn [x] ((first x) (second x))) constraint-pairs-B) 
        ;; lookupfn ;; constraint-predicates ;; (f1 f2 ...) ;; files 
        pred-fn (fn [input-tick] 
                  (every? (fn [pred] 
                            (pred input-tick)) 
                          constraint-predicates))]
    (lookupfn files pred-fn)))

#_(defn lookup-combined' [mode & constraints] ;; ensure constraints are in pairs -> Preconditions 
   {:pre [(even? (count constraints))]} ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg 
   (let [files (generate-input-list constraints) 
         constraint-pairs (generate-constraint-pairs constraints) ;; OR 
         constraint-pairs-A (map (fn [x] [(find-lookup-fn (first x)) (second x)]) constraint-pairs) 
         lookupfn-fns-A (map (fn [x] (fn [y] (lookupfn y ((first x) (second x))))) constraint-pairs-A)
         constraint-pairs-B (map (fn [x] [(find-lookup-fn (first x)) (second x)]) constraint-pairs) 
         constraint-predicates (map (fn [x] ((first x) (second x))) constraint-pairs-B) 
         pred-fn (fn [x] (every? 
                           (fn [pfn] (pfn x)) constraint-predicates))] ;; OR 
     (apply concat ((apply juxt lookupfn-fns-A) files)) ;; AND 
     (lookupfn files pred-fn)))

(defn apply-juxt-helper [lookupfn-fns]  
  (apply concat ((apply juxt lookupfn-fns) files))) 

(defn choose-constraint [mode files constraint-pairs]
  (if (= :or mode)
    (do
      ;(prn (quote ~constraint-pairs))      
      (->> (map (fn [x] [(find-lookup-fn (first x)) (second x)]) constraint-pairs) 
        (map (fn [x] (fn [y] (lookupfn y ((first x) (second x))))))        
        (apply-juxt-helper))) 
    (->> (map (fn [x] [(find-lookup-fn (first x)) (second x)]) constraint-pairs) 
      (map (fn [x] ((first x) (second x)))) 
      (fn [x] (fn [y] (every? (fn [pfn] (pfn y)) x))) 
      (lookupfn files))))

#_(defn choose-constraint [mode files constraint-pairs]
  (if (= :or mode)
    (do
      ;(prn (quote ~constraint-pairs))      
      (->> #_(quote ~constraint-pairs)
        (map (fn [x#] [(find-lookup-fn (first x#)) (second x#)]) `~constraint-pairs) 
        (map (fn [x#] (fn [y#] (lookupfn y# ((first x#) (second x#))))))        
        (apply-juxt-helper))) 
    (->> #_(quote ~constraint-pairs)
      (map (fn [x#] [(find-lookup-fn (first x#)) (second x#)]) `~constraint-pairs) 
      (map (fn [x#] ((first x#) (second x#)))) 
      (fn [x#] (fn [y#] (every? (fn [pfn#] (pfn# y#)) x#))) 
      (lookupfn files)))) 

(defmacro lookup-combined [mode & constraints]
 {:pre [(even? (count constraints))]}
 (let [files (generate-input-list constraints)  
       constraint-pairs (generate-constraint-pairs constraints)]    
   (choose-constraint mode files constraint-pairs)))

(defn lookup [query-params] ;; ensure constraints are in pairs -> Preconditions 
  {:pre [(even? (count (rest query-params)))]} ;; map over pairs - find predicate fn based on keyword - partially apply fn with arg
  (let [mode (first query-params) 
        constraints (rest query-params) 
        files (generate-input-list constraints) 
        constraint-pairs (generate-constraint-pairs constraints)] 
    (choose-constraint mode files constraint-pairs)))

#_(defn lookup-combined [mode & constraints]
   {:pre [(even? (count constraints))]}
   (let [files (generate-input-list constraints)  
         constraint-pairs (generate-constraint-pairs constraints)]    
     (choose-constraint mode files constraint-pairs)))

;(macroexpand-1 '(lookup-combined :or :time-after #inst "2015-08-15T17:18:00.000-00:00" :price-abouve 20))
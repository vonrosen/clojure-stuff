(ns c-finance.primes)

(defn primes [up-to-number]
  (reduce
    (fn [rslt ech]      
      (if-not (some #(= (mod ech %) 0) rslt)  
           (concat rslt (list ech))
           rslt))
    '()
    (take up-to-number (iterate inc 2))))

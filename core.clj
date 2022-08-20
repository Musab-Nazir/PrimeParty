(ns test.core
  (:require [clojure.core.reducers :as r])
    (:import [java.time Instant Duration]))

(defn sieve
  [^long n]
  (loop [primes (range 2 4)
         candidates (range 5 (inc n) 2)
         p 3]
    (if
     (> (Math/pow p 2) n)
      (concat primes candidates)
      (let [remaining (remove #(= 0 (rem % p)) candidates)
            next-p (take 1 remaining)]
        (recur
         (concat primes next-p)
         (drop 1 remaining)
         (first next-p))))))

(last (sieve 1000))
(time (last (sieve 1000000))) ; .48s approx

;; taken from: https://clojureverse.org/t/eratosthenes-party-time-a-k-a-feedback-wanted-on-this-implementation-of-eratosthenes-sieve/3801
(defn primes-below [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (if (<= i n)
                  (do
                    (aset primes i false)
                    (recur (+ i p p))))))
            (recur  (+ p 2))))))))

(time (last (primes-below 1000000))) ;9ms how?!

;; The Sieve of Eratosthenes naive implementation
(defn sieve-reduced
  [n]
  (let [numbers (conj (range 3 (inc n) 2) 2)
        stuff (r/reduce
               (fn
                 [val num]
                 (into []
                       (r/remove #(when (not= % num)
                                    (zero? (mod % num)))
                                 val)))
               numbers
               numbers)]
    stuff))

(time (last (sieve-reduced 1000000))) ; 906s approx yikes

(defn sieve-ba
  "Java boolean array storage
   Returns the raw sieve with only odd numbers present."
  [^long n]
  (if (< n 2)
    (boolean-array 0)
    (let [sqrt-n (unchecked-long (Math/ceil (Math/sqrt (double n))))
          half-n (unchecked-int (bit-shift-right n 1))
          primes (boolean-array half-n)]
      (loop [p 3]
        (when (< p sqrt-n)
          (when-not (aget primes (bit-shift-right p 1))
            (loop [i (long (bit-shift-right (* p p) 1))]
              (when (< i half-n)
                (aset primes i true)
                (recur (+ i p)))))
          (recur (+ p 2))))
      primes)))

(defn benchmark
  "Benchmark Sieve of Eratosthenes algorithm."
  [sieve]
  (let [limit       1000000
        start-time  (Instant/now)
        end-by      (+ (.toEpochMilli start-time) 5000)]
    (loop [pass 1]
      (let [primes   (sieve limit)
            cur-time (System/currentTimeMillis)]
        (if (<= cur-time end-by)
          (recur (inc pass))
          ;; Return benchmark report.
          {:primes primes
           :passes pass
           :limit  limit
           :time   (Duration/between start-time (Instant/now))})))))

(benchmark primes-below)
(benchmark sieve-ba)

(defn sieve [n]
  (let [primes (array/new-filled (+ 1 n) true)
        sqrt-n (math/ceil (math/sqrt n))]
    # remove all even numbers
    (loop [j :range [0 (+ n 1)] :when (even? j)] (put primes j false))
    (var p 3)
    (if (< n 2)
      @[] 
      (do 
        (while (<= p sqrt-n)
          (when (get primes p)
            (var i (* p p))
              (while (<= i n)
                  (put primes i false)
                  (set i (+ i p p))))
          (set p (+ p 2)))
      primes))))

(defn get-prime-list 
  [n]
(array/concat @[2] 
              (filter (fn [x] 
                        (get (sieve n) x)) 
                (range 3 (inc n) 2))))

(sieve 1000000)

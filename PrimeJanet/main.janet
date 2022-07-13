(def limit 1000000)

(defn sieve [n]
  (let [primes (array/new-filled n true)
        sqrt-n (math/ceil (math/sqrt n))]
    # remove all even numbers
    (loop [j :range [0 n] :when (even? j)] (put primes j false))
    # 3 will be the first candidate we test
    (var p 3)
    (if (< n 2)
      @[] 
      (do (while (<= p sqrt-n)
            (when (get primes p)
              (var i (* p p))
                (while (<= i n)
                  (put primes i false)
                  (set i (+ i p p))))
            (set p (+ p 2)))
        # return the primes array as numbers
          (array/concat @[2] 
              (filter (fn [x] 
                        (get primes x)) 
                (range 3 (inc n) 2)))))))



# Just running this is ~170ms which is an order of magnitude slower than the clojure 
(sieve limit)

# just this alone takes 5ms which seems pretty slow not gonna lie:
# (array/new-filled (+ 1 limit) true)
# update on this: apparently just a 'hello world' program takes 5ms so maybe the array
# allocation is not that slow after all. The janet VM spinning up is probably the
# main hold up here

# This takes 30ms
# (range 3 (inc limit) 2)

# concat adds another 10ms to it : 41ms
# (array/concat @[2] (range 3 (inc limit) 2))


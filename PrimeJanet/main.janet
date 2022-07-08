(defn sieve [n]
  (let [primes (array/new-filled (+ 1 n) true)
        sqrt-n (math/ceil (math/sqrt n))]
    # remove all even numbers
    (loop [j :range [0 (+ n 1)] :when (even? j)] (put primes j false))
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
        primes))))

(defn get-prime-list 
  [n]
(array/concat @[2] 
              (filter (fn [x] 
                        (get (sieve n) x)) 
                (range 3 (inc n) 2))))

(def limit 1000000)

# Just running this is ~130ms which is an order of magnitude slower than the clojure 
# version and this is doing less than that implementation!
(sieve limit) 

# this takes 4.3s at 10000 and hangs forever basically at a mil
# Is filter doing some weird stuff? Am I just dumb? Maybe both?
# (get-prime-list 10000) 

# just this alone takes 5ms which seems pretty slow not gonna lie
#(array/new-filled (+ 1 limit) true)

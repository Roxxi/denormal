(ns denormal.core
  (:use
   roxxi.utils.core
   ;roxxi.utils.map
    ;    roxxi.utils.print
        jsonschema.type-system.extract))


;; TODO make one for JSON
(def json-predicator (clojure-predicator))

(defn make-scalar? [pred]
  (fn [val]
    (or (null? pred val)
        (bool? pred val)
        (num? pred val)
        (str? pred val))))

(defn make-array? [pred]
  (fn [val]
    (collection? pred val)))

(defn make-map? [pred]
  (fn [val]
    (document? pred val)))

(def scalar? (make-scalar? json-predicator))
(def array? (make-array? json-predicator))
(def map? (make-map? json-predicator))

;; foo? => fn(keyval => keyval) if (foo? (val keyval))
;; convenient for filtering key-value pairs from a map
(defn make-foo-value? [foo?]
  (fn [keyval]
    (and (foo? (val keyval)) keyval)))

(def scalar-value? (make-foo-value? scalar?))
(def array-value? (make-foo-value? array?))
(def map-value? (make-foo-value? map?))
      
(defn filter-foos [foo-value?]
  (fn [json]
    (extract-map (filter foo-value? json)
                 :key-extractor key
                 :value-extractor val)))

(def filter-scalars (filter-foos scalar-value?))
(def filter-maps (filter-foos map-value?))
(def filter-arrays (filter-foos array-value?))
  
;; # Flattening Nested documents / maps

(def subproperty-connector "_dot_")
(defn make-subprop-iden [prefix suffix]
  (keyword (str (name prefix) subproperty-connector (name suffix))))

(defn hoist-maps [demapped-json map-mappings]
  (loop [map-mappings map-mappings
         map-hoisted-json demapped-json]
      (if (empty? map-mappings)
        map-hoisted-json
        ;; for the prior key that was pointing to this map, prefix
        ;; each of its inner keys with the higher level key
        (recur
         (rest map-mappings)
         (let [key=>map (first map-mappings)
               [base-key-string sub-map] [(key key=>map) (val key=>map)]
               add-hoisted-key-key-val
               (fn add-hoisted-key-key-val [outer-map inner-keyval]
                  (assoc outer-map
                    (make-subprop-iden base-key-string (key inner-keyval))
                   (val inner-keyval)))]
           (reduce add-hoisted-key-key-val map-hoisted-json sub-map))))))


;; # Flattening arrays

(def array-suffix "_arr")
(defn make-arr-iden [iden]
  (keyword (str (name iden) array-suffix)))

(def array-idx-suffix "_idx")
(defn make-arr-idx-iden [iden]
  (keyword (str (name iden) array-idx-suffix)))

(defn cross [& seqs]
  (when seqs
    (if-let [s (first seqs)]
      (if-let [ss (next seqs)]
        (for [x  s
              ys (apply cross ss)]
          (cons x ys))
        (map list s)))))


(defn hoist-arrays [dearrayed-json array-mappings]
  (let [the-keys (map key array-mappings)
        the-arrays (map val array-mappings)
        the-idxes (map (comp range count) the-arrays)
        assoc-vals-n-idxs
        (fn assoc-vals-n-idxs [vals idxs]
          (persistent!
           (loop [json (transient dearrayed-json)
                  keys the-keys
                  vals vals
                  idxs idxs]
             (if-let [key (first keys)]
               (let [val (first vals)
                     idx (first idxs)
                     json-with-val (assoc! json (make-arr-iden key) val)
                     json-with-idx-n-val (assoc! json-with-val (make-arr-idx-iden key) idx)]
                 (recur json-with-idx-n-val
                        (rest the-keys)
                        (rest vals)
                        (rest idxs)))
               json))))]
    (map assoc-vals-n-idxs (apply cross the-arrays) (apply cross the-idxes))))

(def rec {:a "hello" :b 5 :c true :d [1 2 3] :e {:a 1 :b 2} :f {:g 1 :h 2} :f_array {:some-array [1 2 3] :h 2}})
(filter-scalars rec)
(filter-maps rec)
(filter-arrays rec)


(println rec)

(defn every [pred coll]
  (loop [coll (seq coll)]    
    (if (empty? coll)
      true
      (if (pred (first coll))
        (recur (next coll))
        false))))


                                              
;; returns a list of simpified json
(defn hoist-complexity [json]
  (let [scalar-json (filter-scalars json)
        map-hoisted (hoist-maps scalar-json (filter-maps json))
        array-hoisted (hoist-arrays  map-hoisted (filter-arrays json))]
    array-hoisted))

(defn settled? [json]
  (every scalar-value? json))

(defn tabularize-json [json]
  (loop [unsettled-jsons (list json)
         settled-jsons (list)]
    (if (empty? unsettled-jsons)
      settled-jsons
      (let [tabularized-jsons (mapcat hoist-complexity unsettled-jsons)]
        (recur (filter (comp not settled?) tabularized-jsons)
               (concat (filter settled? tabularized-jsons) settled-jsons))))))
            
(def tabularized (tabularize-json rec))



        
        
           
  


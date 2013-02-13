(ns denormal.core
  (:use
   roxxi.utils.print
   roxxi.utils.collections
   jsonschema.type-system.extract))


(def clj-map-predicator (clojure-predicator))

;; TODO defmacro these three things.
(defn make-scalar? [pred]
  (fn [val]
    (or (null? pred val)
        (bool? pred val)
        (num? pred val)
        (str? pred val))))

(defn make-coll? [pred]
  (fn [val]
    (collection? pred val)))

(defn make-map? [pred]
  (fn [val]
    (document? pred val)))

(def scalar? (make-scalar? clj-map-predicator))
(def a-coll? (make-coll? clj-map-predicator))
(def a-map? (make-map? clj-map-predicator))
(defn empty-coll? [foo] (and (a-coll? foo) (empty? foo)))
(defn empty-map? [foo] (and (a-map? foo) (empty? foo)))
(defn an-empty? [foo] (or (empty-coll? foo) (empty-map? foo)))

;; foo? => fn(keyval => keyval) if (foo? (val keyval))
;; convenient for filtering key-value pairs from a map
(defn make-foo-value? [foo?]
  (fn [keyval]
    (and (foo? (val keyval)) keyval)))

(def scalar-value? (make-foo-value? scalar?))
(def coll-value? (make-foo-value? a-coll?))
(def map-value? (make-foo-value? a-map?))
(def empty-value? (make-foo-value? an-empty?))
      
(defn filter-foos [foo-value?]
  (fn [some-map]
    (extract-map (filter foo-value? some-map)
                 :key-extractor key
                 :value-extractor val)))

(def filter-scalars (filter-foos scalar-value?))
(def filter-maps (filter-foos #(and (map-value? %) (not (empty-value? %)))))
(def filter-colls (filter-foos #(and (coll-value? %) (not (empty-value? %)))))
(def filter-empties (filter-foos empty-value?))

(defn hoist-empties [some-map empties]
  (extract-map empties
               :key-extractor key
               :value-extractor (fn [val] nil)
               :initial some-map))
  
;; # Flattening Nested documents / maps
;; TODO Externalize configuration
(def subproperty-connector "_dot_")
(defn make-subprop-iden [prefix suffix]
  (keyword (str (name prefix) subproperty-connector (name suffix))))

(defn hoist-maps [demapped-map map-mappings]
  "demapped-maps are maps with no submappings :) e.g. {:a {:c :d}} is not
   a demapped-map, but {:a_dot_c :d} is now demapped"
  (loop [map-mappings map-mappings
         map-hoisted-map demapped-map]
      (if (empty? map-mappings)
        map-hoisted-map
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
           (reduce add-hoisted-key-key-val map-hoisted-map sub-map))))))



;; # Flattening colls
;; TODO Externalize configuration
(def coll-suffix "_arr")
(defn make-arr-iden [iden]
  (keyword (str (name iden) coll-suffix)))

(def coll-idx-suffix "_idx")
(defn make-arr-idx-iden [iden]
  (keyword (str (name iden) coll-idx-suffix)))



(defn hoist-colls [decolled-map coll-mappings]
  (if (empty? coll-mappings)
    (list decolled-map)
    ;; if any collections are empty, instead of evaluating them, we need
    ;; to replace them with a scalar nil value.
    (let [the-keys (map key coll-mappings)
          the-colls (map val coll-mappings)
          the-idxes (map (comp range count) the-colls)    
          assoc-vals-n-idxs
          (fn assoc-vals-n-idxs [vals idxs]
            (persistent!
             (loop [some-map (transient decolled-map)
                    keys the-keys
                    vals vals
                    idxs idxs]
               (if-let [key (first keys)]
                 (let [val (first vals)
                       idx (first idxs)
                       map-with-val (assoc! some-map (make-arr-iden key) val)
                       map-with-idx-n-val (assoc! map-with-val (make-arr-idx-iden key) idx)]
                   (recur map-with-idx-n-val
                          (rest keys)
                          (rest vals)
                          (rest idxs)))
                 some-map))))]
      (map assoc-vals-n-idxs (apply cross the-colls) (apply cross the-idxes)))))


                                              
;; returns a list of maps simplified by one level
(defn hoist-complexity [some-map]
  (let [scalar-map (filter-scalars some-map)
        empty-hoisted (hoist-empties scalar-map (filter-empties some-map))
        map-hoisted (hoist-maps empty-hoisted (filter-maps some-map))
        coll-hoisted (hoist-colls map-hoisted (filter-colls some-map))]
    coll-hoisted))

(defn denormal? [some-map]
  (every scalar-value? some-map))

(defn denormalize-map [some-map]
  (loop [normal-maps (list some-map)
         denormal-maps (list)]
    (if (empty? normal-maps)
      denormal-maps
      (let [denormalized-maps (mapcat hoist-complexity normal-maps)]
        (recur (filter (comp not denormal?) denormalized-maps)
               (concat (filter denormal? denormalized-maps) denormal-maps))))))
            



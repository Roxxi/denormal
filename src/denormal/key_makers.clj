(ns denormal.key-makers
  (:use
   roxxi.utils.print
   roxxi.utils.collections))


(defprotocol KeyJoiner
  (join-keys  [this property subproperty]
    "Returns a key joining a property name and a subproperty name"))

(deftype SimpleKeyJoiner [path-separator]
  KeyJoiner
  (join-keys [this property subproperty]
    (keyword (str (name property) path-separator (name subproperty)))))

(defn make-simple-key-joiner [path-separator]
  (SimpleKeyJoiner. path-separator))

;; # HoistKeyJoiner
;; When this is used for denoramlization,
;; it will effectivily hoist a deep property
;; all the way to the top.
(deftype HoistKeyJoiner [prefix suffix]
  KeyJoiner
  (join-keys [this property subproperty]
    (keyword (str prefix (name subproperty) suffix))))

(defn make-hoist-joiner
  ([] (make-hoist-joiner "" ""))
  ([prefix suffix]
     (HoistKeyJoiner. prefix suffix)))

(deftype DispatchingKeyJoiner [key*subkey->key-joiner default-key-joiner]
  KeyJoiner
  (join-keys [this property subproperty]
    (let [key-joiner (or (key*subkey->key-joiner property subproperty)
                         default-key-joiner)]
      (join-keys key-joiner property subproperty))))

(defn make-dispatching-key-joiner [fn<-prop*subprop default-key-joiner]
  (DispatchingKeyJoiner. fn<-prop*subprop default-key-joiner))

(defn make-outer-property-dispatch-key-joiner
  [property-name=>key-joiner defalt-key-joiner]
  (make-dispatching-key-joiner
   (fn [prop _] (get property-name=>key-joiner prop))
   defalt-key-joiner))

(defn make-subproperty-dispatch-key-joiner
  [subproperty-name=>key-joiner default-key-joiner]
  (make-dispatching-key-joiner
   (fn [_ subprop] (get subproperty-name=>key-joiner subprop))
   default-key-joiner))

(defprotocol CollectionKeyMaker
  (coll-index [this collection-property]
    "Returns a key indicating that this property represents
the index of an element in a collection")
  (coll-value [this collection-property]
    "Returns a key indicating that this property represents
the value of an element in collection"))

(deftype SimpleCollectionKeyMaker [val-sigil idx-sigil]
  CollectionKeyMaker
  (coll-index [this collection-property]
    (keyword (str (name collection-property) idx-sigil)))
  (coll-value [this collection-property]
    (keyword (str (name collection-property) val-sigil))))

(defn make-simple-coll-key-maker [val-sigil idx-sigil]
  (SimpleCollectionKeyMaker. val-sigil idx-sigil))






(ns denormal.core-test
  (:use clojure.test
        denormal.core))

(defmacro is-denormal
  ([desc some-map denormalized-result]
  `(testing ~desc
     (is (= (denormalize-map ~some-map) ~denormalized-result)))))

(deftest scalar-tests
  (testing "Scalars:"
    (is-denormal "one of each type" {:a "hello" :b 5 :c true :d nil} (list {:a "hello" :b 5 :c true :d nil}))))

(deftest map-tests
  (testing "Nested maps:"
    (is-denormal "An empty nested map" {:empty-map {}} (list {:empty-map nil}))    
    (is-denormal "A single nested map"
                 {:l1 {:a "hello" :b 5 :c true :d nil}}
                 (list {:l1_dot_d nil, :l1_dot_b 5,
                        :l1_dot_c true, :l1_dot_a "hello"}))
    (is-denormal "A single nested map with an empty nested map and an empty nested, nested map"
                 {:l1 {:a "hello" :b 5 :c true :d nil :empty-nested-map {}} :empty-map {}}
                 (list {:l1_dot_d nil, :l1_dot_c true, :l1_dot_b 5,
                        :l1_dot_a "hello", :empty-map nil, :l1_dot_empty-nested-map nil}))
    (is-denormal "A doubley nested map"
                 {:l1 {:l2 {:a "hello" :b 5 :c true :d nil}}}
                 (list {:l1_dot_l2_dot_d nil,
                        :l1_dot_l2_dot_b 5,
                        :l1_dot_l2_dot_c true,
                        :l1_dot_l2_dot_a "hello"}))
    (is-denormal "Two doubley nested maps"
                 {:l1_A {:l2_A {:a "hello" :b 5 :c true :d nil}}
                  :l1_B {:l2_B {:a "hello" :b 5 :c true :d nil}}}
                 (list {:l1_A_dot_l2_A_dot_d nil,
                        :l1_A_dot_l2_A_dot_b 5,
                        :l1_A_dot_l2_A_dot_c true,
                        :l1_A_dot_l2_A_dot_a "hello",
                        :l1_B_dot_l2_B_dot_d nil,
                        :l1_B_dot_l2_B_dot_b 5,
                        :l1_B_dot_l2_B_dot_c true,
                        :l1_B_dot_l2_B_dot_a "hello"}))
    (is-denormal "Mixed level maps with scalars"
                 {:l1_a "1a"
                  :l1_map {:l2_a "2a"
                           :l2_map {:l3_a "3a" 
                                    :l3_map {:l4_a "4a" 
                                             :l4_map {:l5_a "5a"
                                                      :l5_b "5b"}}}}}
                 (list {:l1_map_dot_l2_map_dot_l3_map_dot_l4_map_dot_l5_a "5a",
                        :l1_map_dot_l2_map_dot_l3_map_dot_l4_map_dot_l5_b "5b",
                        :l1_map_dot_l2_map_dot_l3_map_dot_l4_a "4a",
                        :l1_map_dot_l2_map_dot_l3_a "3a",
                        :l1_map_dot_l2_a "2a",
                        :l1_a "1a"}))))

(deftest array-tests
  (testing "Nested Arrays:"
    (is-denormal "Empty array" [] (list {}))
    (is-denormal "Simple array" {:a [1 2 3 4 5]}
                 (list {:a_arr 1, :a_idx 0}
                       {:a_arr 2, :a_idx 1}
                       {:a_arr 3, :a_idx 2}
                       {:a_arr 4, :a_idx 3}
                       {:a_arr 5, :a_idx 4}))
    (is-denormal "Simple array with scalar value"
                 {:scalar 200 :a [1 2 3 4 5]}
                 (list {:scalar 200, :a_arr 1, :a_idx 0}
                       {:scalar 200, :a_arr 2, :a_idx 1}
                       {:scalar 200, :a_arr 3, :a_idx 2}
                       {:scalar 200, :a_arr 4, :a_idx 3}
                       {:scalar 200, :a_arr 5, :a_idx 4}))
    (is-denormal "An Empty array inside a map"
                 {:map {:a 200 :b []}}
                 (list {:map_dot_a 200, :map_dot_b nil}))
    (is-denormal "Array with map with key-clash"
                 {:map {:a 200} :a [1 2 3 4 5]}
                 (list {:map_dot_a 200, :a_arr 1, :a_idx 0}
                       {:map_dot_a 200, :a_arr 2, :a_idx 1}
                       {:map_dot_a 200, :a_arr 3, :a_idx 2}
                       {:map_dot_a 200, :a_arr 4, :a_idx 3}
                       {:map_dot_a 200, :a_arr 5, :a_idx 4}))
    (is-denormal "Array of symmetric maps"
                 {:maps [{:a 100 :b 200} {:a 101 :b 201} {:a 102 :b 202} {:a 103 :b 203}]}
                 (list {:maps_arr_dot_b 200, :maps_arr_dot_a 100, :maps_idx 0}
                       {:maps_arr_dot_b 201, :maps_arr_dot_a 101, :maps_idx 1}
                       {:maps_arr_dot_b 202, :maps_arr_dot_a 102, :maps_idx 2}
                       {:maps_arr_dot_b 203, :maps_arr_dot_a 103, :maps_idx 3}))
    (is-denormal "Array of asymmetric maps"
                 {:maps [{:a 100} {:a 101 :b 201 :c 301} {:a 102 :b 202 :c 302 :d 402} {:a 103}]}
                 (list {:maps_arr_dot_a 100, :maps_idx 0}
                       {:maps_arr_dot_b 201, :maps_arr_dot_c 301, :maps_arr_dot_a 101, :maps_idx 1}
                       {:maps_arr_dot_d 402, :maps_arr_dot_b 202,
                        :maps_arr_dot_c 302, :maps_arr_dot_a 102, :maps_idx 2}
                       {:maps_arr_dot_a 103, :maps_idx 3}))
    (is-denormal "Array of nested arrays"
                 {:arr0 [{:arr0_0 [0 1 2]}
                         {:array0_1 [10 11 12]}]}                 
                 (list {:arr0_idx 0, :arr0_arr_dot_arr0_0_arr 0, :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr0_arr_dot_arr0_0_arr 1, :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr0_arr_dot_arr0_0_arr 2, :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 1, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}))
    (is-denormal "Map of arrays of nested arrays"
                 {:arr0 [{:arr0_0 ["a" "b" "c"]}
                         {:array0_1 [10 11 12]}]
                  :arr1 [{:arr1_0 [100 101 201]}
                         {:array1_1 [1000 1001 1002]}]}
                 (list {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_arr0_0_arr "a", :arr0_arr_dot_arr0_0_idx 0}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_arr0_0_arr "b", :arr0_arr_dot_arr0_0_idx 1}
                       {:arr0_idx 0, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_arr0_0_arr "c", :arr0_arr_dot_arr0_0_idx 2}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 100, :arr1_arr_dot_arr1_0_idx 0, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 101, :arr1_arr_dot_arr1_0_idx 1, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 0, :arr1_arr_dot_arr1_0_arr 201, :arr1_arr_dot_arr1_0_idx 2, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1000, :arr1_arr_dot_array1_1_idx 0, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1001, :arr1_arr_dot_array1_1_idx 1, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_array0_1_arr 10, :arr0_arr_dot_array0_1_idx 0}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_array0_1_arr 11, :arr0_arr_dot_array0_1_idx 1}
                       {:arr0_idx 1, :arr1_idx 1, :arr1_arr_dot_array1_1_arr 1002, :arr1_arr_dot_array1_1_idx 2, :arr0_arr_dot_array0_1_arr 12, :arr0_arr_dot_array0_1_idx 2}
                       ))))
  
;; (def rec {:a "hello" :b 5 :c true :d [1 2 3] :e {:a 1 :b 2} :f {:g 1 :h 2} :f_array {:some-array [1 2 3] :h 2}})

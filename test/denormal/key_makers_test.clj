(ns denormal.key-makers-test
  (:use
   clojure.test
   denormal.key-makers
   denormal.core
   roxxi.utils.print
   roxxi.utils.collections))


(def five-layer-map
  {:l1_a "1a"
   :l1_map {:l2_a "2a"
            :l2_map {:l3_a "3a" 
                     :l3_map {:l4_a "4a" 
                              :l4_map {:l5_a "5a"
                                       :l5_b "5b"}}}}})

(def dot-key-joiner
  (make-simple-key-joiner "_dot_"))

(def arr-idx-collection-key-maker
  (make-simple-coll-key-maker "_arr" "_idx"))

(deftest join-key-tests
  (let [denorm (fn join-key-denorm [key-joiner]
                 (denormalize-map five-layer-map
                                  key-joiner
                                  arr-idx-collection-key-maker))]
    (testing "Simple Key Joiners"
      (testing "joining with '_test_'"
        (let [test-key-joiner (make-simple-key-joiner "_test_")]        
          (is (= (denorm test-key-joiner)
                 (list {:l1_map_test_l2_map_test_l3_map_test_l4_map_test_l5_a "5a",
                        :l1_map_test_l2_map_test_l3_map_test_l4_map_test_l5_b "5b",
                        :l1_map_test_l2_map_test_l3_map_test_l4_a "4a",
                        :l1_map_test_l2_map_test_l3_a "3a",
                        :l1_map_test_l2_a "2a",
                        :l1_a "1a"})))))
      (testing "joining with an empty string"
        (let [blank-key-joiner (make-simple-key-joiner "")]
          (is (= (denorm blank-key-joiner)               
                 (list {:l1_mapl2_mapl3_mapl4_mapl5_a "5a",
                        :l1_mapl2_mapl3_mapl4_mapl5_b "5b",
                        :l1_mapl2_mapl3_mapl4_a "4a",
                        :l1_mapl2_mapl3_a "3a",
                        :l1_mapl2_a "2a",
                        :l1_a "1a"}))))))
    (testing "Hoist Key Joiners"
      (testing "hoisting keys with a prefix"
        (let [prefix-hoist-joiner (make-hoist-joiner "prefix" "")]
          (is (= (denorm prefix-hoist-joiner)
                 (list {:prefixl5_a "5a",
                        :prefixl5_b "5b",
                        :prefixl4_a "4a",
                        :prefixl3_a "3a",
                        :prefixl2_a "2a",
                        :l1_a "1a"})))))
      (testing "hoisting keys with a suffix"
        (let [prefix-hoist-joiner (make-hoist-joiner "" "suffix")]
          (is (= (denorm prefix-hoist-joiner)
                 (list {:l5_asuffix "5a",
                        :l5_bsuffix "5b",
                        :l4_asuffix "4a",
                        :l3_asuffix "3a",
                        :l2_asuffix "2a",
                        :l1_a "1a"}))))))))


(def dispatching-sample-map
  {:url "www.google.com"
   :time "10:30"
   :user "Alex"
   :user-info {:favorite-color "blue"
               :address {:street "555 somewhere"
                         :city "San Francisco"
                         :state "California"
                         :zip 92630}
               :phone {:1st {:cell "555-555-5555"
                             :hours "9am-5pm"}
                       :2nd {:home "111-222-3333"
                             :hours "5pm-9am"}}}
   :date "1/1/2005"
   :params {:item1 {:protein "chicken"
                    :vegetable "potato"
                    :fruit "pineapple"}
            :item2 {:protein "beef"
                    :vegetable "carrot"
                    :fruit "banana"}
            :items-total 2}
   :repeating {:nested {:repeating {:nested {:repeating "repeating"}}}}})




(deftest dispatching-join-key-tests
  (let [denorm (fn join-key-denorm [key-joiner]
                 (denormalize-map dispatching-sample-map
                                  key-joiner
                                  arr-idx-collection-key-maker))]
    (testing "Dispatching Key Joiners"
      (testing "hoist the user info fields, join the others"
        (let [user-info-hoister
              (make-dispatching-key-joiner
               (fn [prop subprop]
                 (when (= (name prop) "user-info")
                   (make-hoist-joiner)))
               dot-key-joiner)]
          (is (= (denorm user-info-hoister)
                 (list {:favorite-color "blue",
                        :date "1/1/2005",
                        :params_dot_item2_dot_fruit "banana",
                        :params_dot_item1_dot_vegetable "potato",
                        :phone_dot_2nd_dot_hours "5pm-9am",
                        :address_dot_state "California",
                        :params_dot_item1_dot_protein "chicken",
                        :params_dot_item2_dot_protein "beef",
                        :address_dot_city "San Francisco",
                        :address_dot_zip 92630,
                        :params_dot_item2_dot_vegetable "carrot",
                        :repeating_dot_nested_dot_repeating_dot_nested_dot_repeating "repeating",
                        :address_dot_street "555 somewhere",
                        :phone_dot_1st_dot_hours "9am-5pm",
                        :params_dot_item1_dot_fruit "pineapple",
                        :url "www.google.com",
                        :phone_dot_1st_dot_cell "555-555-5555",
                        :user "Alex",
                        :time "10:30",
                        :params_dot_items-total 2,
                        :phone_dot_2nd_dot_home "111-222-3333"})))))
      (testing "hoist the user info fields, with a prefix, join the others"
        (let [user-info-hoister
              (make-dispatching-key-joiner
               (fn [prop subprop]
                 (when (= (name prop) "user-info")
                   (make-hoist-joiner "prefix!" "")))
               dot-key-joiner)]
          (is (= (denorm user-info-hoister)
                 (list {:date "1/1/2005",
                        :params_dot_item2_dot_fruit "banana",
                        :params_dot_item1_dot_vegetable "potato",
                        :params_dot_item1_dot_protein "chicken",
                        :params_dot_item2_dot_protein "beef",
                        :prefix!phone_dot_1st_dot_hours "9am-5pm",
                        :prefix!address_dot_city "San Francisco",
                        :prefix!phone_dot_1st_dot_cell "555-555-5555",
                        :params_dot_item2_dot_vegetable "carrot",
                        :prefix!address_dot_zip 92630,
                        :repeating_dot_nested_dot_repeating_dot_nested_dot_repeating "repeating",
                        :prefix!favorite-color "blue",
                        :params_dot_item1_dot_fruit "pineapple",
                        :prefix!phone_dot_2nd_dot_home "111-222-3333",
                        :url "www.google.com",
                        :user "Alex",
                        :prefix!phone_dot_2nd_dot_hours "5pm-9am",
                        :prefix!address_dot_state "California",
                        :time "10:30",
                        :params_dot_items-total 2,
                        :prefix!address_dot_street "555 somewhere"})))))
      (testing (str "hoist the 'repeating' key field, prefixing with repeated, "
                    "join the others. Notice that only the first occurance of the "
                    "'repeating' property is prefixed. Properties are folded coarsest to "
                    "finest, downward into the map. So after we prefix it the first time, "
                    "it no longer matches 'repeating', so it isn't subsequently prefixed.")
        (let [not-really-repeating-hoister
              (make-dispatching-key-joiner
               (fn [prop subprop]
                 (when (= (name prop) "repeating")                   
                   (make-hoist-joiner "repeated" "")))
               dot-key-joiner)]
          (is (= (denorm not-really-repeating-hoister)
                 (list {:date "1/1/2005",
                        :params_dot_item2_dot_fruit "banana",
                        :repeatednested_dot_repeating_dot_nested_dot_repeating "repeating",
                        :params_dot_item1_dot_vegetable "potato",
                        :params_dot_item1_dot_protein "chicken",
                        :params_dot_item2_dot_protein "beef",
                        :user-info_dot_phone_dot_2nd_dot_hours "5pm-9am",
                        :user-info_dot_address_dot_state "California",
                        :user-info_dot_address_dot_city "San Francisco",
                        :params_dot_item2_dot_vegetable "carrot",
                        :user-info_dot_phone_dot_1st_dot_cell "555-555-5555",
                        :user-info_dot_phone_dot_2nd_dot_home "111-222-3333",
                        :user-info_dot_favorite-color "blue",
                        :params_dot_item1_dot_fruit "pineapple",
                        :url "www.google.com",
                        :user "Alex",
                        :time "10:30",
                        :user-info_dot_phone_dot_1st_dot_hours "9am-5pm",
                        :user-info_dot_address_dot_street "555 somewhere",
                        :params_dot_items-total 2,
                        :user-info_dot_address_dot_zip 92630})))))
      (testing (str "hoist the 'repeating' key field, prefixing with repeated,"
                    "join the others. In contrast to above, we now use the "
                    "subproperty to determine if we prefix. Because the final "
                    "property is named repeating, our final field is merely "
                    "that field named, prefixed, at the top level.")
        (let [repeating-hoister
              (make-dispatching-key-joiner
               (fn [prop subprop]
                 (when (= (name subprop) "repeating")
                   (make-hoist-joiner "repeated" "")))
               dot-key-joiner)]
          (is (= (denorm repeating-hoister)
                 (list {:repeatedrepeating "repeating",
                        :date "1/1/2005",
                        :params_dot_item2_dot_fruit "banana",
                        :params_dot_item1_dot_vegetable "potato",
                        :params_dot_item1_dot_protein "chicken",
                        :params_dot_item2_dot_protein "beef",
                        :user-info_dot_phone_dot_2nd_dot_hours "5pm-9am",
                        :user-info_dot_address_dot_state "California",
                        :user-info_dot_address_dot_city "San Francisco",
                        :params_dot_item2_dot_vegetable "carrot",
                        :user-info_dot_phone_dot_1st_dot_cell "555-555-5555",
                        :user-info_dot_phone_dot_2nd_dot_home "111-222-3333",
                        :user-info_dot_favorite-color "blue",
                        :params_dot_item1_dot_fruit "pineapple",
                        :url "www.google.com",
                        :user "Alex",
                        :time "10:30",
                        :user-info_dot_phone_dot_1st_dot_hours "9am-5pm",
                        :user-info_dot_address_dot_street "555 somewhere",
                        :params_dot_items-total 2,
                        :user-info_dot_address_dot_zip 92630}))))))))



(dispatching-join-key-tests)
(ns sqly.core-test
  (:require
   [clojure.test :refer :all]
   [sqly.core :as sql]))

(deftest as-ident
  (is (= "foo_bar" (sql/as-ident :foo-bar)))
  (is (= "\"foo_bar.quux\"" (sql/as-ident :foo-bar.quux)))
  (is (= "\"foo_bar.quux\" as foo_quux"
         (sql/as-ident {:foo-quux :foo-bar.quux})))
  (is (= "month(datetime) as month"
         (sql/as-ident {:month '(month :datetime)})))
  (is (= "count(1) as cnt"
         (sql/as-ident {:cnt '(count 1)}))))

(def select-example
  (str
   "select *"
   " from \"zapbuy.cues\""
   " where (zb_id is not null and event_type is not null)"
   " order by name asc, \"foo.datetime\" desc"
   " group by \"context.event_type\", month(datetime), day(datetime)"))

(deftest sql
  (is (= select-example
         (sql/sql
          {:select   :*
           :from     :zapbuy.cues
           :where    '[(and [:zb-id :not-nil]
                            [:event-type "is not null"])]
           :order-by [[:name :asc]
                      [:foo.datetime :desc]]
           :group-by '[:context.event-type
                       (month :datetime)
                       (day :datetime)]})))
  (is (= select-example
         (sql/sql
          {:select   :*
           :from     :zapbuy.cues
           :where    {:zb-id :not-nil
                      :event-type :not-nil}
           :order-by [[:name :asc]
                      [:foo.datetime :desc]]
           :group-by '[:context.event-type
                       (month :datetime)
                       (day :datetime)]}))))

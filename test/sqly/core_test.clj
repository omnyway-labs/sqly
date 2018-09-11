(ns sqly.core-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [sqly.core :as sql]))

(deftest as-ident
  (is (= "foo_bar" (sql/as-ident :foo-bar)))
  (is (= "\"foo_bar.quux\"" (sql/as-ident :foo-bar.quux)))
  (is (= "\"foo_bar.quux\" as foo_quux"
         (sql/as-ident {:foo-quux :foo-bar.quux})))
  (is (= "month(datetime) as month"
         (sql/as-ident {:month '(month :datetime)})))
  (is (= "count(1) as cnt"
         (sql/as-ident {:cnt '(count 1)})))
  (is (= "foo.\"foo.col\""
         (sql/as-ident :foo/foo.col)))
  (is (= "\"foo.tbl\".\"foo.col\""
         (sql/as-ident :foo.tbl/foo.col))))

(defn canon [s]
  (->> s
       (str/join "")
       (str/lower-case)))

(def select-example
  (canon
   ["select *"
    " from zapbuy.cues"
    " where (zb_id is not null) and (event_type is not null)"
    " group by \"context.event_type\",month(datetime),day(datetime)"
    " order by name asc,\"foo.datetime\" desc"]))

(deftest sql
  (is (= select-example
         (sql/sql
          '{:select   :*
            :from     :zapbuy.cues
            :where    [(and [:zb-id :is-not-nil]
                            [:event-type :is-not-nil])]
            :order-by [[:name :asc]
                       [:foo.datetime :desc]]
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]})))
  (is (= select-example
         (sql/sql
          '{:select   :*
            :from     :zapbuy.cues
            :where    {:zb-id :is-not-nil
                       :event-type :is-not-nil}
            :order-by [[:name :asc]
                       [:foo.datetime :desc]]
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]}))))

(deftest queries-edn
  ;; :zapbuy-api-count-monthly
  (is (= (canon
          ["SELECT"
           " month(datetime) as month"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM zapbuy.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",month(datetime)"
           " ORDER BY month DESC"])
         (sql/sql
          '{:select {:month (month :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :zapbuy.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (month :datetime)]
            :order-by [[:month :desc]]})))

  ;; :zapbuy-api-count-daily
  (is (= (canon
          ["SELECT"
           " month(datetime) as month"
           ",day(datetime) as day"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM zapbuy.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",month(datetime),day(datetime)"
           " ORDER BY month DESC,day DESC"])
         (sql/sql
          '{:select {:month (month :datetime)
                     :day (day :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :zapbuy.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]
            :order-by [[:month :desc]
                       [:day :desc]]})))

  ;; :zapbuy-api-count-hourly
  (is (= (canon
          ["SELECT"
           " date_trunc('hour',datetime) as hour"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM zapbuy.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",date_trunc('hour',datetime)"
           " ORDER BY hour DESC"])
         (sql/sql
          '{:select {:hour (date-trunc "hour" :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :zapbuy.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (date-trunc "hour" :datetime)]
            :order-by [[:hour :desc]]}))))

(deftest where-test
  (is (= (canon
          ["select *"
           " from zapbuy.cues"
           " where (event_type is not null) or (error is not null)"])
         (sql/sql
          '{:select :*
            :from :zapbuy.cues
            :where (or {:event-type :not-nil}
                       {:error :not-nil})}))))

(deftest with-form-test
  (is (= (canon
          ["WITH"
           " load_offer AS (select count(1) as count from zapbuy.cues WHERE cue = 'zapbuy.load-offer')"
           ",purchase AS (select count(1) as count from zapbuy.cues WHERE cue = 'zapbuy.purchase')"
           " SELECT 1.0"
           " * ((SELECT \"purchase.count\" FROM purchase)"
           " / (SELECT \"load_offer.count\" FROM load_offer))"
           " AS purchases_per_offer_loads"])
         (sql/sql
          '{:with {:load-offer (sql
                                {:select {:count (count 1)}
                                 :from :zapbuy.cues
                                 :where (= :cue "zapbuy.load-offer")})
                   :purchase (sql
                              {:select {:count (count 1)}
                               :from :zapbuy.cues
                               :where (= :cue "zapbuy.purchase")})}
            :do {:select {:purchases-per-offer-loads
                          (* 1.0
                             (/ (sql {:select :purchase.count :from :purchase})
                                (sql {:select :load-offer.count :from :load-offer})))}}}))))

(deftest ops-test
  (is (= "select * where 1.0 / 5.0"
         (sql/sql
          {:select :*
           :where '(/ 1.0 5.0)})))
  (is (= "\"context.event\" is null"
         (sql/emit-postfix-op '(nil? :context.event))))
  (is (= (canon
          ["select * where (\"context.event\" is null)"
           " and (\"context.reason\" is not null)"])
         (sql/sql
          {:select :*
           :where '(and (nil? :context.event)
                        (not-nil? :context.reason))})))
  (is (= (canon
          ["select * from basket"
           " where (\"context.merchant_id\" = '{{merchant-id}}')"
           " and (\"context.event_type\" is not null)"
           " and ((timestamp '2018-09-09') <= datetime)"
           " and (datetime <= (timestamp '2018-09-10'))"])
         (sql/sql
          '{:select :*
            :from :basket
            :where (and
                    (= :context.merchant-id "{{merchant-id}}")
                    {:context.event-type :is-not-nil}
                    (<= (timestamp "2018-09-09") :datetime)
                    (<= :datetime (timestamp "2018-09-10")))}))))


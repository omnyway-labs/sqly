(ns sqly.core-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [camel-snake-kebab.core :as csk]
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
    " from myapp.cues"
    " where (id is not null) and (event_type is not null)"
    " group by \"context.event_type\",month(datetime),day(datetime)"
    " order by name asc,\"foo.datetime\" desc"]))

(deftest sql
  (is (= select-example
         (sql/sql
          '{:select   :*
            :from     :myapp.cues
            :where    [(and [:id :is-not-nil]
                            [:event-type :is-not-nil])]
            :order-by [[:name :asc]
                       [:foo.datetime :desc]]
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]})))
  (is (= select-example
         (sql/sql
          '{:select   :*
            :from     :myapp.cues
            :where    {:id :is-not-nil
                       :event-type :is-not-nil}
            :order-by [[:name :asc]
                       [:foo.datetime :desc]]
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]}))))

(deftest queries-edn
  ;; :myapp-api-count-monthly
  (is (= (canon
          ["SELECT"
           " month(datetime) as month"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM myapp.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",month(datetime)"
           " ORDER BY month DESC"])
         (sql/sql
          '{:select {:month (month :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :myapp.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (month :datetime)]
            :order-by [[:month :desc]]})))

  ;; :myapp-api-count-daily
  (is (= (canon
          ["SELECT"
           " month(datetime) as month"
           ",day(datetime) as day"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM myapp.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",month(datetime),day(datetime)"
           " ORDER BY month DESC,day DESC"])
         (sql/sql
          '{:select {:month (month :datetime)
                     :day (day :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :myapp.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (month :datetime)
                       (day :datetime)]
            :order-by [[:month :desc]
                       [:day :desc]]})))

  ;; :myapp-api-count-hourly
  (is (= (canon
          ["SELECT"
           " date_trunc('hour',datetime) as hour"
           ",\"context.event_type\" as type"
           ",count(1) as count"
           " FROM myapp.cues"
           " WHERE \"context.event_type\" IS NOT NULL"
           " GROUP BY \"context.event_type\",date_trunc('hour',datetime)"
           " ORDER BY hour DESC"])
         (sql/sql
          '{:select {:hour (date-trunc "hour" :datetime)
                     :type :context.event-type
                     :count (count 1)}
            :from :myapp.cues
            :where {:context.event-type :is-not-nil}
            :group-by [:context.event-type
                       (date-trunc "hour" :datetime)]
            :order-by [[:hour :desc]]}))))

(deftest where-test
  (is (= (canon
          ["select *"
           " from myapp.cues"
           " where (event_type is not null) or (error is not null)"])
         (sql/sql
          '{:select :*
            :from :myapp.cues
            :where (or {:event-type :not-nil}
                       {:error :not-nil})}))))=

(defn mixed-case? [s]
  (some? (re-find #"[A-Z]" s)))

(deftest ident-style-test
  (is (= (str/join
          ""
          ["select *"
           " from myapp.cues"
           " where (\"eventType\" is not null) or (error is not null)"])
         (sql/with-output-ident-style #(if (mixed-case? %)
                                         (str \" (csk/->camelCase %) \")
                                         %)
           (sql/sql
            '{:select :*
              :from :myapp.cues
              :where (or {:eventType :not-nil}
                         {:error :not-nil})})))))

(deftest with-form-test
  (is (= (canon
          ["WITH"
           " get_offer AS (select count(1) as count from myapp.cues WHERE cue = 'myapp.get-offer')"
           ",purchase AS (select count(1) as count from myapp.cues WHERE cue = 'myapp.purchase')"
           " SELECT 1.0"
           " * ((SELECT \"purchase.count\" FROM purchase)"
           " / (SELECT \"get_offer.count\" FROM get_offer))"
           " AS purchases_per_offer_loads"])
         (sql/sql
          '{:with {:get-offer (sql
                                {:select {:count (count 1)}
                                 :from :myapp.cues
                                 :where (= :cue "myapp.get-offer")})
                   :purchase (sql
                              {:select {:count (count 1)}
                               :from :myapp.cues
                               :where (= :cue "myapp.purchase")})}
            :do {:select {:purchases-per-offer-loads
                          (* 1.0
                             (/ (sql {:select :purchase.count :from :purchase})
                                (sql {:select :get-offer.count :from :get-offer})))}}}))))

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
                    (<= :datetime (timestamp "2018-09-10")))})))

  (is (= (canon
          ["select * from basket"
           " where (\"context.merchant_id\" = '{{merchant-id}}')"
           " and (\"context.event_type\" is not null)"
           " and (datetime between timestamp '2018-09-09' and timestamp '2018-09-10')"])
         (sql/sql
          '{:select :*
            :from :basket
            :where (and
                    (= :context.merchant-id "{{merchant-id}}")
                    {:context.event-type :is-not-nil}
                    (between :datetime
                             (timestamp "2018-09-09")
                             (timestamp "2018-09-10")))}))))


(deftest ident-test
  (is (= "select * from basket where value.skus[0]['name'] is not null"
         (sql/sql
          '{:select :*
            :from :basket
            :where (not-nil?
                    (ident "value.skus[0]['name']"))}))))

(deftest create-table-test
  (is (= "create table payers (id varchar,payer_id varchar,vault_id varchar)"
         (sql/sql
          '{:create-table :payers
            :columns      {:id       :varchar
                           :payer-id :varchar
                           :vault-id :varchar}})))

  (is (= (str
          "create table payers "
          "(id varchar"
          ",payer_id varchar default 'foo'"
          ",vault_id varchar"
          ",primary key(id))")
         (sql/sql
          '{:create-table :payers
            :columns      {:id              :varchar
                           :payer-id [:varchar :default "foo"]
                           :vault-id        :varchar}
            :constraints  {:primary-key :id}})))

  (is (= (str
          "create table payers "
          "(id varchar"
          ",payer_id varchar default 'foo'"
          ",vault_id varchar"
          ",primary key(id)"
          ",constraint fk_payer_address_id foreign key(payer_id) references payer(id))")
         (sql/sql
          '{:create-table :payers
            :columns      {:id              :varchar
                           :payer-id [:varchar :default "foo"]
                           :vault-id        :varchar}
            :constraints  {:primary-key :id
                           :foreign-key [:fk-payer-address-id :payer-id :payer :id]}}))))

(deftest drop-table-test
  (is (= "drop table payers"
         (sql/sql
          '{:drop-table :payers}))))

(deftest where-clause-map
  (is (= (canon
          ["select * from basket"
           " where (\"context.merchant_id\" = '{{merchant-id}}')"
           " and (\"context.event_type\" is not null)"
           " and (\"context.count\" = 1)"])
         (sql/sql
          '{:select :*
            :from :basket
            :where {:context.merchant-id "{{merchant-id}}"
                    :context.event-type :is-not-nil
                    :context.count 1}}))))

(deftest join-test
  (is
   (=
    (canon
     ["SELECT"
      " DATE_FORMAT(abc_clnt.datetime,'%Y-%m-%d %T')"
      " AS \"abc_clnt.datetime_time\""
      ",abc_cues.\"context.request_id\""
      " AS \"abc_cues.context_request_id\""
      ",abc_clnt.abc_id AS \"abc_clnt.abc_id\""
      ",abc_pmnts.error AS \"abc_pmnts.error\""
      ",abc_clnt.prd_sku AS \"abc_clnt.prd_sku\""
      ",abc_clnt.product_id AS \"abc_clnt.product_id\""
      ",abc_clnt.prd_price AS \"abc_clnt.prd_price\""
      ",abc_clnt.prd_sale_price AS \"abc_clnt.prd_sale_price\""
      ",abc_clnt.prd_sale_desc AS \"abc_clnt.prd_sale_desc\""
      ",abc_pmnts.total / 100 AS \"abc_pmnts.total\""
      ",abc_clnt.device_model AS \"abc_clnt.device_model\""
      ",abc_cues.\"value.rcpt.id\""
      " AS \"abc_cues.value_rcpt_id\""
      ",abc_cues.\"value.rcpt.rcpt.id\""
      " AS \"abc_cues.value_rcpt_rcpt_id\""
      ",abc_cues.\"value.rcpt.rcpt.txn_info.merch_txn_id\""
      " AS \"zc.valuercptrcpt_txn_info_merch_txn_id\""
      ",abc_cues.\"value.shopper.profile.billing_address.state\""
      " AS \"abc_cues.value_shopper_profile_billing_address_state\""
      ",abc_cues.\"value.shopper.profile.email\""
      " AS \"abc_cues.value_shopper_profile_email\""
      " FROM abc_prod.abc_cues AS abc_cues"
      " WHERE abc_clnt.event_name = 'btn-clicked'"
      " LEFT JOIN abc_prod.abc_clnt"
      " AS abc_clnt"
      " ON abc_clnt.abc_id = abc_cues.\"value.abc_id\""
      " LEFT JOIN abc_prod.abc_pmnts"
      " AS abc_pmnts"
      " ON abc_pmnts.basket_id = abc_clnt.abc_id"])
    (->
     '{:select [{:abc-clnt.datetime-time
                 (date-format :abc-clnt/datetime "%Y-%m-%d %T")}
                {:abc-cues.context-request-id
                 :abc-cues/context.request-id}
                {:abc-clnt.abc-id :abc-clnt/abc-id}
                {:abc-pmnts.error :abc-pmnts/error}
                {:abc-clnt.prd-sku :abc-clnt/prd-sku}
                {:abc-clnt.product-id :abc-clnt/product-id}
                {:abc-clnt.prd-price :abc-clnt/prd-price}
                {:abc-clnt.prd-sale-price
                 :abc-clnt/prd-sale-price}
                {:abc-clnt.prd-sale-desc
                 :abc-clnt/prd-sale-desc}
                {:abc_pmnts.total (/ :abc-pmnts/total 100)}
                {:abc-clnt.device-model :abc-clnt/device-model}
                {:abc-cues.value-rcpt-id
                 :abc-cues/value.rcpt.id}
                {:abc-cues.value-rcpt-rcpt-id
                 :abc-cues/value.rcpt.rcpt.id}
                {:zc.valuercptrcpt-txn-info-merch-txn-id
                 :abc-cues/value.rcpt.rcpt.txn-info.merch-txn-id}
                {:abc-cues.value-shopper-profile-billing-address-state
                 :abc-cues/value.shopper.profile.billing-address.state}
                {:abc-cues.value-shopper-profile-email
                 :abc-cues/value.shopper.profile.email}]
       :from   {:abc-cues :abc-prod/abc-cues}
       :join   [{:type :left
                 :from {:abc-clnt :abc-prod/abc-clnt}
                 :on   {:abc-clnt/abc-id :abc-cues/value.abc-id}}
                {:type :left
                 :from {:abc-pmnts :abc-prod/abc-pmnts}
                 :on   {:abc-pmnts/basket-id :abc-clnt/abc-id}}]
       :where  {:abc-clnt/event-name "btn-clicked"}}
     sql/sql
     canon))))

(deftest canonicalize-alpha-numeric-idents
  (is (= "a_v3" (sql/ident-str :a-v3)))
  (is (= "foo/a_v3" (sql/ident-str :foo/a-v3))))

(deftest union-test
  (is (= (canon
          ["select related_id,type,description from a_events"
           " where related_id = '{{related-id}}'"
           " union "
           "select related_id,type,description from b_events"
           " where related_id = '{{related-id}}'"])
         (sql/sql
          '{:union [{:select [:related_id :type :description]
                     :from :a_events
                     :where {:related_id "{{related-id}}"}}
                    {:select [:related_id :type :description]
                     :from :b_events
                     :where {:related_id "{{related-id}}"}}]}))))

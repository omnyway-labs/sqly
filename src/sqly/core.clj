(ns sqly.core
  (:require
   [clojure.string :as str]
   [camel-snake-kebab.core :refer [->kebab-case-keyword ->snake_case]]))

(defn name-with-ns [k]
  (->> k
       (pr-str)
       (replace {\: ""})
       (apply str)
       (->snake_case)))

(defn ident-str [s]
  (cond
    (string? s)
    (str "'" s "'")

    (instance? clojure.lang.Named s)
    (name-with-ns s)

    :else
    s))

(defn quote-iff
  ([s]
   (quote-iff s {}))
  ([s opts]
   (if (and (not (:disable-quoting? opts))
            (string? s))
     (->> (str/split s #"/")
          (map (fn [s]
                 (if (re-find #"\." s)
                   (pr-str s)
                   s)))
          (str/join "."))
     s)))

(declare as-ident)

(defn emit-infix-op
  ([v]
   (emit-infix-op v {}))
  ([[op & args] opts]
   (->> args
        (map #(let [s (as-ident % opts)]
                (if (and (coll? %) (< 1 (count args)))
                  (str "(" s ")")
                  s)))
        (str/join (str " "
                       (as-ident op
                                 (assoc opts :disable-quoting? true))
                       " ")))))

(defn emit-postfix-op
  ([v]
   (emit-postfix-op v {}))
  ([[op & args] opts]
   (let [args (->> args
                   (map #(let [s (as-ident % opts)]
                           (if (and (coll? %) (< 1 (count args)))
                             (str "(" s ")")
                             s)))
                   (str/join " "))]
     (str args " " (as-ident op (assoc opts :disable-quoting? true))))))

(defn emit-prefix-op
  ([v]
   (emit-prefix-op v {}))
  ([[op & args] opts]
   (let [args (->> args
                   (map #(let [s (as-ident % opts)]
                           (if (and (coll? %) (< 1 (count args)))
                             (str "(" s ")")
                             s)))
                   (str/join " "))]
     (str (as-ident op (assoc opts :disable-quoting? true)) " " args))))

(declare sql)

(defn emit-sql [[_sql expr] & _]
  (sql expr))

(defn emit-between
  ([v]
   (emit-between v {}))
  ([[op arg lower higher] opts]
   (str/join " "
             [(as-ident arg opts)
              (as-ident op)
              (as-ident lower opts)
              "and"
              (as-ident higher opts)])))

(defn emit-literal-identifier
  ([v]
   (emit-literal-identifier v {}))
  ([[_op v] opts]
   v))

(def function-handlers (atom {}))

(defn def-ops [emitter ops]
  (->> ops
       (map
        (fn [op]
          [op emitter]))
       (into {})
       (swap! function-handlers merge)))

(def-ops #'emit-infix-op
  '[and or + - * / = < <= > >= =/= not= sql])

(def-ops #'emit-postfix-op
  '[nil? not-nil?])

(def-ops #'emit-prefix-op
  '[timestamp])

(def-ops #'emit-sql '[sql])
(def-ops #'emit-between '[between])
(def-ops #'emit-literal-identifier '[ident])

(defn emit-function
  ([v]
   (emit-function v {}))
  ([v opts]
   (let [[fname & args] v]
     (if-let [handler (@function-handlers fname)]
       (handler v opts)
       (let [args-str (->> args
                           (map #(as-ident % opts))
                           (str/join ","))]
         (str (as-ident fname) "(" args-str ")"))))))

(def ^:dynamic *remapped-idents*
  '{:not-nil "is not null"
    :is-not-nil "is not null"
    :is-nil "is null"
    not-nil? "is not null"
    nil? "is null"})

(defn remap-ident [v]
  (get *remapped-idents* v))

(defn emit-map
  ([m]
   (emit-map m {}))
  ([m {:keys [separator]}]
   (->> m
        (map
         (fn [[k v]]
           (format "%s as %s" (as-ident v) (as-ident k))))
        (str/join (or separator " ")))))

(defn emit-coll
  ([c]
   (emit-coll c {}))
  ([c {:keys [separator]}]
   (->> c
        (map as-ident)
        (str/join (or separator " ")))))

(defn as-ident
  ([v]
   (as-ident v {}))
  ([v {:as opts :keys [emitters]}]
   (cond
     (map? v)
     ((or (-> emitters :map) emit-map) v opts)

     (list? v)
     ((or (-> emitters :function) emit-function) v opts)

     (coll? v)
     ((or (-> emitters :coll) emit-coll) v opts)

     (string? v)
     (ident-str v)

     :else
     (or (remap-ident v)
         (-> v
             (ident-str)
             (quote-iff opts))))))

(defn emit-idents
  ([idents]
   (emit-idents idents {:separator ","}))
  ([idents {:as opts :keys [separator]}]
   (when idents
     (->> idents
          (map #(as-ident % opts))
          (str/join separator)))))

(defn ensure-seq [v]
  (if (or (nil? v) (sequential? v)) v [v]))

(defn emit-clause
  ([clause content]
   (emit-clause clause content {:separator ","}))
  ([clause content opts]
   (when content
     (str/join " "
               [clause
                (emit-idents (ensure-seq content) opts)]))))

(defn emit-clause-spaced [clause content]
  (emit-clause clause content {:separator " "}))

(defn emit-order-by-clause [clause content]
  (when content
    (->> content
         (map #(emit-idents % {:separator " "}))
         (str/join ",")
         (str clause " "))))

(defn emit-from-clause [clause content]
  (emit-clause clause content {:disable-quoting? true
                               :separator ","}))

(defn emit-where-map
  ([m]
   (emit-where-map m {}))
  ([m opts]
   (emit-function (cons 'and (into [] m)) opts)))

(defn emit-where-clause [clause content]
  (when content
    (emit-clause clause
                 [content]
                 {:separator " "
                  :emitters {:map emit-where-map}})))

(defmulti sql* :op)

(defn emit-select [{:keys [select from where order-by group-by limit]}]
  (when select
    (->> [["select" select]
          ["from" from #'emit-from-clause]
          ["where" where #'emit-where-clause]
          ["group by" group-by]
          ["order by" order-by #'emit-order-by-clause]
          ["limit" limit]]
         (map
          (fn [[clause content emitter]]
            ((or emitter emit-clause) clause content)))
         (remove nil?)
         (str/join " "))))

(defmethod sql* :select [clause]
  (emit-select clause))

(defmethod sql* :with [{:keys [with do]}]
  (let [with-clauses (->> with
                          (map
                           (fn [[as query]]
                             (str (as-ident as)
                                  " as ("
                                  (as-ident query)
                                  ")")))
                          (str/join ","))]
    (->> ["with"
          with-clauses
          (emit-select do)]
         (remove nil?)
         (str/join " "))))

(defn emit-columns [columns]
  (when columns
    (->> columns
         (map
          (fn [[col-name & col-specs]]
            (str
             (as-ident col-name)
             " "
             (->> col-specs
                  (map as-ident)
                  (str/join " ")))))
         (str/join ","))))

(defn as-seq [s] (if (sequential? s) s [s]))

(def constraint-syntax
  {:primary-key (fn [cols]
                  (format "primary key(%s)" (emit-idents (as-seq cols))))
   :foreign-key (fn [[cols foreign-table foreign-cols]]
                  (format "foreign key(%s) references %s(%s)"
                          (emit-idents (as-seq cols))
                          (as-ident foreign-table)
                          (emit-idents (as-seq foreign-cols))))})

(defn emit-constraints [constraints]
  (when constraints
    (->> constraints
         (map
          (fn [[constraint arg]]
            (when-let [handler (constraint-syntax constraint)]
              (handler arg))))
         (str/join ","))))

(emit-constraints {:primary-key :id})

(defmethod sql* :create-table [{:keys [create-table columns constraints]}]
  (when create-table
    (str
     "create table " (as-ident create-table)
     " ("
     (emit-columns columns)
     (when constraints ",")
     (emit-constraints constraints)
     ")")))

(defmethod sql* :drop-table [{:keys [drop-table]}]
  (when drop-table
    (str "drop table " (as-ident drop-table))))

(def sql-keywords #{:select :insert :with :create-table :drop-table})

(defn sql [m]
  (when-let [op (some sql-keywords (keys m))]
    (sql* (assoc m :op op))))

(ns sqly.core
  (:require
   [camel-snake-kebab.core :refer [->kebab-case-keyword ->snake_case]]
   [clojure.string :as str]))

(defn ident-str [s]
  (cond
    (string? s)
    (str "'" s "'")

    (instance? clojure.lang.Named s)
    (-> s name ->snake_case)

    :else
    s))

(defn quote-iff
  ([s]
   (quote-iff s {}))
  ([s opts]
   (if (and (not (:disable-quoting? opts))
            (string? s)
            (re-find #"\." s))
     (pr-str s)
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
        (str/join (str " " op " ")))))

(declare sql)

(defn emit-sql [[_sql expr] & _]
  (sql expr))

(def function-handlers
  {'and  #'emit-infix-op
   'or   #'emit-infix-op
   '+    #'emit-infix-op
   '-    #'emit-infix-op
   '*    #'emit-infix-op
   '/    #'emit-infix-op
   '=    #'emit-infix-op
   '<    #'emit-infix-op
   '<=   #'emit-infix-op
   '>    #'emit-infix-op
   '>=   #'emit-infix-op
   '=/=  #'emit-infix-op
   'not= #'emit-infix-op
   'sql  #'emit-sql})

(defn emit-function
  ([v]
   (emit-function v {}))
  ([v opts]
   (let [[fname & args] v]
     (if-let [handler (function-handlers fname)]
       (handler v opts)
       (let [args-str (->> args
                           (map #(as-ident % opts))
                           (str/join ","))]
         (str (as-ident fname) "(" args-str ")"))))))

(def ^:dynamic *remapped-idents*
  {:not-nil "is not null"
   :is-not-nil "is not null"
   :is-nil "is null"})

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
   (->> idents
        (map #(as-ident % opts))
        (str/join separator))))

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
  (emit-clause clause content {:disable-quoting? true}))

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
                                  " as "
                                  (as-ident query))))
                          (str/join ","))]
    (->> ["with"
          with-clauses
          (emit-select do)]
         (remove nil?)
         (str/join " "))))

(def sql-keywords #{:select :insert :with})

(defn sql [m]
  (when-let [op (some sql-keywords (keys m))]
    (sql* (assoc m :op op))))

(ns sqly.core
  (:require
   [camel-snake-kebab.core :refer [->kebab-case-keyword ->snake_case]]
   [clojure.string :as str]))

(defn ident-str [s]
  (if (instance? clojure.lang.Named s)
    (-> s name ->snake_case)
    s))

(defn quote-iff [s]
  (if (and (string? s) (re-find #"\." s))
    (pr-str s)
    s))

(declare as-ident)

(def infix-fns '#{and or = < <= > >= =/= not=})

(defn emit-infix-op [op & args]
  (str "("
       (->> args
            (map as-ident)
            (str/join (str " " op " ")))
       ")"))

(defn emit-function [fname & args]
  (if (infix-fns fname)
    (apply emit-infix-op fname args)
    (let [args-str (->> args
                        (map as-ident)
                        (str/join ","))]
      (str fname "(" args-str ")"))))

(defn as-ident [v]
  (cond
    (map? v)
    (->> v
         (map
          (fn [[k v]]
            (format "%s as %s" (as-ident v) (as-ident k))))
         (str/join " "))

    (list? v)
    (apply emit-function v)

    (coll? v)
    (->> v
         (map as-ident)
         (str/join " "))

    :else
    (quote-iff (ident-str v))))

(defn emit-idents
  ([idents]
   (emit-idents idents ","))
  ([idents separator]
   (->> idents
        (map as-ident)
        (str/join separator))))

(defn ensure-seq [v]
  (if (or (nil? v) (sequential? v)) v [v]))

(defn emit-clause
  ([clause content]
   (emit-clause clause content ","))
  ([clause content separator]
   (when content
     (str clause " "
          (emit-idents (ensure-seq content) separator)))))

(defn emit-clause-spaced [clause content]
  (emit-clause clause content " "))

(defn emit-order-by-clause [clause content]
  (->> content
       (map #(emit-idents % " "))
       (str/join ",")
       (str clause " ")))

(defn emit-select [{:keys [select from where order-by group-by limit]}]
  (->> [["select" select]
        ["from" from]
        ["where" where emit-clause-spaced]
        ["order by" order-by emit-order-by-clause]
        ["group by" group-by]
        ["limit" limit]]
       (map
        (fn [[clause content emitter]]
          ((or emitter emit-clause) clause content)))
       (remove nil?)
       (str/join " ")))

(defmulti sql* :op)

(defmethod sql* :select [m]
  (emit-select (dissoc m :op)))

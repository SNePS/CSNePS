(ns csneps.core.arithmetic
  (:require [csneps.core.build :as build])
  (:refer-clojure :exclude [+ - * / < <= > >= == not=])
  (:use [csneps.core :only (term?)]
        [csneps.util]))

(defn box
  "Returns a term whose name looks like n."
  [n]
  (build/build n :Entity {} #{}))

(defn unbox
  "If term is a number, return it;
   if term's name looks like a number, return the number;
   else throw an error."
  [term]
  (if (number? term)
    term
    (let [read-term (read-string (str term))]
      (if (number? read-term)
        read-term
        (let [n (ignore-errors (read-string (str (:name term))))]
          (if (number? n)
            n
            (error (str term " does not look like a number."))))))))

(defn +
  "Returns a term whose name looks like the sum of the numbs,
      which can be boxed or unboxed numbers."
  [& numbs]
  (if (some term? numbs)
    (box (apply clojure.core/+ (map unbox numbs)))
    (apply clojure.core/+ numbs)))

(defn -
  "Returns a term whose name looks like the difference of the numbs,
      which can be boxed or unboxed numbers."
  [& numbs]
  (if (some term? numbs)
    (box (apply clojure.core/- (map unbox numbs)))
    (apply clojure.core/- numbs)))

(defn *
  "Returns a term whose name looks like the product of the numbs,
      which can be boxed or unboxed numbers."
  [& numbs]
  (if (some term? numbs)
    (box (apply clojure.core/* (map unbox numbs)))
    (apply clojure.core/* numbs)))

(defn /
  "Returns a term whose name looks like the quotient of the numbs,
      which can be boxed or unboxed numbers."
  [& numbs]
  (if (some term? numbs)
    (box (apply clojure.core// (map unbox numbs)))
    (apply clojure.core// numbs)))

(defn <
  "Returns t if each num is less than the next,
     nil otherwise."
  [& numbs]
  (apply clojure.core/< (map unbox numbs)))

(defn <=
  "Returns t if each num is less than or equal to the next,
     nil otherwise."
  [& numbs]
  (apply clojure.core/<= (map unbox numbs)))

(defn >
  "Returns t if each num is greater than the next,
     nil otherwise."
  [& numbs]
  (apply clojure.core/> (map unbox numbs)))

(defn >=
  "Returns t if each num is greater than or equal to the next,
     nil otherwise."
  [& numbs]
  (apply clojure.core/>= (map unbox numbs)))

(defn ==
  "Returns t if each num is equal to the next,
     nil otherwise."
  [& numbs]
  (apply clojure.core/== (map unbox numbs)))

(defn not=
  "Returns t if none of the numbs are equal,
     nil otherwise."
  [& numbs]
  (apply clojure.core/not= (map unbox numbs)))

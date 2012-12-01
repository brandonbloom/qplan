(ns qplan.core
  (:require [clojure.set :as set]))

(defmacro dbg [x]
  `(let [x# ~x]
     (println)
     (println (pr-str '~x) "<<<")
     (clojure.pprint/pprint x#)
     (println ">>>\n")
     x#))

;;;

;; constraint:
;;   some set of variables
;;   some set of methods for those variables
;;   strength

;; method:
;; {:ins {}, :f f, :outs {}}
;;
;; a method is free if all of it's output variables are free
;; a variable is free if it's only attached to one constraint

;; Constraint graph
; union of variables from all constraints
; connection between all the variables via methods
; constraints as hyper edges, so there is a place to look up strengths
;     -- disj the methods ? or replace them with names of methods?
;
; G = <V + M, E>
;   V -> variables
;   M -> methods
;   E -> directed edges connecting inputs to outputs through a method

; when creating constraint graph, add stays

; methods have input and output variables
; variables have source and sink methods

;; Needed operations
; test if no more methods
; get free methods from a constraint graph
;    implies get methods for which a variable only has one constraint
;    free methods should be sorted by ascending smallest number of output variables
; remove a constraint and it's methods from a graph
; add a method to a graph
; get constraint with greatest strength
; remove constraint from set of constraints


; constraints sorted by strength descending    -- could call it "priority" and do ascending
; variables sorted by number of source methods ascending
; methods sorted by number of output variables ascending

;; This is a naive algorithm, but I think methods is generally small here
(defn free-method [methods neighborhoods]
  (let [outputs (for [method methods, variable (:outputs method)]
                  [variable method])
        free-vars (->> outputs
                    (reduce (fn [sources [variable method]]
                              (let [source-set (conj (get sources #{}) method)]
                                (assoc sources variable source-set)))
                            {})
                    (filter #(= (-> % val count) 1))
                    keys set)
        free-methods (filter (fn [{:keys [outputs] :as method}]
                               (every? free-vars outputs))
                             methods)]
    (apply min-key #(-> % :outputs count) free-methods)))

(defn plan [methods neighborhoods]
  (loop [satisfied #{}, unsatisfied methods]
    (if (empty? unsatisfied)
      satisfied
      (when-let [method (free-method unsatisfied neighborhoods)]
        (recur (conj satisfied method)
               (set/difference unsatisfied (neighborhoods method)))))))


(defn make-neighborhoods [constraints]
  (into {} (for [c constraints
                 :let [methods (:methods c)]
                 m methods]
             [m methods])))

(defn solve [required prioritized]
  (let [neighborhoods (make-neighborhoods (concat required prioritized))
        methods (set (mapcat :methods required))]
    (when-let [solution (plan methods neighborhoods)]
      (loop [solution solution
             satisfied methods
             unsatisfied prioritized]
        (if (empty? unsatisfied)
          solution
          (let [constraint (first unsatisfied)
                satisfied* (set/union satisfied (:methods constraint))
                unsatisfied* (next unsatisfied)]
            (if-let [solution* (plan satisfied* neighborhoods)]
              (recur solution* satisfied* unsatisfied*)
              (recur solution satisfied unsatisfied*))))))))

(defn stay-constraint [variable value]
  {:methods #{{:inputs #{}
               :outputs #{variable}
               :f (constantly {variable value})}}})

(defn evaluate [constraints values]
  (let [stays (map #(apply stay-constraint %) values)
        variables (into {} values)]
    (reduce (fn [variables {:keys [inputs outputs f] :as method}]
              (into variables (f variables)))
            variables
            (solve constraints stays))))



;; should sort methods by ascending number of constraints
(def midpoint-constraint
  {:variables '#{a b c}
   :methods #{{:inputs '#{a b}
               :outputs '#{c}
               :f (fn [{:syms [a b]}] {'c (- (* 2 b) a)})}
              {:inputs '#{b c}
               :outputs '#{a}
               :f (fn [{:syms [b c]}] {'a (- (* 2 b) c)})}
              {:inputs '#{a c}
               :outputs '#{b}
               :f (fn [{:syms [a c]}] {'b (/ ( + a c) 2.0)})}}})

(ns qplan.core
  "Provides a solver for multi-way, multi-output dataflow property models.
  See Bradley Vander Zanden (1996) and Freeman, Jarvi, & Smith (2009).

  The primary interface for this library is the evaluate function. See its
  docstring for usage.

  A dataflow problem can be represented by a hypergraph whose vertices are
  variables and whose hyperedges are constraints. However, here, dataflow
  problems are represented by a biparite graph whose vertices are variables
  and methods. A method is a function from some set of variables to some
  disjoint set of variables. Each constraint must specify some set of methods,
  a subset of which can be evaluated to satisfy the constraint. The directed
  edges of the biparite graph connect variables to the inputs of methods and
  then from the outputs of methods back to variables. In this representation,
  constraints need not be reified during solving. Instead, the notion of a
  method's neighborhood is used to represent all methods produced by the same
  constraint."
  (:require [clojure.set :as set]))

(defn conjs [set x]
  (conj (or set #{}) x))

(defn- free-method
  "Selects the best available free method from a given collection of methods
  and a mapping from method to a neighborhood of methods."
  [methods neighborhoods]
  ;;TODO: Can this work be optimized out of the plan loop?
  (let [outputs (for [method methods, variable (:outputs method)]
                  [variable (neighborhoods method)])
        ;; A variable is free if it can only be provided by one constraint.
        free-vars (->> outputs
                    (reduce (fn [sources [var neighborhood]]
                              (update-in sources [var] conjs neighborhood))
                            {})
                    (filter #(= (-> % val count) 1))
                    keys set)
        ;; A method is free, if all of its output variables are free.
        free-methods (filter (fn [{:keys [outputs] :as method}]
                               (every? free-vars outputs))
                             methods)]
    ;; Prefer methods which output fewer variables.
    (when (seq free-methods)
      (apply min-key #(-> % :outputs count) free-methods))))

(defn- plan
  "Returns a set of methods which can be traversed to to reassign each
  variable at most once. Exactly one method will be selected from each
  neighborhood represented in the methods set. Additional methods in the
  neighborhoods mapping will be ignored. Returns nil if no solution exists."
  [methods neighborhoods]
  (loop [satisfied #{}, unsatisfied methods]
    (if (empty? unsatisfied)
      satisfied
      (when-let [method (free-method unsatisfied neighborhoods)]
        (recur (conj satisfied method)
               (set/difference unsatisfied (neighborhoods method)))))))

(defn- make-neighborhoods
  "Given a collection of constraints, returns a mapping of each method to
  the set of methods owned by the same constraint."
  [constraints]
  (into {} (for [c constraints
                 :let [methods (:methods c)]
                 m methods]
             [m methods])))

(defn- solve
  "Given a collection of required constraints and a sequence of prioritized
  constraints, returns a solution, as per the plan function, optimized to
  maximize the number of satisfied prioritized constraints.
  Returns nil if one or more required constraints can not satisfied."
  [required prioritized]
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

(defn stay-constraint
  "Returns a constraint which binds some variable to a constant value."
  [variable value]
  {:methods #{{:inputs #{}
               :outputs #{variable}
               :f (constantly {variable value})}}})

(defn evaluate
  "Given a collection of constraints and a sequence of [variable value] pairs
  returns a map of variables to their new values. New values are selected such
  that all constraints have been satisfied and minimizing the number of
  changes subject to the prioritized order of the [variable value] pairs.
  If the constraints cannot be satisfied, the result map preserves all values."
  [constraints values]
  (let [stays (map #(apply stay-constraint %) values)
        variables (into {} values)]
    (reduce (fn [variables {:keys [inputs outputs f] :as method}]
              (into variables (f variables)))
            variables
            (solve constraints stays))))

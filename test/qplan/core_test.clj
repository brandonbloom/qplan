(ns qplan.core-test
  (:use clojure.test
        qplan.core))

(deftest midpoint-test
  (let [constraint {:variables '#{a b c}
                    :methods #{{:inputs '#{a b}
                                :outputs '#{c}
                                :f (fn [{:syms [a b]}]
                                     {'c (- (* 2 b) a)})}
                               {:inputs '#{b c}
                                :outputs '#{a}
                                :f (fn [{:syms [b c]}]
                                     {'a (- (* 2 b) c)})}
                               {:inputs '#{a c}
                                :outputs '#{b}
                                :f (fn [{:syms [a c]}]
                                     {'b (/ ( + a c) 2.0)})}}}]
    (testing "Pre-solved"
      (is (= '{a 10 b 20 c 30}
             (evaluate [constraint] '[[a 10] [b 20] [c 30]]))))
    (testing "Moved endpoint"
      (is (= '{a 15 b 20 c 25}
             (evaluate [constraint] '[[a 15] [b 20] [c 30]])))
      (is (= '{a 15 b 22.5 c 30}
             (evaluate [constraint] '[[a 15] [c 30] [b 20]]))))
    (testing "Moved midpoint"
      (is (= '{a 10 b 25 c 40}
             (evaluate [constraint] '[[b 25] [a 10] [c 30]])))
      (is (= '{a 20  b 25 c 30}
             (evaluate [constraint] '[[b 25] [c 30] [a 10]]))))))

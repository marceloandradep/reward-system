(ns reward-system.domain.reward-calculator-test
  (:require [clojure.test :refer :all]
            [reward-system.domain.reward-calculator :refer :all]
            [reward-system.graphs.graph :refer :all]))

(deftest calculate-reward-for-two-nodes
  (testing "Calculating points for two connected nodes."
    (let [graph (create-graph [["1" "2"]])
          reward (reward-graph graph)]
      (is (= [] (get-value-from-node reward "1")))
      (is (= [] (get-value-from-node reward "2"))))))

(deftest calculate-reward-for-three-nodes
  (testing "Calculating points for three connected nodes."
    (let [graph (create-graph [["1" "2"] ["2" "3"]])
          reward (reward-graph graph)]
      (is (= [1] (get-value-from-node reward "1")))
      (is (= [] (get-value-from-node reward "2")))
      (is (= [] (get-value-from-node reward "3"))))))

(deftest calculate-reward-for-four-nodes
  (testing "Calculating points for three connected nodes."
    (let [graph (create-graph [["1" "2"] ["2" "3"] ["1" "4"] ["4" "5"]])
          reward (reward-graph graph)]
      (is (= [2] (get-value-from-node reward "1")))
      (is (= [] (get-value-from-node reward "2")))
      (is (= [] (get-value-from-node reward "3")))
      (is (= [] (get-value-from-node reward "4")))
      (is (= [] (get-value-from-node reward "5"))))))

(deftest calculate-reward-for-full-graph
  (testing "Calculating points for full graph."
    (let [graph (create-graph [["1" "2"] ["1" "3"] ["3" "4"] ["2" "4"] ["4" "5"] ["4" "6"] ["6" "7"]])
          reward (reward-graph graph)]
      (is (= [2 1 1] (get-value-from-node reward "1")))
      (is (= [] (get-value-from-node reward "2")))
      (is (= [1 1] (get-value-from-node reward "3")))
      (is (= [1] (get-value-from-node reward "4")))
      (is (= [] (get-value-from-node reward "5")))
      (is (= [] (get-value-from-node reward "6")))
      (is (= [] (get-value-from-node reward "7"))))))

(deftest calculate-reward-for-cyclic-graph
  (testing "Calculating points for cyclic graph."
    (let [graph (create-graph [["1" "2"] ["2" "3"] ["3" "4"]])
          reward (reward-graph graph)]
      (is (= [1 1] (get-value-from-node reward "1"))))))

(deftest calculate-reward-values
  (testing "Calculating reward values."
    (let [graph (create-graph [["1" "2"] ["1" "3"] ["3" "4"] ["2" "4"] ["4" "5"] ["4" "6"] ["6" "7"]])
          reward (calculate-reward (reward-graph graph))]
      (is (= '(
                {:id "1" :score 2.75}
                {:id "3" :score 1.5}
                {:id "4" :score 1.0}
                {:id "2" :score 0.0}
                {:id "5" :score 0.0}
                {:id "6" :score 0.0}
                {:id "7" :score 0.0}) reward)))))

(deftest calculate-reward-values2
  (testing "Calculating reward values."
    (let [graph (create-graph [["49" "50"] ["50" "150"] ["50" "100"] ["50" "99"] ["50" "98"] ["50" "97"] ["50" "96"] ["50" "95"] ["96" "146"]])
          reward-graph (reward-graph graph)
          reward (calculate-reward reward-graph)]
      (is (= '(
                {:id "49" :score 1.5}
                {:id "50" :score 1.0}
                {:id "146" :score 0.0}
                {:id "96" :score 0.0}
                {:id "95" :score 0.0}
                {:id "98" :score 0.0}
                {:id "100" :score 0.0}
                {:id "99" :score 0.0}
                {:id "97" :score 0.0}
                {:id "150" :score 0.0}) reward)))))
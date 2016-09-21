(ns reward-system.graphs.graph-test
  (:require [clojure.test :refer :all]
             [reward-system.graphs.graph :refer :all]))

(deftest graph-with-single-edge
  (testing "Creates a graph with a single edge."
    (is (=
          {
           :1 {
               :id "1"
               :value []
               :reached false
               :edges [{:from "1" :to "2" :walkable true}]
               }
           :2 {
               :id "2"
               :value []
               :reached true
               }
           } (add-edge-to-graph "1" "2" {})))))

(deftest graph-with-many-edges
  (testing "Creates a graph with an arbitrary number of edges."
    (is (= {
            :1 {
                :id "1"
                :value []
                :reached true
                :edges [
                        {:from "1" :to "3" :walkable true}
                        {:from "1" :to "2" :walkable true}
                        ]
                }
            :2 {
                :id "2"
                :value []
                :reached true
                :edges [
                        {:from "2" :to "4" :walkable true}
                        ]
                }
            :3 {
                :id "3"
                :value []
                :reached true
                :edges [
                        {:from "3" :to "4" :walkable false}
                        ]
                }
            :4 {
                :id "4"
                :value []
                :reached true
                :edges [
                        {:from "4" :to "1" :walkable true}
                        ]
                }
            } (create-graph [["1" "2"] ["1" "3"] ["2" "4"] ["3" "4"] ["4" "1"]])))))

(deftest edges-must-be-one-direction
  (testing "Tests if the graph is preventing the client from adding edges in two directions."
    (is (= {
            :1 {
                :id "1"
                :value []
                :reached false
                :edges [{:from "1" :to "2" :walkable true}]
                }
            :2 {
                :id "2"
                :value []
                :reached true
                }
            } (create-graph [["1" "2"] ["2" "1"]])))))

(deftest when-obtain-root-nodes-return-only-unreached
  (testing "Pass an graph and receives just the unreached nodes."
    (let [graph (create-graph [["1" "2"] ["1" "3"] ["2" "4"] ["3" "4"]])]
      (is (= [(:1 graph)] (root-nodes graph))))))

(deftest setting-value-to-node-in-graph
  (testing "Setting a value to a node in a graph."
    (let [graph (create-graph [["1" "2"]])]
      (is (= {
              :1 {
                  :id "1"
                  :value []
                  :reached false
                  :edges [{:from "1" :to "2" :walkable true}]
                  }
              :2 {
                  :id "2"
                  :value [1]
                  :reached true
                  }
              } (set-value-to-node graph "2" [1]))))))

(deftest visiting-node
  (testing "Tests visitation to nodes in a graph."
    (let [graph (create-graph [["1" "2"]])
          visited-graph (visit graph (get-node graph "1"))]
      (is (:visited (get-node visited-graph "1"))))))

(deftest check-unvisited-edges
  (testing "Tests if a node has unvisited edges."
    (let [graph (create-graph [["1" "2"]])
          visited-graph (visit graph (get-node graph "2"))]
      (is (not (has-unvisited-edges? visited-graph (get-node visited-graph "1"))))
      (is (has-unvisited-edges? graph (get-node graph "1"))))))

(deftest retrieve-unvisited-edges
  (testing "Tests the retrieval of unvisited edges."
    (let [graph (create-graph [["1" "2"] ["1" "3"]])
          visited-graph (visit graph (get-node graph "2"))]
      (is (= [{:from     "1"
               :to       "3"
               :walkable true}] (unvisited-edges visited-graph (get-node visited-graph "1")))))))

(deftest finding-first-unvisited-node
  (testing "Tests the retrieval of the first unvisited node of a graph."
    (let [graph (create-graph [["1" "2"] ["1" "3"]])
          visited-graph (visit graph (get-node graph "1"))]
      (is (= (get-node visited-graph "2") (first-unvisited-node visited-graph))))))

(deftest finding-unvisited-node-when-all-visited
  (testing "Tests the retrieval of the first unvisited node of a graph."
    (let [graph (create-graph [["1" "2"] ["1" "3"]])
          visited-graph (reduce (fn [graph node] (visit graph node)) (vals graph))]
      (is (= [] (first-unvisited-node visited-graph))))))
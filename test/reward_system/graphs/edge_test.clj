(ns reward-system.graphs.edge-test
  (:require [clojure.test :refer :all]
            [reward-system.graphs.edge :refer :all]))

(deftest create-edge-from-pair
  (testing "Test the creation of an edge from a pair of values"
    (let [edge (create-edge "1" "2")]
      (is (= "1" (:from edge)))
      (is (= "2" (:to edge)))
      (is (walkable? edge)))))

(deftest create-not-walkable-edge
  (testing "Creates an edge not walkable."
    (let [edge (create-edge "1" "2" false)]
      (is (not (walkable? edge))))))

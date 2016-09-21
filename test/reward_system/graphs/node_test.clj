(ns reward-system.graphs.node-test
  (:require [clojure.test :refer :all]
            [reward-system.graphs.node :refer :all]))

(deftest create-node-from-an-id
  (testing "Test the creation of a node from an id value."
    (let [node (create-node "1")]
      (is (= "1" (:id node)))
      (is (= [] (:value node))))))

(deftest connect-two-nodes
  (testing "Testing the connection of two nodes through an edge"
    (let [from (create-node "1")
          to (create-node "2")
          connection (connect-nodes from to)]
      (is (= {:id "1" :value [] :reached false :edges [{:from "1" :to "2" :walkable true}]} connection)))))

(deftest are-nodes-connected
  (testing "Testing if a node is connected to another."
    (let [from (create-node "1")
          to (create-node "2")
          connected (connect-nodes from to)
          unconnected (create-node "3")]
      (is (are-connected? connected "2"))
      (is (not (are-connected? unconnected "2"))))))

(deftest adding-new-connections-to-a-node
  (testing "Testing the addition of new connections to a already connected node."
    (let [already-connected (connect-nodes (create-node "1") (create-node "2"))
          new-node (connect-nodes already-connected (create-node "3"))]
      (is (=
            {
             :id "1"
             :value []
             :reached false
             :edges [
                     {:from "1" :to "3" :walkable true}
                     {:from "1" :to "2" :walkable true}]
             } new-node)))))

(deftest when-two-nodes-already-connected-should-do-nothing
  (testing "Testing the case where two nodes are connected already and we attempt to connect it again."
    (let [already-connected (connect-nodes (create-node "1") (create-node "2"))
          new-node (connect-nodes already-connected (create-node "2"))]
      (is (= {:id "1" :value [] :reached false :edges [{:from "1" :to "2" :walkable true}]} new-node)))))

(deftest reach-node
  (testing "Reaching a node."
    (let [node (create-node "1")
          reached-node (reach node)]
      (is (:reached reached-node)))))

(deftest node-has-edges
  (testing "Tests if a node has edges going out."
    (let [connected-node (connect-nodes (create-node "1") (create-node "2"))
          unconnected-node (create-node "1")]
      (is (has-edges? connected-node))
      (is (not (has-edges? unconnected-node))))))

(deftest set-value-to-node
  (testing "Tests setting a value to a node."
    (is (= {:id "1" :value [1] :reached false} (set-value (create-node "1") [1])))))

(deftest merging-collections
  (testing "Merging collections."
    (is (= [] (merge-collections [] [])))
    (is (= [1] (merge-collections [] [1])))
    (is (= [1] (merge-collections [0] [1])))
    (is (= [2 1] (merge-collections [1 1] [1])))
    (is (= [5 2 4 0 1 1] (merge-collections [3 1 2 0 1 1] [2 1 2])))))

(deftest merge-value-of-node
  (testing "Tests merging a value into the node's value."
    (let [node (set-value (create-node "1") [3 1 2 0 1 1])]
      (is (= {:id "1" :value [5 2 4 0 1 1] :reached false} (merge-value node [2 1 2]))))))
(ns reward-system.graphs.graph
  (:require [reward-system.graphs.edge :refer :all]
            [reward-system.graphs.node :refer :all]))

(defn add-edge-to-graph
  "Receives a pair of values, creates two connected nodes and adds it to an existing graph."
  [from to graph]
  (let [node-to (get graph (keyword to) (create-node to))
        node-from (get graph (keyword from) (create-node from))]
    (if (not (are-connected? node-to from))
      (assoc graph
        (keyword from) (connect-nodes node-from node-to)
        (keyword to) (reach node-to))
      graph)))

(defn create-graph
  "Creates an graph from a list of pairs representing the graph's edges."
  [pairs]
  (reduce (fn [graph [from to]]
            (add-edge-to-graph from to graph))
          {}
          pairs)
  )

(defn root-nodes
  "Returns a list of all unreached nodes in a graph."
  [graph]
  (filter #(not (:reached %)) (map (fn [[_ node]] node) graph)))

(defn get-node
  "Returns the node specified by node-id from graph."
  [graph node-id]
  (get graph (keyword node-id)))

(defn set-value-to-node
  "Returns a graph with a value setted to a node."
  [graph node-id value]
  (assoc graph
    (keyword node-id)
    (set-value (get-node graph node-id) value)))

(defn merge-value-to-node
  "Returns a graph with a value merged into the value of a existing node."
  [graph node-id value]
  (assoc graph
    (keyword node-id)
    (merge-value (get-node graph node-id) value)))

(defn get-value-from-node
  "Returns the value of a node specified by node-id"
  [graph node-id]
  (:value (get-node graph node-id)))

(defn visit
  "Mark a node as visited."
  [graph node]
  (assoc graph (keyword (:id node)) (assoc node :visited true)))

(defn has-unvisited-edges?
  "Returns true if there is some edge pointing to an unvisited node."
  [graph node]
  (some #(not (:visited (get-node graph (:to %)))) (:edges node)))

(defn unvisited-edges
  "Returns only the edges pointing to unvisited nodes."
  [graph node]
  (filter #(not (:visited (get-node graph (:to %)))) (:edges node)))

(defn first-unvisited-node
  "Returns the first node that has not been visited."
  [graph]
  (some #(and (not (:visited %)) %) (vals graph)))
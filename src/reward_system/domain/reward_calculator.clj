(ns reward-system.domain.reward-calculator
  (:require [reward-system.graphs.graph :refer :all]
            [reward-system.graphs.node :refer :all]))

(defn reward-graph
  ([graph]
   (loop [output-graph graph]
     (let [node (first-unvisited-node output-graph)]
       (if (empty? node)
         output-graph
         (let [visited-graph (visit output-graph node)
               visited-node (get-node visited-graph (:id node))
               reward-graph (reward-graph visited-graph visited-node)]
           (recur (if reward-graph reward-graph visited-graph)))))))
  ([graph node]
   (if (has-edges? node)
     ;; node is not a leaf
     (reduce
       (fn [graph edge]
         ;; a walkable edge is an edge that we should walk recursively to calculate the rewards
         (if (:walkable edge)
           (if (:visited (get-node graph (:to edge)))
             ;; is a node is already visited just use the value already calculated
             (let [to-value (get-value-from-node graph (:to edge))]
               (if to-value
                 (merge-value-to-node graph (:id node) (concat [1] to-value))
                 graph))

             ;; is a node is not visited we have to calculate its subgraph
             (let [to-id (:to edge)
                   visited-graph (visit graph (get-node graph to-id))
                   to (get-node visited-graph to-id)
                   subgraph (reward-graph visited-graph to)
                   to-value (get-value-from-node subgraph to-id)]
               (if to-value
                 (merge-value-to-node subgraph (:id node) (concat [1] (get-value-from-node subgraph to-id)))
                 visited-graph)))

           ;; if it is not walkable then we just return the graph as it is
           graph)) graph (:edges node))

     ;; return nil to indicate that it is a leaf node
     (set-value-to-node graph (:id node) nil))))

(defn calculate-points
  [coll]
  (loop [points 0
         factor 1
         [first & remaining] coll]
    (if first
      (recur (+ points (* factor first)) (/ factor 2) remaining)
      points)))

(defn calculate-reward
  "Calculates the rewards based on a list of invites."
  [graph]
  (sort-by :score > (map #(hash-map :id (:id %), :score (double (calculate-points (:value %)))) (vals graph))))

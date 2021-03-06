(ns reward-system.graphs.edge)

(defn create-edge
  "Create an edge map from a pair of integer values"
  ([from to]
   {:from from :to to :walkable true})
  ([from to walkable]
   {:from from :to to :walkable walkable}))

(defn walkable?
  "Tests if an edge should be visited while walking in a graph."
  [edge]
  (:walkable edge))
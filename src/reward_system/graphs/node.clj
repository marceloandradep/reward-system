(ns reward-system.graphs.node
  (:require [reward-system.graphs.edge :refer :all]))

(defn create-node
  "Creates a node with an id and a default value"
  [id]
  (hash-map :id id :value [] :reached false))

(defn are-connected?
  "Test if a node is already connected to another"
  [from to]
  (some #(= (:to %) to) (:edges from)))

(defn reached?
  "Test if a node is reached by another."
  [node]
  (:reached node))

(defn reach
  [node]
  (assoc node :reached true))

(defn connect-nodes
  "Creates a map representing the connections of two nodes"
  [from to]
  (if (not (are-connected? from (:id to)))
    (let [new-edge (create-edge (:id from) (:id to) (not (reached? to)))
          out-edges (conj (:edges from) new-edge)]
      (assoc from :edges out-edges))
    from))

(defn has-edges?
  "Returns true if a node has edges goint out."
  [node]
  (let [edges (get node :edges [])]
    (not (empty? edges))))

(defn set-value
  "Returns a node with the value setted."
  [node value]
  (assoc node :value value))

(defn merge-collections
  "Merges two collections with reward points summing each corresponding items.
  Ex. [1] merged with [1 2] returns [2 2]"
  [coll-a coll-b]
  (loop [[first-a & remaining-a] coll-a
         [first-b & remaining-b] coll-b
         merged []]
    (if (and first-a first-b)
      (recur remaining-a
             remaining-b
             (into merged [(+ first-a first-b)]))
      (let [count-a (count coll-a)
            count-b (count coll-b)]
        (if (> count-a count-b)
          (into merged (drop (count merged) coll-a))
          (into merged (drop (count merged) coll-b)))))))

(defn merge-value
  "Returns a node with its value merged with the passed value argument."
  [node value]
  (set-value node (merge-collections (:value node) value)))
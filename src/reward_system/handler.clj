(ns reward-system.handler
  (:require [clojure.java.io :refer :all]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response]]
            [reward-system.graphs.graph :refer :all]
            [reward-system.domain.reward-calculator :refer :all]
            [clojure.string :as str]))

(def graph {})

(defn ranking []
  (response (calculate-reward (reward-graph graph))))

(defn invite [from to]
  (def graph (add-edge-to-graph from to graph))
  {:status 200})

(defroutes app-routes
           (GET "/ranking" [] (ranking))
           (PUT "/invite/:from/:to" [from to] (invite from to))
           (route/not-found "Not Found"))

(defn init
  []
  (with-open [rdr (reader (resource "input.txt"))]
    (def graph (create-graph (reduce (fn [acc line] (concat acc [(str/split line #" ")])) [] (line-seq rdr))))))

(def app
  (-> app-routes
      wrap-json-body
      wrap-json-response))

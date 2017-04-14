(ns path-finding.path-finding
  (:require [path-finding.map-loading :as ml]))

(def tilemap (ml/load-map-from-file "maps/ambiente.txt"))

(defn tilemap-size []
  (count tilemap))

(defn clamp-to-map [x]
  (max 0 (min x (tilemap-size))))

(defn is-within-map? [[x y]]
  (not (or (< x 0)
           (< y 0)
           (>= x (tilemap-size))
           (>= y (tilemap-size)))))

(defn get-tile-cost [[x y]]
  (get {0 1
        1 5
        2 10
        3 15}
       (get-in tilemap [y x])))

(defn get-neighbors [[x y]]
  (filter #(and (not= [x y] %)
                (is-within-map? %))
          (distinct [[x (dec y)]
                     [(inc x) y]
                     [x (inc y)]
                     [(dec x) y]])))

(defn pseudo-search [from to]
  (loop [visited [from] current from]
    (if (= (last visited) to)
      visited
      (do
        (Thread/sleep 1)
        (let [neighbors (get-neighbors current)
              not-visited-neighbors (remove (fn [i] (some #(= % i) visited)) neighbors)]
          (println current ":" not-visited-neighbors)
          (if (empty? not-visited-neighbors)
            visited
            (let [to-visit (apply min-key get-tile-cost not-visited-neighbors)]
              (recur (conj visited to-visit) to-visit))))))))

;;  procedure DFS-iterative(G,v):
;;      let S be a stack
;;      S.push(v)
;;      while S is not empty
;;          current = S.pop()
;;          set v as visited
;;          for all edges from v to w in G.adjacentEdges(v) do
;;              if w was not visited:
;;                  S.push(w)

(defn dfs [from to]
  (loop [visited []
         to-visit [from]]
    (let [current (last to-visit)]
      (if (or (= current to) (empty? to-visit))
        (conj visited current)
        (let [neighbors (get-neighbors current)
              not-visited-neighbors (remove (fn [i] (some #(= % i) visited)) neighbors)]
          (recur (conj visited current)
                 (concat (butlast to-visit) (reverse not-visited-neighbors))))))))

(defn calculate-path [from to]
  (dfs from to))

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

(defn remove-in [target to-remove]
  (remove (fn [i] (some #(= % i) to-remove)) target))

(defn path-from-parents [target cells parents]
  (loop [path [target]]
    (let [current (first path)
          parent (nth parents (.indexOf cells current))]
      (if (nil? parent)
        path
        (recur (cons parent path))))))

(defn dfs [from to]
  (loop [discovered []
         parents [nil]
         to-visit [from]]
    (let [current (last to-visit)]
      (if (or (= current to) (empty? to-visit))
        {:path (path-from-parents current (conj discovered current) parents)
         :discovered (conj discovered current)
         :leaves to-visit}
        (let [neighbors (get-neighbors current)
              not-visited-neighbors (remove-in neighbors discovered)]
          (recur (conj discovered current)
                 (conj parents current)
                 (concat (butlast to-visit) (reverse not-visited-neighbors))))))))


(defn bfs [from to]
  (loop [discovered [from]
         parents [nil]
         to-visit [from]]
    (let [current (first to-visit)]
      (if (or (= current to) (empty? to-visit))
        (do
          {:path (path-from-parents current discovered parents)
           :discovered discovered
           :leaves to-visit})
        (let [neighbors (get-neighbors current)
              not-discovered-neighbors (remove-in neighbors discovered)]
          (recur (concat discovered not-discovered-neighbors)
                 (concat parents (into [] (repeat (count not-discovered-neighbors) current)))
                 (concat (rest to-visit) not-discovered-neighbors)))))))

(defn iddfs [from to]
  (loop [discovered []
         parents [nil]
         to-visit [from]]
    (let [current (last to-visit)]
      (if (or (= current to) (empty? to-visit))
        {:path (path-from-parents current (conj discovered current) parents)
         :discovered (conj discovered current)
         :leaves to-visit}
        (let [neighbors (get-neighbors current)
              not-visited-neighbors (remove-in neighbors discovered)]
          (recur (conj discovered current)
                 (conj parents current)
                 (concat (butlast to-visit) (reverse not-visited-neighbors))))))))

(defn calculate-path [from to]
  (dfs from to))


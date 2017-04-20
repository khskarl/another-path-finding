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

;; function IDDFS(root)
;;   for depth from 0 to ∞
;;     found ← DLS(root, depth)
;;     if found ≠ null
;;       return found

;; function DLS(node, depth)
;;   if depth = 0 and node is a goal
;;     return node
;;   if depth > 0
;;     foreach child of node
;;       found ← DLS(child, depth−1)
;;     if found ≠ null
;;       return found
;;   return null

(defn get-value [node]
  (:value node))

(defn get-depth [node]
  (:depth node))

(defn get-values [nodes]
  (map #(:value %) nodes))

(defn get-depths [nodes]
  (map #(:depth %) nodes))

(defn iterative-iddfs
  ([from to]
   (loop [max-depth 0]
     (let [result (iterative-iddfs from to max-depth)]
       (if (or (nil? result) (> max-depth 50000))
         (recur (inc max-depth))
         result))))
  ([from to max-depth] 
   (loop [discovered []
          parents [nil]
          to-visit [{:value from :depth 0}]] 
     (let [current (last to-visit)]
       ;; (println current (last parents))
       (cond
         (= (get-value current) to)
         (do
           {:path (path-from-parents (:value current)
                                     (get-values (conj discovered current))
                                     (get-values parents))
            :discovered (get-values (conj discovered current))
            :leaves (get-values to-visit)})
         (empty? to-visit)
         nil
         :else
         (if (= (get-depth current) max-depth)
           (recur discovered
                  parents
                  (butlast to-visit))
           (let [neighbors (get-neighbors (get-value current))
                 not-visited-neighbors (remove-in neighbors (get-values discovered))]
             (recur (conj discovered current)
                    (conj parents current)
                    (concat (butlast to-visit)
                            (map #(assoc {:value %} :depth (inc (get-depth current)))
                                 (reverse not-visited-neighbors)))))))))))

(iterative-iddfs [0 0] [10 10])

({:value 10 :depth 20})
(assoc {:value 10} :depth 20)
(defn dls [current to depth]
  (if (and (= depth 0) (= current to))
    current
    (if (< depth 0)
      (let [neighbors (get-neighbors current)]
        (first (filter #(not (nil? %)) (map #(dls % to (dec depth))))))
      nil)))

(first (filter #(not (nil? %)) [nil nil]))

(defn iddfs [from to]
  (loop [depth 0]
    (if (> depth 5000)
      )))

(defn calculate-path [from to]
  (iterative-iddfs from to))


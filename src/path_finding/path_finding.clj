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

(defn calculate-path-cost [path]
  (reduce + (map get-tile-cost path)))

(defn get-neighbors [[x y]]
  (filter #(and (not= [x y] %)
                (is-within-map? %))
          (distinct [[x (dec y)]
                     [(inc x) y]
                     [x (inc y)]
                     [(dec x) y]])))

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

(defn get-values [nodes]
  (pmap #(:value %) nodes))

(defn get-depths [nodes]
  (pmap #(:depth %) nodes))

(defn iddfs
  ([from to]
   (loop [max-depth 0]
     (let [result (iddfs from to max-depth)]
       (if (or (nil? result) (> max-depth 50000))
         (recur (inc max-depth))
         result))))
  
  ([from to max-depth] 
   (loop [discovered []
          parents []
          to-visit [{:value from :depth 0 :parent nil}]] 
     (let [current (last to-visit)]
       ;; (println current (last parents))
       (cond
         (= (:value current) to)
         (do
           ;; (println "Found:" current
           ;;          "\nDiscovered:" discovered
           ;;          "\nParents:     " parents
           ;;          "\nTo Visit:" to-visit)
           {:path (path-from-parents (:value current)
                                     (conj discovered (:value current))
                                     (conj parents (:parent current)))
            :discovered (conj discovered (:value current))
            :leaves (get-values to-visit)})
         (empty? to-visit)
         nil
         (some #(= % (:value current)) discovered)
         (recur discovered
                parents
                (butlast to-visit))
         :else
         (if (= (:depth current) max-depth)
           (recur (conj discovered (:value current))
                  (conj parents (:parent current))
                  (butlast to-visit))
           (let [neighbors (get-neighbors (:value current))
                 not-visited-neighbors (remove-in neighbors discovered)]
             (recur (conj discovered (:value  current))
                    (conj parents    (:parent current))
                    (concat (butlast to-visit)
                            (pmap #(assoc {:value % :parent (:value current)}
                                          :depth (inc (:depth current)))
                                  (reverse (remove-in not-visited-neighbors to-visit))))))))))))

(defn ucs [from to]
  (loop [discovered [from]
         parents [nil]
         to-visit [{:value from :cost (get-tile-cost from) :parent nil}]]
    (let [current (first to-visit)]
      (if (= (:value current) to)
        {:path (path-from-parents (:value current)
                                  discovered
                                  parents)
         :discovered discovered
         :leaves [0 0]}
        (let [neighbors (get-neighbors (:value current))
              not-visited-neighbors (remove-in neighbors discovered)]
          (recur (concat discovered not-visited-neighbors)
                 (concat parents (repeat (count not-visited-neighbors) (:value current)))
                 (sort-by #(:cost %)
                          (concat (rest to-visit)
                                  (pmap #(into {} [{:value %}
                                                   {:cost (+ (:cost current) (get-tile-cost %))}
                                                   {:parent (:value current)}])
                                        not-visited-neighbors)))))))))

(defn calculate-path [from to]
  (ucs from to))


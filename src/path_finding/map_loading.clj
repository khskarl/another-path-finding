(ns path-finding.map-loading)

(def path "maps/ambiente.txt")

(defn load-map-from-file [filepath]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (mapv #(mapv read-string (clojure.string/split % #" "))
          (apply vector (line-seq rdr)))))

(load-map-from-file path)

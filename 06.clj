(defn winning-moves [[duration distance]]
  (filter
    #(> % distance)
    (map (fn [t]
           (* (- duration t) t))
         (range 1 duration))))

(def input-ex
  [[7 9] [15 40] [30 200]])

(def input
  [[46 214] [80 1177] [78 1402] [66 1024]])

(defn part-one []
  (println (apply * (map #(count (winning-moves %)) input))))

(def input-two-ex
  [[71530 940200]])

(def input-two
  [[46807866 214117714021024]])

(defn part-two []
  (println (apply * (map #(count (winning-moves %)) input-two))))

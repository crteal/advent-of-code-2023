(defn create-game [line]
  (->> (-> line
           (clojure.string/split #": ")
           last
           (clojure.string/split #";"))
       (map #(clojure.string/split % #","))
       (map (fn [s]
              (map #(clojure.string/split (clojure.string/trim %) #" ") s)))
       flatten
       (partition 2)))

(def counts
  {"red" 12
   "blue" 14
   "green" 13})

(defn part-one []
  (with-open [reader (clojure.java.io/reader "./resources/2.txt")]
    (println (reduce (fn [{:keys [idx sum]} line]
                       (let [game (create-game line)
                             filtered (filter (fn [[n color]]
                                                (<= (Integer/parseInt n)
                                                    (get counts color)))
                                              game)]
                         {:idx (inc idx)
                          :sum (cond-> sum
                                 (= (count game) (count filtered)) (+ idx))}))
                     {:idx 1 :sum 0}
                     (line-seq reader)))))

(defn part-two []
  (with-open [reader (clojure.java.io/reader "./resources/2.txt")]
    (println (reduce (fn [sum line]
                       (let [game (create-game line)
                             maximums (reduce (fn [memo [n color]]
                                                (update memo color #(max (Integer/parseInt n) %)))
                                              {"red" 0 "blue" 0 "green" 0}
                                              game)]
                         (+ sum (apply * (vals maximums)))))
                     0
                     (line-seq reader)))))

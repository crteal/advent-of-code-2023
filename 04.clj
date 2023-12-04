(require '[clojure.math])
(require '[clojure.set])

(defn score-card [card]
  (let [[winners values] (map (fn [s]
         (set (map #(Integer/parseInt %)
                   (filter not-empty
                           (map #(clojure.string/trim %)
                                (clojure.string/split s #"\s"))))))
       (-> card
           (clojure.string/split #": ")
           last
           (clojure.string/split #" \| ")))
        held (count (clojure.set/intersection winners values))]
    (if-not (zero? held)
      (clojure.math/pow 2 (dec held))
      0)))

(defn part-one []
  (with-open [reader (clojure.java.io/reader "./resources/4.txt")]
    (println (apply + (map score-card (line-seq reader))))))

(defn total-cards [cards]
  (let [card-wins (map-indexed (fn [n card]
                                 (let [[winning played] (map (fn [s]
                                                               (set (map #(Integer/parseInt %)
                                                                         (filter not-empty
                                                                                 (map #(clojure.string/trim %)
                                                                                      (clojure.string/split s #"\s"))))))
                                                             (-> card
                                                                 (clojure.string/split #": ")
                                                                 last
                                                                 (clojure.string/split #" \| ")))]
                                   {:number n :matching (count (clojure.set/intersection winning played))}))
                               cards)]
    (reduce (fn [memo {:keys [number matching]}]
              (merge-with + memo
                (reduce (fn [m n]
                          (let [copy-number (+ number n)]
                            (assoc m copy-number (get memo number))))
                        {}
                        (range 1 (inc matching)))))
            (zipmap (map :number card-wins)
                    (repeat 1))
            card-wins)))

(defn part-two []
  (with-open [reader (clojure.java.io/reader "./resources/4.txt")]
    (println (apply + (vals (total-cards (line-seq reader)))))))

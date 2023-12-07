(defn parse-map [input]
  (let [lines (clojure.string/split input #"\n")
        ranges (map (fn [line]
                      (map #(Long/parseLong %)
                           (clojure.string/split line #" ")))
                    (rest lines))]
    (fn [n]
      (reduce (fn [y [dest src length]]
                (if (or (< n src) (> n (+ length src)))
                  y
                  (reduced (+ dest (- n src)))))
              n
              ranges))))

(defn parse-input []
  (let [input (slurp "./resources/5.txt")
        sections (clojure.string/split input #"\n\n")
        seeds (map #(Long/parseLong %)
                   (-> (first sections)
                       (clojure.string/split #": ")
                       last
                       (clojure.string/split #" ")))]
    (reduce #(map %2 %1)
            seeds
            (map parse-map (rest sections)))))

(defn part-one []
  (println (apply min (parse-input))))

; NOTE this is excruciatingly slow, therefore must be profoundly wrong
(defn seeds-to-location []
  (let [input (slurp "./resources/5.txt")
        sections (clojure.string/split input #"\n\n")
        seeds (partition 2 (map #(Long/parseLong %)
                   (-> (first sections)
                       (clojure.string/split #": ")
                       last
                       (clojure.string/split #" "))))
        maps (map parse-map (rest sections))]
    (flatten (pmap (fn [[seed-start length]]
                     (println "seed range" seed-start length)
            (let [seed-end (+ seed-start length)]
              (loop [i seed-start
                      m Long/MAX_VALUE]
                 (if (= i seed-end)
                   m
                   (let [x (reduce #(%2 %1) i maps)]
                     (recur (inc i)
                            (min x m)))))))
          seeds))))

(defn part-two []
  (println (apply min (seeds-to-location))))

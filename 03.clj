(require '[clojure.set])

(defn build-engine [reader]
  (let [schematic (map-indexed (fn [y line]
                                 (map-indexed (fn [x c]
                                                (when-not (= c \.)
                                                  (let [n (Character/digit c 10)]
                                                    {:x x
                                                     :y y
                                                     :c c
                                                     :n n
                                                     :number? (not= n -1)})))
                                              line))
                               (line-seq reader))]
    {:height (count schematic)
     :width (count (first schematic))
     :schematic schematic}))

(defn get-coord [{:keys [schematic width height]} {:keys [x y]}]
  (when (and (>= x 0) (< x width)
             (>= y 0) (< y height))
    (nth (nth schematic y) x)))

(defn engine-number? [pos]
  (and (some? pos)
       (get pos :number?)))

(defn get-adjacent [schematic pos]
  (filter engine-number?
          (map #(get-coord schematic %)
               (map #(merge-with + pos %)
                    [{:y -1} {:x 1 :y -1} {:x 1} {:x 1 :y 1}
                     {:y 1} {:x -1 :y 1} {:x -1} {:x -1 :y -1}]))))

(defn get-numbers-from [schematic dir {:keys [x y]}]
  (let [next-x (+ x dir)
        next-coord {:x next-x :y y}
        pos (get-coord schematic next-coord)]
    (if (engine-number? pos)
      (conj (get-numbers-from schematic dir next-coord) (:c pos))
      '())))

(defn engine-symbol? [pos]
  (and (some? pos)
       (not (engine-number? pos))))

(defn get-part-numbers [schematic]
  (reduce (fn [memo pos]
            (concat memo
              (set (map (fn [num-pos]
                          (Integer/parseInt (apply str (concat (reverse (get-numbers-from schematic -1 num-pos))
                                                               `(~(:c num-pos))
                                                               (get-numbers-from schematic 1 num-pos)))))
                        (get-adjacent schematic pos)))))
          '() ; NOTE this was a set, but the schematic contains duplicates, :(
          (filter engine-symbol? (flatten (:schematic schematic)))))

(defn part-one []
  (with-open [reader (clojure.java.io/reader "./resources/3.txt")]
    (println (apply + (get-part-numbers (build-engine reader))))))

(defn engine-gear-symbol? [pos]
  (and (engine-symbol? pos)
       (= (:c pos) \*)))

(defn get-gear-ratios [schematic]
  (reduce (fn [memo pos]
            (let [parts (set (map (fn [num-pos]
                                    (Integer/parseInt (apply str (concat (reverse (get-numbers-from schematic -1 num-pos))
                                                                         `(~(:c num-pos))
                                                                         (get-numbers-from schematic 1 num-pos)))))
                                  (get-adjacent schematic pos)))]
              (cond-> memo
                (= (count parts) 2) (conj (apply * parts)))))
          '()
          (filter engine-gear-symbol? (flatten (:schematic schematic)))))

(defn part-two []
  (with-open [reader (clojure.java.io/reader "./resources/3.txt")]
    (println (apply + (get-gear-ratios (build-engine reader))))))

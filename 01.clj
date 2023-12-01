(defn calibration-value [line]
  (let [nums (reduce (fn [nums c]
                       (let [i (Character/digit c 10)]
                         (cond-> nums
                           (>= i 0) (conj i))))
                     '()
                     line)]
    (Integer/parseInt (str (last nums) (first nums)))))

(defn part-one []
  (with-open [reader (clojure.java.io/reader "./resources/1.txt")]
    (println (reduce #(+ %1 (calibration-value %2)) 0 (line-seq reader)))))

(def words-to-num
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(def word-regex #"(one|two|three|four|five|six|seven|eight|nine)$")

(defn calibration-value-2 [line]
  (let [{:keys [nums]} (reduce (fn [{:keys [nums stack]} c]
                                 (let [i (Character/digit c 10)]
                                   (if (>= i 0)
                                     {:stack ""
                                      :nums (conj nums i)}
                                     (let [next-stack (str stack c)
                                           match (re-find word-regex next-stack)]
                                       {:stack next-stack
                                        :nums (cond-> nums
                                                match (conj (get words-to-num (last match))))}))))
                               {:stack "" :nums '()}
                               line)]
    (Integer/parseInt (str (last nums) (first nums)))))

(defn part-two []
  (with-open [reader (clojure.java.io/reader "./resources/1.txt")]
    (println (reduce #(+ %1 (calibration-value-2 %2)) 0 (line-seq reader)))))

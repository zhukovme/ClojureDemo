(ns demo.core
  (:import (java.util Date))
  (:gen-class))

(defn count_down
  [n]
  (if (zero? n)
    (println :BOOM!)
    (do (println n)
        (recur (dec n)))))

(def only_strings (partial filter string?))

(defn doubler
  [f]
  (fn [& args]
    (* 2 (apply f args))))

(defn print_logger
  [writer]
  #(binding [*out* writer]
    (println %)))

(def *out*_logger (print_logger *out*))

(defn file_logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
    ((print_logger f) %)))

(def log->file (file_logger "messages.log"))

(defn multi_logger
  [& logger-fns]
  #(doseq [f logger-fns]
    (f %)))

(def log (multi_logger
           *out*_logger
           log->file))

(defn timestamped_logger
  [logger]
  #(logger (format "[%1$te-%1$tm-%1$tY %1$tH:%1$tM:%1$tS] %2$s"
                   (Date.) %)))

(def log_timestamped (timestamped_logger log))

(defn swap_pairs
  [sequential]
  (into (empty sequential)
        (interleave
          (take-nth 2 (drop 1 sequential))
          (take-nth 2 sequential))))

(defn map_map
  [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))

(defn random_ints
  [limit]
  (lazy-seq
    (cons (rand-int limit)
          (random_ints limit))))

(defn magnitude
  [x]
  (-> x Math/log10 Math/floor))

(defn compare_magnitude
  [a b]
  (let [dif (- (magnitude a) (magnitude b))]
    (if (zero? dif)
      (compare a b)
      dif)))

(defn euclidian_division
  [x y]
  [(quot x y) (rem x y)])

(defn numeric?
  [s]
  (every? (set "0123456789") s))

(def playlist
  [{:title "Elephant", :artist "The White Stripes", :year 2003}
   {:title "Helioself", :artist "Papas Fritas", :year 1997}
   {:title "Stories from the City, Stories from the Sea", :artist "PJ Harvey", :year 2000}
   {:title "Buildings and Grounds", :artist "Papas Fritas", :year 2000}
   {:title "Zen Rodeo", :artist "Mardi Gras BB", :year 2002}])

(defn summarize
  [{:keys [title artist year]}]
  (str title " / " artist " / " year))

(defn why
  [m]
  (println (map summarize m))
  ((comp println summarize) {:title "test_test", :artist "test", :year 0})
  (map #((comp println summarize) %) m)
  (map #(println (summarize %)) m))

(defn -main
  [& args]
  (why playlist))

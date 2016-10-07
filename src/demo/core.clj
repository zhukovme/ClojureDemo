(ns demo.core
  (:import (java.util Date)
           (java.awt Dimension BasicStroke Graphics2D Graphics)
           (javax.swing JPanel JFrame))
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

(defn reduce_by
  [key_fn f init coll]
  (reduce (fn [summaries x]
            (let [k (key_fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))

(defn empty_board
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  [board living_cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living_cells))

(def glider (populate (empty_board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count_neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed_step
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new_board board x 0 y 0]
      (cond
        (>= x w) new_board
        (>= y h) (recur new_board (inc x) 0)
        :else
        (let [new_liveness
              (case (count_neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new_board [x y] new_liveness) x (inc y)))))))

(defn indexed_step2
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new_board x]
        (reduce
          (fn [new_board y]
            (let [new_liveness
                  (case (count_neighbours board [x y])
                    2 (get-in board [x y])
                    3 :on
                    nil)]
              (assoc-in new_board [x y] new_liveness)))
          new_board (range h)))
      board (range w))))

(defn indexed_step3
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new_board [x y]]
        (let [new_liveness
              (case (count_neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (assoc-in new_board [x y] new_liveness)))
      board
      (for [x (range h) y (range w)] [x y]))))

(defn window
  ([coll] (window nil coll))
  ([pad coll]
  (partition 3 1 (concat [pad] coll [pad]))))

(defn cell_block
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step_row
  [row_triple]
  (vec (map liveness (cell_block row_triple))))

(defn index_free_step
  [board]
  (vec (map step_row (window (repeat nil) board))))

(defn step
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn stepper
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))

(defn hex_neighbours
  [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])]
    [(+ dx x) (+ dy y)]))

(def hex_step (stepper hex_neighbours #{2} #{3 4}))

(defn maze
  [walls]
  (let  [paths (reduce (fn [index [a b]]
                         (merge-with into index {a [b] b [a]}))
                       {} (map seq walls))
         start_loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start_loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walls) (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))

(defn grid
  [w h]
  (set (concat
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn draw
  [w h maze]
  (doto (JFrame. "Maze")
    (.setContentPane
      (doto (proxy [JPanel] []
              (paintComponent [^Graphics g]
                (let [g (doto ^Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc))))))
        (.setPreferredSize (Dimension.
                             (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(defn -main
  [& args]
  (draw 40 40 (maze (grid 40 40))))

(ns ants.core)

(def board ["..*.%"
            "a?.b."])

(def game {:board board
           :loadtime 2000  
           :turntime 2000  
           :rows 20  
           :cols 20  
           :turns 500  
           :viewradius2 93  
           :attackradius2 6  
           :spawnradius2 6 })

(defn ant?
  ([c]
     (not (#{\. \* \% \?} c)))
  ([c owner]
     (and (ant? c)
          (= owner c))))

(defn water? [c]
  (= \% c))

(defn food? [c]
  (= \* c))

(defn unknown? [c]
  (= \? c))

(defn get-cell [board r c]
  (get-in board [r c]))

(defn index [x]
  (partition 2 (interleave x (range (count x)))))

(defn locations [board predicate]
  (let [rows (count board)
        cols (count (first board))]
    (for [r (range rows)
          c (range cols) :when (predicate (get-cell board r c))]
      [r c])))

(locations board #(ant? % \b))

(defn next-move [board row col]
  "N")

(for [[r c] (locations board #(ant? % \a))]
  (str "o " r " " c " " (next-move board r c)))

(for [[r c] (locations board #(ant? % \a))]
  (str "o " r " " c " " (next-move board r c)))

(for [[r c] (locations board #(ant? % \a))]
  [r c (next-move board r c)])

(defn new-board [moves]
  (let [moves [[1 0 "N"]]]
    ))

(defn empty-board [rows cols]
  (vec (repeat rows (apply str (repeat cols \.)))))

(defn apply-fact [board [type r c owner]]
  (assoc-in board [r c] type))

(defn new-board [facts rows cols]
  (reduce apply-fact (empty-board rows cols) facts))

(new-board [[\a 7 9 1]] 20 10)

(defn dump [board]
  (apply str (interleave board (repeat \newline))))

(print (dump board))
..*.%
a?.b.


(locations board food?)

(() ([1 0] [1 3]))
((nil nil nil nil nil) ([1 0] nil nil [1 3] nil))

board
["..*.%" "a?.b."]




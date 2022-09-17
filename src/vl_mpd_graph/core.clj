(ns vl-mpd-graph.core
  (:require [libcdb.core :as db]
            [libcdb.configure :as cf]
            [tangle.core :as tangle]
            [clojure.string :as string]
            [clojure.java.io :as io]))


(defn get-mpd [id]
  (->> {:prot "http",
        :host "localhost",
        :port 5984,
        :usr (System/getenv "CAL_USR")
        :pwd (System/getenv "CAL_PWD")
        :name "vl_db_work"}
       cf/config
       (db/get-doc id)))

(defn flattenv [v] (into [] (flatten v)))

(defn post-edge [v]
  (->> v
       (filterv some?)
       (mapv (fn [{:keys [from to]}] [from to]))))

(defn walk-definition [groups f]
  (flattenv
   (mapv (fn [{:keys [Definition] :as group} ndx]
           (mapv (fn [p idx]
                   (mapv (fn [task jdx]
                           (f group task ndx idx jdx))
                         p (range)))
                 Definition (range)))
         groups (range))))

(defn prefix [group]
  (cond
    (:Group group) "prime"
    (:Condition group) "defins"
    (:Title group) "cont"))

(defn id [group ndx idx jdx]
  (keyword (string/join "_" [(prefix group) ndx idx jdx]))) 

(defn task-label [{:keys [TaskName Replace Use]}]
  [:i TaskName])

(defn group-label [{:keys [Title Description] }]
  [:TABLE {:BORDER 0}
   [:TR [:TD "Title"] [:TD {:BORDER 1} Title]]
   [:TR [:TD "Description"] [:TD {:BORDER 1} Description]]])

(defn group-node [group task ndx idx jdx]
  {:id (id (assoc group :Group :prime) ndx idx jdx)
   :color "white"
   :style "filled"
   :fontcolor "white"
   :fillcolor "gray20"
   :shape :box
   :label (group-label group)})

(defn task-node [group task ndx idx jdx]
  {:id (id group  ndx idx jdx)
   :color "black"
   :shape :box
   :style "filled"
   :fillcolor "gray95"
   :label (task-label task)})

(defn nodes [group task ndx idx jdx]
  (if (and (zero? idx) (zero? jdx))
    [(group-node group task ndx idx jdx)
     (task-node group task ndx idx jdx)]
    [(task-node group task ndx idx jdx)]))

(defn edges [group task ndx idx jdx]
  (if (and (zero? idx) (zero? jdx))
    {:from (id (assoc group :Group :prime) ndx 0 0)
     :to (id group ndx idx jdx)}
    {:from (id group ndx (dec idx) 0)
     :to (id group ndx idx jdx)}))

(defn id->image [id]
  (let [conf {:node->id (fn [n] (if (keyword? n) (name n) (:id n)))
              :node->descriptor (fn [n] (when-not (keyword? n) n))}
        mpd (get-mpd id)
        cont (-> mpd :Mp :Container)
        defis (-> mpd :Mp :Definitions)
        cont-nodes (walk-definition cont nodes)
        cont-edges (post-edge (walk-definition cont edges))
        dot (tangle/graph->dot cont-nodes cont-edges conf)]
    (io/copy (tangle/dot->image dot "png") (io/file (str id ".png")))))

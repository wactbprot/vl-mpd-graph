(ns vl-mpd-graph.core
  (:require [libcdb.core :as db]
            [libcdb.configure :as cf]
            [tangle.core :as tangle]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn flattenv [v] (into [] (flatten v)))

(defn post-edge [m]
  (->> m
       (filterv some?)
       (mapv (fn [{:keys [from to]}] [from to]))))

(defn walk [groups f]
  (mapv (fn [{:keys [Definition] :as group} ndx]
          (mapv (fn [p idx]
                  (mapv (fn [task jdx]
                          (f group task ndx idx jdx))
                        p (range)))
                Definition (range)))
        groups (range)))

(defn prefix [group] (if (:Condition group) "defins" "cont"))

(defn id [group ndx idx jdx]
  (keyword (string/join "_" [(prefix group) ndx idx jdx]))) 

(defn nodes [group {:keys [TaskName Replace Use]} ndx idx jdx]
  {:id (id group  ndx idx jdx)
   :color "blue"
   :label [:b TaskName]})

(defn edges [group task ndx idx jdx]
  (if (pos? idx)
    {:from (id group ndx (dec idx) jdx)
     :to (id group ndx idx jdx)}))

(defn get-mpd [id]
  (->> {:prot "http",
        :host "localhost",
        :port 5984,
        :usr (System/getenv "CAL_USR")
        :pwd (System/getenv "CAL_PWD")
        :name "vl_db"}
       cf/config
       (db/get-doc id)))


(comment
  (def mpd (get-mpd "mpd-se3-calib"))
  
  (def cont (-> mpd :Mp :Container))
  
  (def n (flattenv (walk cont nodes)))
  
  (def e (post-edge (flattenv (walk cont edges))))
  
  (def dot (tangle/graph->dot n e {:node {:shape :box}
                                   :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
                                   :node->descriptor (fn [n] (when-not (keyword? n) n))}))

  (io/copy (tangle/dot->image dot "png") (io/file "hello.png")))

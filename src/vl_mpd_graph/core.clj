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

(defn add-br [s]
  (string/join " "
   (mapv (fn [s i]
           (if (and (pos? i) (zero? (mod i 4)))
             (str s "<br/>")
             s))
         (string/split s #"\s") (range))))

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
    (and
     (:Group group)
     (:Condition group)) "defins_prime"
    (and
     (:Group group)
     (:Title group)) "cont_prime"
    (:Condition group) "defins"
    (:Title group) "cont"))

(defn id [group ndx idx jdx]
  (keyword (string/join "_" [(prefix group) ndx idx jdx]))) 

(defn safe-str [s]
  (-> s
      (string/replace #"&" "&#38;")
      (string/replace #"<" "&#60;")
      (string/replace #">" "&#62;")))

(defn map->table [m]
    (into [:TABLE {:BORDER 0}]
          (mapv (fn [[k v]]
                  [:TR
                   [:TD {:BORDER 1} (safe-str (name k)) ]
                   [:TD {:BORDER 1} (safe-str (str v))]])
                m)))
  
(defn task-label [{:keys [TaskName Replace Use]}]
  [:TABLE {:BORDER 0}
   [:TR
    [:TD {:BGCOLOR "lemonchiffon2"} "TaskName"]
    [:TD {:BGCOLOR "lemonchiffon2"} [:b TaskName]]]
   (when Replace
     [:TR
      [:TD {:BGCOLOR "lemonchiffon2"} "Replace"]
      [:TD (map->table Replace)]])
   (when Use
     [:TR
      [:TD {:BGCOLOR "lemonchiffon2"} "Use"]
      [:TD  (map->table Use)]])])

(defn display-cond [v]
  (into [:TD]
        (mapv (fn [{:keys [ExchangePath Methode Value]}]  
                (string/join " " ["<br/>" ExchangePath Methode Value "<br/>"]))
              v)))

(defn defins-label  [{:keys [DefinitionClass ShortDescr Condition]}]
  [:TABLE {:BORDER 0}
   [:TR
    [:TD [:b "DefinitionClass"]]
    [:TD {:BORDER 1} (safe-str DefinitionClass)]]
   [:TR
    [:TD "Description"]
    [:TD {:BORDER 1} (add-br (safe-str ShortDescr))]]
   [:TR
    [:TD "Conditions"]
    (display-cond Condition)]])

(defn cont-label [{:keys [Title Description]}]
  [:TABLE {:BORDER 0}
   [:TR
    [:TD [:b "Title"]]
    [:TD {:BORDER 1} (safe-str Title)]]
   [:TR
    [:TD "Description"]
    [:TD {:BORDER 1} (add-br (safe-str Description))]]])


(defn cont-node  [group task ndx idx jdx]
  {:id (id (assoc group :Group :prime) ndx idx jdx)
   :color "white"
   :style "filled"
   :fontcolor "white"
   :fillcolor "dodgerblue4"
   :shape :box
   :label (cont-label group)})

(defn defins-node  [group task ndx idx jdx]
  {:id (id (assoc group :Group :prime) ndx idx jdx)
   :color "white"
   :style "filled"
   :fontcolor "white"
   :fillcolor "darkslateblue"
   :shape :box
   :label (defins-label group)})

(defn group-node [{:keys [Title DefinitionClass] :as group} task ndx idx jdx]
  (cond
    (string? Title) (cont-node group task ndx idx jdx)
    (string? DefinitionClass) (defins-node group task ndx idx jdx)))
  
(defn task-node [group task ndx idx jdx]
  {:id (id group  ndx idx jdx)
   :color "black"
   :shape :box
   :style "filled"
   :fillcolor "lemonchiffon"
   :label (task-label task)})

(defn nodes [group task ndx idx jdx]
  (if (and (zero? idx) (zero? jdx))
    [(group-node group task ndx idx jdx)
     (task-node group task ndx idx jdx)]
    [(task-node group task ndx idx jdx)]))

(defn edges [group task ndx idx jdx]
  (if (and (zero? idx))
    {:from (id (assoc group :Group :prime) ndx 0 0)
     :to (id group ndx idx jdx)}
    {:from (id group ndx (dec idx) 0)
     :to (id group ndx idx jdx)}))



(defn id->image [id]
  (let [conf {:directed? true
              :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
              :node->descriptor (fn [n] (when-not (keyword? n) n))}
        mpd (get-mpd id)
        cont (-> mpd :Mp :Container)
        defins (-> mpd :Mp :Definitions)
        cont-nodes (walk-definition cont nodes)
        cont-edges (post-edge (walk-definition cont edges))
        defins-nodes (walk-definition defins  nodes)
        defins-edges (post-edge (walk-definition defins edges))
        dot (tangle/graph->dot (into defins-nodes (reverse cont-nodes))
                               (into defins-edges cont-edges ) conf)]
    (io/copy (tangle/dot->image  dot  "pdf") (io/file (str id ".pdf")))))

(comment
  (string/replace dot #"rankdir=TP" "rankdir=LR"))

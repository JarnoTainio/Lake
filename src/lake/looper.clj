(ns lake.looper
  (:gen-class))
(require '[clj-http.client :as client])
(require '[cheshire.core :as json])

(defn to_date [str]
  (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd'T'hh:mm:ss") str))

(def now (java.util.Date.))

(defn is_valid_time [time]
  (< (- (.getTime now) (.getTime time) (* 1000 60 60 3)) 0) ;; 3 hours
  )

(defn is_valid_time_str [str]
  (is_valid_time (to_date str)))

(defrecord Lake [name link temp measured_at])

(defn data_to_lake [meta data]
  (->Lake (meta :name) (meta :servicemap_url) (data :temp_water) (data :time)))

(defn handle_data [data]
  (data_to_lake (data :meta) (last (data :data))))

(defn compare_response [best response]
 (let [current (handle_data response)] 
    (cond
      (= (is_valid_time_str (.get (current :measured_at))) false) best
      (= best nil) current
      (< (.get best :temp) (.get current :temp)) current
      :else best)
 ))

(defn loop_lakes [list best index]
  (cond
    ;; TODO: Add async operators to enable the actual looping (go <! etc.)
    (> (count list) index) (
                            ((client/get (apply str ["https://iot.fvh.fi/opendata/uiras/", (name (.get list index)), "_v1.json"]) {:as :json, :async? true}
                                         (fn [response] (loop_lakes list (compare_response best (response :body)) (+ index 1)))
                                         (fn [exception] (loop_lakes list best (+ index 1))))))
    :else best)
)

(defn find_warmest_lake [] 
  (client/get "https://iot.fvh.fi/opendata/uiras/uiras-meta.json" {:as :json, :async? true}
              (fn [response] (loop_lakes (keys (response :body)) nil 0))
              (fn [exception] (println "Exception message is: " (.getMessage exception)))))

(defn -main [& args]
  (println find_warmest_lake))
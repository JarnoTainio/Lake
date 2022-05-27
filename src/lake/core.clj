(ns lake.core
  (:gen-class))
(require '[clj-http.client :as client])

(defrecord Lake [name link temp measured_at])

(defn to_date [str]
  (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd'T'hh:mm:ss") str))

(def now (java.util.Date.))

(defn data_to_lake [meta data]
  (->Lake (meta :name) (meta :servicemap_url) (data :temp_water) (data :time)))

(defn is_valid_time [time]
  (< (- (.getTime now) (.getTime time) (* 1000 60 60 3)) 0)) ;; 3 hours

(defn is_valid_time_str [str]
  (is_valid_time (to_date str)))

(defn get_JSON [path func]
  (client/get path {:as :json, :async? true}
              (fn [response] (func (response :body)))
              (fn [exception] (println "exception message is: " (.getMessage exception)))))

(defn handle_data [data]
  (data_to_lake (data :meta) (nth (data :data) 0)))

(defn find_warmest [list best index]
  (cond
    (> (count list) index) (find_warmest list
                                         (cond
                                           (= (is_valid_time_str (.get (.get list index) :measured_at)) false) best
                                           (= best nil) (.get list index)
                                           (< (.get best :temp) (.get (.get list index) :temp)) (.get list index)
                                           :else best)
                                         (+ index 1))
    :else best))

(defn get_lakes [response]
  (client/with-connection-pool {:timeout 5 :threads 4 :insecure? false :default-per-route 10}
    ;; TODO: add async commands (get <!)?
    ((doseq [[k _] response]
      (get_JSON (apply str ["https://iot.fvh.fi/opendata/uiras/", (name k), "_v1.json"]) handle_data)))))

(defn load_lakes []
  (get_JSON "https://iot.fvh.fi/opendata/uiras/uiras-meta.json" get_lakes))

(def testing_lakes
  [(->Lake "Järvi1" "link" 24.3 "2022-05-25T22:15:20")
   (->Lake "Järvi2" "link" 27.3 "2022-04-25T22:15:20")
   (->Lake "Järvi3" "link" 17.3 "2022-06-25T22:15:20")
   (->Lake "Järvi4" "link" 19.3 "2022-07-20T22:15:20")
   (->Lake "Järvi5" "link" 26.4 "2022-02-07T22:15:20")]
  )

(defn -main [& args]
  (println "Warmest fake lake at the moment: " (find_warmest testing_lakes nil 0))
  (println "Warmest real lake at the moment: " (find_warmest load_lakes nil 0))
  )


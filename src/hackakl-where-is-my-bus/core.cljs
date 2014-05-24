(ns hackakl_where_is_my_bus.core
  (:require [reagent.core :as reagent :refer [atom]]
            [datascript :as d]
            [goog.net.Jsonp :as jsonp]
            [cljs.reader :as reader]
           ))

(def api_key "cc2c65c9-5213-404e-8dda-45d17c2dd817")
(def at_server "https://api.at.govt.nz/v1/")
(def opt_server "http://54.79.37.164:8080/otp-rest-servlet/ws/plan")

;;; not my code --stu
(enable-console-print! )

(defn bind
  ([conn q]
   (bind conn q (atom nil)))
  ([conn q state]
   (let [k (rand)]
     (reset! state (d/q q @conn))
     (d/listen! conn k (fn [tx-report]
                         (let [novelty (d/q q (:tx-data tx-report))]
                           (when (not-empty novelty) ;; Only update if query results actually changed
                             (reset! state (d/q q (:db-after tx-report)))))))
     (set! (.-__key state) k)
     state)))

(defn unbind
  [conn state]
  (d/unlisten! conn (.-__key state)))

;;; Creates a DataScript "connection" (really an atom with the current DB value)
(def conn (d/create-conn))

;;; Maintain DB history.
(def history (atom []))

(d/listen! conn :history (fn [tx-report] (swap! history conj tx-report)))

(defn undo
  []
  (when (not-empty @history)
    (let [prev (peek @history)
          before (:db-before prev)
          after (:db-after prev)
          ;; Invert transition, adds->retracts, retracts->adds
          tx-data (map (fn [{:keys [e a v t added]}] (d/Datom. e a v t (not added))) (:tx-data prev))]
      (reset! conn before)
      (swap! history pop)
      (doseq [[k l] @(:listeners (meta conn))]
        (when (not= k :history) ;; Don't notify history of undos
          (l (d/TxReport. after before tx-data)))))))

;;; back to my code --stu

;;; Some non-DB state
(def state (atom {:live-routes nil
                  :short-routes {}
                  :long-routes {}
                  :route nil
                  :where-am-i-clicked false
                  :where-am-i-going-clicked false
                  :destination nil
                  :my-location nil}))

;;; Query to get positions corresponding to a route
(defn q-positions [route_id]
    (d/q '[:find ?p ?v
      :in $ ?route_id
      :where
       [?e :position ?p]
       [?e :route_id ?r]
       [?e :vehicle_id ?v]
       [(= ?r ?route_id)]
       ] @conn route_id)
    )

;;; sets the map marker
(defn set-map-marker
  [[lat-long vehicle_id]]
  (let [lat (:latitude lat-long)
        lon (:longitude lat-long)]
    (js/set_marker lat lon vehicle_id)
    ))

;;; sets the markers
(defn set-markers [route]
  (let [lat-longs (:lat-longs @state)
       [my-lat my-long] (:my-location @state)]
     (js/delete_markers)
     (dorun (map set-map-marker lat-longs))
     (js/set_my_location my-lat my-long)
     ))

;;; finds one trip id
(defn q-find-one-trip-id
  [route_id]
  (first (d/q '[:find ?t
      :in $ ?route_id
      :where
       [?e :trip_id ?t]
       [?e :route_id ?route_id]
       ] @conn route_id)
         ))

;;; uses Jsonp to get data from the shape info
(defn retrieve-route-shape
  [callback error-callback route]
   (let [[trip_id] (q-find-one-trip-id route)]
     (.send (goog.net.Jsonp. (str at_server "gtfs/shapes/tripId/" trip_id)
                           "callback")
    (doto (js-obj)
      (aset "api_key" api_key)
      )
    callback error-callback))
  )

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
               (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

;;; unpacks the response from the at route api
(defn set-route-shape [json-obj]
  (let [data (js->clj json-obj :keywordize-keys true)
        shape (:response data)]
    ;; dorun is needed because map is lazy
    (js/draw_route (clj->js
                     (map #(vector (:shape_pt_lat %) (:shape_pt_lon %)) shape)))
    (js/fitBounds)
    ))

;;; updates the choosen route
(defn route-change
  [route]
    (do
     (swap! state assoc-in [:route] route)
     (swap! state assoc-in [:lat-longs] (q-positions route))
     (swap! state assoc-in [:route-vehicles] (q-route-vehicles route))
     (set-markers route)
     (retrieve-route-shape set-route-shape
                           #(js/alert "error getting route shape") route)
    ))


;;; defines a reagent component that generates a select box of routes
(defn routes-view
  []
    (let [short-routes (:short-routes @state)
          long-routes (:long-routes @state)
          live-routes (:live-routes @state)]
      (if (not= (count short-routes) 0)
        [:div
         [:h2 "Choose a route "]
         [:select {:on-change #(route-change (.. % -target -value)) }
          (map (fn [r] [:option {:value r}
                        [:span (get short-routes r) " - "
                         (get long-routes r)]])
               live-routes)]]
        [:div]
        )))

;;; Query to find vehicles corresponding to a route
(defn q-route-vehicles [route_id]
  (d/q '[:find ?v
    ;; the :in cause is how you supply a parameter into the query
    :in $ ?route_id
    :where
     [?e :vehicle_id ?v]
     [?e :route_id ?route_id]
     ] @conn route_id)
)


;;; Query to get route info from the id
(defn q-route-info [route_id]
    (d/q '[:find ?short-name ?long-name
      :in $ ?route_id
      :where
       [?e :route_id ?r]
       [?e :route_short_name ?short-name]
       [?e :route_long_name ?long-name]
       [(= ?r ?route_id)]
       ] @conn route_id)
    )

;;; once a route is chosen shows the vechicles on the route and their lat-longs
(defn lat-long-view
  []
  (let [route (:route @state)
        ;; I am not sure how to bind a query with a parameter
        route-vehicles (:route-vehicles @state)
        ]
      (if route
                 [:div
                 [:h3 "We have information for " (count route-vehicles) " bus/es"]]
                 [:div])
     ))

;;; produces the where-am-i component
(defn where-am-i-view
  []
  (let [temp (atom {:clicked false})]
  (fn []
    [:div
     [:div {:style {:text-align "center"}}
      [:img {:src "static/Where_am_i_ok.png"
          :on-click  (fn []
                       (swap! temp assoc-in
                              [:clicked] (not (:clicked @temp))))}] ]
     (when (:clicked @temp)
       [:div {:style {:text-align "center"}} [:span "X using GPS"] ])
     ])))

;;; unpacks the response from the opt feed
(defn set-otp-info [xml-obj]
  (let [data (clojure.data/parse xml-obj :keywordize-keys true)]
    (print data)
    ))


;;; uses Jsonp to get data from the opt feed
(defn retrieve-opt-data
  [callback error-callback]
   (let [to-place (:lat-long (:destination @state))
         from-place (:my-location @state)]
     (do (print (doto (js-obj)
      (aset "maxWalkDistance" 750)
      (aset "mode" "TRANSIT,WALK")
      (aset "fromPlace" from-place)
      (aset "toPlace" to-place)))
     (.send (goog.net.Jsonp. opt_server "callback")
    (doto (js-obj)
      (aset "maxWalkDistance" 750)
      (aset "mode" "TRANSIT,WALK")
      (aset "fromPlace" from-place)
      (aset "toPlace" to-place))
    callback error-callback)
     )))

;;; validates and processes the destination
(defn process-destination
  [destination]
    (hash-map :lat-long
              (cljs.reader/read-string
                (str "[ " (:destination destination) " ]"))
    ))

;;; changes the destination
(defn destination-change
  [destination]
  (do
    (swap! state assoc-in [:destination]
                           (process-destination destination))
    ;(retrieve-opt-data set-otp-info #(js/alert "problem retreiving opt info"))
    ))

;;; produces the where-am-i component
(defn where-am-i-going-view
  []
  (let [temp (atom {:destination "-36.8532289 174.7660285" :time "now"})]
   (fn []
    (let [clicked (:where-am-i-going-clicked @temp)]
      [:div
       [:div {:style {:text-align "center"}}
        [:img {:src "static/Where_am_i_going.png"
               :on-click  (fn []
                            (swap! temp assoc-in
                                   [:where-am-i-going-clicked] (not clicked)))}] ]
       (when clicked
         [:div [:div {:style {:text-align "center"}}
                [:span "Destination: "]
                [:input {:type "text"
                         :value (:destination @temp)
                         :on-change #(swap! temp assoc-in
                                            [:destination]
                                            (.. % -target -value))}]]
          [:div {:style {:text-align "center"}}
           [:span "Time: "]
           [:input {:type "text"
                    :value (:time @temp)
                    ;:on-change #(swap! temp assoc-in [:time]
                    ;                   (.. % -target -value))
                    }]]
          [:div {:style {:text-align "center"}}
           [:button {:onClick (fn [] (destination-change @temp))}
       "Add Destination"]]]
      )]))))

;;; debug information
(defn debug-view []
  [:div
   [:div [:span "my-location: "] [:span (pr-str (:my-location @state))]]
   [:div [:span "destination: "][:span (pr-str (:destination @state))]]]
  )
;;; Uber component, contains/controls routes-view and lat-long-view.
(defn uber
  []
  [:div
   [:div [where-am-i-view]]
   [:div [where-am-i-going-view]]
   [:div [routes-view]]
   [:div [lat-long-view]]
   [:div [debug-view]]
   ])

;;; Initial render
(reagent/render-component [uber] (. js/document (getElementById "app")))

;;; below gets info from the real-time feed and puts it in the database
;;; only called once atm

;;; uses Jsonp to get data from the real-time feed
(defn retrieve-realtime-data
  [callback error-callback]
  (.send (goog.net.Jsonp. (str at_server "public/realtime/vehiclelocations") "callback")
    (doto (js-obj)
      (aset "api_key" api_key) )
    callback error-callback)
  )

(defn q-vehicle-db-id
  [vehicle_id]
      (d/q '[:find ?e ?t
      :in $ ?vehicle_id
      :where
       [?e :vehicle_id ?vehicle_id]
       [?e :timestamp ?t]
       ] @conn vehicle_id)
  )

;;; adds the information for each vehichle into the client-side db
(defn add-realtime [vehicle]
  (let [trip (:trip vehicle)
        trip_id (:trip_id trip)
        route_id (:route_id trip)
        position (:position vehicle)
        timestamp (:timestamp vehicle)
        vehicle_id (:id (:vehicle vehicle))
        [[id old-timestamp]] (seq (q-vehicle-db-id vehicle_id))]
    (if id ;there is already an entry
      (d/transact! conn [{:db/id id
                        :route_id route_id
                        :trip_id trip_id
                        :position position
                        :vehicle_id vehicle_id
                        :timestamp timestamp}])
      (d/transact! conn [{:db/id -1
                        :route_id route_id
                        :trip_id trip_id
                        :position position
                        :vehicle_id vehicle_id
                        :timestamp timestamp}])
      )))

;;; unpacks the response from the at realtime api
(defn set-realtime-info [json-obj]
  (let [data (js->clj json-obj :keywordize-keys true)
        items (:entity (:response data))
        vehicles (map :vehicle items)
        routes (set (map #(:route_id (:trip %)) vehicles))
        route (:route @state)]
    (swap! state assoc-in [:live-routes] routes)
    ;; dorun is needed because map is lazy
    (dorun (map add-realtime vehicles))
    (when route
      (swap! state assoc-in [:lat-longs] (q-positions route))
      (swap! state assoc-in [:route-vehicles] (q-route-vehicles route))
      )))

(retrieve-realtime-data set-realtime-info #(js/alert (str "error getting realtime data" %)))

;;; uses Jsonp to get data from the route feed
(defn retrieve-route-data
  [callback error-callback]
   (.send (goog.net.Jsonp. (str at_server "gtfs/routes") "callback")
    (doto (js-obj)
      (aset "api_key" api_key)
      )
    callback error-callback)
  )

;;; adds the information for each vehichle into the client-side db
(defn add-route-info [route]
  (let [route_id (:route_id route)
        route_short_name (:route_short_name route)
        route_long_name (:route_long_name route)]
    (d/transact! conn [{:db/id -1 :route_id route_id
                        :route_short_name route_short_name
                        :route_long_name route_long_name}])
    (swap! state assoc-in [:short-routes route_id] route_short_name)
    (swap! state assoc-in [:long-routes route_id] route_long_name)
    ))

;;; unpacks the response from the at route api
(defn set-route-info [json-obj]
  (let [data (js->clj json-obj :keywordize-keys true)
        routes (:response data)]
    ;; dorun is needed because map is lazy
    (dorun (map add-route-info routes))
    ))

(retrieve-route-data set-route-info #(js/alert (str "error getting route data" %)))


 ; Geolocation
 (def auckland-point [-36.85462580128022, 174.75643157958984])
  (defn geolocation [position]
    (def longitude (.-longitude js/position.coords))
    (def latitude (.-latitude js/position.coords))
    (swap! state assoc-in [:my-location] [latitude, longitude]))

  (.getCurrentPosition js/navigator.geolocation
                       geolocation
                       (fn []
                         (swap! state assoc-in [:my-location] auckland-point))
                       )

; Realtime loop for locations
(defn update-realtime-location
  []
  (let [route (:route @state)
        ]
    (when route
      (set-markers route)
      )
    ;run every 30 secs
    (retrieve-realtime-data set-realtime-info
                              #(js/alert (str "error getting realtime data" %)))
    (js/setTimeout update-realtime-location 30000)
    ))

(update-realtime-location)

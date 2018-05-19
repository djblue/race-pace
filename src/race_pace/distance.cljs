(ns race-pace.distance)

(defn meters [n] n)

(defn kilometers [n] (* n 1000))

(defn ->kilometers [n] (/ n (kilometers 1)))

(defn miles [n] (* n 1609.344))

(defn ->miles [n] (/ n (miles 1)))

(ns race-pace.time)

(defn create-units []
  (let [millisecond 1
        second      (* 1000 millisecond)
        minute      (* 60 second)
        hour        (* 60 minute)]
    {:milliseconds millisecond
     :seconds      second
     :minutes      minute
     :hours        hour}))

(def units (create-units))

(defn milliseconds [n] n)

(defn seconds [n] (* 1000 (milliseconds n)))
(defn ->seconds [n] (/ n (seconds 1)))

(defn minutes [n] (* 60 (seconds n)))
(defn ->minutes [n] (/ n (minutes 1)))

(defn hours [n] (* 60 (minutes n)))
(defn ->hours [n] (/ n (hours 1)))

(defn parse [duration]
  (->> [:hours :minutes :seconds :milliseconds]
       (reduce
        (fn [[result duration] unit]
          (let [u (unit units)
                v (js/Math.floor (/ duration u))]
            [(assoc result unit v) (- duration (* v u))]))
        [{} duration])
       first))

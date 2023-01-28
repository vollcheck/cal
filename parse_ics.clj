(ns parse-ics
  (:require [clojure.string :as str]))

(def cal-name "resources/calendar_user.ics")
(def cal-content (slurp cal-name))
(def events (str/split cal-content #"BEGIN:VEVENT"))

(defn fakin-parse-date
  "Apparently there is some weird datetime format, let me parse it manually"
  [date part]
  (let [year (subs date 0 4)
        month (subs date 4 6)
        day (subs date 6 8)
        hour (subs date 9 11)
        minute (subs date 11 13)]

    ;; TODO: `start-date` and `end-date` are duplicates, take only time
    (if (= part :start)
      {:start-date (format "%s-%s-%s" year month day)
       :start-time (format "%s:%s" hour minute)}
      {:end-date (format "%s-%s-%s" year month day)
       :end-time (format "%s:%s" hour minute)})
    ))

(defn parse-event
  [event]
  (let [lines (str/split-lines event)]
    (reduce
     (fn [acc line]
       (let [[k v] (str/split line #":")]
         (if (= v "Reminder") ;; omit the "Reminder" field which is a mess
           acc
           (condp str/starts-with? k
             "DESCRIPTION" (assoc acc :prof v)
             "SUMMARY;LANGUAGE=pl" (assoc acc :subject v)
             "LOCATION" (assoc acc :location v)
             "DTSTART;TZID=\"Central European Standard Time\"" (merge acc (fakin-parse-date v :start))
             "DTEND;TZID=\"Central European Standard Time\"" (merge acc (fakin-parse-date v :end))
             acc))))
     {}
     lines)))

(def processed-events
  (mapv
   parse-event
   events))

(defn spit-events! [events]
  (spit "out/processed-events.edn" events))

(comment
  (def processed-events
    (mapv parse-event events))

  (spit-events! processed-events)

  ;; get exact

  ;; get hours count that we will spend with <REDACTED>
  (->> processed-events
       (drop 2)
       (filter #(= (:prof %) "<REDACTED>"))
       (count)
       (* 1.5)
       )

  ;; get a list of subjects
  (->> processed-events
       (drop 2)
       (map (juxt :subject :prof))
       (distinct)
       )

  ;; get a list of profs
  (->> processed-events
       (drop 2)
       (map :prof)
       (distinct)
       )

  ;; get a list of studies days
  (->> processed-events
       (drop 2)
       (map :start)
       (map #(first (str/split % #" ")))
       (remove #(or (str/starts-with? % "2022") ;; some leavings from previous year
                    (str/starts-with? % "2023-01"))) ;; omit the exam session
       (distinct)
       )
  )

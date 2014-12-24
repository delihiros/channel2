(ns channel2.core
  (:use [net.cgrand.enlive-html])
  (:import [java.net URL]))

(defrecord NichanThread
  [title url responses])

(defrecord Response
  [response-number id date handle-name mail-address content response-anchor])

(defn br-tag->newline
  [content]
  (->> content
       (map (fn [c] (if (= :br (:tag c)) "\n" c)))
       (partition-by type)
       (map (fn [part]
              (if (string? (first part))
                (clojure.string/join part)
                part)))
       flatten))

(defn parse-response
  [[dt dd]]
  (let [dt-content      (:content dt)
        response-number (re-find #"[0-9]+" (first dt-content))
        id              (re-find #"ID:[a-zA-Z0-9]+" (last dt-content))
        date            (->> (last dt-content)
                             (partition-by (fn [c] (= c \space)))
                             drop-last flatten rest
                             clojure.string/join)
        handle-name     (-> dt-content second :content first :content first)
        mail-address    (-> dt-content second :attrs :href)
        content         (text {:tag :dd :content (-> (:content dd) br-tag->newline)})
        response-anchor (or (re-seq #">>[0-9]+" content) [])]
    (Response. response-number id date handle-name mail-address content response-anchor)))

(defn parse-thread
  [url]
  (let [raw-content
        (-> url
            URL. .getContent
            (java.io.InputStreamReader. "shift-jis")
            html-resource)]
    (NichanThread. (-> raw-content (select [:title]) first :content first)
                   url
                   (-> raw-content (select [:dl.thread]) first :content rest
                       (as-> reses
                         (partition-all 2 reses)
                         (map parse-response reses))))))

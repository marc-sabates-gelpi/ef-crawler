(ns ef.core
  (:require [clojure.string :as s]
            [cemerick.url :as url])
  (:gen-class))
;;; C-c RET h d [com.cemerick/url "0.1.1"]
(def ^:private github-url-pattern #"https?://github.com/[^\"\s]+(?=\")")
(def ^:private twitter-url-pattern #"https?://twitter.com/[^\"\s]+(?=\")")
(def ^:private error-message "[Error] %s\n")

(defn- scrape-page
  "Gets the Twitter and Github urls from `html` content."
  [html]
  (when html
    (hash-map :github-url (re-find github-url-pattern html) 
              :twitter-url (re-find twitter-url-pattern html))))

(defn- sanitise-url
  "Returns the `url` without anchor and with empty path."
  [url]
  (-> url
      (url/url)
      (dissoc :anchor)
      (update :path #(when-not (= "/" %) %))
      (url/map->URL)
      str))

(defn- absolute?
  "Logical true if `url` is absolute."
  [url]
  (or
   (< 1 (count (filter #{\/} (take 2 url))))
   (re-find #":" url)))

(defn- escape-dots
  "Prepends \\ to all dots."
  [s]
  (clojure.string/replace s "." "\\."))

(defn- make-absolute
  "Adds the `current` path/url to the `base-url` when needed."
  [base-url current]
  (if (absolute? current)
    current
    (str (url/url base-url current))))

(defn- subpage?
  "Logical true if the `current` url is a subpage of the `base` url.
   A subpage is at the same level or deeper than `base` when comparing
   paths. It does assume they belong to the same site, i.g. domain."
  [base current]
  (let [base (:path (url/url base)) current (:path (url/url current))]
    (clojure.string/starts-with? current base)))

(defn- crawl-others
  "Gets all the a's href urls on `html`.
   Urls from other domains or shallower levels are filtered out."
  [html url]
  (when html
    (let [domain-pattern (re-pattern (escape-dots (:host (url/url url))))]
      (->> html
           (re-seq #"a\s+href=\"([^\"]+)\"")
           (map last)
           (map (partial make-absolute url))
           (filter #(re-find domain-pattern %))
           (filter (partial subpage? url))))))

(defn- parse-site
  "Goes through a site looking for github & twitter urls."
  [url]
  (loop [todo #{(sanitise-url url)} done #{} result {}]
    (if (or
         (and (:github-url result) (:twitter-url result))
         (empty? todo))
      (assoc result :url url)
      (let [current (first todo)
            html (try
                   (slurp current)
                   (catch Exception e (printf error-message (.getMessage e))))
            updated-done (conj done current)]
        (recur (->> current
                    (crawl-others html)
                    (map sanitise-url)
                    (into todo)
                    (remove updated-done)) ;; NICE: Uses sets as preds! Genius! 
               updated-done
               (->> html
                    scrape-page
                    (merge result)))))))

(defn -main
  "Crawls candidates profiles looking for github and/or twitter urls."
  [& _]
  (-> "resources/urls"
      slurp
      clojure.string/split-lines
      (as-> urls (sequence (comp
                            #_(take 20)
                            (map parse-site)
                            (map (fn [res]
                                   (clojure.pprint/pprint res)
                                   res)))
                           urls))))

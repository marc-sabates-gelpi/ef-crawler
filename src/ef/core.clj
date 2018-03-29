(ns ef.core
  (:require [clojure.string :as s]
            [cemerick.url :as url])
  (:gen-class))
;;; C-c RET h d [com.cemerick/url "0.1.1"]
;;; REVIEW: Non greedy patterns +? 
#_(def ^:private github-url-pattern #"\"(https?://github.com/.+?)\"")
(def ^:private twitter-url-pattern #"\"(https?://twitter.com/.+?)\"")
;;; REVIEW: Lookahead patterns
(def ^:private github-url-pattern #"https?://github.com/[^\"\s]+(?=\")")

(defn- scrape-page
  "Gets the Twitter and Github urls from a url. "
  [url]
  (try 
    (let [html (slurp url)
          github-url (->> html
                          (re-find github-url-pattern)
                          next
                          first)
          twitter-url (->> html
                           (re-find twitter-url-pattern)
                           next
                           first)]
      (hash-map :github-url github-url 
                :twitter-url twitter-url
                :url url))
    (catch Exception e (hash-map :error (str (.getMessage e))))))

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

(defn- crawl-others
  "Gets all the href urls on a page."
  [url]
  (try
    (let [domain-pattern (re-pattern (escape-dots (:host (url/url url))))]
      (->> url
           slurp
           (re-seq #"href=\"([^\"]+)\"")
           (map last)
           (map (partial make-absolute url))
           (filter #(re-find domain-pattern %))))
    (catch Exception e (prn (.getMessage e)))))

(defn- parse-site
  "Goes through a site looking for github & twitter urls."
  [url]
  (loop [todo #{url} done #{} result {}]
    (if (empty? todo)
      result
      (recur (->> (conj todo (crawl-others url))
                 (remove done)) ;; NICE: Uses sets as preds! Genius!
             (conj done url)
             (merge result (scrape-page url))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> "resources/urls"
      slurp
      clojure.string/split-lines
      (map parse-site)
      clojure.pprint/pprint))

(ns counting-stars.core
  (:require [tentacles.users]
            [tentacles.repos]
            [cheshire.core :as json]
            [clojure.string :as str]
            [cemerick.url :as url]))

(def oauth-token "REDACTED")

(defn search-repos [query page]
  (json/parse-string (slurp
                      (str "https://api.github.com/search/repositories?"
                           "q=" query
                           "&sort=stars&order=desc&"
                           "page=" page "&"
                           "per_page=100&"
                           "access_token=" oauth-token))))

(defn search-all-repos [query]
  (mapcat
   #((search-repos query %) "items")
   (map inc (range 10))))

(defn extract-useful-keys [full-repo-details]
  (assoc (select-keys full-repo-details
                      ["name" "stargazers_count" "language"])
    "user" (get-in full-repo-details ["owner" "login"])))

(defn str-keys-to-keywords [m]
  (reduce (fn [a [k v]] (assoc a (keyword k) v)) {} m))

(def top-repos
    (mapcat identity
          (for [c (map char (range 97 123))]
            (do
              (println "Working on" c)
              (Thread/sleep 31000) ;; https://developer.github.com/v3/search/#rate-limit
              (map (comp str-keys-to-keywords
                         extract-useful-keys)
                   (search-all-repos c))))))

(def cutoff-stars
  (->> top-repos
       (partition 1000)
       (map last)
       (map :stargazers_count)
       (apply max)))

(def top-5000
  (->> top-repos
       (filter #(< cutoff-stars (:stargazers_count %)))
       ;; There may be repos that were starred during the querying and show up
       ;; twice with different star counts, so we can't just use `distinct`
       (group-by (juxt :name :user))
       (map (comp first second))
       (sort-by :stargazers_count)
       (reverse)
       (take 5000)))

(def top-with-rank
  (map #(assoc %1 :rank %2)
       top-5000
       (map inc (range))))

(defn language-count [repos]
  (frequencies (map :language repos)))

(def ordered-languages
  (->> top-with-rank
       (map :language)
       distinct
       (sort-by (language-count top-with-rank))
       reverse))

(def top-repo-per-language
  (->> top-with-rank
       (group-by :language)
       (map (fn [[language repos]] {language (first (sort-by :rank repos))}))
       (apply merge)))

(def count-in-range-table
  (sort-by #(->> % rest drop-last vec)
   (let [limits [10 100 1000 5000]
         count-map (apply merge
                          (for [top limits] {top (language-count
                                                  (take top top-with-rank))}))]
     (for [language ordered-languages]
       (concat [language]
               (map #(get-in count-map [% language]) limits)
               [(->> language
                     top-repo-per-language
                     ((juxt :user :name :rank))
                     (#(str (first %) \/ (second %) " (" (last %) ")")))])))))

(doseq [line (map (partial str/join " | ")
                  (reverse count-in-range-table))]
  (println line))

#!/usr/bin/env bb

(require '[clojure.repl]
         '[babashka.process :refer [shell pipeline pb]]
         '[clojure.string :as string])

(def +api-url+ "https://gitlab.com/api/v4")
(def +gitlab-personal-token+ (System/getenv "GITLAB_ACCESS_TOKEN"))
(def +gitlab-user+ (System/getenv "GITLAB_USERNAME"))

;;
;; Utils
;;

(defn not-empty [x] (seq x))

;;
;; Gitlab API
;;

(defn gitlab-rest-action
  [action]
  (str +api-url+ action))

(defn gitlab-headers
  []
  {"Accept" "application/json"
   "Authorization" (str "Bearer " +gitlab-personal-token+)})

(defn make-gitlab-api-query
  [action headers]
  (-> (curl/get action {:headers headers})
      :body
      (json/parse-string true)))

(defn get-mege-requests
  [state]
  (str (gitlab-rest-action "/merge_requests?state=") state))

(defn get-open-merge-requests-for-user
  [username]
  (let [action (str (get-mege-requests "opened") "&author_username=" username)
        headers (gitlab-headers)]
    (make-gitlab-api-query action headers)))

(defn find-url-by-branch
  [merge-requests predicate-branch]
  (->> merge-requests
       (filter (fn [{:keys [source_branch]}] (= source_branch predicate-branch)))
       (map :web_url)
       last))

;;
;; Git
;;

(def get-source-branches (partial map :source_branch))

(defn local-branch-exists?
  [branch-name]
  (let [branch-checksum (-> (pipeline (pb "git" "rev-parse" "--verify" branch-name)
                                      (pb "cat"))
                            last
                            :out
                            slurp)]
    (if (not-empty branch-checksum)
      [branch-checksum branch-name]
      false)))

(def local-branch-does-not-exist? (complement local-branch-exists?))

(defn get-current-branch-name
  []
  (-> (pipeline (pb "git" "name-rev" "--name-only" "HEAD")
                (pb "cat"))
                last
                :out
                slurp
                clojure.string/trim-newline))
;;
;; CLI
;;

(def get-meta-doc-and-command (juxt :command :doc))

(defn- get-command-metadata
  [sym]
  (let [symbol-metadata (meta (resolve sym))]
    (if (:doc symbol-metadata)
      (get-meta-doc-and-command symbol-metadata))))

(defn open-url
  ([] (print "No URL found for branch\n"))
  ([url] (if url
            (shell (str "open " url))
            (open-url))))

(defn- print-fn-documentation
  []
  (let [fns (->> (ns-publics *ns*) keys)
        public-commands (->> fns
                             (map get-command-metadata)
                             (filter identity))]
        (doseq [[command summary] public-commands]
          (print (str command " - " summary "\n")))))

(defn print-usage
  []
  (print "List of supported commands:\n\n")
  (print-fn-documentation))

;;
;; Commands
;;

(defn- command-ls
  [predicate-fn]
  (let [branches
        (->> (get-open-merge-requests-for-user +gitlab-user+)
             (get-source-branches)
             (map predicate-fn)
             (map last)
             )]
        (doseq [branch-name branches]
          (print (str branch-name "\n")))))

(defn command-open
  "Open the current branch merge request in browser"
  {:command "open"}
  []
  (let [user-merge-requests (get-open-merge-requests-for-user +gitlab-user+)
        current-branch (get-current-branch-name)
        web-url (find-url-by-branch user-merge-requests current-branch)]
        (open-url web-url)))

(defn command-ls-open
  "List all local git branches that have an open merge request"
  {:command "ls"}
  []
  (command-ls local-branch-exists?))

(defn main
  []
  (let [command (clojure.string/join " " *command-line-args*)]
    (case command
      "open" (command-open)
      "ls" (command-ls-open)
      (print-usage))))

(main)


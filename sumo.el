;;; sumo.el --- SumoLogic Management Interface
;;; -*- lexical-binding: t -*-

;; Copyright (C) 2014 Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: Sumo Logic Management Interface
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Some of this is cribbed from:
;;; hackernews.el --- Hacker News Client for Emacs
;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

;; time, and many revisions before I would expect it to be useful to anyone.
;;

;; Define sumo_email to your login email.
;; Define sumo_pass to your password. (Yes same as webpage login)

;; Requires the nice "web" package by Nic, request, and json.


;;; Code:

(require 'web)
(require 'request)
(require 'json)

(setq sumo_basic_auth (concat "Basic " (base64-encode-string (concat sumo_email ":" sumo_pass))))

(defun get-sumo-collectors ()
  (interactive)
  (sumo-do-get-collectors "https://api.sumologic.com/api/v1/collectors"))

(defun get-sumo-sources (id)
  (sumo-do-get-sources (format "https://api.sumologic.com/api/v1/collectors/%s/sources" id)))

(defun get-sumo-base (id)
  (sumo-do-get-base (format "https://api.sumologic.com/api/v1/collectors/%s" id)))

(defun sumo-do-get-collectors (uri)
  (let ((data `(("Authorization" . ,sumo_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*sumo*"
         (switch-to-buffer-other-window "*sumo*")
         (mapcar #'sumo-print-collector (cdr (assoc 'collectors (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun sumo-do-get-sources (uri)
  (let ((data `(("Authorization" . ,sumo_basic_auth)))
	(uri uri)
	)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*sumo-sources*"
         (switch-to-buffer-other-window "*sumo-sources*")
	 (mapcar #'sumo-print-sources (cdr (assoc 'sources (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun sumo-do-get-base (uri)
  (let ((data `(("Authorization" . ,sumo_basic_auth)))
	(uri uri)
	)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*sumo-sources*"
         (switch-to-buffer-other-window "*sumo-sources*")
	 (mapcar #'sumo-print-message (json-read-from-string my-data))))
     ;;(mapcar #'sumo-print-sources (cdr (assoc 'sources (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun sumo-print-sources (element)
  (let* (
	 (alive (format "%s" (cdr (assoc 'alive element))))
	 (sourceType (format "%s" (cdr (assoc 'sourceType element))))
	 (pathExpression (format "%s" (cdr (assoc 'pathExpression element))))
	 (encoding (format "%s" (cdr (assoc 'encoding element))))
	 (name (format "%s" (cdr (assoc 'name element))))
	 (id (format "%s" (cdr (assoc 'id element))))
	 )
    (insert (propertize (format " Type:%s " sourceType) 'face '(:foreground "red")))
    (insert (propertize (format " path:%s " pathExpression) 'face '(:foreground "red")))
    (insert (propertize (format " id:%s " id) 'face '(:foreground "purple")))
    (insert (propertize (format " Alive?:%s " alive) 'face '(:foreground "blue")))
    (princ " ")
    (princ "\n")))


(defun sumo-print-collector (element)
  (let* (
	 (alive (format "%s" (cdr (assoc 'alive element))))
	 (links (format "%s" (cdr (assoc 'href (elt (cdr (assoc 'links element)) 0)))))
	 (id (format "%s" (cdr (assoc 'id element))))
	 (timeZone (format "%s" (cdr (assoc 'timeZone element))))
	 (name (format "%s" (cdr (assoc 'name element))))
	 (collectorVersion (format "%s" (cdr (assoc 'collectorVersion element))))
	 (collectorType (format "%s" (cdr (assoc 'collectorType element))))
	 )
    (sumo-create-link-for-collectors name (concat "https://api.sumologic.com/api/" links))
    (insert (propertize (format " %s" id) 'face '(:foreground "purple")))
    (insert (propertize (format " %s" timeZone) 'face '(:foreground "red")))
    (insert (propertize (format " Alive?:%s" alive) 'face '(:foreground "blue")))

    (princ "\n")))

(defun sumo-print-search (element)
  (let* (
	 (messagetime (format "%s" (cdr (assoc '_messagetime element))))
	 (sourcename (format "%s" (cdr (assoc '_sourcename element))))
	 (receipttime (format "%s" (cdr (assoc '_receipttime element))))
	 (sourcehost (format "%s" (cdr (assoc '_sourcehost element))))
	 (raw (format "%s" (cdr (assoc '_raw element))))
	 (sourcecategory (format "%s" (cdr (assoc '_sourcecategory element)))))
    (insert (propertize (format "* %s" messagetime) 'face '(:foreground "blue")))
    (insert (propertize (format " %s" sourcename) 'face '(:foreground "yellow")))
    (insert (propertize (format " %s" receipttime) 'face '(:foreground "green")))
    (insert (propertize (format " %s" sourcehost) 'face '(:foreground "orange")))
    (insert (propertize (format " %s\n" sourcecategory) 'face '(:foreground "purple")))
    (insert (propertize (format "** Raw\n%s" raw) 'face '(:foreground "green")))
    (princ "\n")))

(defun sumo-create-link-for-collectors (title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((title title)
                (id id)
                (map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (sumo-do-get-sources id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (sumo-do-get-sources id)))
    (define-key map (kbd "b")
      #'(lambda (e) (interactive "p") (sumo-do-get-base id)))
    (insert
     (propertize
      title
      'face '(:foreground "green")
      'keymap map
      'mouse-face 'highlight))))

(defun sumo-create-link-for-sources (title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((title title)
                (id id)
                (map (make-sparse-keymap)))
    (define-key map (kbd "a")
      #'(lambda (e) (interactive "p") (sumo-add-all-logs id)))
    (insert
     (propertize
      title
      'face '(:foreground "green")
      'keymap map
      'mouse-face 'highlight))))


(defun sumo-print-message (element)
  (message "SSS: %s" element) )

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun sumo-get-json-content (file)
  (get-string-from-file file))

(defun upload-file (file)
  (let (
	(data (get-string-from-file "~/.emacs.d/nginx.json"))
	(heads `(
		 ("Authorization" . ,sumo_basic_auth)
		 ("Content-Type" . "application/json")
		 )))
  ;; (with-current-buffer myfile
  ;;    (insert "hello!!!!")
  ;;    (write-file "my-file.txt"))
    (message "A:%s" data)
    (web-http-post
     (lambda (con hdr data)
       (message "the file was uploaded!"))
   :url "http://localhost:4567/foo"
   :data data
   ;;:data `(("my-file" . ,myfile))
   :extra-headers heads
   ;;:mime-type web-multipart-mimetype
   )))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun sumo-do-search (query)
  (let ((data `(("Authorization" . ,sumo_basic_auth)))
	(url (format "https://api.sumologic.com/api/v1/logs/search?q=%s" query)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*sumo*"
         (switch-to-buffer-other-window "*sumo*")
         ;;(mapcar #'sumo-print-collector (json-read-from-string my-data))))
	 (mapcar #'sumo-print-search (json-read-from-string my-data))))
     ;;(mapcar #'sumo-print-collector (cdr (assoc 'collectors (json-read-from-string my-data))))))
     :url (format "https://api.sumologic.com/api/v1/logs/search?q=%s" query)
     :extra-headers data
     )))

(defun sumo-do-search-time (query from to)
  (lexical-let ((my-search-buffer (concat "*sumo-" query "*"))
	(data `(("Authorization" . ,sumo_basic_auth))))
    (message "XXX: we got my-search-buffer%s" my-search-buffer)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer my-search-buffer
         (switch-to-buffer-other-window my-search-buffer
					(mapcar #'sumo-print-search (json-read-from-string my-data)))))
     :url (format "https://api.sumologic.com/api/v1/logs/search?q=%s&from=%s&to=%s" query from to)
     :extra-headers data
     )))

(defun sumo-do-search-job (query)
  (let ((data `(("Authorization" . ,sumo_basic_auth)))
	(url (format "https://api.sumologic.com/api/v1/logs/search?q=%s" query)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*sumo*"
         (switch-to-buffer-other-window "*sumo*")
         (mapcar #'sumo-print-message my-data)))
     ;;(mapcar #'sumo-print-collector (cdr (assoc 'collectors (json-read-from-string my-data))))))
     :url (format "https://api.sumologic.com/api/v1/logs/search?q=%s" query)
     :exotra-headers data
     )))

(defun sumo-search (query)
  (interactive "sQuery: ")
  (sumo-do-search query))

(defun sumo-search-time (query from to)
  (interactive "sQuery:\nsFrom:\nsTo:")
  (sumo-do-search-time query from to))

(defun sumo-add-monitor (id json-file)
  (let* (
	 (id id)
	 (json-file json-file)
	 (url (format "https://api.sumologic.com/api/v1/collectors/%s/" id))
	 ;;.(url "http://localhost:4567/foo")
	 )
    (message "XXX: blasting %s" url)
    (request url
	     :type "POST"
	     :data (json-encode '((source (pathExpression . "/tmp/test.log") (cutoffTimestamp . 0) (sourceType . "LocalFile") (name . "TestError"))))
	     ;;:data data ;; (get-string-from-file json-file) ;;(json-encode '(("key" . "value") ("key2" . "value2")))
	     :headers '(
			("Authorization" . ,sumo_basic_auth)
			("Content-Type" . "application/json")
			)
	     :parser 'json-read
	     :error (function*
		       (lambda (&key data &allow-other-keys)
			 (message "Error: %S" (assoc-default 'json data)))))
    	     :status-code (function*
		       (lambda (&key data &allow-other-keys)
			 (message "StatusCode: %S" (assoc-default 'json data))))

	     :success (function*
		       (lambda (&key data &allow-other-keys)
			 (message "I sent: %S" (assoc-default 'json data))))
    ))

(defun sumo-add-all-logs (id)
  ;;https://api.sumologic.com/api/v1/collectors/213812770/sources
  ;;curl -u "$auth" -X POST -H "Content-Type: application/json" -T nginx.json "https://api.sumologic.com/api${collector}"
  (setq sumo-nginx (sumo-get-json-content "~/.emacs.d/nginx.json"))
  (setq sumo-prod (sumo-get-json-content "~/.emacs.d/prod.json"))
  (setq sumo-unicorn (sumo-get-json-content "~/.emacs.d/unicorn.json"))
  ;;(sumo-add-source-to-id id sumo-nginx)
  (sumo-add-source-to-id id sumo-unicorn)
  )

(defun sumo-add-source-to-id (id data)
  (let (
	(heads `(
		("Content-Type" . "application/json")
		("Authorization" . ,sumo_basic_auth)
		))
	(url "http://127.0.0.1:4567/foo")
	(data data)
	(query-data (make-hash-table :test 'equal))
	)
	(puthash 'name "jaime" query-data)
    (message "XXX: id:%s heads:%s data:%s type%s" id heads data (type-of data))

    (web-http-post
     (lambda (con header data)
       ;;(with-output-to-temp-buffer "*sumo-update*"
         (mapcar #'sumo-print-message data))
     :url url
     :extra-headers heads
     :data query-data
     :mime-type "application/json"
     )))

;;; douban-emacs.el --- douban client in Emacs
(defvar douban-emacs-version "0.1")
;; Copyright 2009 Binjo
;;
;; Author: binjo.cn@gmail.com
;; Version: $Id: douban-emacs.el,v 0.0 2009/12/08 17:38:28 binjo Exp $
;; Keywords: douban
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Douban client

;;; History:

;; 12/08/2009, init
;; 01/19/2010, push to github and 1st release

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'douban-emacs)
;;   (setq douban-api-public-key "your-acquired-pub-key")
;;   (setq douban-api-secret-key "your-acquired-sec-key")

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'xml)
(require 'xmlgen)
(require 'url)
(require 'url-http)
(require 'oauth)


(defgroup douban nil
  "douban-emacs.el is an emacs package for interacting with douban(http://www.douban.com)."
  :version "0.1"
  :group 'douban)

(defcustom douban-api-public-key
  nil
  "Pulib api key to access api.douban.com."
  :group 'douban
  :type 'string)

(defcustom douban-api-secret-key
  nil
  "Secret api key."
  :group 'douban
  :type 'string)

;; const url
(defconst douban-api-host "http://api.douban.com")
(defconst douban-www-host "http://www.douban.com")
(defconst douban-start-index 1)
(defconst douban-max-results 10)

;; oauth related url
(defvar douban-request-token-url
  (concat douban-www-host "/service/auth/request_token"))
(defvar douban-authorize-url
  (concat douban-www-host "/service/auth/authorize"))
(defvar douban-access-token-url
  (concat douban-www-host "/service/auth/access_token"))

(defvar douban-access-token nil)
(defcustom douban-token-dir
  "~/.emacs.d/"
  "The token will be saved in this directory."
  :group 'douban
  :type 'string)

(defconst douban-buffer "*douban*")

(defvar douban-notes-backup
  (concat org-directory "douban-notes.org")
  "Leverages org-mode's functionality.")

;; common url
(defvar douban-people-url
  (concat douban-api-host "/people"))
(defvar douban-people-friends
  (concat douban-people-url "/%s/friends"))
(defvar douban-people-contacts
  (concat douban-people-url "/%s/contacts"))

(defvar douban-notes-url
  (concat douban-api-host "/notes"))
(defvar douban-people-notes-url
  (concat douban-api-host "/people/%s/notes"))

;; xmlparse var
(defvar douban-async-buffer 'nil
  "Buffer that stores the temporary XML result for douban-emacs.el")

;; keymap
(defvar douban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'douban-post-notes)
    map)
  "Keymap for douban mode.")

(defun douban-oauth-authenticate (consumer-key consumer-secret)
  "Get authentication token."
  (if (file-exists-p (concat douban-token-dir ".douban-token"))
      (progn
        (save-excursion
          (find-file (concat douban-token-dir ".douban-token"))
          (let ((str (buffer-substring (point-min) (point-max))))
            (if (string-match "\\([^:]*\\):\\(.*\\)" str)
                (setq douban-access-token
                      (make-oauth-access-token
                       :consumer-key consumer-key
                       :consumer-secret consumer-secret
                       :auth-t (make-oauth-t
                                :token (match-string 1 str)
                                :token-secret (match-string 2 str))))))
          (save-buffer)
          (kill-this-buffer))))
  (unless douban-access-token
    (setq douban-access-token
          (oauth-authorize-app consumer-key consumer-secret
                               douban-request-token-url douban-access-token-url
                               douban-authorize-url))
    (save-excursion
      (find-file (concat douban-token-dir ".douban-token"))
      (end-of-buffer)
      (let ((token (oauth-access-token-auth-t douban-access-token)))
        (insert (format "%s:%s\n"
                        (oauth-t-token token)
                        (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer)))
  douban-access-token)

;; macro auth
(defmacro with-douban-auth (consumer-key consumer-secret &rest forms)
  "Set `douban-access-token' with CONSUMER-KEY and CONSUMER-SECRET, and execute FORMS."
  `(let ((douban-access-token
          (douban-oauth-authenticate ,consumer-key ,consumer-secret))
         (oauth-use-curl nil))
     ,@forms))

(defmacro with-douban-buffer (buffer-name &rest forms)
  "Create a douban buffer with BUFFER-NAME, and execute FORMS."
  `(save-excursion
     (with-current-buffer (get-buffer-create ,buffer-name)
       (delete-region (point-min) (point-max))
       (goto-char (point-min))
       ,@forms
       (pop-to-buffer ,buffer-name)
       (douban-mode))))

(defun douban-xml-gen (title content privacy reply)
  "Call `xmlgen' to generate specific xml data with TITLE and CONTENT.

PRIVACY - public/private.
REPLY   - yes/no."
  (let ((xmldata (concat
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                  (xmlgen `(entry :xmlns "http://www.w3.org/2005/Atom" :xmlns:db "http://www.douban.com/xmlns/"
                                  (title ,title)
                                  (content ,content)
                                  (db:attribute :name "privacy" ,privacy)
                                  (db:attribute :name "can_reply" ,reply))))))
    (encode-coding-string xmldata 'utf-8 t)))

(defun douban-handle-post (err)
  (cond
   ((null err) (message "Post sent success!"))
   (t (message "Post sent failed! %s" err)))
  (kill-buffer (current-buffer)))

(defun douban-internal-post (title message)
  "Post message to douban."             ; TODO public/private control, from arg?
  (let ((url-request-data (douban-xml-gen title message "public" "yes"))
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/atom+xml"))))
    (with-douban-auth douban-api-public-key douban-api-secret-key
                      (oauth-url-retrieve douban-access-token
                                          douban-notes-url 'douban-handle-post))))

(defun douban-parse-xml-async (url callback)
  "Retrieve the resource at URL, and call CALLBACK."
  (let ((url-request-method "GET")
        (url-show-status nil))
    (setq douban-async-buffer
          (url-retrieve url 'douban-parse-xml-async-retrieve
                        (list url callback)))))

(defun douban-parse-xml-async-retrieve (status url callback)
  "Called by `douban-parse-xml-async'.

STATUS is the status from http, URL and CALLBACK were the args from `douban-parse-xml-async'."
  (message status)
  (let (result)
    (if (bufferp douban-async-buffer)
        (save-excursion
          (set-buffer douban-async-buffer)
          (goto-char (point-min))
          (setq result (xml-parse-fragment))
          (kill-buffer (current-buffer)))
      (funcall callback status url result))))

(defun douban-people-notes (user)
  "Retrieve USER's all notes."
  (interactive "sUsername:")
  (let ((url (format douban-people-notes-url user)))
    (douban-parse-xml-async url 'douban-people-notes-cb)))

(defun douban-people-notes-cb (status url result)
  "Callback of `douban-people-notes'."
  (message status))

;;;###autoload
(defun douban-create-note ()
  "Create a note with `douban-buffer'."
  (interactive)
  (with-douban-buffer douban-buffer
                      (message "there we go...")))

;;;###autoload
(defun douban-post-notes (title)
  "Post notes with TITLE and buffer string.
Save the notes to `douban-notes-backup'."  ;TODO privacy & reply control
  (interactive "sTitle: ")
  (message "Posting with title : %s" title)
  (let ((content (buffer-substring (point-min) (point-max)))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer (find-file-noselect douban-notes-backup)
      (goto-char (point-max))
      (insert "* " "<" (format-time-string "%Y-%m-%d %H:%M:%S") "> " title)
      (insert "\n\n" content)
      (save-buffer))
    (douban-internal-post title content)
    (kill-buffer (current-buffer))))

;; * mode
(defun douban-mode ()
  "Major mode for posting notes to douban.

\\{douban-mode-map}."
  (interactive)
  (kill-all-local-variables)
  (use-local-map douban-mode-map)
  (setq mode-name "Douban")
  (setq major-mode 'douban-mode))

;;;###autoload
(define-derived-mode douban-mode org-mode "Douban"
  "Toggle douban-minor-mode, a minor mode that binds keys for posting.

\\{douban-mode-map}" nil
" Douban"
:global t
:group 'douban
:version douban-emacs-version
(auto-fill-mode 1))

(provide 'douban-emacs)
;;; douban-emacs.el ends here

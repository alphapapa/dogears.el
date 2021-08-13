;;; dogears.el --- Never lose your place again       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library automatically and smartly remembers where you've been,
;; in and across buffers, and helps you quickly return to any of those
;; places.  It uses the Emacs bookmarks system internally (but without
;; modifying the bookmarks-alist) to save and restore places with
;; mode-specific functionality.

;;; Code:

;;;; Requirements

(require 'bookmark)
(require 'map)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'which-func)

;;;; Variables

(defvar dogears-list nil
  "List of dogeared places.")

(defvar dogears-idle-timer nil
  "Idle timer which dogears the current place.")

;;;; Customization

(defgroup dogears nil
  "Never lose your place again."
  :group 'convenience)

(defcustom dogears-functions nil
  "Functions which should dogear a place when called.
These are advised when `dogears-mode' is activated."
  :type '(repeat function))

(defcustom dogears-hooks
  '(imenu-after-jump-hook)
  "Hooks which should dogear a place when run.
Dogears adds itself to these hooks when `dogears-mode' is
activated."
  :type '(repeat variable))

(defcustom dogears-idle 10
  "Remember place when Emacs is idle for this many seconds."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Never" nil)))

(defcustom dogears-limit 100
  "How many places to remember."
  :type 'integer)

(defcustom dogears-line-width 25
  "How many characters from a place's line to show."
  :type 'integer)

(defcustom dogears-within-function #'which-function
  "Function that returns what a place is \"within\"."
  :type '(choice (function-item which-function)
                 (function-item dogears--within)
                 (function :tag "Custom function")))

;;;; Commands

(define-minor-mode dogears-mode
  "Never lose your place again.  Dogears mode keeps track of
where you've been and helps you easily find your way back."
  :global t
  (if dogears-mode
      (progn
        (dolist (fn dogears-functions)
          (advice-add fn :after #'dogears-remember))
        (dolist (hook dogears-hooks)
          (add-hook hook #'dogears-remember))
        (when dogears-idle
          (setf dogears-idle-timer (run-with-idle-timer dogears-idle 'repeat #'dogears-remember))))
    ;; Disable mode.
    (dolist (fn dogears-functions)
      (advice-remove fn #'dogears-remember))
    (dolist (hook dogears-hooks)
      (remove-hook hook #'dogears-remember))
    (when dogears-idle-timer
      (cancel-timer dogears-idle-timer)
      (setf dogears-idle-timer nil))))

(defun dogears-remember (&rest _ignore)
  "Remember the current place."
  (interactive)
  (if-let ((record (ignore-errors
                     (funcall bookmark-make-record-function))))
      (progn
        (unless (stringp (car record))
          ;; Like `bookmark-make-record', we may have to add a string
          ;; ourselves.  And we want every record to have one as its
          ;; first element, for consistency.
          (push "" record))
        (when-let ((within (funcall dogears-within-function)))
          (setf (map-elt (cdr record) 'within) within))
        (setf (map-elt (cdr record) 'mode) major-mode
              (map-elt (cdr record) 'line) (buffer-substring (point-at-bol)
                                                             (point-at-eol)))
        (push record dogears-list)
        (setf dogears-list (delete-dups dogears-list)
              dogears-list (seq-take dogears-list dogears-limit)))
    (when (called-interactively-p 'interactive)
      (message "Dogears: Couldn't dogear this place"))))

(defun dogears-go ()
  "Go to a dogeared place with completion."
  (interactive)
  (let* ((collection (cl-loop for record in dogears-list
                              for key = (dogears--format-record record)
                              collect (cons key record)))
         (choice (completing-read "Place: " collection nil t))
         (record (alist-get choice collection nil nil #'equal)))
    (bookmark-jump record)))

;;;; Functions

(defun dogears--within ()
  "Return string representing what the current place is \"within\"."
  (ignore-errors
    (save-excursion
      (beginning-of-defun)
      (buffer-substring (point-at-bol) (point-at-eol)))))

(defun dogears--format-record (record)
  "Return bookmark RECORD formatted."
  (pcase-let* ((`(,name . ,(map filename position line within mode)) record)
               (base (if filename
                         (file-name-nondirectory filename )
                       name))
               (line (truncate-string-to-width (string-trim line) dogears-line-width))
               (relevance (dogears--relevance record))
               (dir))
    (when filename
      (setf dir (split-string (file-name-directory filename) "/" t)
            dir (nreverse dir)
            dir (cl-loop for d in dir
                         concat (truncate-string-to-width d 10)
                         concat "\\")))
    (when within
      (setf within (truncate-string-to-width within 25)))
    (format "[%10s]  (%25s)  \"%15s\"  %14s  %s:%s\\%s"
            relevance within line mode base position dir)))

(defun dogears--relevance (record)
  "Return the relevance string for RECORD."
  (pcase-let* ((`(,_name . ,(map filename within mode)) record))
    (when filename
      (setf filename (expand-file-name filename)))
    (cond ((when-let ((now-within (dogears--within)))
             (equal within now-within))
           "definition")
          ((when filename
             ;; FIXME: Doesn't make sense for, e.g. info buffers.
             (when-let ((buffer (find-buffer-visiting filename)))
               (equal buffer (current-buffer))))
           "buffer")
          ((when filename
             (equal filename (buffer-file-name)))
           "file")
          ((when filename
             (when-let ((project (project-current)))
               (equal project (project-current nil (file-name-directory filename)))))
           "project")
          ((when filename
             (equal (file-name-directory filename) default-directory))
           "directory")
          ((equal mode major-mode)
           "mode")
          (t ""))))

;;;; Footer

(provide 'dogears)

;;; dogears.el ends here

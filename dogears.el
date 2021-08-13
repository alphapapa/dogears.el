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

(defcustom dogears-ignore-places-functions
  (list #'minibufferp
        #'dogears--ignored-mode-p)
  "Don't remember any places for which any of these functions return non-nil."
  :type '(repeat function))

(defcustom dogears-ignore-modes
  '(dogears-list-mode fundamental-mode helm-major-mode)
  "Don't remember any places in buffers in or derived from these modes."
  :type '(repeat symbol))

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
          (setf dogears-idle-timer
                (run-with-idle-timer dogears-idle 'repeat #'dogears-remember))))
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
  (unless (seq-some #'funcall dogears-ignore-places-functions)
    (if-let ((record (or (ignore-errors
                           (funcall bookmark-make-record-function))
                         (dogears--buffer-record))))
        (progn
          (setf (map-elt (cdr record) 'manual)
                (if (called-interactively-p 'interactive) "✓" ""))
          (unless (stringp (car record))
            ;; Like `bookmark-make-record', we may have to add a string
            ;; ourselves.  And we want every record to have one as its
            ;; first element, for consistency.
            (push "" record))
          (unless (map-elt (cdr record) 'buffer)
            (setf (map-elt (cdr record) 'buffer) (buffer-name)))
          (when-let ((within (or (funcall dogears-within-function)
                                 (dogears--within))))
            (setf (map-elt (cdr record) 'within) within))
          (setf (map-elt (cdr record) 'mode) major-mode
                (map-elt (cdr record) 'line) (buffer-substring (point-at-bol)
                                                               (point-at-eol)))
          (push record dogears-list)
          (setf dogears-list (delete-dups dogears-list)
                dogears-list (seq-take dogears-list dogears-limit)))
      (when (called-interactively-p 'interactive)
        (message "Dogears: Couldn't dogear this place")))))

(defun dogears-go (place)
  "Go to dogeared PLACE.
Interactively, select PLACE with completion.  PLACE should be a
bookmark record."
  (interactive (let* ((collection (cl-loop for place in dogears-list
                                           for key = (dogears--format-record place)
                                           collect (cons key place)))
                      (choice (completing-read "Place: " collection nil t)))
                 (list (alist-get choice collection nil nil #'equal))))
  (or (ignore-errors
        (bookmark-jump place))
      (when-let ((buffer (map-elt (cdr place) 'buffer)))
        (if (buffer-live-p buffer)
            (switch-to-buffer buffer)
          (user-error "Buffer no longer exists: %s" buffer)))))

;;;; Functions

(defun dogears--buffer-record ()
  "Return a bookmark-like record for the current buffer.
Intended as a fallback for when `bookmark-make-record-function'
returns nil."
  (list (buffer-name)
        (cons 'buffer (current-buffer))
        (cons 'location (buffer-name))
        (cons 'within (buffer-name))
        (cons 'mode major-mode)
        (cons 'position (point))))

(defun dogears--within ()
  "Return string representing what the current place is \"within\"."
  (ignore-errors
    (save-excursion
      (beginning-of-defun)
      (buffer-substring (point-at-bol) (point-at-eol)))))

(defun dogears--format-record (record)
  "Return bookmark RECORD formatted."
  (pcase-let* ((`(,relevance ,within ,line ,mode ,buffer ,position ,dir)
                (dogears--format-record-list record)))
    (format "[%10s]  (%25s)  \"%25s\"  %17s  %s:%s\\%s"
            relevance within line mode buffer position dir)))

(defun dogears--format-record-list (record)
  "Return a list of elements in RECORD formatted."
  (pcase-let* ((`(,name . ,(map filename position line within mode manual)) record)
               (buffer (copy-sequence
                        (if filename
                            (file-name-nondirectory filename )
                          name)))
               (line (truncate-string-to-width
                      (string-trim (copy-sequence line)) dogears-line-width))
               (relevance (dogears--relevance record))
               (dir))
    ;; NOTE: To avoid weird "invalid face" errors that may result from adding text
    ;; properties to strings every time this function is called, we copy all strings.
    (if position
        (setf position (number-to-string position))
      (setf position ""))
    (if filename
        (setf dir (split-string (file-name-directory filename) "/" t)
              dir (nreverse dir)
              dir (cl-loop for d in dir
                           concat (truncate-string-to-width d 10)
                           concat "\\")
              dir (propertize dir 'face 'font-lock-comment-face)
              )
      (setf dir ""))
    (if within
        (progn
          ;; Does `truncate-string-to-width' return a copy of the string if it's already
          ;; that short?  Who knows.  So we have to be sure, because we're modifying the
          ;; properties of it, and we don't want to do that to the original.
          (setf within (copy-sequence within)
                within (truncate-string-to-width within 25))
          (add-face-text-property 0 (length within) '(:inherit (font-lock-function-name-face)) 'append within))
      (setf within ""))
    ;; Add more faces.
    (setf buffer (propertize buffer 'face 'font-lock-constant-face)
          relevance (propertize relevance 'face 'font-lock-keyword-face)
          mode (propertize (symbol-name mode) 'face 'font-lock-type-face))
    (add-face-text-property 0 (length line) '(:inherit (font-lock-string-face))
                            'append line)
    (list manual relevance within line mode buffer position dir)))

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
          ((equal mode 'help-mode)
           "help")
          (t ""))))

(defun dogears--ignored-mode-p ()
  "Return non-nil if current buffer's major mode is ignored.
Compares against modes in `dogears-ignore-modes'."
  (or (member major-mode dogears-ignore-modes)
      (apply #'derived-mode-p dogears-ignore-modes)))

;;;; Tabulated list mode

(require 'tabulated-list)

(defvar dogears-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'dogears-list-go)
    map))

(defun dogears-list-go ()
  "Go to place at point."
  (interactive)
  (dogears-go (tabulated-list-get-id)))

(defun dogears-list ()
  "Show dogears list."
  (interactive)
  (with-current-buffer (get-buffer-create "*Dogears List*")
    (dogears-list-mode)
    (pop-to-buffer (current-buffer))))

(define-derived-mode dogears-list-mode tabulated-list-mode
  "Dogears-List"
  :group 'dogears
  (setf tabulated-list-format (vector
                               '("#" 3 (lambda (a b)
                                         (< (string-to-number (elt (cadr a) 0))
                                            (string-to-number (elt (cadr b) 0)))))
                               (list (propertize "✓" 'help-echo "Manually remembered") 1 t)
                               '("Relevance" 9 t :right-align t)
                               '("Within" 25 t)
                               '("Line" 25 t)
                               '("Mode" 17 t :right-align t)
                               '("Buffer" 15 t :right-align t)
                               '("Pos" 4)
                               '("Directory" 25 t))
        tabulated-list-sort-key '("#" . nil))
  (add-hook 'tabulated-list-revert-hook #'dogears-list--set-entries nil 'local)
  (tabulated-list-init-header)
  (dogears-list--set-entries)
  (tabulated-list-revert))

(defun dogears-list--set-entries ()
  "Set `tabulated-list-entries'."
  (setf tabulated-list-entries
        (cl-loop for place in dogears-list
                 for i from 0 to 20
                 collect (list place
                               (cl-coerce (cons (number-to-string i)
                                                (dogears--format-record-list place))
                                          'vector)))))

;;;; Footer

(provide 'dogears)

;;; dogears.el ends here

;;; dog-ears.el --- Never lose your place again       -*- lexical-binding: t; -*-

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

(defvar dog-ears-list nil
  "List of dogeared places.")

(defvar dog-ears-idle-timer nil
  "Idle timer which dog-ears the current place.")

;;;; Customization

(defgroup dog-ears nil
  "Never lose your place again."
  :group 'convenience)

(defcustom dog-ears-functions nil
  "Functions which should dogear a place when called.
These are advised when `dog-ears-mode' is activated."
  :type '(repeat function))

(defcustom dog-ears-hooks
  '(imenu-after-jump-hook)
  "Hooks which should dogear a place when run.
Dog-Ears adds itself to these hooks when `dog-ears-mode' is
activated."
  :type '(repeat variable))

(defcustom dog-ears-ignore-places-functions
  (list #'minibufferp
        #'dog-ears--ignored-mode-p)
  "Don't remember any places for which any of these functions return non-nil."
  :type '(repeat function))

(defcustom dog-ears-ignore-modes
  '(dog-ears-list-mode fundamental-mode helm-major-mode)
  "Don't remember any places in buffers in these modes."
  :type '(repeat symbol))

(defcustom dog-ears-idle 5
  "Remember place when Emacs is idle for this many seconds."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Never" nil)))

(defcustom dog-ears-limit 100
  "How many places to remember."
  :type 'integer)

(defcustom dog-ears-line-width 25
  "How many characters from a place's line to show."
  :type 'integer)

(defcustom dog-ears-within-function #'dog-ears--which-function
  "Function that returns what a place is \"within\"."
  :type '(choice (function-item dog-ears--which-function)
                 (function-item dog-ears--within)
                 (function :tag "Custom function")))

;;;; Commands

;;;###autoload
(define-minor-mode dog-ears-mode
  "Never lose your place again.
Dog-Ears mode keeps track of where you've been and helps you
easily find your way back."
  :global t
  (if dog-ears-mode
      (progn
        (dolist (fn dog-ears-functions)
          (advice-add fn :after #'dog-ears-remember))
        (dolist (hook dog-ears-hooks)
          (add-hook hook #'dog-ears-remember))
        (when dog-ears-idle
          (setf dog-ears-idle-timer
                (run-with-idle-timer dog-ears-idle 'repeat #'dog-ears-remember))))
    ;; Disable mode.
    (dolist (fn dog-ears-functions)
      (advice-remove fn #'dog-ears-remember))
    (dolist (hook dog-ears-hooks)
      (remove-hook hook #'dog-ears-remember))
    (when dog-ears-idle-timer
      (cancel-timer dog-ears-idle-timer)
      (setf dog-ears-idle-timer nil))))

;;;###autoload
(defun dog-ears-remember (&rest _ignore)
  "Remember the current place."
  (interactive)
  (unless (seq-some #'funcall dog-ears-ignore-places-functions)
    (if-let ((record (or (ignore-errors
                           (funcall bookmark-make-record-function))
                         (dog-ears--buffer-record))))
        (progn
          (setf (map-elt (cdr record) 'manual)
                (if (called-interactively-p 'interactive) "✓" " "))
          (unless (stringp (car record))
            ;; Like `bookmark-make-record', we may have to add a string
            ;; ourselves.  And we want every record to have one as its
            ;; first element, for consistency.
            (push "" record))
          (unless (map-elt (cdr record) 'buffer)
            (setf (map-elt (cdr record) 'buffer) (buffer-name)))
          (when-let ((within (or (funcall dog-ears-within-function)
                                 (dog-ears--within)
                                 (car record))))
            (setf (map-elt (cdr record) 'within) within))
          (setf (map-elt (cdr record) 'mode) major-mode
                (map-elt (cdr record) 'line) (buffer-substring
                                              (point-at-bol) (point-at-eol)))
          (push record dog-ears-list)
          (setf dog-ears-list (delete-dups dog-ears-list)
                dog-ears-list (seq-take dog-ears-list dog-ears-limit)))
      (when (called-interactively-p 'interactive)
        (message "Dog-Ears: Couldn't dogear this place")))))

;;;###autoload
(defun dog-ears-go (place)
  "Go to dogeared PLACE.
Interactively, select PLACE with completion.  PLACE should be a
bookmark record."
  (interactive (let* ((collection (cl-loop for place in dog-ears-list
                                           for key = (dog-ears--format-record place)
                                           collect (cons key place)))
                      (choice (completing-read "Place: " collection nil t)))
                 (list (alist-get choice collection nil nil #'equal))))
  (or (ignore-errors
        (bookmark-jump place))
      (when-let ((buffer (map-elt (cdr place) 'buffer)))
        (when (stringp buffer)
          (setf buffer (get-buffer buffer)))
        (if (buffer-live-p buffer)
            (switch-to-buffer buffer)
          (user-error "Buffer no longer exists: %s" buffer)))))

;;;; Functions

(defun dog-ears--buffer-record ()
  "Return a bookmark-like record for the current buffer.
Intended as a fallback for when `bookmark-make-record-function'
returns nil."
  (list (buffer-name)
        (cons 'buffer (current-buffer))
        (cons 'location (buffer-name))
        (cons 'within (buffer-name))
        (cons 'mode major-mode)
        (cons 'position (point))))

(defun dog-ears--within ()
  "Return string representing what the current place is \"within\"."
  (cl-case major-mode
    (Info-mode
     ;; HACK: To make Info buffer records more useful, return nil so the
     ;; bookmark name is used.
     nil)
    (otherwise (ignore-errors
                 (save-excursion
                   (beginning-of-defun)
                   (buffer-substring (point-at-bol) (point-at-eol)))))))

(defun dog-ears--format-record (record)
  "Return bookmark RECORD formatted."
  (pcase-let* ((`(,manual ,relevance ,within ,line ,mode ,buffer ,position ,dir)
                (dog-ears--format-record-list record)))
    (format "%s [%9s]  (%25s)  \"%25s\"  %12s  %s:%s\\%s"
            manual relevance within line mode buffer position dir)))

(defun dog-ears--format-record-list (record)
  "Return a list of elements in RECORD formatted."
  (pcase-let* ((`(,name . ,(map filename position line within mode manual)) record)
               (buffer (copy-sequence
                        (if filename
                            (file-name-nondirectory filename)
                          name)))
               (line (truncate-string-to-width
                      (string-trim (copy-sequence line)) dog-ears-line-width))
               (relevance (dog-ears--relevance record))
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
              dir (propertize dir 'face 'font-lock-comment-face))
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
          mode (propertize (string-remove-suffix "-mode" (symbol-name mode)) 'face 'font-lock-type-face))
    (add-face-text-property 0 (length line) '(:inherit (font-lock-string-face))
                            'append line)
    (list manual relevance within line mode buffer position dir)))

(defun dog-ears--relevance (record)
  "Return the relevance string for RECORD."
  (pcase-let* ((`(,name . ,(map filename within mode)) record))
    (when filename
      (setf filename (expand-file-name filename)))
    (cond ((when-let ((now-within (or (funcall dog-ears-within-function)
                                      (dog-ears--within)
                                      name)))
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
          ((equal mode 'Info-mode)
           "Info")
          (t ""))))

(defun dog-ears--ignored-mode-p ()
  "Return non-nil if current buffer's major mode is ignored.
Compares against modes in `dog-ears-ignore-modes'."
  (member major-mode dog-ears-ignore-modes))

(defun dog-ears--which-function ()
  "Call `which-function' while preventing it from using `add-log-current-defun'."
  (cl-letf (((symbol-function 'add-log-current-defun) #'ignore))
    (which-function)))

;;;; Tabulated list mode

(require 'tabulated-list)

(defvar dog-ears-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'dog-ears-list-go)
    (define-key map (kbd "k") #'dog-ears-list-delete)
    map))

(defvar dog-ears-list-called-from nil
  "Buffer that `dog-ears-list' was called from.")

(defun dog-ears-list-go ()
  "Go to place at point."
  (interactive)
  (dog-ears-go (tabulated-list-get-id)))

(defun dog-ears-list-delete ()
  "Delete entry at point."
  (interactive)
  (let ((place (tabulated-list-get-id)))
    (setf dog-ears-list (cl-delete place dog-ears-list)))
  (tabulated-list-revert))

;;;###autoload
(defun dog-ears-list ()
  "Show dog-ears list."
  (interactive)
  (let ((called-from (current-buffer)))
    (with-current-buffer (get-buffer-create "*Dog-Ears List*")
      (setf dog-ears-list-called-from called-from)
      (dog-ears-list-mode)
      (pop-to-buffer (current-buffer)))))

(define-derived-mode dog-ears-list-mode tabulated-list-mode
  "Dog-Ears-List"
  :group 'dog-ears
  (setf tabulated-list-format (vector
                               '("#" 3 (lambda (a b)
                                         (< (string-to-number (elt (cadr a) 0))
                                            (string-to-number (elt (cadr b) 0)))))
                               (list (propertize "✓" 'help-echo "Manually remembered") 1 t)
                               '("Relevance" 10 t :right-align t)
                               '("Within" 25 t)
                               '("Line" 25 t)
                               '("Mode" 12 t :right-align t)
                               '("Buffer" 15 t :right-align t)
                               '("Pos" 5)
                               '("Directory" 25 t))
        tabulated-list-sort-key '("#" . nil))
  (add-hook 'tabulated-list-revert-hook
            (lambda ()
              (setf tabulated-list-entries
                    (with-current-buffer (or dog-ears-list-called-from
                                             (current-buffer))
                      (dog-ears-list--entries))))
            nil 'local)
  (tabulated-list-init-header)
  (tabulated-list-revert))

(defun dog-ears-list--entries ()
  "Return `tabulated-list-entries'."
  (cl-loop for place in dog-ears-list
           for i from 0
           collect (list place
                         (cl-coerce (cons (number-to-string i)
                                          (dog-ears--format-record-list place))
                                    'vector))))

;;;; Footer

(provide 'dog-ears)

;;; dog-ears.el ends here

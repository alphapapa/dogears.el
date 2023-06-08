;;; dogears.el --- Never lose your place again       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/dogears.el
;; Version: 0.2-pre
;; Package-Requires: ((emacs "26.3") (map "2.1"))
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

(defvar dogears-index 0
  "Index of last-visited dogeared place.
Used in `dogears-back' and `dogears-forward'.")

(defvar dogears-last-place nil
  "Last-visited dogeared place.")

(defvar dogears-idle-timer nil
  "Idle timer which dogears the current place.")

(defvar dogears-list-buffer nil
  "The \"*Dogears List*\" buffer.")

(defvar dogears-functions-advice nil
  "Alist mapping advised functions to advice functions.
Used to remove advice when `dogears-mode' is disabled.")

;;;; Customization

(defgroup dogears nil
  "Never lose your place again."
  :group 'convenience)

(defcustom dogears-ellipsis "…"
  "Ellipsis string used to truncate strings."
  :type 'string)

(defcustom dogears-functions nil
  "Functions which should dogear a place when called.
These are advised when `dogears-mode' is activated."
  :type '(repeat function))

(defcustom dogears-hooks
  '(imenu-after-jump-hook)
  "Hooks which should dogear a place when run.
Dogears adds `dogears-remember' to these hooks when
`dogears-mode' is activated."
  :type '(repeat variable))

(defcustom dogears-ignore-places-functions
  (list #'minibufferp
        #'dogears--ignored-mode-p)
  "Ignore places for which any of these functions return non-nil."
  :type '(repeat function))

(defcustom dogears-ignore-modes
  '(fundamental-mode dogears-list-mode exwm-mode helm-major-mode)
  "Don't remember places in buffers in these modes."
  :type '(repeat symbol))

(defcustom dogears-idle 5
  "Remember place when Emacs is idle for this many seconds."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Never" nil)))

(defcustom dogears-limit 100
  "How many places to remember."
  :type 'integer)

(defcustom dogears-line-width 25
  "How many characters from a place's line to show."
  :type 'integer)

(defcustom dogears-position-delta 100
  ;; We use buffer positions rather than lines to avoid calculating
  ;; `line-number-at-pos' every time a place record is made.
  "Maximum difference between places' positions for them to be considered equal.
If, all else being equal, two places are within this many
characters of each other, they are considered the same and are
deduplicated from `dogears-list'.  (See function
`dogears--equal'.)"
  :type 'integer)

(defcustom dogears-sidebar-alist
  '((side . right)
    (window-parameters
     (no-delete-other-windows . t)))
  "Alist passed to `display-buffer-in-side-window', which see."
  :type 'sexp)

(defcustom dogears-within-function #'dogears--which-function
  "Function that returns what a place is \"within\"."
  :type '(choice (function-item dogears--which-function)
                 (function-item dogears--within)
                 (function :tag "Custom function")))

(defcustom dogears-update-list-buffer t
  "Automatically update the `dogears-list' buffer.
The buffer is updated when commands like `dogears-remember',
`dogears-go', and `dogears-back' are called."
  :type 'boolean)

(defcustom dogears-message t
  "Echo a message when moving back/forward."
  :type 'boolean)

;;;; Commands

;;;###autoload
(define-minor-mode dogears-mode
  "Never lose your place again.
Like dogeared pages in a book, Dogears mode keeps track of where
you've been and helps you retrace your steps."
  :global t
  (if dogears-mode
      (progn
        (dolist (fn dogears-functions)
          ;; The interactive form of an advice function overrides the
          ;; form of an advised function, so we must use a lambda with
          ;; the advised function's interactive form.
          (let* ((advice-fn-symbol (intern (format "dogears--remember-after-%s" fn)))
                 (advice-fn
		  `(lambda (&rest _ignore)
                     ,(format "Call `dogears-remember'.  Used as :after advice for `%s'."
                              fn)
                     ,(interactive-form fn)
                     (dogears-remember))))
            (fset advice-fn-symbol advice-fn)
            (advice-add fn :after advice-fn-symbol )
            (setf (map-elt dogears-functions-advice fn) advice-fn-symbol)))
        (dolist (hook dogears-hooks)
          (add-hook hook #'dogears-remember))
        (when dogears-idle
          (setf dogears-idle-timer
                (run-with-idle-timer dogears-idle 'repeat #'dogears-remember))))
    ;; Disable mode.
    (dolist (fn dogears-functions)
      (advice-remove fn (map-elt dogears-functions-advice fn))
      (unintern (map-elt dogears-functions-advice fn) nil)
      (setf dogears-functions-advice (map-delete dogears-functions-advice fn)))
    (dolist (hook dogears-hooks)
      (remove-hook hook #'dogears-remember))
    (when dogears-idle-timer
      (cancel-timer dogears-idle-timer)
      (setf dogears-idle-timer nil))))

(defun dogears--place (&optional manualp)
  "Return record for current buffer at point."
  (when-let ((record (or (ignore-errors
                           (funcall bookmark-make-record-function))
                         (dogears--buffer-record))))
    (pcase (car record)
      ;; Like `bookmark-make-record', we may have to add a string ourselves.
      ;; And we want every record to have one as its first element, for
      ;; consistency.  And sometimes, records have a nil name rather than an
      ;; empty string, depending on the bookmark-make-record-function (I'm
      ;; not sure if there are defined standards for what the first element
      ;; of a bookmark record should be).
      ((pred stringp)
       ;; Record already has a string as its first element: do nothing.
       nil)
      (`nil (setf (car record) ""))
      (_ (push "" record)))
    (setf (map-elt (cdr record) 'manualp) manualp)
    (unless (map-elt (cdr record) 'buffer)
      (setf (map-elt (cdr record) 'buffer) (buffer-name)))
    (when-let ((within (or (funcall dogears-within-function)
                           (dogears--within)
                           (car record))))
      (setf (map-elt (cdr record) 'within) within))
    (setf (map-elt (cdr record) 'mode) major-mode
          (map-elt (cdr record) 'line) (buffer-substring
                                        (point-at-bol) (point-at-eol)))
    record))

;;;###autoload
(defun dogears-remember (&rest _ignore)
  "Remember (\"dogear\") the current place."
  (interactive)
  (unless (cl-some #'funcall dogears-ignore-places-functions)
    (if-let ((record (dogears--place (called-interactively-p 'interactive))))
        (progn
          ;; It's hard to say whether push or pushnew is the best choice.  When returning
          ;; to a dogeared place, that place gets moved to the front of the list, or it
          ;; remains where it was.  Either way, unless we allow dupes, the list changes.
          (cl-pushnew record dogears-list :test #'dogears--equal)
          (setf dogears-list (delete-dups dogears-list)
                dogears-list (seq-take dogears-list dogears-limit))
          (dogears--update-list-buffer)
          (when (called-interactively-p 'interactive)
            (message "Dogeared")))
      (when (called-interactively-p 'interactive)
        (message "Dogears: Couldn't dogear this place")))))

;;;###autoload
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
        (when (stringp buffer)
          (setf buffer (get-buffer buffer)))
        (if (buffer-live-p buffer)
            (switch-to-buffer buffer)
          (user-error "Buffer no longer exists: %s" buffer))))
  (dogears--update-list-buffer))

(defun dogears-back (&optional manualp)
  "Go to previous dogeared place.
If MANUALP (interactively, with prefix), go to previous manually
remembered place."
  (interactive "P")
  (dogears-move 'backward manualp))

(defun dogears-forward (&optional manualp)
  "Go to next dogeared place.
If MANUALP (interactively, with prefix), go to next manually
remembered place."
  (interactive "P")
  (dogears-move 'forward manualp))

;;;; Functions

(defun dogears-move (direction &optional manualp)
  "Move to next/previous dogeared place in DIRECTION.
DIRECTION may be `forward' or `backward'.  If MANUALP, only
consider manually dogeared places."
  (let* ((current-place (dogears--place) )
         (predicate (lambda (place)
                      (and (not (dogears--equal place current-place))
                           (or (not manualp)
                               (map-elt (cdr place) 'manualp)))))
         (place (cl-find-if predicate dogears-list
                            :start (pcase direction
                                     ('forward (1+ dogears-index)))
                            :end (pcase direction
                                   ('backward dogears-index))
                            :from-end (equal 'backward direction))))
    (if (and place (not (dogears--equal place current-place :ignore-manual-p manualp)))
        (progn
          (dogears-go place)
          (setf dogears-index (cl-position place dogears-list :test #'dogears--equal))
          (when dogears-message
            (message "Dogears: %s to %s/%s"
                     (pcase direction
                       ('backward "Back")
                       ('forward "Forward"))
                     dogears-index (length dogears-list))))
      (user-error "At %s %sdogeared place"
                  (pcase direction
                    ('backward "oldest")
                    ('forward "newest"))
                  (if manualp "manually " "")))))

(defun dogears--update-list-buffer ()
  "Update list buffer if it is open and so-configured."
  (when (and dogears-update-list-buffer (buffer-live-p dogears-list-buffer))
    (with-current-buffer dogears-list-buffer
      (revert-buffer))))

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

(cl-defun dogears--equal (a b &key ignore-manual-p)
  "Return non-nil if places A and B are considered equal.
A and B should be bookmark records as stored in `dogears-list'.
They are considered equal if they have the same elements, with
two exceptions: their `line's may differ, and their `position's
may differ by up to `dogears-position-delta'.  If
IGNORE-MANUAL-P, ignore whether places were manually remembered."
  (pcase-let* ((`(,a-name . ,(map ('filename a-filename) ('line _a-line)
                                  ('manualp a-manualp) ('mode a-mode)
                                  ('position a-position) ('within a-within)))
                a)
               (`(,b-name . ,(map ('filename b-filename) ('line _b-line)
                                  ('manualp b-manualp) ('mode b-mode)
                                  ('position b-position) ('within b-within)))
                b))
    ;; Test elements in, roughly, the order of some balance of factors
    ;; involving what's quickest to compare and what's most likely to differ.
    (and (equal a-mode b-mode)
         (or ignore-manual-p (equal a-manualp b-manualp))
         (equal a-within b-within)
         (equal a-filename b-filename)
         (equal a-name b-name)
         ;; FIXME: Some bookmarks don't have a position, and that prevents
         ;; them from ever being equal, even if they should be.
         (and a-position b-position)
         (<= (abs (- a-position b-position)) dogears-position-delta))))

(defun dogears--within ()
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

(defun dogears--format-record (record)
  "Return bookmark RECORD formatted."
  (pcase-let* ((`(,manualp ,relevance ,within ,line ,mode ,buffer ,position ,dir)
                (dogears--format-record-list record)))
    (format "%s [%9s]  (%25s)  \"%25s\"  %s %12s %s\\%s"
            (if manualp "✓" " ") relevance within line buffer mode position dir)))

(defun dogears--format-record-list (record)
  "Return a list of elements in RECORD formatted."
  (cl-labels ((face-propertize
               (string face)
               ;; Return copy of STRING with FACE appended, but only if it doesn't already
               ;; contain FACE.  (I don't know a better way to prevent faces being added
               ;; repeatedly, which eventually, drastically slows down redisplay).
               (setf string (copy-sequence string))
               (let ((property (get-text-property 0 'face string)))
                 (unless (or (equal face property)
			     (and (listp property) (member face property)))
                   (add-face-text-property 0 (length string) face 'append string)))
               string))
    (pcase-let* ((`(,name . ,(map filename line manualp mode position within)) record)
                 (manual (if manualp "✓" " "))
                 (buffer (face-propertize (if filename
                                              (file-name-nondirectory filename)
                                            name)
                                          'font-lock-constant-face))
                 (line (string-trim line))
                 (mode (face-propertize (string-remove-suffix "-mode" (symbol-name mode))
                                        'font-lock-type-face))
                 (position (if position
                               (number-to-string position)
                             ""))
                 (relevance (face-propertize (dogears--relevance record)
                                             'font-lock-keyword-face))
                 (within (if within
                             (face-propertize within 'font-lock-function-name-face)
                           ""))
                 ;; The filename may not always *be* a filename; e.g. somehow in
                 ;; EWXM buffers it gets set to " - no file -", instead of just nil.
                 (dir (when filename
                        (file-name-directory filename))))
      (if dir
          (setf dir (split-string dir "/" t)
                dir (nreverse dir)
                dir (cl-loop for d in dir
                             concat (truncate-string-to-width d 10)
                             concat "\\")
                dir (face-propertize dir 'font-lock-comment-face))
        (setf dir ""))
      (list manual relevance within line buffer mode position dir))))

(defun dogears--relevance (record)
  "Return the relevance string for RECORD."
  (pcase-let* ((`(,name . ,(map filename within mode)) record))
    (when filename
      (setf filename (expand-file-name filename)))
    (cond ((when-let ((now-within (or (funcall dogears-within-function)
                                      (dogears--within)
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

(defun dogears--ignored-mode-p ()
  "Return non-nil if current buffer's major mode is ignored.
Compares against modes in `dogears-ignore-modes'."
  (member major-mode dogears-ignore-modes))

(defun dogears--which-function ()
  "Call `which-function' while preventing it from using `add-log-current-defun'."
  (cl-letf (((symbol-function 'add-log-current-defun) #'ignore))
    (which-function)))

;;;; Tabulated list mode

(require 'tabulated-list)

(defvar truncate-string-ellipsis)

(defvar dogears-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'dogears-list-go)
    (define-key map (kbd "k") #'dogears-list-delete)
    map))

(defun dogears-list-go ()
  "Go to place at point."
  (interactive)
  (dogears-go (tabulated-list-get-id)))

(defun dogears-list-delete ()
  "Delete entry at point."
  (interactive)
  (let ((place (tabulated-list-get-id)))
    (setf dogears-list (cl-delete place dogears-list)))
  (tabulated-list-revert))

;;;###autoload
(defun dogears-list ()
  "Show dogears list."
  (interactive)
  (with-current-buffer (get-buffer-create "*Dogears List*")
    (setf dogears-list-buffer (current-buffer))
    (dogears-list-mode)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun dogears-sidebar ()
  "Show the Dogears list in a side window."
  (interactive)
  (let* ((buffer (save-window-excursion
                   (dogears-list)
                   (window-buffer)))
         (display-buffer-mark-dedicated t)
         (window (display-buffer-in-side-window buffer dogears-sidebar-alist)))
    (when window
      ;; t has special meaning to `set-window-dedicated-p', and I
      ;; don't know if that's what we want.
      (set-window-dedicated-p window 'non-nil))))

(define-derived-mode dogears-list-mode tabulated-list-mode
  "Dogears-List"
  :group 'dogears
  (setf tabulated-list-format (vector
                               '("#" 3 (lambda (a b)
                                         (< (string-to-number (elt (cadr a) 0))
                                            (string-to-number (elt (cadr b) 0)))))
                               (list (propertize "✓" 'help-echo "Manually remembered") 1 t)
                               '("Relevance" 10 t :right-align t)
                               '("Within" 25 t)
                               '("Line" 20 t)
                               '("Buffer" 20 t)
                               '("Mode" 12 t :right-align t)
                               '("Pos" 5)
                               '("Directory" 25 t))
        tabulated-list-sort-key '("#" . nil)
        truncate-string-ellipsis dogears-ellipsis)
  (add-hook 'tabulated-list-revert-hook
            (lambda ()
              (setf tabulated-list-entries
                    (with-current-buffer (window-buffer (get-mru-window t nil nil))
                      (dogears-list--entries))))
            nil 'local)
  (tabulated-list-init-header)
  (tabulated-list-revert))

(defun dogears-list--entries ()
  "Return `tabulated-list-entries'."
  (cl-loop for place in dogears-list
           for i from 0
           for index = (if (= i dogears-index)
                           (propertize (number-to-string i)
                                       'face 'font-lock-keyword-face)
                         (number-to-string i))
           collect (list place
                         (cl-coerce (cons index (dogears--format-record-list place))
                                    'vector))))

;;;; Footer

(provide 'dogears)

;;; dogears.el ends here

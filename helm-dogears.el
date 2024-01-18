;;; helm-dogears.el --- Helm source for Dogears      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/dogears.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.3") (dogears "0.1-pre") (helm "3.6"))
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

;; This library provides a Helm source for Dogears.  It may be used in
;; any Helm command by adding it to the sources list.  Note that it's
;; not necessary to have a `helm-dogears' command, because `helm-mode'
;; is already compatible with the `dogears-go' command.

;;; Code:

;;;; Requirements

(require 'helm)

(require 'dogears)

;;;; Variables
(defvar helm-dogears--multiline nil
  "Set to t or nil by `helm-dogears-multiline-count''s setter function.")

;; We autoload this so people can use it in other Helm commands
;; without having to remember to load this library first.  (An easy
;; oversight to make--I have many times.)

;;;###autoload
(defvar helm-dogears-source
  (helm-make-source "Dogears" 'helm-source-sync
    :candidates (lambda ()
                  (cl-loop for place in dogears-list
                           collect (cons (dogears--format-record place helm-dogears--multiline)
                                         place)))
    :multiline t
    :action (list (cons "Go to place" #'dogears-go))))


;;;; Customization
;; Setter functions
(defun helm-dogears--multiline-set (sym val)
  "Set SYM to VAL and `helm-dogears--multiline' to t if VAL is a non-null integer.
This is a setter function for the `helm-dogears-multiline-count' customizable variable."
  (set sym val)
  (setq helm-dogears--multiline (not (= val 0))))

;; Customizable variables
(defcustom helm-dogears-multiline-count 0
  "Number of surrounding lines to display for each record.
For example, a value of 3 will display 3 lines above and 3 lines
beneath each record.  Set to 0 if you don't want the surrounding
lines to be displayed."
  :group 'dogears
  :type 'integer
  :set 'helm-dogears--multiline-set
  :initialize 'custom-initialize-set)

;;;; Commands

;;;###autoload
(defun helm-dogears ()
  "Show `helm-dogears-source' with Helm."
  (interactive)
  (helm :sources 'helm-dogears-source))


;;;; Functions

;;;; Footer

(provide 'helm-dogears)

;;; helm-dogears.el ends here

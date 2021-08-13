;;; helm-dogears.el --- Helm source for Dogears      -*- lexical-binding: t; -*-

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

;; This library provides a Helm source for Dogears.  It may be used in
;; any Helm command by adding it to the sources list.  Note that it's
;; not necessary to have a `helm-dogears' command, because `helm-mode'
;; is already compatible with the `dogears-go' command.

;;; Code:

;;;; Requirements

(require 'dogears)

(require 'helm)

;;;; Variables

(defvar helm-dogears-source
  (helm-make-source "Dogears" 'helm-source-sync
    :candidates (lambda ()
                  (cl-loop for place in dogears-list
                           collect (cons (dogears--format-record place)
                                         place)))
    :action (list (cons "Go to place" #'dogears-go))))


;;;; Customization


;;;; Commands


;;;; Functions


;;;; Footer

(provide 'helm-dogears)

;;; helm-dogears.el ends here

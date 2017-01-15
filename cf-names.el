;;; cf-names.el --- Look up CF standard names in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2017 Constantine Khrulev

;; Author: Constantine Khrulev <ckhroulev@alaska.edu>
;; URL: http://github.com/ckhroulev/cf-names/
;; Version: 1.0
;; Created: 2017-1-13
;; By: Constantine Khrulev
;; Keywords: convenience, science

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Install:

;; To install, put cf-names.el in your load path and add the following
;; to your .emacs file:
;;
;; (require 'cf-names)
;;
;; You also need to download the XML standard names table from
;; http://cfconventions.org/standard-names.html and customize
;; cf-names-table-filename to point to the downloaded file.
;;
;; Once this is done, use M-x cf-names-lookup RET
;;
;; to search the table.
;;
;; This package requires Helm and Emacs built with libxml.
;;
;; Tested using Emacs 25.1.

;;; Commentary:

;; This package simplifies searching the CF standard names table by
;; creating a Helm source and a function that uses it.

;;; Code:

(require 'helm)
(require 'dom)
(require 'cl-lib)

(defcustom cf-names-table-filename
  "~/github/ckhroulev/cf-names/cf-standard-name-table.xml"
  "Path to the CF standard names table in XML.

Get the latest version from

http://cfconventions.org/standard-names.html

Note: you can gzip this file to save space.")

(defgroup cf-names
  '((cf-names-table-filename custom-variable))
  "CF standard name lookup using Helm.")

(defun cf-names-init ()
  "Parse the XML CF standard names table and build the list of
entries that can be used as helm candidates.

Each entry is a list; the first element is the summary, followed
by the standard name, canonical units, description, GRIB name,
and AMIP name."
  (let ((standard-names-table (with-temp-buffer
                                (insert-file-contents cf-names-table-filename)
                                (libxml-parse-xml-region (point-min) (point-max)))))
    (mapcar (lambda (entry)
              (let ((id          (dom-attr entry 'id))
                    (units       (dom-text (dom-by-tag entry 'canonical_units)))
                    (description (dom-text (dom-by-tag entry 'description)))
                    (grib        (dom-text (dom-by-tag entry 'grib)))
                    (amip        (dom-text (dom-by-tag entry 'amip))))
                (list (format "%s (%s)%s%s" id units
                              (if (string-empty-p grib) "" (format " grib: '%s'" grib))
                              (if (string-empty-p amip) "" (format " amip: '%s'" amip)))
                      id units description grib amip)))
            (dom-by-tag standard-names-table 'entry))))


(defun cf-names-init-aliases ()
  "Parse the XML CF standard names table and build the alist of
aliases of CF standard names.
"
  (let ((standard-names-table (with-temp-buffer
                                (insert-file-contents cf-names-table-filename)
                                (libxml-parse-xml-region (point-min) (point-max)))))
    (mapcar (lambda (alias)
              (let ((id   (dom-attr alias 'id))
                    (name (dom-text (dom-by-tag alias 'entry_id))))
                (cons id name)))
            (dom-by-tag standard-names-table 'alias))))

(defvar cf-names-aliases nil)

(defvar cf-names nil
  "List of CF standard names. This list does not change very
  often, so this code does not try to detect changes to the XML
  standard names table. Set this to `nil' to re-initialize.")

(defun cf-names ()
  "Return the list of standard names. See `cf-names-init' for
details."
  (or cf-names (setq cf-names-aliases (cf-names-init-aliases)
                     cf-names (cf-names-init))))

(defun cf-names-row (a b)
  "Format a row of the table (internal)."
  `(tr ()
       (td () (b () ,a))
       (td () ,(if (string-empty-p b) "empty" b))))

(defun cf-names-display-entry (data)
  "Display an entry of the CF standard names table.
`data' is a list consisting of (id units description grib amip)."
  (cl-multiple-value-bind (id units description grib amip) data
    (let ((inhibit-read-only t))
      (with-current-buffer (get-buffer-create "*cf-names-info*")
        (erase-buffer)
        ;; we need to display this buffer because shr uses window width
        (display-buffer (current-buffer))
        (shr-insert-document
         `(table ()
                 (caption () (b () ,id))
                 (tr)
                 ,(cf-names-row "Units"       units)
                 ,(cf-names-row "Description" description)
                 ,(cf-names-row "GRIB"        grib)
                 ,(cf-names-row "AMIP"        amip)))
        (view-mode nil)))))

(defvar cf-names-source
  (helm-build-sync-source "CF standard names"
    :candidates #'cf-names
    :candidate-number-limit 5000 ; about 2800 names at the time of writing
    :action '(("Display entry" . cf-names-display-entry)
              ("Insert standard name" . (lambda (data) (insert (car data))))))
  "Helm CF standard names source.")

;;;###autoload
(defun cf-names-lookup ()
  "Look up an entry of the CF standard names table using Helm."
  (interactive)
  (helm :sources cf-names-source
        :buffer "*cf-names*"))

(provide 'cf-names)
;; cf-names.el ends here

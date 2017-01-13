;; -*- lexical-binding: t -*-
(require 'helm)
(require 'dom)
(require 'cl-lib)

(defcustom cf-names-table-filename
  "~/github/ckhroulev/helm-cf-standard-names/cf-standard-name-table.xml.gz"
  "Path to the CF standard names table (an XML document).")

(defun cf-names-parse ()
  (let ((xml (with-temp-buffer
               (insert-file-contents cf-names-table-filename)
               (libxml-parse-xml-region (point-min) (point-max)))))
    (mapcar (lambda (entry)
              (let ((id (dom-attr entry 'id))
                    (units (dom-text (dom-by-tag entry 'canonical_units)))
                    (doc   (dom-text (dom-by-tag entry 'description)))
                    (grib  (dom-text (dom-by-tag entry 'grib)))
                    (amip  (dom-text (dom-by-tag entry 'amip))))
                (list (format "%s (%s)" id units)
                      id units doc grib amip)))
            (dom-by-tag xml 'entry))))

(defvar cf-names nil)
(defun  cf-names () (or cf-names (setf cf-names (cf-names-parse))))

(defun cf-names-row (a b)
  "Format a row of the table."
  `(tr ()
       (td () (b () ,a))
       (td () ,(if (string= b "") "empty" b))))

(defun cf-names-display-entry (data)
  "Display an entry of the CF standard names table.
`data' is a list consisting of (name units description grib amip)."
  (multiple-value-bind (id units doc grib amip) data
   (let ((buffer (get-buffer-create "*cf-names-info*"))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (display-buffer buffer) ; shr uses window info to render, so we need to display it
      (shr-insert-document
       `(html ()
              (body ()
                    (table ()
                           ,(cf-names-row "Units"       units)
                           ,(cf-names-row "Description" doc)
                           ,(cf-names-row "GRIB"        grib)
                           ,(cf-names-row "AMIP"        amip)))))
      (view-mode nil)))))

(defvar cf-names-source
  (helm-build-sync-source "CF standard names"
    :candidates #'cf-names
    :candidate-number-limit 5000 ; about 2800 names at the time of writing
    :action '(("Display entry" . cf-names-display-entry)
              ("Insert" . (lambda (data) (insert (car data)))))))

(defun cf-names-lookup ()
  "Look up an entry of the CF standard names table using Helm."
  (interactive)
  (helm :sources cf-names-source
        :buffer "*cf-names*"))

(provide 'cf-names)

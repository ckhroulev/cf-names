;; -*- lexical-binding: t -*-
(require 'helm)
(require 'dom)
(require 'cl)

(defcustom cf-standard-names-table-filename
  "~/github/ckhroulev/helm-cf-standard-names/cf-standard-name-table.xml.gz"
  "Path to the CF standard names table (an XML document).")

(defun cf-standard-names-parse ()
  (let ((xml (with-temp-buffer
               (insert-file-contents cf-standard-names-table-filename)
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

(defvar cf-standard-names nil)

(defun cf-standard-names ()
  (or cf-standard-names
      (setf cf-standard-names (cf-standard-names-parse))))

(defun cf-standard-names-display-entry (data)
  "Display an entry of the CF standard names table.
`data' is a list consisting of (name units description grib amip)."
  (multiple-value-bind (id units doc grib amip) data
   (let ((buffer (get-buffer-create "*cf-standard-names-info*"))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (display-buffer buffer) ; shr uses window info to render, so we need to display it
      (shr-insert-document
       `(html ()
              (body ()
                    (table ()
                           (tr () (td () (b () "Name"))        (td () ,id))
                           (tr () (td () (b () "Units"))       (td () ,units))
                           (tr () (td () (b () "Description")) (td () ,doc))
                           (tr () (td () (b () "GRIB"))
                               (td () ,(if (string= grib "") "empty" grib)))
                           (tr () (td () (b () "AMIP"))
                               (td () ,(if (string= amip "") "empty" amip)))
                           ))))
      (view-mode nil)))))

(defvar cf-standard-names-source
  (helm-build-sync-source "CF standard names"
    :candidates #'cf-standard-names
    :candidate-number-limit 5000 ; about 2800 names at the time of writing
    :action '(("Display entry" . cf-standard-names-display-entry)
              ("Insert" . (lambda (data) (insert (car data)))))))

(defun cf-standard-names-lookup ()
  "Look up an entry of the CF standard names table using Helm."
  (interactive)
  (helm :sources cf-standard-names-source
        :buffer "*cf-standard-names*"))

(provide 'cf-standard-names)

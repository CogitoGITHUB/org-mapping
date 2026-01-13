;;; org-mapping.el --- Enhanced Org Mode experience suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024 CogitoGITHUB

;; Author: CogitoGITHUB
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (svg-tag-mode "0.3"))
;; Keywords: outlines, hypermedia, org, writing
;; URL: https://github.com/CogitoGITHUB/org-mapping

;;; Commentary:
;; org-mapping is a comprehensive suite for enhanced Org Mode experience.
;; It provides two main modules:
;;
;; - org-view-mode: Word processor-style view with intelligent pagination
;; - org-svg-tags: Beautiful SVG tags and interactive buttons for org documents
;;
;; To use both modules:
;;   (require 'org-mapping)
;;   (org-mapping-enable-all)
;;
;; To use individually:
;;   (require 'org-view)
;;   (require 'org-svg-tags)

;;; Code:

(require 'org)

(defgroup org-mapping nil
  "Enhanced Org Mode experience suite."
  :group 'org
  :prefix "org-mapping-")

;;; Autoload the modules

;;;###autoload
(defun org-mapping-enable-all ()
  "Enable all org-mapping modules in current org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mapping only works in org-mode buffers"))
  (require 'org-view)
  (require 'org-svg-tags)
  (org-view-mode 1)
  (org-svg-tags-mode 1)
  (message "org-mapping: All modules enabled"))

;;;###autoload
(defun org-mapping-disable-all ()
  "Disable all org-mapping modules in current buffer."
  (interactive)
  (when (featurep 'org-view)
    (org-view-mode -1))
  (when (featurep 'org-svg-tags)
    (org-svg-tags-mode -1))
  (message "org-mapping: All modules disabled"))

;;;###autoload
(defun org-mapping-setup-defaults ()
  "Setup recommended defaults for org-mapping.
This configures both org-view and org-svg-tags with sensible defaults."
  (interactive)
  
  ;; org-view defaults
  (setq org-view-body-width 85
        org-view-lines-per-page 50
        org-view-header-format 'both
        org-view-footer-format "Page %p • %d"
        org-view-respect-org-structure t)
  
  ;; org-svg-tags defaults
  (setq org-svg-tags-font-lock-case-insensitive t
        org-svg-tags-indent t
        org-svg-tags-hide-blocks t)
  
  (message "org-mapping: Default configuration applied"))

;;;###autoload
(define-minor-mode org-mapping-mode
  "Enable the complete org-mapping experience.
This activates both org-view-mode and org-svg-tags-mode."
  :init-value nil
  :lighter " OrgMap"
  :group 'org-mapping
  (if org-mapping-mode
      (org-mapping-enable-all)
    (org-mapping-disable-all)))

;; Provide the main package
(provide 'org-mapping)

;;; org-mapping.el ends here

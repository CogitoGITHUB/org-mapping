;;; org-mapping.el --- Enhanced Org Mode experience suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024 CogitoGITHUB

;; Author: CogitoGITHUB
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (svg-tag-mode "0.3") (denote "2.3.0") (org-roam "2.2.0") (org-roam-ui "0.1") (magit-section "3.3.0"))
;; Keywords: outlines, hypermedia, org, writing, knowledge-management
;; URL: https://github.com/CogitoGITHUB/org-mapping

;;; Commentary:
;; org-mapping is a comprehensive suite for enhanced Org Mode experience.
;; It provides multiple integrated modules:
;;
;; - org-view-mode: Word processor-style view with intelligent pagination
;; - org-svg-tags-mode: Beautiful SVG tags and interactive buttons
;; - org-roam-tree: Tree-style backlinks display for org-roam
;; - Integration with denote and org-roam for knowledge management
;;
;; Quick start:
;;   (require 'org-mapping)
;;   (org-mapping-setup-defaults)
;;   (org-mapping-mode 1)  ; Enable in org buffers
;;
;; Or enable specific modules:
;;   (org-view-mode 1)
;;   (org-svg-tags-mode 1)

;;; Code:

(require 'org)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup org-mapping nil
  "Enhanced Org Mode experience suite."
  :group 'org
  :prefix "org-mapping-")

(defcustom org-mapping-enable-roam-integration t
  "Enable org-roam integration features when available.
This includes org-roam-tree and integration with org-roam-ui."
  :type 'boolean
  :group 'org-mapping)

(defcustom org-mapping-enable-denote-integration t
  "Enable denote integration features when available.
This includes denote metadata handling in org-view."
  :type 'boolean
  :group 'org-mapping)

(defcustom org-mapping-auto-setup-on-load t
  "Automatically run setup when org-mapping is loaded.
If nil, you must call `org-mapping-setup-defaults' manually."
  :type 'boolean
  :group 'org-mapping)

;;; ============================================================================
;;; Module Loading
;;; ============================================================================

(defvar org-mapping--modules-loaded nil
  "List of successfully loaded org-mapping modules.")

(defun org-mapping--load-module (module feature)
  "Load MODULE if FEATURE is available.
Adds MODULE to `org-mapping--modules-loaded' on success."
  (condition-case err
      (progn
        (require feature)
        (add-to-list 'org-mapping--modules-loaded module)
        t)
    (error
     (message "org-mapping: Could not load %s (%s)" module err)
     nil)))

;;;###autoload
(defun org-mapping-load-modules ()
  "Load all available org-mapping modules.
Returns list of successfully loaded modules."
  (interactive)
  (setq org-mapping--modules-loaded nil)
  
  ;; Core modules (always load)
  (org-mapping--load-module 'org-view 'org-view)
  (org-mapping--load-module 'org-svg-tags 'org-svg-tags)
  
  ;; Optional modules based on dependencies
  (when org-mapping-enable-roam-integration
    (when (org-mapping--load-module 'org-roam 'org-roam)
      (org-mapping--load-module 'org-roam-tree 'org-roam-tree)
      (org-mapping--load-module 'org-roam-ui 'org-roam-ui)))
  
  (when org-mapping-enable-denote-integration
    (org-mapping--load-module 'denote 'denote))
  
  (message "org-mapping: Loaded modules: %s" 
           (mapconcat #'symbol-name org-mapping--modules-loaded ", "))
  org-mapping--modules-loaded)

;;; ============================================================================
;;; Module Control
;;; ============================================================================

;;;###autoload
(defun org-mapping-enable-all ()
  "Enable all available org-mapping modules in current org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mapping only works in org-mode buffers"))
  
  ;; Ensure modules are loaded
  (unless org-mapping--modules-loaded
    (org-mapping-load-modules))
  
  ;; Enable available modules
  (when (memq 'org-view org-mapping--modules-loaded)
    (org-view-mode 1))
  
  (when (memq 'org-svg-tags org-mapping--modules-loaded)
    (org-svg-tags-mode 1))
  
  (message "org-mapping: All modules enabled"))

;;;###autoload
(defun org-mapping-disable-all ()
  "Disable all org-mapping modules in current buffer."
  (interactive)
  (when (and (featurep 'org-view) (bound-and-true-p org-view-mode))
    (org-view-mode -1))
  
  (when (and (featurep 'org-svg-tags) (bound-and-true-p org-svg-tags-mode))
    (org-svg-tags-mode -1))
  
  (message "org-mapping: All modules disabled"))

;;; ============================================================================
;;; Configuration Presets
;;; ============================================================================

;;;###autoload
(defun org-mapping-setup-defaults ()
  "Setup recommended defaults for org-mapping.
This configures all modules with sensible defaults."
  (interactive)
  
  ;; Load modules if needed
  (unless org-mapping--modules-loaded
    (org-mapping-load-modules))
  
  ;; org-view defaults
  (setq org-view-body-width 85
        org-view-lines-per-page 50
        org-view-header-format 'both
        org-view-footer-format "Page %p • %d"
        org-view-respect-org-structure t
        org-view-center-headings t)
  
  ;; org-svg-tags defaults
  (setq org-svg-tags-font-lock-case-insensitive t
        org-svg-tags-indent t
        org-svg-tags-hide-blocks t)
  
  ;; org-roam-tree defaults (if available)
  (when (memq 'org-roam-tree org-mapping--modules-loaded)
    (setq org-roam-tree-default-visible t))
  
  (message "org-mapping: Default configuration applied"))

;;;###autoload
(defun org-mapping-setup-minimal ()
  "Setup minimal configuration for org-mapping.
Focuses on performance and simplicity."
  (interactive)
  
  (unless org-mapping--modules-loaded
    (org-mapping-load-modules))
  
  (setq org-view-body-width 80
        org-view-lines-per-page 60
        org-view-header-format 'none
        org-view-footer-format "%p"
        org-view-respect-org-structure nil
        org-view-center-headings nil
        org-svg-tags-indent nil
        org-svg-tags-hide-blocks nil)
  
  (message "org-mapping: Minimal configuration applied"))

;;;###autoload
(defun org-mapping-setup-writer ()
  "Setup writer-focused configuration.
Emphasizes distraction-free writing and clean layout."
  (interactive)
  
  (unless org-mapping--modules-loaded
    (org-mapping-load-modules))
  
  (setq org-view-body-width 90
        org-view-lines-per-page 45
        org-view-header-format 'title
        org-view-footer-format ""
        org-view-respect-org-structure t
        org-view-center-headings t
        org-view-line-spacing 0.3
        org-svg-tags-indent t
        org-svg-tags-hide-blocks t)
  
  (message "org-mapping: Writer configuration applied"))

;;; ============================================================================
;;; Org-Roam Integration
;;; ============================================================================

;;;###autoload
(defun org-mapping-setup-roam ()
  "Setup org-roam integration with tree view.
Adds tree-style backlinks section to org-roam buffer."
  (interactive)
  
  (unless (memq 'org-roam-tree org-mapping--modules-loaded)
    (user-error "org-roam-tree module not available"))
  
  ;; Add tree backlinks section
  (add-to-list 'org-roam-mode-sections
               #'org-roam-tree-backlinks-section
               t)
  
  ;; Optional: Setup org-roam-ui if available
  (when (memq 'org-roam-ui org-mapping--modules-loaded)
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))
  
  (message "org-mapping: Org-roam integration configured"))

;;;###autoload
(defun org-mapping-roam-tree-only ()
  "Configure org-roam buffer to show only tree view."
  (interactive)
  
  (unless (memq 'org-roam-tree org-mapping--modules-loaded)
    (user-error "org-roam-tree module not available"))
  
  (setq org-roam-mode-sections '(org-roam-tree-backlinks-section))
  (message "org-mapping: Org-roam buffer set to tree-only mode"))

;;; ============================================================================
;;; Denote Integration
;;; ============================================================================

;;;###autoload
(defun org-mapping-setup-denote ()
  "Setup denote integration features.
Configures denote to work harmoniously with org-mapping."
  (interactive)
  
  (unless (memq 'denote org-mapping--modules-loaded)
    (user-error "denote module not available"))
  
  ;; Configure denote basics
  (setq denote-directory (expand-file-name "~/Documents/notes/")
        denote-known-keywords '("project" "reference" "fleeting" "permanent")
        denote-infer-keywords t
        denote-sort-keywords t)
  
  (message "org-mapping: Denote integration configured"))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

;;;###autoload
(defun org-mapping-info ()
  "Display information about loaded org-mapping modules and configuration."
  (interactive)
  (let ((info (concat
               "Org-Mapping Status\n"
               "==================\n\n"
               "Loaded modules: "
               (if org-mapping--modules-loaded
                   (mapconcat #'symbol-name org-mapping--modules-loaded ", ")
                 "None")
               "\n\n"
               "Active in buffer: "
               (if (derived-mode-p 'org-mode)
                   (concat
                    (if (bound-and-true-p org-view-mode) "org-view " "")
                    (if (bound-and-true-p org-svg-tags-mode) "org-svg-tags " "")
                    (if (and (= 0 (+ (if (bound-and-true-p org-view-mode) 1 0)
                                    (if (bound-and-true-p org-svg-tags-mode) 1 0))))
                        "None" ""))
                 "Not in org-mode buffer")
               "\n\n"
               "Configuration:\n"
               (format "  org-view-body-width: %d\n" 
                       (if (boundp 'org-view-body-width) 
                           org-view-body-width 
                           0))
               (format "  org-view-header-format: %s\n" 
                       (if (boundp 'org-view-header-format) 
                           org-view-header-format 
                           'none))
               "\n"
               "Integration:\n"
               (format "  org-roam: %s\n" 
                       (if (memq 'org-roam org-mapping--modules-loaded) 
                           "Available" 
                           "Not available"))
               (format "  denote: %s\n" 
                       (if (memq 'denote org-mapping--modules-loaded) 
                           "Available" 
                           "Not available")))))
    (message "%s" info)))

;;; ============================================================================
;;; Mode Definition
;;; ============================================================================

;;;###autoload
(define-minor-mode org-mapping-mode
  "Enable the complete org-mapping experience.
This activates available org-mapping modules for enhanced org editing."
  :init-value nil
  :lighter " OrgMap"
  :group 'org-mapping
  (if org-mapping-mode
      (org-mapping-enable-all)
    (org-mapping-disable-all)))

;;; ============================================================================
;;; Global Minor Mode
;;; ============================================================================

;;;###autoload
(define-globalized-minor-mode global-org-mapping-mode
  org-mapping-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-mapping-mode 1)))
  :group 'org-mapping)

;;; ============================================================================
;;; Auto-setup
;;; ============================================================================

(when org-mapping-auto-setup-on-load
  (org-mapping-load-modules))

(provide 'org-mapping)
;;; org-mapping.el ends here

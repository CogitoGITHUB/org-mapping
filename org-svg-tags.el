;;; org-svg-tags.el --- SVG tags and buttons for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 CogitoGITHUB
;; Original concept from notebook.el

;; Author: CogitoGITHUB
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (svg-tag-mode "0.3"))
;; Keywords: outlines, convenience, org, svg
;; URL: https://github.com/CogitoGITHUB/org-mapping

;;; Commentary:
;; A minor mode to populate an org document with various SVG tags and
;; interactive buttons for a beautiful, modern org experience.
;;
;; Features:
;; - SVG tags for org keywords (#+BEGIN_SRC, #+CAPTION, etc.)
;; - Interactive buttons for running code blocks
;; - Citation tags with modern styling
;; - Export and execution shortcuts
;;
;; Usage:
;;   (require 'org-svg-tags)
;;   (add-hook 'org-mode-hook #'org-svg-tags-mode)

;;; Code:

(require 'org)
(require 'svg-tag-mode)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup org-svg-tags nil
  "Customization options for `org-svg-tags-mode'."
  :group 'org
  :prefix "org-svg-tags-")

(defcustom org-svg-tags-babel-python-command
  "python3"
  "Python interpreter's path for babel execution."
  :type 'string
  :group 'org-svg-tags)

(defcustom org-svg-tags-cite-csl-styles-dir
  "."
  "CSL styles citations' directory."
  :type 'directory
  :group 'org-svg-tags)

(defcustom org-svg-tags-font-lock-case-insensitive t
  "Make the keywords fontification case insensitive if non-nil."
  :type 'boolean
  :group 'org-svg-tags)

(defcustom org-svg-tags-indent t
  "Enable org-indent when mode is activated.
If non-nil, `org-indent-mode' is called when the mode is turned on."
  :type 'boolean
  :group 'org-svg-tags)

(defcustom org-svg-tags-hide-blocks t
  "Default visibility of org blocks.
If non-nil, org blocks are hidden when the mode is turned on."
  :type 'boolean
  :group 'org-svg-tags)

(defcustom org-svg-tags-tags
  '(
    ;; Inline code
    ;; ------------------------------------------------------------------------
    ("^#\\+call:" . ((lambda (tag) (svg-tag-make "CALL"
                                                 :face 'org-meta-line))
                     (lambda () (interactive) (org-svg-tags-call-at-point)) 
                     "Call function"))
    
    ("call_" . ((lambda (tag) (svg-tag-make "CALL"
                                           :face 'default
                                           :margin 1
                                           :alignment 0))
                (lambda () (interactive) (org-svg-tags-call-at-point)) 
                "Call function"))
    
    ("src_" . ((lambda (tag) (svg-tag-make "CALL"
                                          :face 'default
                                          :margin 1
                                          :alignment 0))
               (lambda () (interactive) (org-svg-tags-call-at-point)) 
               "Execute code"))
    
    ;; Code blocks
    ;; ------------------------------------------------------------------------
    ("^#\\+begin_src\\( [a-zA-Z\-]+\\)" . 
     ((lambda (tag) (svg-tag-make (upcase tag)
                                  :face 'org-meta-line
                                  :crop-left t))))
    
    ("^#\\+begin_src" . 
     ((lambda (tag) (svg-tag-make "RUN"
                                  :face 'org-meta-line
                                  :inverse t
                                  :crop-right t))
      (lambda () (interactive) (org-svg-tags-run-at-point)) 
      "Run code block"))
    
    ("^#\\+end_src" . ((lambda (tag) (svg-tag-make "END"
                                                   :face 'org-meta-line))))
    
    ;; Export blocks
    ;; ------------------------------------------------------------------------
    ("^#\\+begin_export" . 
     ((lambda (tag) (svg-tag-make "EXPORT"
                                  :face 'org-meta-line
                                  :inverse t
                                  :alignment 0
                                  :crop-right t))))
    
    ("^#\\+begin_export\\( [a-zA-Z\-]+\\)" . 
     ((lambda (tag) (svg-tag-make (upcase tag)
                                  :face 'org-meta-line
                                  :crop-left t))))
    
    ("^#\\+end_export" . ((lambda (tag) (svg-tag-make "END"
                                                      :face 'org-meta-line))))
    
    ;; :noexport: tag
    ;; ------------------------------------------------------------------------
    ("\\(:no\\)export:" . ((lambda (tag) (svg-tag-make "NO"
                                                       :face 'org-meta-line
                                                       :inverse t
                                                       :crop-right t))))
    
    (":no\\(export:\\)" . ((lambda (tag) (svg-tag-make "EXPORT"
                                                       :face 'org-meta-line
                                                       :crop-left t))))
    
    ;; Miscellaneous keywords
    ;; ------------------------------------------------------------------------
    ("|RUN|" . ((lambda (tag) (svg-tag-make "RUN"
                                           :face 'org-meta-line
                                           :inverse t))))
    
    ("|RUN ALL|" . ((lambda (tag) (svg-tag-make "RUN ALL"
                                               :face 'org-meta-line))
                    (lambda () (interactive) (org-svg-tags-run-all)) 
                    "Run all code blocks"))
    
    ("|SETUP|" . ((lambda (tag) (svg-tag-make "SETUP"
                                             :face 'org-meta-line))
                  (lambda () (interactive) (org-svg-tags-setup)) 
                  "Setup environment"))
    
    ("|EXPORT|" . ((lambda (tag) (svg-tag-make "EXPORT"
                                              :face 'org-meta-line))
                   (lambda () (interactive) (org-svg-tags-export-html)) 
                   "Export to HTML"))
    
    ("|CALL|" . ((lambda (tag) (svg-tag-make "CALL"
                                            :face 'org-meta-line))))
    
    ;; References
    ;; ------------------------------------------------------------------------
    ("\\(\\[cite:@[A-Za-z]+:\\)" .
     ((lambda (tag) (svg-tag-make (upcase tag)
                                  :face 'default
                                  :inverse t
                                  :beg 7 :end -1
                                  :crop-right t))))
    
    ("\\[cite:@[A-Za-z]+:\\([0-9a-z]+\\]\\)" .
     ((lambda (tag) (svg-tag-make (upcase tag)
                                  :face 'default
                                  :end -1
                                  :crop-left t))))
    
    ;; Miscellaneous properties
    ;; ------------------------------------------------------------------------
    ("^#\\+caption:" . ((lambda (tag) (svg-tag-make "CAPTION"
                                                   :face 'org-meta-line))))
    
    ("^#\\+latex:" . ((lambda (tag) (svg-tag-make "LATEX"
                                                 :face 'org-meta-line))))
    
    ("^#\\+html:" . ((lambda (tag) (svg-tag-make "HTML"
                                                :face 'org-meta-line))))
    
    ("^#\\+name:" . ((lambda (tag) (svg-tag-make "NAME"
                                                :face 'org-meta-line))))
    
    ("^#\\+header:" . ((lambda (tag) (svg-tag-make "HEADER"
                                                  :face 'org-meta-line))))
    
    ("^#\\+label:" . ((lambda (tag) (svg-tag-make "LABEL"
                                                 :face 'org-meta-line))))
    
    ("^#\\+results:" . ((lambda (tag) (svg-tag-make "RESULTS"
                                                   :face 'org-meta-line)))))
  "The `org-svg-tags-mode' tags alist.
This alist defines SVG tags for various org elements and keywords.
Each entry follows the pattern: (REGEX . (TAG-FUNCTION [ACTION-FUNCTION] [HELP]))
where TAG-FUNCTION creates the SVG tag, ACTION-FUNCTION is called on click,
and HELP is the tooltip text."
  :type '(alist :key-type regexp :value-type sexp)
  :group 'org-svg-tags)

;;; ============================================================================
;;; Interactive Functions
;;; ============================================================================

(defun org-svg-tags-run-at-point ()
  "Execute code block at point and update rendering."
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-redisplay-inline-images))

(defalias 'org-svg-tags-call-at-point 'org-ctrl-c-ctrl-c
  "Execute code or function call at point.")

(defun org-svg-tags-setup ()
  "Setup org-svg-tags environment for citations and babel."
  (interactive)
  (setq org-cite-csl-styles-dir org-svg-tags-cite-csl-styles-dir)
  (setq org-babel-python-command org-svg-tags-babel-python-command)
  (require 'ob-python)
  (when (require 'oc-csl nil t)
    (message "org-svg-tags: Citations and babel configured")))

(defalias 'org-svg-tags-run-all 'org-babel-execute-buffer
  "Execute all code blocks in buffer.")

(defalias 'org-svg-tags-export-html 'org-html-export-to-html
  "Export current org buffer to HTML.")

;;; ============================================================================
;;; Mode Activation/Deactivation
;;; ============================================================================

(defun org-svg-tags-mode-on ()
  "Activate org-svg-tags mode."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-svg-tags-mode only works in org-mode buffers"))
  
  ;; Setup font lock
  (add-to-list 'font-lock-extra-managed-props 'display)
  (setq font-lock-keywords-case-fold-search 
        org-svg-tags-font-lock-case-insensitive)
  
  ;; Configure inline images
  (setq org-image-actual-width 
        `(,(truncate (* (frame-pixel-width) 0.85))))
  (setq org-startup-with-inline-images t)
  
  ;; Add our tags to svg-tag-mode
  (mapc (lambda (tag) (add-to-list 'svg-tag-tags tag)) 
        org-svg-tags-tags)
  
  ;; Setup org display
  (org-redisplay-inline-images)
  (when org-svg-tags-indent 
    (org-indent-mode 1))
  (when org-svg-tags-hide-blocks 
    (org-hide-block-all))
  
  ;; Add hook for auto image updates
  (add-hook 'org-babel-after-execute-hook 
            'org-redisplay-inline-images nil t)
  
  ;; Enable svg-tag-mode
  (svg-tag-mode 1)
  
  (message "org-svg-tags mode enabled"))

(defun org-svg-tags-mode-off ()
  "Deactivate org-svg-tags mode."
  ;; Disable svg-tag-mode
  (svg-tag-mode -1)
  
  ;; Restore org settings
  (when org-svg-tags-indent 
    (org-indent-mode -1))
  (when org-svg-tags-hide-blocks 
    (org-show-all))
  
  ;; Remove hook
  (remove-hook 'org-babel-after-execute-hook 
               'org-redisplay-inline-images t)
  
  (message "org-svg-tags mode disabled"))

;;; ============================================================================
;;; Mode Definition
;;; ============================================================================

;;;###autoload
(define-minor-mode org-svg-tags-mode
  "Minor mode for beautiful SVG tags in Org Mode.

This mode adds interactive SVG tags and buttons to org documents,
making them more visually appealing and functional. Features include:

- Code block execution buttons
- Beautiful keyword tags
- Citation styling
- Interactive export/setup buttons

\\{org-svg-tags-mode-map}"
  :init-value nil
  :lighter " SVG"
  :group 'org-svg-tags
  (if org-svg-tags-mode
      (org-svg-tags-mode-on)
    (org-svg-tags-mode-off)))

;;;###autoload
(define-globalized-minor-mode global-org-svg-tags-mode 
  org-svg-tags-mode 
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-svg-tags-mode 1)))
  :group 'org-svg-tags)

(provide 'org-svg-tags)
;;; org-svg-tags.el ends here

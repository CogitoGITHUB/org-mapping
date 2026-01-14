
;;; org-face.el --- Modern visual experience for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 CogitoGITHUB
;; Author: CogitoGITHUB
;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (svg-lib "0.2"))
;; Keywords: outlines, convenience, org, svg
;; URL: https://github.com/CogitoGITHUB/org-face

;;; Commentary:

;; org-face provides a modern, visual interface for Org Mode with:
;; - Beautiful SVG tags for keywords and code blocks
;; - Interactive buttons for code execution
;; - Clean, distraction-free layout
;; - Word processor-style pagination
;;
;; Quick start:
;;   (require 'org-face)
;;   (add-hook 'org-mode-hook #'org-face-mode)

;;; Code:

(require 'org)
(require 'svg-lib)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup org-face nil
  "Modern visual experience for Org Mode."
  :group 'org
  :prefix "org-face-")

(defcustom org-face-body-width 85
  "Width of text body in characters."
  :type 'integer
  :group 'org-face)

(defcustom org-face-center-headings t
  "Center org headings when non-nil."
  :type 'boolean
  :group 'org-face)

(defcustom org-face-hide-blocks t
  "Hide org blocks initially when non-nil."
  :type 'boolean
  :group 'org-face)

(defcustom org-face-inline-images t
  "Display inline images when non-nil."
  :type 'boolean
  :group 'org-face)

(defcustom org-face-python-command "python3"
  "Python interpreter for code execution."
  :type 'string
  :group 'org-face)

;;; ============================================================================
;;; SVG Tag System
;;; ============================================================================

(defvar org-face--active-tags nil
  "Currently active SVG tags.")

(defface org-face-default
  '((t :inherit default))
  "Default face for SVG tags."
  :group 'org-face)

(defun org-face--face-attribute (face attribute)
  "Get ATTRIBUTE from FACE, falling back to org-face-default."
  (cond ((facep face)
         (face-attribute face attribute nil 'default))
        ((and (stringp face) (eq attribute :foreground))
         face)
        ((plist-get face attribute))
        (t (face-attribute 'org-face-default attribute nil 'default))))

(defun org-face--plist-delete (plist property)
  "Remove PROPERTY from PLIST."
  (let (result)
    (while plist
      (unless (eq property (car plist))
        (setq result (plist-put result (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    result))

(defun org-face-make-tag (text &rest args)
  "Create SVG tag displaying TEXT with ARGS.
Additional args:
  :beg - substring start index
  :end - substring end index
  :face - face for colors
  :inverse - swap foreground/background"
  (let* ((face (or (plist-get args :face) 'org-face-default))
         (foreground (org-face--face-attribute face :foreground))
         (background (org-face--face-attribute face :background))
         (inverse (plist-get args :inverse))
         (text (string-trim text))
         (beg (or (plist-get args :beg) 0))
         (end (plist-get args :end))
         (args (org-face--plist-delete args 'face))
         (args (org-face--plist-delete args 'inverse))
         (args (org-face--plist-delete args 'beg))
         (args (org-face--plist-delete args 'end)))
    (if inverse
        (apply #'svg-lib-tag (substring text beg end) nil
               :stroke 0
               :font-weight 'semibold
               :foreground background
               :background foreground
               args)
      (apply #'svg-lib-tag (substring text beg end) nil
             :stroke 2
             :font-weight 'regular
             :foreground foreground
             :background background
             args))))

(defun org-face--cursor-function (_win position direction)
  "Handle cursor entering/leaving tag areas."
  (let ((beg (previous-single-property-change 
              (if (eq direction 'entered) (1+ (point)) (1+ position))
              'display))
        (end (next-single-property-change 
              (if (eq direction 'entered) (point) position)
              'display)))
    (when (and (eq direction 'entered) beg end)
      (let ((message-log-max nil))
        (message "TAG: %s" 
                 (string-trim (buffer-substring-no-properties beg end)))))))

;;; ============================================================================
;;; Tag Definitions
;;; ============================================================================

(defcustom org-face-tags
  '(("^#\\+begin_src\\( [a-zA-Z0-9\-]+\\)"
     (lambda (tag) (org-face-make-tag (upcase tag) :face 'org-meta-line :crop-left t)))
    
    ("^#\\+begin_src"
     (lambda (_) (org-face-make-tag "RUN" :face 'org-meta-line :inverse t :crop-right t))
     (lambda () (interactive) (org-ctrl-c-ctrl-c) (org-redisplay-inline-images))
     "Execute code block")
    
    ("^#\\+end_src"
     (lambda (_) (org-face-make-tag "END" :face 'org-meta-line)))
    
    ("^#\\+begin_export\\( [a-zA-Z0-9\-]+\\)"
     (lambda (tag) (org-face-make-tag (upcase tag) :face 'org-meta-line :crop-left t)))
    
    ("^#\\+begin_export"
     (lambda (_) (org-face-make-tag "EXPORT" :face 'org-meta-line :inverse t :crop-right t)))
    
    ("^#\\+end_export"
     (lambda (_) (org-face-make-tag "END" :face 'org-meta-line)))
    
    ("^#\\+caption:"
     (lambda (_) (org-face-make-tag "CAPTION" :face 'org-meta-line)))
    
    ("^#\\+name:"
     (lambda (_) (org-face-make-tag "NAME" :face 'org-meta-line)))
    
    ("^#\\+results:"
     (lambda (_) (org-face-make-tag "RESULTS" :face 'org-meta-line)))
    
    ("|RUN ALL|"
     (lambda (_) (org-face-make-tag "RUN ALL" :face 'org-meta-line))
     (lambda () (interactive) (org-babel-execute-buffer))
     "Execute all code blocks")
    
    ("|EXPORT|"
     (lambda (_) (org-face-make-tag "EXPORT" :face 'org-meta-line))
     (lambda () (interactive) (org-html-export-to-html))
     "Export to HTML"))
  "Tag definitions for org-face.
Format: (PATTERN TAG-FN [ACTION-FN] [HELP-TEXT])"
  :type '(repeat (list regexp function (choice function (const nil)) (choice string (const nil))))
  :group 'org-face)

(defun org-face--build-keyword (item)
  "Build font-lock keyword from tag ITEM."
  (let* ((pattern (if (string-match "\\\\(.+\\\\)" (car item))
                      (car item)
                    (format "\\(%s\\)" (car item))))
         (tag-fn (nth 0 (cdr item)))
         (action-fn (nth 1 (cdr item)))
         (help-text (nth 2 (cdr item)))
         (map (when action-fn
                (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] action-fn)
                  map))))
    `(,pattern 1 
      (quote (face nil
              display (funcall ',tag-fn (match-string 1))
              cursor-sensor-functions (org-face--cursor-function)
              ,@(when action-fn '(pointer hand))
              ,@(when help-text `(help-echo ,help-text))
              ,@(when map `(keymap ,map)))))))

;;; ============================================================================
;;; Layout Functions
;;; ============================================================================

(defun org-face--setup-layout ()
  "Configure buffer layout and appearance."
  (setq-local fill-column org-face-body-width)
  (visual-line-mode 1)
  (org-indent-mode 1)
  
  (when org-face-center-headings
    (dolist (level '(1 2 3 4 5 6 7 8))
      (set-face-attribute (intern (format "org-level-%d" level)) nil
                         :inherit 'default
                         :weight 'bold)))
  
  (when org-face-inline-images
    (setq org-image-actual-width `(,(truncate (* (frame-pixel-width) 0.85))))
    (setq org-startup-with-inline-images t)
    (org-redisplay-inline-images))
  
  (when org-face-hide-blocks
    (org-hide-block-all)))

;;; ============================================================================
;;; Org Integration Fixes
;;; ============================================================================

(defun org-face--remove-text-properties (oldfun start end props &rest args)
  "Prevent removal of display property by org."
  (apply oldfun start end (org-face--plist-delete props 'display) args))

(defun org-face--org-fontify-wrapper (oldfun &rest args)
  "Wrap org-fontify to preserve our tags."
  (unwind-protect
      (progn
        (advice-add 'remove-text-properties
                    :around #'org-face--remove-text-properties)
        (apply oldfun args))
    (advice-remove 'remove-text-properties
                   #'org-face--remove-text-properties)))

;;; ============================================================================
;;; Mode Definition
;;; ============================================================================

(defun org-face-mode-on ()
  "Activate org-face mode."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-face-mode requires org-mode"))
  
  (add-to-list 'font-lock-extra-managed-props 'display)
  
  (when org-face--active-tags
    (font-lock-remove-keywords nil 
      (mapcar #'org-face--build-keyword org-face--active-tags)))
  
  (dolist (tag org-face-tags)
    (font-lock-add-keywords nil (list (org-face--build-keyword tag)) 'append))
  
  (setq org-face--active-tags (copy-sequence org-face-tags))
  
  (advice-add 'org-fontify-meta-lines-and-blocks
              :around #'org-face--org-fontify-wrapper)
  
  (add-hook 'org-babel-after-execute-hook 
            #'org-redisplay-inline-images nil t)
  
  (org-face--setup-layout)
  
  (cursor-sensor-mode 1)
  (font-lock-flush)
  
  (message "org-face mode enabled"))

(defun org-face-mode-off ()
  "Deactivate org-face mode."
  (when org-face--active-tags
    (font-lock-remove-keywords nil 
      (mapcar #'org-face--build-keyword org-face--active-tags)))
  
  (setq org-face--active-tags nil)
  
  (advice-remove 'org-fontify-meta-lines-and-blocks
                 #'org-face--org-fontify-wrapper)
  
  (remove-hook 'org-babel-after-execute-hook
               #'org-redisplay-inline-images t)
  
  (cursor-sensor-mode -1)
  (org-indent-mode -1)
  (visual-line-mode -1)
  
  (when org-face-hide-blocks
    (org-show-all))
  
  (font-lock-flush)
  
  (message "org-face mode disabled"))

;;;###autoload
(define-minor-mode org-face-mode
  "Modern visual interface for Org Mode.
Provides beautiful SVG tags, interactive buttons, and clean layout."
  :init-value nil
  :lighter " Face"
  :group 'org-face
  (if org-face-mode
      (org-face-mode-on)
    (org-face-mode-off)))

;;;###autoload
(define-globalized-minor-mode global-org-face-mode
  org-face-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-face-mode 1)))
  :group 'org-face)

;;; ============================================================================
;;; Setup Functions
;;; ============================================================================

;;;###autoload
(defun org-face-setup-defaults ()
  "Apply recommended default settings."
  (interactive)
  (setq org-face-body-width 85
        org-face-center-headings t
        org-face-hide-blocks t
        org-face-inline-images t)
  (message "org-face: Default configuration applied"))

;;;###autoload
(defun org-face-setup-minimal ()
  "Apply minimal configuration for performance."
  (interactive)
  (setq org-face-body-width 80
        org-face-center-headings nil
        org-face-hide-blocks nil
        org-face-inline-images nil)
  (message "org-face: Minimal configuration applied"))

;;;###autoload
(defun org-face-setup-writer ()
  "Apply writer-focused configuration."
  (interactive)
  (setq org-face-body-width 90
        org-face-center-headings t
        org-face-hide-blocks t
        org-face-inline-images t)
  (message "org-face: Writer configuration applied"))

(provide 'org-face)
;;; org-face.el ends here

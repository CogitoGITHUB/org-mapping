;;; org-face.el --- Modern visual interface for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 CogitoGITHUB
;; Copyright (C) 2020-2025 D. Williams, sabof
;; Author: CogitoGITHUB, D. Williams <d.williams@posteo.net>
;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (svg-lib "0.2"))
;; Keywords: outlines, convenience, org, svg, faces
;; URL: https://github.com/CogitoGITHUB/org-face

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; org-face provides a comprehensive modern visual interface for Org Mode:
;;
;; Features:
;; - Beautiful SVG tags for keywords and code blocks
;; - UTF-8 bullets for headings and lists
;; - Centered headings with proper alignment
;; - Interactive buttons for code execution
;; - Clean, distraction-free layout
;; - Special TODO item styling
;;
;; Quick start:
;;   (require 'org-face)
;;   (add-hook 'org-mode-hook #'org-face-mode)

;;; Code:

(require 'org)
(require 'svg-lib)
(require 'org-element)

(defgroup org-face nil
  "Modern visual interface for Org Mode."
  :group 'org
  :prefix "org-face-")

;;; ============================================================================
;;; Customization - Display Settings
;;; ============================================================================

(defcustom org-face-body-width 85
  "Width of text body in characters."
  :type 'integer
  :group 'org-face)

(defcustom org-face-center-headings t
  "Center org headings when non-nil.
This is always enabled in org-face mode."
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

;;; ============================================================================
;;; Customization - Bullets
;;; ============================================================================

(defcustom org-face-headline-bullets
  '(?◉ ?○ ?✸ ?✿)
  "List of UTF-8 bullets for org headings.
Each entry corresponds to a heading level."
  :type '(repeat character)
  :group 'org-face)

(defcustom org-face-item-bullets
  '((?* . ?•)
    (?+ . ?➤)
    (?- . ?–))
  "Alist of UTF-8 bullets for plain lists.
Maps list bullet characters to display characters."
  :type '(alist :key-type character :value-type character)
  :group 'org-face)

(defcustom org-face-todo-bullets
  '(("TODO" . ?☐)
    ("DONE" . ?☑))
  "Alist of UTF-8 bullets for TODO items.
Maps TODO keywords to display characters."
  :type '(alist :key-type string :value-type character)
  :group 'org-face)

(defcustom org-face-leading-bullet " ․"
  "Character or string for leading stars.
Used to replace leading stars in headings."
  :type '(choice character string)
  :group 'org-face)

(defcustom org-face-cycle-bullets t
  "Cycle through bullet list when non-nil.
If nil, repeat the last bullet for deep levels."
  :type 'boolean
  :group 'org-face)

(defcustom org-face-special-todo-items t
  "Use special bullets for TODO items when non-nil."
  :type 'boolean
  :group 'org-face)

(defcustom org-face-remove-leading-stars nil
  "Remove leading stars completely when non-nil.
This creates a cleaner appearance but removes indentation."
  :type 'boolean
  :group 'org-face)

;;; ============================================================================
;;; Faces
;;; ============================================================================

(defface org-face-default
  '((t :inherit default))
  "Default face for org-face elements."
  :group 'org-face)

(defface org-face-leading
  '((t :inherit default :foreground "gray"))
  "Face for prettified leading stars."
  :group 'org-face)

(defface org-face-header-bullet
  '((t :inherit default))
  "Face for headline bullets."
  :group 'org-face)

(defface org-face-item
  '((t :inherit default))
  "Face for list item bullets."
  :group 'org-face)

(defface org-face-centered-heading
  '((t :inherit default :weight bold))
  "Face for centered headings."
  :group 'org-face)

;;; ============================================================================
;;; Variables
;;; ============================================================================

(defvar-local org-face--active-tags nil
  "Currently active SVG tags.")

(defvar-local org-face--font-lock-keywords nil
  "Font lock keywords for org-face.")

(defvar-local org-face--heading-overlays nil
  "List of overlays used for centering headings.")

;;; ============================================================================
;;; SVG Tags - Utility Functions
;;; ============================================================================

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
  "Create SVG tag displaying TEXT with ARGS."
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

;;; ============================================================================
;;; SVG Tags - Definitions
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
  "SVG tag definitions for org-face."
  :type '(repeat (list regexp function (choice function (const nil)) (choice string (const nil))))
  :group 'org-face)

(defun org-face--build-svg-keyword (item)
  "Build font-lock keyword from SVG tag ITEM."
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
              ,@(when action-fn '(pointer hand))
              ,@(when help-text `(help-echo ,help-text))
              ,@(when map `(keymap ,map)))))))

;;; ============================================================================
;;; Bullets - Predicates
;;; ============================================================================

(defun org-face--plain-list-p ()
  "Return non-nil if at a valid plain list."
  (save-match-data
    (org-list-in-valid-context-p)))

(defun org-face--headline-p ()
  "Return non-nil if at a valid headline."
  (save-match-data
    (org-with-limited-levels
     (and (org-at-heading-p) t))))

(defun org-face--graphic-p ()
  "Return non-nil if display supports graphics."
  (display-graphic-p))

;;; ============================================================================
;;; Bullets - Accessor Functions
;;; ============================================================================

(defun org-face--heading-level ()
  "Return heading level from match data."
  (- (match-end 0) (match-beginning 0) 1))

(defun org-face--get-bullet (&optional level)
  "Get bullet character for LEVEL."
  (let* ((level (or level (org-face--heading-level)))
         (n (if org-odd-levels-only (/ (1- level) 2) (1- level)))
         (len (length org-face-headline-bullets)))
    (if org-face-cycle-bullets
        (elt org-face-headline-bullets (% n len))
      (elt org-face-headline-bullets (min n (1- len))))))

(defun org-face--get-todo-bullet ()
  "Get TODO item bullet if defined."
  (when org-face-special-todo-items
    (let* ((todo-kw (save-match-data
                      (cdar (org-entry-properties (match-beginning 0) "TODO"))))
           (bullet (cdr (assoc todo-kw org-face-todo-bullets))))
      (when (and (stringp todo-kw) bullet)
        bullet))))

(defun org-face--get-item-bullet (bullet-string)
  "Get display bullet for BULLET-STRING."
  (if-let ((new-bullet (cdr (assq (string-to-char bullet-string)
                                  org-face-item-bullets))))
      (string new-bullet)
    bullet-string))

;;; ============================================================================
;;; Bullets - Fontification Functions
;;; ============================================================================

(defun org-face--prettify-item-bullet ()
  "Prettify plain list bullets."
  (when (org-face--plain-list-p)
    (let ((bullet (match-string 1)))
      (put-text-property (match-beginning 1) (match-end 1)
                         'display (org-face--get-item-bullet bullet))
      'org-face-item)))

(defun org-face--prettify-main-bullet ()
  "Prettify main heading bullet."
  (when (org-face--headline-p)
    (let ((bullet (or (org-face--get-todo-bullet)
                      (org-face--get-bullet))))
      (when bullet
        (compose-region (match-beginning 1) (match-end 1) bullet))
      'org-face-header-bullet)))

(defun org-face--prettify-leading-bullets ()
  "Prettify leading stars."
  (when (org-face--headline-p)
    (let ((star-beg (match-beginning 3))
          (star-end (match-end 2)))
      (while (< star-beg star-end)
        (compose-region star-beg (setq star-beg (1+ star-beg))
                        org-face-leading-bullet))
      'org-face-leading)))

(defun org-face--make-invisible (subexp)
  "Make SUBEXP invisible."
  (when-let ((start (match-beginning subexp))
             (end (match-end subexp)))
    (put-text-property start end 'invisible 'org-face)))

;;; ============================================================================
;;; Centered Headings
;;; ============================================================================

(defun org-face--center-heading ()
  "Center the current heading."
  (when (org-face--headline-p)
    (let* ((beg (match-beginning 0))
           (end (line-end-position))
           (heading-text (buffer-substring beg end))
           (heading-width (string-width heading-text))
           (window-width (window-body-width))
           (padding (max 0 (/ (- window-width heading-width) 2)))
           (ov (make-overlay beg beg)))
      (overlay-put ov 'before-string (make-string padding ?\s))
      (overlay-put ov 'org-face-centered t)
      (push ov org-face--heading-overlays))))

(defun org-face--center-all-headings ()
  "Center all headings in buffer."
  (org-face--remove-heading-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (org-face--center-heading))))

(defun org-face--remove-heading-overlays ()
  "Remove all heading centering overlays."
  (mapc #'delete-overlay org-face--heading-overlays)
  (setq org-face--heading-overlays nil))

(defun org-face--recenter-headings ()
  "Recenter headings after window size change."
  (when org-face-mode
    (org-face--center-all-headings)))

;;; ============================================================================
;;; Font Lock Keywords
;;; ============================================================================

(defun org-face--update-font-lock ()
  "Update font-lock keywords."
  (setq org-face--font-lock-keywords
        `(,@(mapcar #'org-face--build-svg-keyword org-face-tags)
          
          ("^[ \t]*?\\(?:\\(?1:[-+]\\)\\|[ \t]\\(?1:\\*\\)\\) "
           (1 (org-face--prettify-item-bullet)))
          
          ("^[ \t]*\\(?1:[[:digit:]]+[.)]\\) "
           (1 (progn (when (org-face--plain-list-p) 'org-face-item))))
          
          ("^\\(?3:\\(?4:\\*?\\)\\**?\\(?2:\\*?\\)\\)\\(?1:\\*\\) "
           (1 (org-face--prettify-main-bullet) prepend)
           ,@(unless (or org-hide-leading-stars
                         org-face-remove-leading-stars)
               '((3 (org-face--prettify-leading-bullets) t)))
           ,@(when org-face-remove-leading-stars
               '((3 (org-face--make-invisible 3))))
           (0 (org-face--center-heading))))))

;;; ============================================================================
;;; Layout Functions
;;; ============================================================================

(defun org-face--setup-layout ()
  "Configure buffer layout."
  (setq-local fill-column org-face-body-width)
  (visual-line-mode 1)
  (org-indent-mode 1)
  
  (when org-face-inline-images
    (setq-local org-image-actual-width 
                `(,(truncate (* (frame-pixel-width) 0.85))))
    (setq-local org-startup-with-inline-images t)
    (org-redisplay-inline-images))
  
  (when org-face-hide-blocks
    (org-hide-block-all)))

(defun org-face--fontify-buffer ()
  "Fontify entire buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

;;; ============================================================================
;;; Mode Commands
;;; ============================================================================

(defun org-face-mode-on ()
  "Activate org-face mode."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-face-mode requires org-mode"))
  
  (add-to-list 'font-lock-extra-managed-props 'display)
  (add-to-invisibility-spec '(org-face))
  
  (font-lock-remove-keywords nil org-face--font-lock-keywords)
  (org-face--update-font-lock)
  (font-lock-add-keywords nil org-face--font-lock-keywords 'append)
  
  (org-face--setup-layout)
  (org-face--fontify-buffer)
  (org-face--center-all-headings)
  
  (add-hook 'org-babel-after-execute-hook 
            #'org-redisplay-inline-images nil t)
  (add-hook 'window-size-change-functions
            #'org-face--recenter-headings nil t)
  
  (message "org-face mode enabled"))

(defun org-face-mode-off ()
  "Deactivate org-face mode."
  (remove-from-invisibility-spec '(org-face))
  
  (font-lock-remove-keywords nil org-face--font-lock-keywords)
  (setq org-face--font-lock-keywords nil)
  
  (org-face--remove-heading-overlays)
  (org-face--fontify-buffer)
  
  (remove-hook 'org-babel-after-execute-hook
               #'org-redisplay-inline-images t)
  (remove-hook 'window-size-change-functions
               #'org-face--recenter-headings t)
  
  (when org-face-hide-blocks
    (org-show-all))
  
  (org-indent-mode -1)
  (visual-line-mode -1)
  
  (message "org-face mode disabled"))

;;;###autoload
(define-minor-mode org-face-mode
  "Modern visual interface for Org Mode with centered headings."
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
        org-face-inline-images t
        org-face-cycle-bullets t
        org-face-special-todo-items t
        org-face-remove-leading-stars nil)
  (message "org-face: Default configuration applied"))

;;;###autoload
(defun org-face-setup-minimal ()
  "Apply minimal configuration."
  (interactive)
  (setq org-face-body-width 80
        org-face-center-headings t
        org-face-hide-blocks nil
        org-face-inline-images nil
        org-face-cycle-bullets t
        org-face-special-todo-items nil
        org-face-remove-leading-stars nil)
  (message "org-face: Minimal configuration applied"))

;;;###autoload
(defun org-face-setup-writer ()
  "Apply writer-focused configuration."
  (interactive)
  (setq org-face-body-width 90
        org-face-center-headings t
        org-face-hide-blocks t
        org-face-inline-images t
        org-face-cycle-bullets nil
        org-face-special-todo-items t
        org-face-remove-leading-stars t)
  (message "org-face: Writer configuration applied"))

;;;###autoload
(defun org-face-restart ()
  "Restart org-face mode if enabled."
  (interactive)
  (when org-face-mode
    (org-face-mode 0)
    (org-face-mode 1)))

(provide 'org-face)
;;; org-face.el ends here

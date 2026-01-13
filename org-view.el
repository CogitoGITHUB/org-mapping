;;; org-view.el --- Org-aware word processor view -*- lexical-binding: t; -*-

;; Author: CogitoGITHUB
;; Version: 3.5.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: outlines, hypermedia, calendar, wp, org
;; URL: https://github.com/CogitoGITHUB/org-view

;;; Commentary:
;; Word processor view built specifically for Org Mode.
;; Integrates with org structure, respects org elements,
;; and provides intelligent page breaks around org headings.

;;; Code:

(require 'org)
(require 'org-element)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup org-view nil
  "Word processor-style view for Org Mode."
  :group 'org
  :prefix "org-view-")

(defcustom org-view-body-width 80
  "Text body width in characters."
  :type 'integer
  :group 'org-view)

(defcustom org-view-line-spacing 0.2
  "Line spacing (can be integer pixels or float multiplier)."
  :type '(choice integer float)
  :group 'org-view)

(defcustom org-view-lines-per-page 45
  "Target lines per page for pagination."
  :type 'integer
  :group 'org-view)

(defcustom org-view-respect-org-structure t
  "When non-nil, avoid page breaks in the middle of org elements.
Page breaks will prefer positions before headings, between blocks, etc."
  :type 'boolean
  :group 'org-view)

(defcustom org-view-show-document-title t
  "When non-nil, show document title in headers."
  :type 'boolean
  :group 'org-view)

(defcustom org-view-header-format 'title
  "Format for page headers.
  'title   - Show document title
  'heading - Show current top-level heading
  'both    - Show title and heading
  'none    - No header"
  :type '(choice (const :tag "Document title" title)
                 (const :tag "Current heading" heading)
                 (const :tag "Title and heading" both)
                 (const :tag "No header" none))
  :group 'org-view)

(defcustom org-view-footer-format "%p"
  "Format string for page footer.
  %p - Page number
  %t - Total pages (requires recalculation)
  %d - Current date"
  :type 'string
  :group 'org-view)

(defcustom org-view-center-headings t
  "When non-nil, center headings and hide org stars for a clean look."
  :type 'boolean
  :group 'org-view)

;;; ============================================================================
;;; Faces
;;; ============================================================================

(defface org-view-body
  '((t :height 1.1))
  "Face for body text."
  :group 'org-view)

(defface org-view-pagebreak
  '((t :background "#e8e8e8" :extend t :height 0.3))
  "Face for page break separators."
  :group 'org-view)

(defface org-view-header
  '((t :inherit org-meta-line :slant italic))
  "Face for page headers."
  :group 'org-view)

(defface org-view-footer
  '((t :inherit org-meta-line :slant italic))
  "Face for page footers."
  :group 'org-view)

(defface org-view-heading-1
  '((t :inherit org-level-1 :height 1.8 :weight bold))
  "Face for level 1 headings in org-view."
  :group 'org-view)

(defface org-view-heading-2
  '((t :inherit org-level-2 :height 1.5 :weight bold))
  "Face for level 2 headings in org-view."
  :group 'org-view)

(defface org-view-heading-3
  '((t :inherit org-level-3 :height 1.3 :weight bold))
  "Face for level 3 headings in org-view."
  :group 'org-view)

(defface org-view-heading-4
  '((t :inherit org-level-4 :height 1.2 :weight bold))
  "Face for level 4 headings in org-view."
  :group 'org-view)

(defface org-view-heading-5
  '((t :inherit org-level-5 :height 1.1 :weight bold))
  "Face for level 5+ headings in org-view."
  :group 'org-view)

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defvar-local org-view--overlays nil
  "List of page break overlays.")

(defvar-local org-view--heading-overlays nil
  "List of heading formatting overlays.")

(defvar-local org-view--face-cookie nil
  "Face remapping cookie.")

(defvar-local org-view--timer nil
  "Idle timer for page break insertion.")

(defvar-local org-view--last-change-time nil
  "Time of last buffer change.")

(defvar-local org-view--total-pages nil
  "Cached total page count.")

;;; ============================================================================
;;; Org Integration Functions
;;; ============================================================================

(defun org-view--get-document-title ()
  "Get the document title from #+TITLE keyword."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.+\\)$" nil t)
      (match-string-no-properties 1))))

(defun org-view--get-current-heading (&optional pos)
  "Get the current org heading at POS (or point)."
  (save-excursion
    (when pos (goto-char pos))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-get-heading t t t t)))

(defun org-view--get-top-level-heading (&optional pos)
  "Get the current top-level (level 1) heading at POS."
  (save-excursion
    (when pos (goto-char pos))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (while (and (not (= (org-current-level) 1))
                (org-up-heading-safe)))
    (org-get-heading t t t t)))

(defun org-view--at-element-boundary-p ()
  "Return t if point is at a good place for a page break."
  (or (org-at-heading-p)
      (looking-at "^#\\+BEGIN_")
      (looking-at "^#\\+END_")
      (looking-at "^[ \t]*$")
      (looking-at "^|")  ; tables
      (looking-at "^[ \t]*[-+*][ \t]")))  ; lists

(defun org-view--find-next-break-position (start target-line)
  "Find best position for page break near TARGET-LINE from START."
  (save-excursion
    (goto-char start)
    (forward-line target-line)
    
    (if org-view-respect-org-structure
        (let ((target-pos (point))
              (search-range 5))  ; Look 5 lines up/down
          ;; Try to find a heading or element boundary
          (or
           ;; Look backward for a boundary
           (save-excursion
             (dotimes (i search-range)
               (when (org-view--at-element-boundary-p)
                 (cl-return (point)))
               (forward-line -1)))
           ;; Look forward for a boundary
           (save-excursion
             (goto-char target-pos)
             (dotimes (i search-range)
               (when (org-view--at-element-boundary-p)
                 (cl-return (point)))
               (forward-line 1)))
           ;; Fall back to target position
           target-pos))
      (point))))

;;; ============================================================================
;;; Layout (Centered Text)
;;; ============================================================================

(defun org-view--set-margins ()
  "Set window margins to center text."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t)))
    (dolist (window windows)
      (let* ((window-width (window-body-width window))
             (margin-width (max 0 (/ (- window-width org-view-body-width) 2))))
        (set-window-margins window margin-width margin-width)))))

(defun org-view--reset-margins ()
  "Reset window margins to default."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (dolist (window windows)
      (set-window-margins window nil nil))))

;;; ============================================================================
;;; Heading Formatting
;;; ============================================================================

(defun org-view--clear-heading-overlays ()
  "Remove all heading formatting overlays."
  (when org-view--heading-overlays
    (mapc #'delete-overlay org-view--heading-overlays)
    (setq org-view--heading-overlays nil)))

(defun org-view--format-heading (start end level text)
  "Format heading between START and END with LEVEL and TEXT."
  (let* ((face (pcase level
                 (1 'org-view-heading-1)
                 (2 'org-view-heading-2)
                 (3 'org-view-heading-3)
                 (4 'org-view-heading-4)
                 (_ 'org-view-heading-5)))
         (width org-view-body-width)
         (text-length (length text))
         (padding (max 0 (/ (- width text-length) 2)))
         ;; Create overlay to hide the stars and original text
         (hide-ov (make-overlay start end))
         ;; Create overlay for centered formatted text
         (text-ov (make-overlay start start)))
    
    ;; Hide original heading
    (overlay-put hide-ov 'invisible t)
    (overlay-put hide-ov 'org-view-heading t)
    
    ;; Show centered, formatted heading
    (overlay-put text-ov 'org-view-heading t)
    (overlay-put text-ov 'after-string
                (propertize (concat "\n"
                                  (make-string padding ?\s)
                                  text
                                  "\n\n")
                          'face face))
    
    (push hide-ov org-view--heading-overlays)
    (push text-ov org-view--heading-overlays)))

(defun org-view--format-all-headings ()
  "Format all headings in the buffer."
  (when org-view-center-headings
    (org-view--clear-heading-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let* ((element (org-element-at-point))
               (level (org-element-property :level element))
               (text (org-element-property :raw-value element))
               (begin (org-element-property :begin element))
               (end (save-excursion
                      (goto-char begin)
                      (line-end-position))))
          (when (and level text)
            (org-view--format-heading begin end level text)))))))

;;; ============================================================================
;;; Header/Footer Generation
;;; ============================================================================

(defun org-view--make-header (page-num pos)
  "Create header string for PAGE-NUM at position POS."
  (let ((text
         (pcase org-view-header-format
           ('none "")
           ('title (or (org-view--get-document-title) ""))
           ('heading (or (org-view--get-top-level-heading pos) ""))
           ('both 
            (let ((title (org-view--get-document-title))
                  (heading (org-view--get-top-level-heading pos)))
              (cond
               ((and title heading) (format "%s • %s" title heading))
               (title title)
               (heading heading)
               (t ""))))
           (_ ""))))
    text))

(defun org-view--make-footer (page-num)
  "Create footer string for PAGE-NUM."
  (let ((text org-view-footer-format))
    (setq text (replace-regexp-in-string "%p" (number-to-string page-num) text))
    (setq text (replace-regexp-in-string "%t" 
                                         (if org-view--total-pages
                                             (number-to-string org-view--total-pages)
                                           "?")
                                         text))
    (setq text (replace-regexp-in-string "%d" (format-time-string "%Y-%m-%d") text))
    text))

(defun org-view--make-pagebreak-string (page-num pos)
  "Create page break string for PAGE-NUM at POS."
  (let* ((header (org-view--make-header (1+ page-num) pos))
         (footer (org-view--make-footer page-num))
         (width org-view-body-width)
         (header-pad (max 0 (/ (- width (length header)) 2)))
         (footer-pad (max 0 (/ (- width (length footer)) 2))))
    (concat
     "\n"
     ;; Footer of previous page
     (when (> (length footer) 0)
       (propertize (concat (make-string footer-pad ?\s) footer "\n")
                  'face 'org-view-footer))
     ;; Visual separator
     (propertize (make-string width ?─) 
                'face 'org-view-pagebreak)
     "\n"
     ;; Header of next page
     (when (> (length header) 0)
       (propertize (concat (make-string header-pad ?\s) header "\n\n")
                  'face 'org-view-header)))))

;;; ============================================================================
;;; Page Break Management
;;; ============================================================================

(defun org-view--clear-pagebreaks ()
  "Remove all page break overlays."
  (when org-view--overlays
    (mapc #'delete-overlay org-view--overlays)
    (setq org-view--overlays nil)))

(defun org-view--count-visible-lines (start end)
  "Count visible lines between START and END, respecting folding."
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (and (< (point) end)
                  (not (eobp)))
        (unless (org-fold-folded-p)
          (setq count (1+ count)))
        (forward-line 1))
      count)))

(defun org-view--insert-pagebreaks ()
  "Insert page breaks intelligently based on org structure."
  (save-excursion
    (goto-char (point-min))
    (let ((line-count 0)
          (page-num 0)
          (last-break-pos (point-min)))
      
      (while (not (eobp))
        (let ((visible-lines (org-view--count-visible-lines 
                             last-break-pos (point))))
          (setq line-count (+ line-count visible-lines)))
        
        (when (>= line-count org-view-lines-per-page)
          ;; Find best position for break
          (let ((break-pos (org-view--find-next-break-position 
                           last-break-pos 
                           org-view-lines-per-page)))
            (goto-char break-pos)
            
            ;; Insert page break overlay
            (let ((ov (make-overlay (line-beginning-position) 
                                   (line-beginning-position))))
              (overlay-put ov 'org-view-pagebreak t)
              (overlay-put ov 'before-string 
                          (org-view--make-pagebreak-string page-num break-pos))
              (push ov org-view--overlays))
            
            (setq page-num (1+ page-num))
            (setq line-count 0)
            (setq last-break-pos (point))))
        
        (forward-line 1))
      
      ;; Store total pages
      (setq org-view--total-pages (1+ page-num)))))

(defun org-view--update-pagebreaks ()
  "Update page breaks (called on idle timer)."
  (when (and org-view-mode
             (buffer-live-p (current-buffer)))
    (with-current-buffer (current-buffer)
      (let ((inhibit-modification-hooks t))
        (org-view--clear-pagebreaks)
        (org-view--insert-pagebreaks)))))

;;; ============================================================================
;;; Width Control
;;; ============================================================================

(defun org-view-set-width (width)
  "Set text body width to WIDTH."
  (interactive "nSet text body width: ")
  (setq org-view-body-width width)
  (org-view--set-margins)
  (org-view-refresh)
  (message "Text width set to %d" width))

(defun org-view-expand ()
  "Increase text width."
  (interactive)
  (setq org-view-body-width (+ org-view-body-width 2))
  (org-view--set-margins)
  (message "Text width: %d" org-view-body-width))

(defun org-view-shrink ()
  "Decrease text width."
  (interactive)
  (setq org-view-body-width (max 40 (- org-view-body-width 2)))
  (org-view--set-margins)
  (message "Text width: %d" org-view-body-width))

(defun org-view-refresh ()
  "Refresh page breaks."
  (interactive)
  (when org-view-mode
    (org-view--format-all-headings)
    (org-view--update-pagebreaks)
    (message "org-view: %d pages" (or org-view--total-pages 1))))

;;; ============================================================================
;;; Event Handlers
;;; ============================================================================

(defun org-view--on-window-change (_frame)
  "Handle window size changes."
  (when org-view-mode
    (org-view--set-margins)))

(defun org-view--on-buffer-change (_beg _end _len)
  "Handle buffer changes - schedule page break update."
  (when org-view-mode
    (setq org-view--last-change-time (current-time))
    ;; Cancel existing timer
    (when org-view--timer
      (cancel-timer org-view--timer))
    ;; Schedule new update (longer delay for complex org buffers)
    (setq org-view--timer
          (run-with-idle-timer 1.5 nil 
                              (lambda ()
                                (when (buffer-live-p (current-buffer))
                                  (with-current-buffer (current-buffer)
                                    (org-view--update-pagebreaks)
                                    (org-view--format-all-headings))))))))

(defun org-view--on-org-cycle ()
  "Handle org folding changes."
  (when org-view-mode
    ;; Brief delay to let folding complete
    (run-with-idle-timer 0.2 nil #'org-view--update-pagebreaks)))

;;; ============================================================================
;;; Keymap
;;; ============================================================================

(defvar org-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v }") #'org-view-expand)
    (define-key map (kbd "C-c v {") #'org-view-shrink)
    (define-key map (kbd "C-c v |") #'org-view-set-width)
    (define-key map (kbd "C-c v r") #'org-view-refresh)
    map)
  "Keymap for `org-view-mode'.")

;;; ============================================================================
;;; Mode Definition
;;; ============================================================================

;;;###autoload
(define-minor-mode org-view-mode
  "Org-aware word processor view for Org Mode.

This mode provides a word processor-like view for Org documents with:
- Centered text layout
- Intelligent page breaks that respect org structure
- Dynamic headers showing document/heading context
- Integration with org folding and cycling

\\{org-view-mode-map}"
  :init-value nil
  :lighter " OrgView"
  :keymap org-view-mode-map
  :group 'org-view
  
  (if org-view-mode
      ;; Enable
      (progn
        ;; Only enable in org-mode buffers
        (unless (derived-mode-p 'org-mode)
          (user-error "org-view-mode only works in org-mode buffers"))
        
        ;; Apply body face
        (setq org-view--face-cookie
              (face-remap-add-relative 'default 'org-view-body))
        
        ;; Set line spacing
        (setq-local line-spacing org-view-line-spacing)
        
        ;; Set margins
        (org-view--set-margins)
        
        ;; Disable conflicting modes
        (when (bound-and-true-p org-indent-mode) 
          (org-indent-mode -1))
        (when (bound-and-true-p hl-line-mode) 
          (hl-line-mode -1))
        
        ;; Enable visual line mode for better wrapping
        (visual-line-mode 1)
        
        ;; Add hooks
        (add-hook 'window-size-change-functions 
                  #'org-view--on-window-change nil t)
        (add-hook 'after-change-functions 
                  #'org-view--on-buffer-change nil t)
        (add-hook 'org-cycle-hook
                  #'org-view--on-org-cycle nil t)
        
        ;; Insert initial page breaks (with delay)
        (run-with-idle-timer 0.5 nil
                            (lambda ()
                              (when (and (buffer-live-p (current-buffer))
                                       org-view-mode)
                                (with-current-buffer (current-buffer)
                                  (org-view--format-all-headings)
                                  (org-view--insert-pagebreaks)
                                  (message "org-view enabled: %d pages" 
                                          (or org-view--total-pages 1)))))))
    
    ;; Disable
    (progn
      ;; Cancel timer
      (when org-view--timer
        (cancel-timer org-view--timer)
        (setq org-view--timer nil))
      
      ;; Remove hooks
      (remove-hook 'window-size-change-functions 
                   #'org-view--on-window-change t)
      (remove-hook 'after-change-functions 
                   #'org-view--on-buffer-change t)
      (remove-hook 'org-cycle-hook
                   #'org-view--on-org-cycle t)
      
      ;; Clear overlays
      (org-view--clear-pagebreaks)
      (org-view--clear-heading-overlays)
      
      ;; Reset face
      (when org-view--face-cookie
        (face-remap-remove-relative org-view--face-cookie)
        (setq org-view--face-cookie nil))
      
      ;; Reset margins
      (org-view--reset-margins)
      
      (message "org-view disabled"))))

;;; ============================================================================
;;; Integration with org commands
;;; ============================================================================

;; Advice to refresh after major org operations
(defun org-view--after-org-command (&rest _args)
  "Refresh page breaks after org commands."
  (when org-view-mode
    (run-with-idle-timer 0.3 nil #'org-view--update-pagebreaks)))

;; Hook into common org commands
(with-eval-after-load 'org
  (advice-add 'org-promote :after #'org-view--after-org-command)
  (advice-add 'org-demote :after #'org-view--after-org-command)
  (advice-add 'org-move-subtree-up :after #'org-view--after-org-command)
  (advice-add 'org-move-subtree-down :after #'org-view--after-org-command))

(provide 'org-view)
;;; org-view.el ends here

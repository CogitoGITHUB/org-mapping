# org-mapping

> **Enhanced Org Mode Experience Suite**

`org-mapping` is a comprehensive package that transforms your Org Mode experience with two powerful modules:

- **org-view-mode**: Word processor-style view with intelligent pagination
- **org-svg-tags-mode**: Beautiful SVG tags and interactive buttons

---

## 📦 Installation

### Manual Installation

1. Clone or download this repository:
```bash
git clone https://github.com/CogitoGITHUB/org-mapping.git ~/.emacs.d/lisp/org-mapping
```

2. Add to your `init.el`:
```elisp
;; Add to load path
(add-to-list 'load-path "~/.emacs.d/lisp/org-mapping")

;; Load the package
(require 'org-mapping)

;; Setup recommended defaults
(org-mapping-setup-defaults)

;; Enable automatically in org-mode
(add-hook 'org-mode-hook #'org-mapping-mode)
```

### Dependencies

- Emacs 29.1 or later
- Org Mode 9.0 or later
- `svg-tag-mode` (available on MELPA)

Install svg-tag-mode:
```elisp
M-x package-install RET svg-tag-mode RET
```

---

## 🎯 Features

### org-view-mode

Transform your org documents into beautiful, paginated word processor views:

- **Centered text layout** with configurable width
- **Professional headings** - centered with no visible stars (clean look!)
- **Intelligent page breaks** that respect org structure
- **Dynamic headers** showing document title and current heading
- **Folding aware** - respects org visibility cycling
- **Org-integrated** - updates on promote/demote/refile operations

**Keybindings:**
- `C-c v }` - Increase text width
- `C-c v {` - Decrease text width
- `C-c v |` - Set specific width
- `C-c v r` - Refresh page breaks

### org-svg-tags-mode

Add beautiful SVG tags and interactive buttons to your org documents:

- **Code block buttons** - Click to run code blocks
- **Keyword tags** - Beautiful styling for `#+BEGIN_SRC`, `#+CAPTION`, etc.
- **Citation styling** - Modern look for `[cite:@...]` references
- **Interactive shortcuts** - `|RUN ALL|`, `|SETUP|`, `|EXPORT|` buttons
- **Custom tags** - Easily add your own SVG tags

---

## 🚀 Usage

### Quick Start

Enable both modes in an org buffer:
```elisp
M-x org-mapping-mode
```

Or enable individually:
```elisp
M-x org-view-mode
M-x org-svg-tags-mode
```

### Configuration

```elisp
(require 'org-mapping)

;; org-view configuration
(setq org-view-body-width 85                    ; Text width in columns
      org-view-lines-per-page 50                ; Lines per page
      org-view-header-format 'both              ; Show title + heading
      org-view-footer-format "Page %p • %d"     ; Footer format
      org-view-respect-org-structure t          ; Respect headings
      org-view-center-headings t)               ; Center headings (clean look)

;; org-svg-tags configuration
(setq org-svg-tags-babel-python-command "python3"
      org-svg-tags-indent t
      org-svg-tags-hide-blocks t)

;; Apply defaults
(org-mapping-setup-defaults)
```

### org-view Header Formats

```elisp
(setq org-view-header-format 'title)    ; Show document title
(setq org-view-header-format 'heading)  ; Show current heading
(setq org-view-header-format 'both)     ; Show both (default)
(setq org-view-header-format 'none)     ; No header
```

### org-view Footer Formats

```elisp
;; Placeholders:
;; %p - Page number
;; %t - Total pages
;; %d - Current date

(setq org-view-footer-format "Page %p")
(setq org-view-footer-format "Page %p of %t")
(setq org-view-footer-format "%p • %d")
```

### Custom SVG Tags

Add your own tags to `org-svg-tags-tags`:

```elisp
(add-to-list 'org-svg-tags-tags
  '("^#\\+TODO:" . ((lambda (tag) 
                      (svg-tag-make "TODO"
                                   :face 'org-warning
                                   :inverse t)))))
```

---

## 📖 Examples

### Example Document with org-view

```
                    My Document • Introduction
────────────────────────────────────────────────────────────────

                        Introduction

Lorem ipsum dolor sit amet, consectetur adipiscing elit.
This text is automatically centered and paginated.
Notice how the heading is centered with no stars!

                          Chapter 1

More content here with professional formatting...

────────────────────────────────────────────────────────────────
                        Page 2 • 2024-01-15
```

### Example with org-svg-tags

Before:
```org
#+BEGIN_SRC python
print("Hello")
#+END_SRC

|RUN ALL| |EXPORT|

[cite:@smith2020]
```

After (visual):
```
[RUN] PYTHON
print("Hello")
[END]

[RUN ALL] [EXPORT]  ← clickable buttons

[CITE:@SMITH2020]  ← styled citation
```

---

## 🎨 Customization

### Faces

Customize the appearance:

```elisp
;; org-view faces
(custom-set-faces
 '(org-view-body ((t (:height 1.2))))
 '(org-view-pagebreak ((t (:background "#d0d0d0"))))
 '(org-view-header ((t (:slant italic :weight bold))))
 '(org-view-footer ((t (:slant italic)))))
```

### Advanced Configuration

```elisp
;; Only enable org-view for specific files
(add-hook 'org-mode-hook
          (lambda ()
            (when (string-match-p "/documents/" (buffer-file-name))
              (org-view-mode 1))))

;; Custom page break logic
(setq org-view-respect-org-structure nil)  ; Break anywhere
(setq org-view-lines-per-page 60)          ; Longer pages
```

---

## 🔧 Troubleshooting

### org-view not showing pages

1. Check if mode is enabled: `M-x org-view-mode`
2. Refresh manually: `C-c v r` or `M-x org-view-refresh`
3. Ensure you're in an org-mode buffer

### SVG tags not appearing

1. Install svg-tag-mode: `M-x package-install RET svg-tag-mode`
2. Check if mode is enabled: `M-x org-svg-tags-mode`
3. Verify display-graphic-p returns t (SVG requires GUI)

### Margins not working

Window margins require a graphical display. In terminal Emacs, consider using:
```elisp
(setq org-view-body-width 72)  ; Narrower for terminal
```

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

---

## 📝 License

GPL-3.0

---

## 🙏 Credits

- **org-view-mode**: Original concept from olivetti.el by Paul W. Rankin
- **org-svg-tags-mode**: Inspired by notebook.el and svg-tag-mode by Nicolas P. Rougier

---

## 📚 See Also

- [Org Mode](https://orgmode.org/)
- [svg-tag-mode](https://github.com/rougier/svg-tag-mode)
- [olivetti](https://github.com/rnkn/olivetti)
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Giulio Centorame"
      user-mail-address "giulio.centorame@outlook.it")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 15
                           :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 17)
      doom-big-font (font-spec :family "monospace" :size 30))

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "monospace" :height 100 ))
  (buffer-face-mode))
(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-dracula)     ;;this is another good one

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Basic settings ;;
;;;;;;;;;;;;;;;;;;;;

;; I nicked the following from https://github.com/sunnyhasija/DOOMEmacs
;; Some more sensible than others, they are just up to preferences.
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line

;; Battery status display
(unless (equal "Battery status not avalible"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Fullscreen on startup
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))


;; Windows behaviour
; Pick what to see in the frame when you split a window

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;; Modeline default to UTF-8

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Windows rotation

(map! :map evil-window-map
      "SPC" #'rotate-layout
      "<left>"     #'evil-window-left
       "<down>"     #'evil-window-down
       "<up>"       #'evil-window-up
       "<right>"    #'evil-window-right
       ;; Swapping windows
       "C-<left>"       #'+evil/window-move-left
       "C-<down>"       #'+evil/window-move-down
       "C-<up>"         #'+evil/window-move-up
       "C-<right>"      #'+evil/window-move-right)

;; Windows title and project name

(setq frame-title-format
    '(""
      (:eval
       (if (s-contains-p org-roam-directory (or buffer-file-name ""))
           (replace-regexp-in-string ".*/[0-9]*-?" "🢔 " buffer-file-name)
         "%b"))
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; Projectile ;;
;;;;;;;;;;;;;;;;

;; Projectile default search path
(setq projectile-project-search-path '("~/Repositories/"))

;; ORG-MODE ;;
;;;;;;;;;;;;;;

;; ox-pandoc
; Sensitive options to convert org files
; from https://github.com/sunnyhasija/DOOMEmacs

(use-package! ox-pandoc
  :after org)
;; default options for all output formats
(setq org-pandoc-options '((standalone . _)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;; Load .md exporter for org

(eval-after-load "org"
  '(require 'ox-md nil t))

;; Use org-super-agenda
; from https://github.com/sunnyhasija/DOOMEmacs

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)
(setq org-agenda-files "~/Dropbox/Org/Daily/")
(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 1)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :face error
                                 :order 7)
                          (:name "Work"
                                 :tag "Work"
                                 :order 10)
                          (:name "Issues"
                                 :tag "Issue"
                                 :order 12)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Research"
                                 :tag "Research"
                                 :order 2)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "University"
                                 :tag "uni"
                                 :order 32)
                          (:name "Trivial"
                                 :priority<= "E"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))


;; TODO: add https://github.com/egh/zotxt-emacs

;; LaTeX ;;
;;;;;;;;;;;

;; LaTeX previews using pdf-tools
(latex-preview-pane-enable)
(setq +latex-viewers '(pdf-tools))

;; cheat.sh ;;
;;;;;;;;;;;;;;
;;
;; Taken from https://www.reddit.com/r/emacs/comments/6ddr7p/snippet_search_cheatsh_using_ivy/
;; TODO pull request on origin for cheat.sh

(defun ejmr-search-cheat-sh ()
  "Search `http://cheat.sh/' for help on commands and code."
  (interactive)
  (ivy-read "Command or Topic: "
      (process-lines "curl" "--silent" "http://cheat.sh/:list?T&q")
      :require-match t
      :sort t
      :history 'ejmr-search-cheat-sh
      :action (lambda (input)
		(eww-browse-url (concat "http://cheat.sh/" input "?T&q")))
      :caller 'ejmr-search-cheat-sh))

;; Elfeed ;;
;;;;;;;;;;;;

(setq elfeed-feeds
      '( ; New line with "link" OR ("link" tags moretags)

        "https://slatestarcodex.com/feed/"
        ("https://statmodeling.stat.columbia.edu/feed/" stats)
        ("https://thehardestscience.com/feed/" psych)
        ("https://kieranhealy.org/blog/index.xml" dataviz stats soc)
        ("https://errorstatistics.com/feed/" stats phil)
        "https://retractionwatch.com/feed/"

        ))

;; Bindings ;;
;;;;;;;;;;;;;;

;; org-noter (from: https://dotdoom.rgoswami.me/config.html)
(map! :leader
      :map (org-mode-map pdf-view-mode-map)
      (:prefix ("o" . "Org")
        (:prefix ("n" . "Noter")
          :desc "Noter" "n" 'org-noter
          )))

;; MISC:
;; Ugly mapping, but that's temporary.
;; TODO a better config management, possibly modular (see MatthewZMD/.emacs.d)

(map! :leader

      (:prefix-map ("z" . "custom")
       :desc "cheat.sh" "c" #'ejmr-search-cheat-sh
       :desc "Elfeed" "e" #'elfeed)

      )


;; Zettlekasten ;;
;;;;;;;;;;;;;;;;;;

;; This config come liberally from here:
;; https://rgoswami.me/posts/org-note-workflow/
;; I am currently testing it, so it might not work oob :(

;; Preparation

(setq
   ;; Assign variables for local folder & library
   ;; org_notes should use a flat hierarchy
   ;; zot_bib should be in sync with Zotero (via Better Bibtex addon)
   org_notes (concat (getenv "HOME") "/Repositories/zk/Notes/")
   zot_bib (concat (getenv "HOME") "/Repositories/zk/library.bib")

   ;; Set mode working folders
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )

;; org-roam settings
;;
(after! org-roam
; KEYBINDINGS
; from: https://www.ianjones.us/2020-05-05-doom-emacs
        (map! :leader
            :prefix "n"
            :desc "org-roam" "l" #'org-roam
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
            :desc "org-roam-find-file" "f" #'org-roam-find-file
            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-capture" "c" #'org-roam-capture)
; Default capture template
; from https://github.com/sunnyhasija/DOOMEmacs/blob/master/config.el
        (setq org-roam-ref-capture-templates
              '(("r" "ref" plain (function org-roam-capture--get-point)
                 "%?"
                 :file-name "websites/${slug}"
                 :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
                 :unnarrowed t)))
        )

;; org-ref

(use-package! org-ref
    :after org
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/home/giuliocentorame/Repositories/zk/library.bib")
         org-ref-bibliography-notes "/home/giuliocentorame/Repositories/zk/Notes/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/home/giuliocentorame/Repositories/zk/Notes/"
         org-ref-notes-function 'orb-edit-notes
    ))

;; helm-bibtex

(after! org-ref
  (setq
   bibtex-completion-notes-path "/home/giuliocentorame/Repositories/zk/Notes/"
   bibtex-completion-bibliography "/home/giuliocentorame/Repositories/zk/library.bib"
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )
   )
  )

;; org-noter

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )

;; org-roam-bibtex

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind  (:map org-mode-map
         (("C-c n a" . orb-note-actions))) ; Using default for now, TODO check if it doesn't break
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

;; Templates

;; Actually start using templates
(after! org-capture
  ;; Firefox and Chrome
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry ; key, name, type
                 (file+headline +org-capture-notes-file "Inbox") ; target
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t ; properties
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t))
)
;; org-roam-server FIXME later on
;; from https://github.com/sunnyhasija/DOOMEmacs/blob/master/config.el
;; (use-package! org-roam-server
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-label-truncate t
;;         org-roam-server-label-truncate-length 60
;;         org-roam-server-label-wrap-length 20)
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

;; Wakatime ;;
;;;;;;;;;;;;;;
(use-package! wakatime-mode
  :ensure t)

;; Enables wt for all buffers
(global-wakatime-mode)

;; Remember to set `wakatime-api-key' and `wakatime-cli-path'
;; OR to keep your config.el clean of personal information, run on CLI
;; `wakatime --config-write api_key [WAKATIME_API_KEY]'


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (wakatime-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

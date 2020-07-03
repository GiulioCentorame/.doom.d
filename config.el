;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Giulio Centorame"
      user-mail-address "giulio.centorame@kcl.ac.uk")

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


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-dracula)     ;;this is another good one

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)

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

;; Projectile ;;
;;;;;;;;;;;;;;;;

;; Projectile default search path
(setq projectile-project-search-path '("~/Repositories/"))

;; ORG-MODE ;;
;;;;;;;;;;;;;;

;; Load .md exporter for org

(eval-after-load "org"
  '(require 'ox-md nil t))

;; Use org-super-agenda

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Big Outcomes"
                                   :tag "bo")))
  :config
  (org-super-agenda-mode))

(require 'org-ref)
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
        ("https://kieranhealy.org/blog/index.xml" viz stats soc)
        ("https://errorstatistics.com/feed/" stats phil)
        "https://retractionwatch.com/feed/"

        ))

;; Bindings ;;
;;;;;;;;;;;;;;

;; Ugly mapping, but that's temporary.
;; TODO a better config management, possibly modular (see MatthewZMD/.emacs.d)

(map! :leader

      (:prefix-map ("z" . "custom")
       :desc "cheat.sh" "c" #'ejmr-search-cheat-sh
       :desc "Elfeed" "e" #'elfeed)

      )

;; Zettlekasten ;;
;;;;;;;;;;;;;;;;;;

;; This config come straight from here:
;; https://rgoswami.me/posts/org-note-workflow/
;; I am currently testing it, so it might not work oob :(

;; Basic variables:

(setq
   org_notes (concat (getenv "HOME") "/Repositories/zk/Notes/")
   zot_bib (concat (getenv "HOME") "/Repositories/zk/library.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )



;; org-ref

(use-package! org-ref
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

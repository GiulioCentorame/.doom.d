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
(setq doom-font (font-spec :family "Source Code Pro" :size 20 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 20 'weight 'regular))

;; Some cool fonts: FiraCode Nerd Font Complete Mono,

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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

;; GPG settings

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default: 7200 (2h)

;; I nicked the following from https://github.com/sunnyhasija/DOOMEmacs
;; Some more sensible than others, they are just up to preferences.
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names window-combination-resize t                      ; take new window space from all other windows (not just current)
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

;; Projectile ;;
;;;;;;;;;;;;;;;;

;; Projectile default search path
(setq projectile-project-search-path '("~/Repositories/"))

;; ORG-MODE ;;
;;;;;;;;;;;;;;

;; ox-pandoc
; Sensible options to convert org files
; from https://github.com/sunnyhasija/DOOMEmacs

;; (use-package! ox-pandoc
;;   :after org)
;; ;; default options for all output formats
;; (setq org-pandoc-options '((standalone . _)))
;; ;; cancel above settings only for 'docx' format
;; (setq org-pandoc-options-for-docx '((standalone . nil)))
;; ;; special settings for beamer-pdf and latex-pdf exporters
;; (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
;; (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; ;; special extensions for markdown_github output
;; (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;; ;; Load .md exporter for org

;; (eval-after-load "org"
;;   '(require 'ox-md nil t))

;; Use org-super-agenda
; from https://github.com/sunnyhasija/DOOMEmacs

;; LaTeX ;;
;;;;;;;;;;;

;; LaTeX previews using pdf-tools
;;(latex-preview-pane-enable)
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

;; Bindings ;;
;;;;;;;;;;;;;;

;; MISC:
;; Ugly mapping, but that's temporary.
;; TODO a better config management, possibly modular (see MatthewZMD/.emacs.d)

(map! :leader

      (:prefix-map ("z" . "custom")
       :desc "cheat.sh" "c" #'ejmr-search-cheat-sh
       ;; :desc "Elfeed" "e" #'elfeed
       )

      )


;; Wakatime ;;
;;;;;;;;;;;;;;
(use-package! wakatime-mode)

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


;; info-colors ;;
;;;;;;;;;;;;;;;;;

;; makes the documentation more readable

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook! 'Info-selection-hook 'info-colors-fontify-node)

(add-hook! 'Info-mode-hook #'mixed-pitch-mode)

;; pdf-tools ;;
;;;;;;;;;;;;;;;

(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
   ;; faster motion
 (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   ;; :n "N"            #'pdf-view-next-page-command
   ;; :n "E"            #'pdf-view-previous-page-command
   ;; :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
   ;; :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
   ;; :localleader
   ;; (:prefix "o"
   ;;  (:prefix "n"
   ;;   :desc "Insert" "i" 'org-noter-insert-note
   ;;   ))
 ))

;; Disable auto RDM ;;
;;;;;;;;;;;;;;;;;;;;;;

; Do not automatically try to run rdm
(remove-hook! 'c-mode-common-hook #'+cc|init-rtags)

(connection-local-set-profile-variables 'bc4-config
                                        '(magit-remote-git-executable . "/mnt/storage/software/tools/git-2.18.0/bin"))
(connection-local-set-profiles  '(:application 'tramp :protocol "ssh" :machine "bc4") 'bc4-config)

(elcord-mode)
;;;;;;;;;;;;;;;;;;;;
;; Enables wt for all buffers
;; Has to stay at the end of file
(global-wakatime-mode)

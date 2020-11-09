;; [[file:config.org::*PERSONAL INFORMATION][PERSONAL INFORMATION:1]]
(setq user-full-name "Facundo Prieto"
      user-mail-address "facundo.prieto321@gmail.com")
;; PERSONAL INFORMATION:1 ends here

;; [[file:config.org::*Simple settings][Simple settings:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t)                        ; Nobody likes to loose work, I certainly don't

(display-time-mode 1)                             ; Enable time in the mode-line
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words
;; Simple settings:1 ends here

;; [[file:config.org::*Windows][Windows:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Windows:1 ends here

;; [[file:config.org::*Windows][Windows:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
;; Windows:2 ends here

;; [[file:config.org::*Windows][Windows:3]]
(setq +ivy-buffer-preview t)
;; Windows:3 ends here

;; [[file:config.org::*Buffer default][Buffer default:1]]
(setq-default major-mode 'org-mode)
;; Buffer default:1 ends here

;; [[file:config.org::*FONTS][FONTS:1]]
(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; remove ligatures on python-mode
(set-ligatures! 'python-mode nil)
;; FONTS:1 ends here

;; [[file:config.org::*THEME][THEME:1]]
(setq doom-theme 'doom-vibrant)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)
;; THEME:1 ends here

;; [[file:config.org::*MISCELLANEOUS][MISCELLANEOUS:1]]
(setq doom-fallback-buffer-name "Doom"
      +doom-dashboard-name "Doom")
;; MISCELLANEOUS:1 ends here

;; [[file:config.org::*MISCELLANEOUS][MISCELLANEOUS:2]]
(custom-set-faces! '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB"))
;; MISCELLANEOUS:2 ends here

;; [[file:config.org::*BOOKMARKS AND BUFFERS][BOOKMARKS AND BUFFERS:1]]
(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)
;; BOOKMARKS AND BUFFERS:1 ends here

;; [[file:config.org::*EVALUATE ELISP EXPRESSIONS][EVALUATE ELISP EXPRESSIONS:1]]
(map! :leader
      :desc "Evaluate elisp in buffer"
      "e b" #'eval-buffer
      :leader
      :desc "Evaluate defun"
      "e d" #'eval-defun
      :leader
      :desc "Evaluate elisp expression"
      "e e" #'eval-expression
      :leader
      :desc "Evaluate last sexpression"
      "e l" #'eval-last-sexp
      :leader
      :desc "Evaluate elisp in region"
      "e r" #'eval-region)
;; EVALUATE ELISP EXPRESSIONS:1 ends here

;; [[file:config.org::*IVY][IVY:1]]
(map! :leader
      :desc "Ivy push view"
      "v p" #'ivy-push-view
      :leader
      :desc "Ivy switch view"
      "v s" #'ivy-switch-view)

(setq ivy-read-action-function #'ivy-hydra-read-action)
(setq ivy-sort-max-size 50000)
;; IVY:1 ends here

;; [[file:config.org::*LINE SETTINGS][LINE SETTINGS:1]]
(setq display-line-numbers-type t)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)
;; LINE SETTINGS:1 ends here

;; [[file:config.org::*MANPAGES][MANPAGES:1]]
(require 'ox-groff)
;; MANPAGES:1 ends here

;; [[file:config.org::*NEOTREE][NEOTREE:1]]
(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'neotree-toggle)
;; NEOTREE:1 ends here

;; [[file:config.org::*Tweaking defaults][Tweaking defaults:1]]
(setq org-directory  "~/Documents/Notes/Org/"     ; let's put files here
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convininet
      org-list-allow-alphabetical t               ; have a. A. a A list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
      org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
      +org-capture-todo-file "~/Documents/Notes/Org/Todo.org"
      org-agenda-files
      '("~/Documents/Notes/Org/Tasks.org"
        "~/Documents/Notes/Org/Habits.org"
        "~/Documents/Notes/Org/Journal.org"
        "~/Documents/Notes/Org/Agenda.org")
      org-journal-dir "~/Documents/Notes/Org/journal/"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-file-format "%Y-%m-%d.org")
;; Tweaking defaults:1 ends here

;; [[file:config.org::*TAGs & TODO keywords][TAGs & TODO keywords:1]]
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "IDEA(i)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "METTING(m)" "FIXME" "|" "DONE(d)")
        (sequence "[ ](T)" "[-](S)" "SOMEDAY(f)" "|" "[X](D)" "CANCELED(c)" "DELEGATED(l)")
        ))
(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("[-]" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold))
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))
;; TAGs & TODO keywords:1 ends here

;; [[file:config.org::*TAGs & TODO keywords][TAGs & TODO keywords:2]]
(setq org-tag-persistent-alist
      '(
        ("HOME" . ?h)
        ("RESEARCH" . ?r)
        ("DEV" . ?d)
        ("EMACS" . ?s)
        ("THINK" . ?t)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        ("URGENT" . ?u)
        ("FACULTAD" . ?f)
        ("FISICA" . ?i)
        ("ANALISIS" . ?n)
        ("ALGORITMOS" . ?l)
        ("PARCIAL" . ?p)
        ("TP" . ?T)
        ))
(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("THINK" . (:foreground "GoldenRod" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("EMACS" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("EASY" . (:foreground "OrangeRed" :weight bold))
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
        ("HARD" . (:foreground "OrangeRed" :weight bold))
        ("FACULTAD" . (:foreground "GoldenRod" :weight bold))
        ("FISICA" . (:foreground "LimeGreen" :weight bold))
        ("ANALISIS" . (:foreground "LimeGreen" :weight bold))
        ("ALGORITMOS" . (:foreground "LimeGreen" :weight bold))
        ("PARCIAL" . (:foreground "LimeGreen" :weight bold))
        ("TP" . (:foreground "LimeGreen" :weight bold))
        ))
;; TAGs & TODO keywords:2 ends here

;; [[file:config.org::*Headers default arguments][Headers default arguments:1]]
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))
;; Headers default arguments:1 ends here

;; [[file:config.org::*Prevents some error with ~visual-line-mode~ and ~auto-fill-mode~][Prevents some error with ~visual-line-mode~ and ~auto-fill-mode~:1]]
(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
;; Prevents some error with ~visual-line-mode~ and ~auto-fill-mode~:1 ends here

;; [[file:config.org::*Arrow keys equivalents][Arrow keys equivalents:1]]
(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-forward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)
;; Arrow keys equivalents:1 ends here

;; [[file:config.org::*Bullets and priorities][Bullets and priorities:1]]
(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◦""•")
        org-superstar-prettify-item-bullets t ))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "■ 5")
                                    (?B . "■ 4")
                                    (?C . "■ 3")
                                    (?D . "■ 2")
                                    (?E . "■ 1"))))
(after! org
  (setq org-ellipsis " ▾ "
        org-cycle-separator-lines -1
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-default ?C
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))
;; Bullets and priorities:1 ends here

;; [[file:config.org::*Make easier the creation of a org buffer][Make easier the creation of a org buffer:1]]
(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))
;; Make easier the creation of a org buffer:1 ends here

;; [[file:config.org::*List bullet sequence][List bullet sequence:1]]
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
;; List bullet sequence:1 ends here

;; [[file:config.org::*Citation][Citation:1]]
(use-package! org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite))
;; Citation:1 ends here

;; [[file:config.org::*Capture templates][Capture templates:1]]
;; semi full screen for capture (I only need in some capture templates this funcionality)
;;(add-hook 'org-capture-mode-hook 'doom/window-enlargen)
(setq +org-capture-tasks  "~/Documents/Notes/Org/Tasks.org")
(setq org-capture-templates
      (doct '(("Facultad" :keys "f"
               :file +org-capture-tasks
               :prepend t
               :template ("* [ ] %^{Description} "
                          ":PROPERTIES:"
                          ":Created: %U"
                          ":END:"
                          )
               :children (("Algoritmos" :keys "a"
                           :olp ("Facultad" "Algoritmos")
                           :hook (lambda () (message "New task on Algoritmos"))
                           :template ("* [ ] %^{Description} "
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "%a"
                                      ))
                          ("Analisis" :keys "n"
                           :olp ("Facultad" "Analisis")
                           :hook (lambda () (message "New task on Analisis")))
                          ("Fisica" :keys "f"
                           :olp ("Facultad" "Fisica")
                           :hook (lambda () (message "New task on Fisica")))))
              ("Tasks" :keys "t"
               :file +org-capture-tasks
               :prepend t
               :headline  "Inbox"
               :children (("Task"  :keys "t"
                           :template ("* [ ] %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      )
                           :hook (lambda () (message "New task on Inbox")))
                          ("Task with reference" :keys "r"
                           :template ("* [ ] %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "%a"
                                      )
                           :hook (lambda () (message "New task on Inbox")))))
              ("Programacion" :keys "p"
               :file +org-capture-tasks
               :prepend t
               :olp ("Programacion" "Inbox")
               :children (("Fixme"  :keys "f"
                           :template ("* FIXME %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "%a"
                                      )
                           :hook (lambda () (message "New task on Programacion/Inbox")))
                          ("Task" :keys "t"
                           :template ("* [ ] %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      )
                           :hook (lambda () (message "New task on Programacion/Inbox")))
                          ("Task with reference" :keys "r"
                           :template ("* [ ] %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "%a"
                                      )
                           :hook (lambda () (message "New task on Programacion/Inbox")))))
              ("Notes" :keys "n"
               :file "~/Documents/Notes/Org/Notes.org"
               :prepend t
               :template ("* %^{Description}"
                          ":PROPERTIES:"
                          ":Created: %U"
                          ":END:"
                          )
               :hook (lambda () (message "New Note")))
              ("Links" :keys "l"
               :file "~/Documents/Notes/Org/Links.org"
               :prepend t
               :type entry
               :headline "Inbox"
               :children (("Simple link" :keys "l"
                           :template ("* [[%^{Link}][%^{Description}]]"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      ))
                          ("Cliplink" :keys "c"
                           :template ("* %(org-cliplink-capture)"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:")))

               :hook (lambda () (message "New Link")))
              ("Agenda" :keys "a"
               :file "~/Documents/Notes/Org/Agenda.org"
               :prepend t
               :headline "Inbox"
               :template ("* [ ] %^{Description}"
                          "DEADLINE: %^{Due date:}t"
                          ":PROPERTIES:"
                          ":Created: %U"
                          ":END:")
               :hook (lambda () (message "New Deadline")))
              ("Journal" :keys "j"
               :file "~/Documents/Notes/Org/Journal.org"
               :datetree nil|t
               :prepend t
               :type entry
               :hook (lambda () (doom/window-enlargen))
               :children (("Journal" :keys "j"
                           :template ("\n* %<%I:%M %p> - Diario \n%?\n\n"))
                          ("Rutina matutina" :keys "r"
                           :template ("* Rutina matutina \n/mr%?"
                                      "* Agradecimientos / Motivacion"))))
              )))
;; Capture templates:1 ends here

;; [[file:config.org::*Habits][Habits:1]]
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
;; Habits:1 ends here

;; [[file:config.org::*Font Display][Font Display:1]]
(custom-set-faces!
  '(outline-1 :weight semi-bold :height 1.25)
  '(outline-2 :weight semi-bold :height 1.15)
  '(outline-3 :weight semi-bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
;; Font Display:1 ends here

;; [[file:config.org::*Font Display][Font Display:2]]
(after! org
  (custom-set-faces!
    '(org-document-title :height 1.2)))
;; Font Display:2 ends here

;; [[file:config.org::*Symbols][Symbols:1]]
(after! org
  (appendq! +ligatures-extra-symbols
            `(:list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :latex_class   "🄲"
              :latex_header  "⇥"
              :beamer_header "↠"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :begin_quote   "❮"
              :end_quote     "❯"
              :caption       "☰"
              :header        "›"
              :results       "➥"
              :begin_export  "⯮"
              :end_export    "⯬"
              :properties    "⚙"
              :end           "∎"))
  (set-ligatures! 'org-mode
    :merge t
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_latex:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"))
(plist-put +ligatures-extra-symbols :name "⁍")
;; Symbols:1 ends here

;; [[file:config.org::*Exporting (general)][Exporting (general):1]]
(after! org (setq org-export-headline-levels 5)) ; I like nesting
(after! org
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;; Exporting (general):1 ends here

;; [[file:config.org::*Babel][Babel:1]]
(setq org-babel-python-command "python3")
;; Babel:1 ends here

;; [[file:config.org::*Babel][Babel:2]]
(defun tec-org-python ()
  (if (eq major-mode 'python-mode)
      (progn (anaconda-mode t)
             (company-mode t))))
(add-hook 'org-src-mode-hook 'tec-org-python)
;; Babel:2 ends here

;; [[file:config.org::*Automatically "Tangle" on Save][Automatically "Tangle" on Save:1]]
;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dw/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))
;; Automatically "Tangle" on Save:1 ends here

;; [[file:config.org::*DEFT][DEFT:1]]
(setq deft-directory "~/Documents/Notes"
      deft-extensions '("txt" "org" "md")
      deft-recursive t)
;; DEFT:1 ends here

;; [[file:config.org::*REGISTERS][REGISTERS:1]]
(map! :leader
      :desc "Copy to register"
      "r c" #'copy-to-register
      :leader
      :desc "Frameset to register"
      "r f" #'frameset-to-register
      :leader
      :desc "Insert contents of register"
      "r i" #'insert-register
      :leader
      :desc "Jump to register"
      "r j" #'jump-to-register
      :leader
      :desc "List registers"
      "r l" #'list-registers
      :leader
      :desc "Number to register"
      "r n" #'number-to-register
      :leader
      :desc "Interactively choose a register"
      "r r" #'counsel-register
      :leader
      :desc "View a register"
      "r v" #'view-register
      :leader
      :desc "Window configuration to register"
      "r w" #'window-configuration-to-register
      :leader
      :desc "Increment register"
      "r +" #'increment-register
      :leader
      :desc "Point to register"
      "r SPC" #'point-to-register)
;; REGISTERS:1 ends here

;; [[file:config.org::*SHELLS][SHELLS:1]]
(setq shell-file-name "/bin/zsh"
      eshell-aliases-file "~/.doom.d/aliases")
;; SHELLS:1 ends here

;; [[file:config.org::*SPLITS][SPLITS:1]]
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)
;; SPLITS:1 ends here

;; [[file:config.org::*SUBLIMITY][SUBLIMITY:1]]
(require 'sublimity-scroll)
(require 'sublimity-attractive)
(sublimity-mode 0)
;; SUBLIMITY:1 ends here

;; [[file:config.org::*CENTAUR TABS][CENTAUR TABS:1]]
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)
;; CENTAUR TABS:1 ends here

;; [[file:config.org::*COMPANY][COMPANY:1]]
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
;; COMPANY:1 ends here

;; [[file:config.org::*COMPANY][COMPANY:2]]
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
;; COMPANY:2 ends here

;; [[file:config.org::*ESS][ESS:1]]
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))
;; ESS:1 ends here

;; [[file:config.org::*EROS-EVAL][EROS-EVAL:1]]
(setq eros-eval-result-prefix "⟹ ")
;; EROS-EVAL:1 ends here

;; [[file:config.org::*EVIL][EVIL:1]]
(after! evil-escape (evil-escape-mode -1))
;; EVIL:1 ends here

;; [[file:config.org::*EVIL][EVIL:2]]
(after! evil (setq evil-ex-substitute-global t)) ; I like my s/../.. to by global by default
;; EVIL:2 ends here

;; [[file:config.org::*INFO COLORS][INFO COLORS:1]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)
;; INFO COLORS:1 ends here

;; [[file:config.org::*MAGIT][MAGIT:1]]
(after! magit
     (magit-delta-mode +1))
;; MAGIT:1 ends here

;; [[file:config.org::*ORG MSG][ORG MSG:1]]
(setq +org-msg-accent-color "#1a5fb4")
(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'org-msg-goto-body)
;; ORG MSG:1 ends here

;; [[file:config.org::*ORG CHEF][ORG CHEF:1]]
(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))
;; ORG CHEF:1 ends here

;; [[file:config.org::*WHICH-KEY][WHICH-KEY:1]]
(setq which-key-idle-delay 0.5)
;; WHICH-KEY:1 ends here

;; [[file:config.org::*WHICH-KEY][WHICH-KEY:2]]
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; WHICH-KEY:2 ends here

;; [[file:config.org::*WRITEROOM][WRITEROOM:1]]
(setq +zen-text-scale 0.6)
;; WRITEROOM:1 ends here

;; [[file:config.org::*WRITEROOM][WRITEROOM:2]]
(after! writeroom-mode
  (add-hook 'writeroom-mode-hook
            (defun +zen-cleaner-org ()
              (when (and (eq major-mode 'org-mode) writeroom-mode)
                (setq-local -display-line-numbers display-line-numbers
                            display-line-numbers nil)
                (setq-local -org-indent-mode org-indent-mode)
                (org-indent-mode -1)
                (when (featurep 'org-superstar)
                  (setq-local -org-superstar-headline-bullets-list org-superstar-headline-bullets-list
                              ;; org-superstar-headline-bullets-list '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                              org-superstar-headline-bullets-list '("🙘" "🙙" "🙚" "🙛")
                              -org-superstar-remove-leading-stars org-superstar-remove-leading-stars
                              org-superstar-remove-leading-stars t)
                  (org-superstar-restart)))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-dirty-org ()
              (when (eq major-mode 'org-mode)
                (setq-local display-line-numbers -display-line-numbers)
                (when -org-indent-mode
                  (org-indent-mode 1))
                (when (featurep 'org-superstar)
                  (setq-local org-superstar-headline-bullets-list -org-superstar-headline-bullets-list
                              org-superstar-remove-leading-stars -org-superstar-remove-leading-stars)
                  (org-superstar-restart))))))
;; WRITEROOM:2 ends here

;; [[file:config.org::*YASNIPPET][YASNIPPET:1]]
(yas-global-mode 1)
(setq yas-triggers-in-field t)
(add-hook 'yas-minor-mode-hook (lambda ()
                                 (yas-activate-extra-mode 'fundamental-mode)))
;; YASNIPPET:1 ends here

;; [[file:config.org::*MARKDOWN][MARKDOWN:1]]
(add-hook! (gfm-mode markdown-mode) #'mixed-pitch-mode)
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
;; MARKDOWN:1 ends here

;; [[file:config.org::*MARKDOWN][MARKDOWN:2]]
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))
;; MARKDOWN:2 ends here

;; BBDB setup.  Must be here because it all has to be set before the 
;; .gnus.el is read
(require 'bbdb)
(require 'bbdb-hooks)
(bbdb-initialize)
(add-hook 'message-mode-hook
          '(lambda()
             (auto-fill-mode)
             (bbdb-insinuate-message)))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus) 
(setq bbdb-extract-address-component-ignore-regexp "\\(doug@lathi\\.net\\)\\|\\(\\(undisclosed\\|unlisted\\)[^,]*recipients\\)\\|no To-header on input")
(setq bbdb-send-mail-style (quote message))
(setq bbdb-offer-save 'auto)            ;just save, don't ask
(setq bbdb-electric-p nil)              ;electric.el has some problems
(setq bbdb-user-mail-names nil)         ;show the To: field when the From field matches this regexp
(setq bbdb-always-add-addresses 'never) ;never add to new net addresses to the list
(setq bbdb-new-nets-always-primary t)   ;always make new addresses the primary one
(setq bbdb-quiet-about-name-mismatches nil) ;prompt about name changes
(setq bbdb-use-alternate-names t)       ;use the aka field
(setq bbdb-canonicalize-redundatn-nets-p t) ;don't notice foo@bar.baz.com if foo@baz.com exists
(setq bbdb/mail-auto-create-p nil)
(setq bbdb/news-auto-create-p nil)
(setq bbdb-completion-type 'primary-or-name) ;completion is done across real name and primary net
(setq bbdb-complete-name-allow-cycling t)
(setq bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-display-omit-fields '(pilot-id permanent last-access creation-date))
(setq bbdb-auto-notes-alist
      '(("user-agent" (".*" interface 0 t))
        ("X-Mailer" (".*" interface 0 t))
        ("X-Newsreader" (".*" interface 0 t))
        ("Organization" (".*" company 0))
        ;;("newsgroups" ("\\([^,]*\\),?" posted-to "\\1"))
        ;;("list-id" (".*" posted-to 0))
        ;;("x-mailing-list" (".*" posted-to 0))
        ))
(setq bbdb-ignore-some-messages-alist nil)
(setq bbdb-use-pop-up 'horiz)

(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

(setq bbdb-dial-program "/home/dalcorn/bin/dial-number")

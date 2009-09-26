;;; mac-key-mode.el --- provide mac-style key bindings on Carbon Emacs

;; Copyright (C) 2004-2005  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: v20050106
;; Keywords: tools, mac
;; Created: 2004-12-27
;; Compatibility: Emacs 21
;; URL(en): http://home.att.ne.jp/alpha/z123/elisp-e.html
;; URL(jp): http://macwiki.sourceforge.jp/cgi-bin/wiki.cgi?mac-key-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `mac-key-mode', a minor mode that provides
;; mac-like key bindings and relevant elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (mac-key-mode)
;;
;; To customize key bindings, modify mac-key-mode-map in your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (define-key mac-key-mode-map [(alt f)] 'goto-line)
;;
;; Caution:
;; When enabled, `mac-key-mode' overrides some printer-related variables.
;; 
;; Note that it requires emacs 21 or later.

;;; Code:


(defgroup mac-key-mode nil
  "mac-style key-binding mode"
  :group 'mac
  :version "21.4")

(defcustom mac-key-mode-gs-command "/usr/local/bin/gs"
  "The command by which to invoke the gs program."
  :type 'string
  :group 'mac-key-mode)

(defcustom mac-key-mode-tmpdir
  (if (featurep 'carbon-emacs-package)
      carbon-emacs-package-tmpdir
    "/tmp/")
  "Document forthcoming..."
  :type 'string
  :group 'mac-key-mode)

(defvar mac-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(alt c)] 'kill-ring-save)
    (define-key map [(alt v)] 'yank)
    (define-key map [(alt x)] 'kill-region)
    (define-key map [(alt a)] 'mark-whole-buffer)
    (define-key map [(alt z)] 'undo)
;    (define-key map [(alt meta z)] 'redo) ; requires redo
    (define-key map [(alt f)] 'isearch-forward)
    (define-key map [(alt meta f)] 'occur)
    (define-key map [(alt g)] 'isearch-repeat-forward)
    (define-key map [(alt o)] 'mac-key-open-file)
    (define-key map [(alt s)] 'save-buffer)
    (define-key map [(alt w)] 'kill-this-buffer)
    (define-key map [(alt m)] 'iconify-frame)
    (define-key map [(alt q)] 'save-buffers-kill-emacs)
    (define-key map [(alt p)] 'ps-print-buffer-with-faces)
    (define-key map [(alt i)] 'mac-key-show-in-finder)
;    (define-key map [(alt t)] 'mac-key-open-terminal)
    (define-key map [(alt l)] 'lookup-word) ; requires lookup
    (define-key map [(alt .)] 'keyboard-quit)
    (define-key map [(alt up)] 'beginning-of-buffer)
    (define-key map [(alt down)] 'end-of-buffer)
    (define-key map [(alt left)] 'beginning-of-line)
    (define-key map [(alt right)] 'end-of-line)
    (define-key map [A-mouse-1] 'browse-url-at-mouse) ; command + click
    map)
  "Keymap for `mac-key-mode'.")

(define-minor-mode mac-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-key-mode
;  :lighter " M"
  :keymap 'mac-key-mode-map
  (if mac-key-mode
      (progn
;        (require 'redo)
        (setq mac-command-key-is-meta nil)

        (when (file-exists-p mac-key-mode-gs-command)
          
          (defalias 'ps-mule-header-string-charsets 'ignore)
          (setq ps-multibyte-buffer 'non-latin-printer)
          (setq ps-lpr-command mac-key-mode-gs-command)
          (setq ps-lpr-switches
                (list "-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=pdfwrite"
                      "-dCompatibilityLevel=1.3" "-dAutoFilterGrayImages=false"
                      "-dAutoFilterColorImages=false"
                      "-dGrayImageFilter=/FlateEncode"
                      "-dColorImageFilter=/FlateEncode"
                      "-dUseFlateCompression=true"
                      (concat "-sOutputFile=\|cat\> "
                              mac-key-mode-tmpdir "preview.pdf"
                              "&& /usr/bin/open -a Preview.app "
                              mac-key-mode-tmpdir "preview.pdf")
                      "-c .setpdfwrite" "-f" "-" "-c quit"))
          
          (eval-after-load "lpr"
            '(progn
               (defun print-buffer()(interactive)(ps-print-buffer-with-faces))
               (defun print-region()(interactive)(ps-print-region-with-faces))
               ))
          
          )
        (define-key-after menu-bar-file-menu [my-file-separator]
          '("--" . nil) 'recover-session)
        (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '("Show In Finder" . mac-key-show-in-finder) 'my-file-separator)
        (define-key-after menu-bar-file-menu [mac-open-terminal]
          '("Open Terminal" . mac-key-open-terminal) 'mac-show-in-finder)
        )
    (progn
      (define-key global-map [menu-bar file my-file-separator] nil)
      (define-key global-map [menu-bar file mac-show-in-finder] nil)
      (define-key global-map [menu-bar file mac-open-terminal] nil)
      ))
  )


(defun mac-key-open-file ()
  "Document forthcoming..."
  (interactive)
  (let ((file (do-applescript "try
POSIX path of (choose file)
end try")))
    (if (> (length file) 3)
        (setq file
              (replace-regexp-in-string
               "\\\\\\(.\\)" "\\1"
               (decode-coding-string
                (substring file 1 (- (length file) 1))
                'sjis-mac))))
    (if (and (not (equal file ""))(file-readable-p file))
        (find-file file)
      (beep))
    ))


;; Show In Finder

(defun mac-key-show-in-finder ()
  "Document forthcoming..."
  (interactive)
  (if (stringp (buffer-file-name))
      (do-applescript
       (format "
tell application \"Finder\"
  activate
  try
    select file \"%s\"
  on error
    beep
  end try
end tell"
               (if (eq selection-coding-system 'sjis-mac)
                   (replace-regexp-in-string
                    "\\\\" "\\\\\\\\"
                    (encode-coding-string
                     (posix-file-name-to-mac (buffer-file-name))
                     selection-coding-system))
                 (encode-coding-string
                  (posix-file-name-to-mac (buffer-file-name))
                  selection-coding-system))
               ))
    (shell-command "/usr/bin/open .")
    ))


;; Open Terminal.app

(defun mac-key-open-terminal ()
  "Document forthcoming..."
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     )
    (do-applescript
     (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))
    ))


(provide 'mac-key-mode)

;; mac-key-mode.el ends here

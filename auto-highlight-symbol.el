;;; auto-highlight-symbol.el --- Automatic highlighting current symbol minor mode

;; Copyright (C) 2009 2010 Mitsuo Saito
;; Created date 2009-03-03 21:44 +0900

;; Author: Mitsuo Saito <arch320@NOSPAM.gmail.com>
;; Version: 1.54 beta
;; Keywords: face match convenience
;; URL: http://github.com/mitsuo-saito/auto-highlight-symbol-mode/raw/master/auto-highlight-symbol.el
;; Compatibility: GNU Emacs 22.3 23.x 24.x later
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; If you have `linkd.el' turn on `linkd-mode'
;; and (setq linkd-use-icons t ) more easily navigation.
;; You can get `linkd.el' here:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
;;  http://www.emacswiki.org/emacs/linkd.tar.gz -- with cool icon
;;

;;; (@* "Index" )
;;
;; (@> "What's this")        I am ...
;; (@> "Setup")              Basic setup
;; (@> "Screencast")         Screencast
;; (@> "Setting")            Setting example
;; (@> "Mode map")           Key binding
;;
;; (@> "Custom variable")    Customizable varible
;; (@> "Face")               Face used in auto-highlight-symbol-mode
;; (@> "Regular expression") Symbol include/exclude regular expression
;; (@> "Internal variable")  Internal variables
;; (@> "Range plugin")       Range plugin function
;; (@> "Built-in plugin")    Built-in plugin section
;; (@> "Timer")              Timer function
;; (@> "Idle")               Idle function
;; (@> "Highlight")          Highlight function
;; (@> "Edit mode")          Overlay's modification-hook function used in edit mode
;; (@> "Select")             Selective function
;; (@> "Interactive")        Interactive function
;; (@> "Define mode")        Register minor mode
;; (@> "Revert")             Protect from revert-buffer
;;

;;; (@* "What's this" )
;;
;;  A minor mode for emacs.
;;
;;   * automatic highlighting current symbol like eclipse IDE.
;;   * cycle through highlighted locations.
;;   * can specify the range to highlight.
;;   * can edit the highlighted symbols at a time.
;;
;;  Tested on GNU Emacs 22.3/23.2/24.0.50
;;

;;; (@* "Setup" )
;;
;; Basic steps to setup:
;;   1. Place `auto-highlight-symbol.el' in your `load-path'.
;;
;;   2. In your `.emacs.el' file
;;      (require 'auto-highlight-symbol )
;;      (global-auto-highlight-symbol-mode t)
;;

;;; (@* "Screencast" )
;;
;;  Screencast available on YouTube and ScreenToaster
;;    YouTube -- http://www.youtube.com/watch?v=xzJ2r4-s7fo
;;    ScreenToaster -- http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast
;;

;;; (@* "Setting" )
;;
;;   * If you want set idle interval before highlighting
;;      (ahs-set-idle-interval 0.5 )
;;
;;      or M-x ahs-set-idle-interval <RET>
;;
;;   * Default mode's behavior(highlighting and editing)
;;     affects display area only   ;; from (window-start) to (window-end)
;;
;;     If you want affects whole buffer
;;
;;      all-buffers
;;         (setq ahs-default-range 'ahs-range-whole-buffer)
;;
;;      buffer-local
;;         (add-hook 'emacs-lisp-mode-hook
;;                   (function
;;                     (lambda()
;;                       (ahs-change-range 'ahs-range-whole-buffer t))))
;;
;;         or M-x ahs-chrange-whole-buffer <RET>
;;
;;      temporary
;;         C-u C-x C-a   ;; call 'ahs-edit-mode with prefix-args
;;
;;     But changing symbol you can't see. so carefully.
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `ahs-forward'
;;    Select highlighted symbols forwardly.
;;  `ahs-backward'
;;    Select highlighted symbols backwardly.
;;  `ahs-forward-definition'
;;    Select highlighted symbols forwardly. only symbol definition.
;;  `ahs-backward-definition'
;;    Select highlighted symbols backwardly. only symbol definition.
;;  `ahs-back-to-start'
;;    Go back to the highlighting start point.
;;  `ahs-change-range'
;;    Change range according to plugin's definition.
;;  `ahs-set-idle-interval'
;;    Set wait until highlighting symbol when emacs is idle.
;;  `ahs-highlight-now'
;;    Highlight NOW!!
;;  `ahs-goto-web'
;;    Go to official? web site.
;;  `ahs-toggle-search-whole-buffer'
;;    Obsolete. please use `ahs-change-range' instead.
;;  `ahs-edit-mode'
;;    Turn on edit mode. if call with prefix args , change range to `whole buffer' temporary.
;;  `auto-highlight-symbol-mode'
;;    Automatic highlighting current symbol minor mode
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `ahs-modes'
;;    Major modes `auto-highlight-symbol-mode' can run on.
;;  `ahs-edit-mode-lighter-pair'
;;    Decorate mode line lighter in edit mode.
;;  `ahs-case-fold-search'
;;    *Non-nil means case fold search.
;;  `ahs-default-range'
;;    Default range plugin
;;  `ahs-select-invisible'
;;    Behavior when selected symbol in hidden text.
;;  `auto-highlight-symbol-mode-hook'
;;    Hook for `auto-highlight-symbol-mode'.
;;  `ahs-edit-mode-on-hook'
;;    Normal hook for run when entering edit mode.
;;  `ahs-edit-mode-off-hook'
;;    Normal hook for run when go out edit mode.
;;  `ahs-idle-interval'
;;    Number of seconds to wait before highlighting symbol.
;;  `ahs-inhibit-face-list'
;;    Face list of inhibit highlighting
;;  `ahs-invisible-face-list'
;;    Face list of not highlighting. make overlay only.
;;  `ahs-definition-face-list'
;;    Face list of symbol definition face
;;  `ahs-include'
;;    Include symbol regular expression pattern.
;;  `ahs-exclude'
;;    Exclude symbol regular expression pattern.

;;
;; Happy Coding !!
;;

;;; SCM Log
;;
;;   $Revision: 81:389de071b80e tip $
;;   $Commiter: Mitso Saito <arch320@NOSPAM.gmail.com> $
;;   $LastModified: Sat, 06 Nov 2010 20:03:06 +0900 $
;;
;;   $Lastlog: font lock 2 $
;;

;;; Changelog
;;
;; v1.54 beta
;;   refactor
;;   minor bug fix
;;   cosmetics
;;
;; v1.53 2010-11-03 22:17 +0900
;;   improve invisible overlay's handling
;;   new plugin property `face' available
;;   add ahs-back-to-start
;;   minor bug fix
;;
;; v1.52 2010-10-31 14:46 +0900
;;   skip folding(select function only)
;;
;; v1.51 2010-10-30 09:17 +0900
;;   plugin minor change
;;
;; v1.5  2010-10-30 02:31 +0900
;;   add range plugin
;;    ahs-whole-of-buffer is not working.
;;    use ahs-default-range instead.
;;    ahs-mode-lighter , ahs-wmode-lighter is not be used
;;
;; v1.03 2010-10-28 07:00 +0900
;;   bug fix
;;
;; v1.02 2010-10-26 23:39 +0900
;;   minor fix
;;
;; v1.01 2010-10-26 20:50 +0900
;;   add edit mode hook for protect overlay
;;
;; v1.0  2010-10-26 16:33 +0900
;;   first release
;;

;;; (@* "TODO" )
;;
;;  fix font-lock-fontify  !!MUST BE FIX!!
;;  more refactor
;;  fix poor doc
;;
;;  sticky highlight - 1.6 maybe :D
;;

;;; Code:

(eval-when-compile
  ;; Suppress bytecompiler error warning
  (require 'easy-mmode)
  (require 'cl)
  (defvar dropdown-list-overlays nil))

(eval-and-compile
  (defconst ahs-web "http://github.com/mitsuo-saito/auto-highlight-symbol-mode/") ;; :D
  ;; Compatibility for GNU Emacs 22.3
  (if (>= emacs-major-version 23)
      (defalias 'ahs-called-interactively-p 'called-interactively-p)
    (defmacro ahs-called-interactively-p (&optional arg)
      '(called-interactively-p))))

(defconst ahs-mode-vers "$Id: auto-highlight-symbol.el,v 81:389de071b80e 2010-11-06 20:03 +0900 arch320 $"
  "auto-highlight-symbol-mode version.")

;;
;; (@* "Custom variable" )
;;
(defgroup auto-highlight-symbol nil
  "Automatic highlighting current symbol minor mode"
  :group 'convenience
  :link `(url-link :tag "Download latest version" ,(eval-when-compile (concat ahs-web "raw/master/auto-highlight-symbol.el")))
  :link `(url-link :tag "Wiki" ,(eval-when-compile (concat ahs-web "wiki/")))
  :link `(url-link :tag "Information" ,(eval-when-compile ahs-web)))

(defcustom ahs-modes
  '(
    actionscript-mode
    apache-mode
    bat-generic-mode
    c++-mode
    c-mode
    csharp-mode
    css-mode
    dos-mode
    emacs-lisp-mode
    html-mode
    ini-generic-mode
    java-mode
    javascript-mode
    js-mode
    lisp-interaction-mode
    lua-mode
    latex-mode
    makefile-mode
    makefile-gmake-mode
    markdown-mode
    moccur-edit-mode
    nxml-mode
    nxhtml-mode
    outline-mode
    perl-mode cperl-mode
    php-mode
    python-mode
    rc-generic-mode
    reg-generic-mode
    ruby-mode
    sgml-mode
    sh-mode
    squirrel-mode
    text-mode
    tcl-mode
    visual-basic-mode
    )
  "Major modes `auto-highlight-symbol-mode' can run on."
  :group 'auto-highlight-symbol
  :type '(repeat symbol))

(defvar ahs-mode-lighter nil
  "Obsolete. Do not use.")

(defvar ahs-wmode-lighter nil
  "Obsolete. Do not use.")

(defvar ahs-edit-mode-lighter nil
  "Obsolete. Do not use.")

(defcustom ahs-edit-mode-lighter-pair '( "*" . "*" )
  "Decorate mode line lighter in edit mode."
  :group 'auto-highlight-symbol
  :type '(choice (cons :tag "Asterisk"    (string "*") (string "*"))
                 (cons :tag "Exclamation" (string "!") (string "!"))
                 (cons :tag "DANGEROUS"   (string "DANGER->") (string "<-DANGER"))
                 (cons :tag "Silence!!"   (string "") (string ""))))

(defcustom ahs-case-fold-search t
  "*Non-nil means case fold search."
  :group 'auto-highlight-symbol
  :type 'boolean)

(defcustom ahs-default-range 'ahs-range-display
  "Default range plugin"
  :group 'auto-highlight-symbol
  :type '(choice (symbol :tag "Display area" ahs-range-display)
                 (symbol :tag "Whole buffer" ahs-range-whole-buffer)))

(defvar ahs-search-whole-buffer nil
  "Obsolete. Do not use.")

(defcustom ahs-select-invisible 'immediate
  "Behavior when selected symbol in hidden text.

When the value is
  `open'      Open hidden text permanently.
  `temporary' Open hidden text. When unhighlight, close opened text except selected.
  `immediate' Open hidden text. When leaving opened text, close immediately.
  `skip'      Skip symbol.

Affects only overlay\(hidden text) has a property `isearch-open-invisible'."

  :group 'auto-highlight-symbol
  :type '(choice (symbol :tag "Open hidden text permanently."         open)
                 (symbol :tag "Open hidden text temporary."           temporary)
                 (symbol :tag "Open hidden text only when necessary." immediate)
                 (symbol :tag "Skip all symbol in hidden text."       skip)))

(defcustom auto-highlight-symbol-mode-hook nil
  "Hook for `auto-highlight-symbol-mode'."
  :group 'auto-highlight-symbol
  :type 'hook)

(defcustom ahs-edit-mode-on-hook nil
  "Normal hook for run when entering edit mode."
  :group 'auto-highlight-symbol
  :type 'hook)

(defcustom ahs-edit-mode-off-hook nil
  "Normal hook for run when go out edit mode."
  :group 'auto-highlight-symbol
  :type 'hook)

(defvar ahs-idle-timer nil
  "Timer used to highlighting symbol whenever emacs is idle.")

(defcustom ahs-idle-interval 1.0
  "Number of seconds to wait before highlighting symbol."
  :group 'auto-highlight-symbol
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp ahs-idle-timer)
           (cancel-timer ahs-idle-timer)
           (setq ahs-idle-timer nil)
           (ahs-start-timer))))

;;
;; (@* "Face" )
;;
(defcustom ahs-inhibit-face-list
  '(
    font-lock-comment-delimiter-face
    font-lock-comment-face
    font-lock-doc-face
    font-lock-doc-string-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    yas/field-highlight-face
    yas/mirror-highlight-face
    )
  "Face list of inhibit highlighting"
  :group 'auto-highlight-symbol
  :type '(repeat symbol))

(defcustom ahs-invisible-face-list
  '(
    bm-face
    bm-persistent-face
    flymake-errline
    flymake-warnline
    )
  "Face list of not highlighting. make overlay only."
  :group 'auto-highlight-symbol
  :type  '(repeat symbol))

(defcustom ahs-definition-face-list
  '(
    font-lock-function-name-face
    font-lock-variable-name-face
    semantic-highlight-func-current-tag-face
    )
  "Face list of symbol definition face"
  :group 'auto-highlight-symbol
  :type  '(repeat symbol))
(defalias 'ahs-defined-face-list 'ahs-definition-face-list)

(defface ahs-face
  '((t (:foreground "GhostWhite" :background "LightYellow4")))
  "Face of highlighted symbol"
  :group 'auto-highlight-symbol)
(defvar ahs-face 'ahs-face)

(defface ahs-definition-face
  '((t (:foreground "moccasin" :background "CadetBlue" :underline t)))
  "Face of highlighted symbol definition"
  :group 'auto-highlight-symbol)
(defvar ahs-definition-face 'ahs-definition-face)
(defalias 'ahs-defined-face 'ahs-definition-face)

(defface ahs-plugin-defalt-face
  '((t (:foreground "Black" :background "Orange1")))
  "Face of default range"
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-defalt-face 'ahs-plugin-defalt-face)
(defalias 'ahs-at-point-face 'ahs-plugin-defalt-face)

(defface ahs-plugin-whole-buffer-face
  '((t (:foreground "Black" :background "GreenYellow")))
  "Face of `whole buffer' range"
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-whole-buffer-face 'ahs-plugin-whole-buffer-face)

(defface ahs-plugin-bod-face
  '((t (:foreground "Black" :background "DodgerBlue")))
  "Face of `beginning of defun' range"
  :group 'auto-highlight-symbol)
(defvar ahs-plugin-bod-face 'ahs-plugin-bod-face)

(defface ahs-edit-mode-face
  '((t (:foreground "White" :background "Coral3")))
  "Face of edit mode"
  :group 'auto-highlight-symbol)
(defvar ahs-edit-mode-face 'ahs-edit-mode-face)

;;
;; (@* "Regular expression" )
;;
(defconst ahs-default-symbol-regexp "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+$"
  "Default symbol regular expression")

(defcustom ahs-include ahs-default-symbol-regexp
  "Include symbol regular expression pattern.

has 3 different ways.
  1. `\\(include\\)' regular expression string
  2. `my-include-function' function predicate
  3. `alist'
        '(
          ( emacs-lisp-mode . \"include\" ) ; regular expression in emacs-lisp-mode
          ( php-mode        . my-include-function ) ; function predicate in php-mode
         )
   if major mode not in list , use ahs-default-symbol-regexp"

  :group 'auto-highlight-symbol
  :type '(choice (regexp   :tag "Regexp" ahs-default-symbol-regexp)
                 (function :tag "Function" (lambda(symbol) t))
                 (alist    :tag "alist")))

(defcustom ahs-exclude nil
  "Exclude symbol regular expression pattern.

has 3 different ways.
  1. `\\(exclude\\)' regular expression string
  2. `my-exclude-function' function predicate
  3. `alist'
        '(
          ( ruby-mode  . \"end\\|def\\|class\" ) ; regular expression in ruby-mode
          ( dos-mode   . i-hate-wxxxxxs ) ; function predicate in dos-mode
         )
   if major mode not in list , no symbols exclude."

  :group 'auto-highlight-symbol
  :type '(choice (regexp   :tag "Regexp" "exclude regexp")
                 (function :tag "Function" (lambda(symbol) nil))
                 (alist    :tag "alist")))

;;
;; (@* "Mode map" )
;;
(defvar auto-highlight-symbol-mode-map nil
  "Keymap used in `auto-highlight-symbol-mode'.")

(if auto-highlight-symbol-mode-map
    nil
  (setq auto-highlight-symbol-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-<left>"    ) 'ahs-backward            )
          (define-key map (kbd "M-<right>"   ) 'ahs-forward             )
          (define-key map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
          (define-key map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
          (define-key map (kbd "M--"         ) 'ahs-back-to-start       )
          (define-key map (kbd "C-x C-'"     ) 'ahs-change-range        )
          (define-key map (kbd "C-x C-a"     ) 'ahs-edit-mode           )
          map)))

;;
;; (@* "Internal variable" )
;;
(defvar auto-highlight-symbol-mode nil
  "Dummy for suppress bytecompiler warning.")

(defconst ahs-modification-hook-list '( ahs-modification-hook-function ))

(defvar ahs-range-plugin-list nil
  "List of installed range plugin.")

;; buffer local variable
(defvar ahs-current-overlay     nil)
(defvar ahs-current-range       nil)
(defvar ahs-edit-mode-enable    nil)
(defvar ahs-highlighted         nil)
(defvar ahs-mode-line           nil)
(defvar ahs-opened-overlay-list nil)
(defvar ahs-overlay-list        nil)
(defvar ahs-start-point         nil)

(make-variable-buffer-local 'ahs-current-overlay     )
(make-variable-buffer-local 'ahs-current-range       )
(make-variable-buffer-local 'ahs-edit-mode-enable    )
(make-variable-buffer-local 'ahs-highlighted         )
(make-variable-buffer-local 'ahs-mode-line           )
(make-variable-buffer-local 'ahs-overlay-list        )
(make-variable-buffer-local 'ahs-opened-overlay-list )
(make-variable-buffer-local 'ahs-start-point         )

;;
;; (@* "Range plugin" )
;;
(defmacro ahs-regist-range-plugin (plugin-name body &optional docstring)
  "Macro of regist range plugin."
  `(progn
     (defvar ,(intern (format "ahs-range-%s" plugin-name))
       nil ,docstring)
     (setq ,(intern (format "ahs-range-%s" plugin-name)) ,body)
     (add-to-list 'ahs-range-plugin-list ',(intern (format "ahs-range-%s" plugin-name)))
     (defun ,(intern (format "ahs-chrange-%s" plugin-name)) ()
       (interactive)
       (ahs-change-range ',(intern (format "ahs-range-%s" plugin-name))))))

(defun ahs-plugin-error-message (err prop plugin)
  "Display plugin error message."
  (message " ")
  (message "---- auto-highlight-symbol-mode plugin error log ----")
  (message "%s in `%s' plugin `%s' property"
           err (ahs-get-plugin-prop 'name plugin) prop) ;; infinite loop? if 'name is badly function
  (message " ")
  (ahs-change-range ahs-default-range t)
  (message "Plugin error occurred. see *Messages*. current range has been changed to `%s'."
           (ahs-current-plugin-prop 'name)))

(defun ahs-get-plugin-prop (prop plugin &optional arg)
  "Get property value from plugin."
  (let ((value (cdr (assoc prop plugin))))
    (cond
     ((equal value 'abort) 'abort)          ;; abort
     ((equal prop 'face)                    ;; face
      (if (not (facep value))
          ahs-plugin-defalt-face
        value))

     ((and (functionp value)                ;; major-mode
           (equal prop 'major-mode)) value)
     ((functionp value)                     ;; function
      (condition-case err
          (if arg
              (funcall value arg)
            (funcall value))
        (error err
               (ahs-plugin-error-message err prop plugin))))

     ((null value) 'none)                   ;; property not found

     ((symbolp value)                       ;; symbol
      (ignore-errors
        (symbol-value value)))
     (t value))))                           ;; others

(defun ahs-current-plugin-prop (prop &optional arg)
  "Get property value from current range plugin."
  (ahs-get-plugin-prop prop ahs-current-range arg))

(defun ahs-change-range-internal (plugin)
  "Internal function of `ahs-change-range'."
  (setq ahs-current-range (symbol-value plugin))
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))) ;; ...orz   any better way?
  (ahs-current-plugin-prop 'init))

;;
;; (@* "Built-in plugin" )
;;
(ahs-regist-range-plugin
 display
 '((name    . "display area")
   (lighter . "HS")
   (face    . ahs-plugin-defalt-face)
   (start   . window-start)
   (end     . window-end))
  "Display area")

(ahs-regist-range-plugin
 whole-buffer
 '((name    . "whole buffer")
   (lighter . "HSA")
   (face    . ahs-plugin-whole-buffer-face)
   (start   . point-min)
   (end     . point-max))
 "Whole buffer")

;; beginning-of-defun
(defvar ahs-plugin-bod-start nil)
(defvar ahs-plugin-bod-end nil)
(defvar ahs-plugin-bod-modes
  '(emacs-lisp-mode lisp-interaction-mode c++-mode c-mode))

(ahs-regist-range-plugin
 beginning-of-defun
 '((name          . "beginning of defun")
   (lighter       . "HSD")
   (face          . ahs-plugin-bod-face)
   (major-mode    . ahs-plugin-bod-modes)
   (before-search . (lambda(x)
                      (save-excursion
                        (let ((opoint (point)))
                          (beginning-of-defun)
                          (setq ahs-plugin-bod-start (point))
                          (end-of-defun)
                          (setq ahs-plugin-bod-end (point))
                          (when (> opoint ahs-plugin-bod-end)
                            (setq ahs-plugin-bod-start ahs-plugin-bod-end)
                            (beginning-of-defun -1)
                            (setq ahs-plugin-bod-end (point)))))))
   (start         . ahs-plugin-bod-start)
   (end           . ahs-plugin-bod-end))
 "beginning-of-defun to end-of-defun like C-x n d \(narrow-to-defun)")

;;
;; (@* "Timer" )
;;
(defun ahs-start-timer ()
  "Start idle timer."
  (unless ahs-idle-timer
    (setq ahs-idle-timer (run-with-idle-timer ahs-idle-interval t 'ahs-idle-function))))

(defun ahs-restart-timer ()
  "Restart idle timer."
  (when (timerp ahs-idle-timer)
    (cancel-timer ahs-idle-timer)
    (setq ahs-idle-timer nil)
    (ahs-start-timer)))

;;
;; (@* "Idle" )
;;
(defun ahs-idle-function ()
  "Idle function"
  (when (and auto-highlight-symbol-mode
             (not ahs-highlighted))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (car bounds))
           (end (cdr bounds))
           (symbol (when bounds
                     (buffer-substring-no-properties beg end))))
      (when (and bounds
                 (not (ahs-dropdown-list-p))
                 (not (ahs-inhibit-face-p (get-char-property (point) 'face)))
                 (not (ahs-symbol-p ahs-exclude symbol t))
                 (ahs-symbol-p ahs-include symbol))
        (ahs-highlight symbol beg end)))))

(defun ahs-symbol-p (pred symbol &optional nodefs)
  "*Non-nil means include/exclude symbol."
  (cond ((null pred) ;; default include/no exclude
         (unless nodefs
           (let ((case-fold-search ahs-case-fold-search))
             (string-match ahs-default-symbol-regexp symbol))))

        ((stringp pred) ;; standard-regexp
         (let ((case-fold-search ahs-case-fold-search))
           (string-match pred symbol)))

        ((listp pred) ;; major mode decision
         (let ((pred (cdr (assoc major-mode pred))))
           (ahs-symbol-p pred symbol nodefs)))

        ((functionp pred) ;; function predicate
         (funcall pred symbol))))

(defun ahs-dropdown-list-p ()
  "Disable highlighting when expand `dropdown-list'."
  (and (featurep 'dropdown-list)
       dropdown-list-overlays))

(defun ahs-inhibit-face-p (face)
  "Disable highlighting when face in `ahs-inhibit-face-list'."
  (if (listp face)
      (loop for x in face
            when (memq x ahs-inhibit-face-list)
            return x)
    (memq face ahs-inhibit-face-list)))

;;
;; (@* "Highlight" )
;;
(defun ahs-highlight (symbol beg end)
  "Highlight Core"
  (if (equal 'abort (ahs-current-plugin-prop 'before-search symbol))
      nil
    (save-excursion
      (let ((case-fold-search ahs-case-fold-search)
            (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
            (range-start (ahs-current-plugin-prop 'start))
            (range-end (ahs-current-plugin-prop 'end)))
        (goto-char range-start)
        (while (re-search-forward regexp range-end t)
          (let* ((start (match-beginning 1))
                 (face (get-char-property start 'face))
                 (overlay))
            (unless (ahs-inhibit-face-p face)
              (setq overlay (make-overlay start (match-end 1) nil nil t))
              (overlay-put overlay 'ahs-symbol t)
              (unless (memq face ahs-invisible-face-list)
                (overlay-put overlay 'face
                             (if (memq face ahs-definition-face-list)
                                 ahs-definition-face
                               ahs-face)))
              (push overlay ahs-overlay-list))))))
    (when ahs-overlay-list
      (ahs-highlight-current-symbol beg end)
      (setq ahs-start-point beg)
      (setq ahs-highlighted t)
      (add-hook 'pre-command-hook 'ahs-unhighlight nil t)) t))

(defun ahs-unhighlight ()
  "Unhighlight"
  (unless (memq this-command
                '(
                  universal-argument
                  universal-argument-other-key
                  ahs-edit-mode
                  ahs-forward
                  ahs-backward
                  ahs-forward-definition
                  ahs-backward-definition
                  ahs-back-to-start
                  ))
    (ahs-remove-all-overlay)
    (remove-hook 'pre-command-hook 'ahs-unhighlight t)))

(defun ahs-highlight-current-symbol (beg end)
  "Highlight current symbol."
  (let* ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'ahs-symbol t)
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'face (ahs-current-plugin-prop 'face))
    (mapc '(lambda(x)
             (overlay-put overlay x ahs-modification-hook-list))
          '(modification-hooks insert-in-front-hooks insert-behind-hooks))
    (setq ahs-current-overlay overlay)))

(defun ahs-remove-all-overlay ()
  "Remove all overlay."
  (delete-overlay ahs-current-overlay)
  (mapc 'delete-overlay ahs-overlay-list)
  (mapc 'ahs-open-necessary-overlay ahs-opened-overlay-list)
  (setq ahs-highlighted         nil
        ahs-current-overlay     nil
        ahs-start-point         nil
        ahs-overlay-list        nil
        ahs-opened-overlay-list nil))

;;
;; (@* "Edit mode" )
;;
(defun ahs-modification-hook-function (overlay after debut fin &optional length)
  "Overlay's `modification-hook' used in edit mode."
  (when (and after
             ahs-edit-mode-enable)
    (let ((source (if (overlayp overlay)
                      (buffer-substring-no-properties
                       (overlay-start overlay)
                       (overlay-end overlay))
                    "")))
      (dolist (change ahs-overlay-list)
        (when (overlayp change)
          (let* ((beg (overlay-start change))
                 (end (overlay-end change))
                 (len (- end beg))
                 (target (buffer-substring-no-properties beg end)))
            (unless (equal source target)
              (save-excursion
                (goto-char beg)
                (insert source)
                (delete-char len)))))))))

(defun ahs-edit-post-command-hook-function ()
  "`post-command-hook' used in edit mode."
  (unless (and (overlayp ahs-current-overlay)
               (ahs-inside-overlay-p ahs-current-overlay))
    (ahs-edit-mode nil)))

(defun ahs-edit-mode-on (temporary)
  "Turn `ON' edit mode."
  (when temporary
    (let ((ahs-current-range ahs-range-whole-buffer)
          (ahs-start-point)
          (ahs-opened-overlay-list))
      (ahs-remove-all-overlay)
      (ahs-idle-function)))
  (setq ahs-edit-mode-enable t)
  (overlay-put ahs-current-overlay 'face ahs-edit-mode-face)
  (remove-hook 'pre-command-hook 'ahs-unhighlight t)
  (add-hook 'post-command-hook 'ahs-edit-post-command-hook-function nil t)
  (run-hooks 'ahs-edit-mode-on-hook))

(defun ahs-edit-mode-off (force)
  "Turn `OFF' edit mode."
  (setq ahs-edit-mode-enable nil)
  (mapc '(lambda (overlay)
           (let ((beg (overlay-start overlay))
                 (end (overlay-end overlay)))
             (when (or (< end (window-start))
                       (> beg (window-end)))
               (save-excursion
                 (font-lock-fontify-region beg end nil))))) ahs-overlay-list)
  (if (and (not force)
           (ahs-inside-overlay-p ahs-current-overlay))
      (progn
        (overlay-put ahs-current-overlay 'face (ahs-current-plugin-prop 'face))
        (add-hook 'pre-command-hook 'ahs-unhighlight nil t))
    (ahs-remove-all-overlay))
  (remove-hook 'post-command-hook 'ahs-edit-post-command-hook-function t)
  (run-hooks 'ahs-edit-mode-off-hook))

;;
;; (@* "Select" )
;;
(defun ahs-select (pred &optional reverse onlydef)
  "Select highlighted symbol."
  (when ahs-highlighted
    (let* ((start)
           (next (loop for overlay in (if reverse
                                          (reverse ahs-overlay-list)
                                        ahs-overlay-list)
                       for skip = (loop for hidden in (overlays-at (overlay-start overlay))
                                        when (and (overlay-get hidden 'invisible)
                                                  (or (equal ahs-select-invisible 'skip)
                                                      (not (overlay-get hidden 'isearch-open-invisible))))
                                        return hidden)
                       for selectable = (and (not skip)
                                             (or (not onlydef)
                                                 (ahs-definition-p overlay)))
                       when selectable append (list overlay) into start
                       when (and selectable (funcall pred overlay))
                       return overlay
                       finally return (if (null (car start))
                                          ahs-current-overlay
                                        (car start))))
           (beg (overlay-start next))
           (end (overlay-end next))
           (hidden-list (unless (equal ahs-select-invisible 'skip)
                          (ahs-get-openable-overlay next))))
      (when hidden-list
        (dolist (overlay hidden-list)
          (ahs-open-invisible-overlay-temporary overlay)))
      (goto-char (+ beg (- (point) (overlay-start ahs-current-overlay))))
      (move-overlay ahs-current-overlay beg end))
    (when (equal ahs-select-invisible 'immediate)
      (ahs-close-unnecessary-overlays))))

(defun ahs-get-openable-overlay (pos)
  "Get openable overlays list."
  (loop for overlay in (overlays-at (overlay-start pos))
        when (overlay-get overlay 'invisible)
        when (overlay-get overlay 'isearch-open-invisible)
        collect overlay))

;; modified from isearch.el
(defun ahs-close-unnecessary-overlays ()
  "Close unnecessary overlay immediately."
  (let ((overlays ahs-opened-overlay-list)
        (newlist))
    (dolist (overlay overlays)
      (if (ahs-inside-overlay-p overlay)
          (push overlay newlist)
        (let ((func-temp (overlay-get overlay 'isearch-open-invisible-temporary)))
          (if func-temp
              (funcall func-temp overlay t)
            (ahs-store-property overlay 'isearch-invisible  'invisible)
            (ahs-store-property overlay 'isearch-intangible 'intangible)))))
    (setq ahs-opened-overlay-list newlist)))

;; modified from isearch.el
(defun ahs-open-necessary-overlay (overlay)
  "Open invisible overlay permanently."
  (when (overlayp overlay)
    (let ((inside-overlay (ahs-inside-overlay-p overlay))
          (func-temp (overlay-get overlay 'isearch-open-invisible-temporary))
          (func (overlay-get overlay 'isearch-open-invisible)))
      (when (or inside-overlay (not func-temp))
        (ahs-store-property overlay 'isearch-invisible  'invisible)
        (ahs-store-property overlay 'isearch-intangible 'intangible))
      (if (or inside-overlay
              (equal ahs-select-invisible 'open))
          (when func (funcall func overlay))
        (when func-temp (funcall func-temp overlay t))))))

;; modified from isearch.el
(defun ahs-open-invisible-overlay-temporary (overlay)
  "Open invisible overlay temporary."
  (let ((func (overlay-get overlay 'isearch-open-invisible-temporary)))
    (if func
        (funcall func overlay nil)
      (ahs-store-property overlay 'invisible  'isearch-invisible)
      (ahs-store-property overlay 'intangible 'isearch-intangible)) ;; intangible need?
    (push overlay ahs-opened-overlay-list)))

(defun ahs-store-property (overlay from to)
  "Store overlay's property."
  (overlay-put overlay to (overlay-get overlay from))
  (overlay-put overlay from nil))

;; no doc xD
(defun ahs-forward-p        (x) (< (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-backward-p       (x) (> (overlay-start ahs-current-overlay) (overlay-start x)))
(defun ahs-definition-p     (x) (eq (overlay-get x 'face) 'ahs-definition-face))
(defun ahs-start-point-p    (x) (equal (overlay-start x) ahs-start-point))
(defun ahs-inside-overlay-p (x) (and (>= (point) (overlay-start x)) (<= (point) (overlay-end x))))

;;
;; (@* "Interactive" )
;;
(defun ahs-forward ()
  "Select highlighted symbols forwardly."
  (interactive)
  (ahs-select 'ahs-forward-p t))

(defun ahs-backward ()
  "Select highlighted symbols backwardly."
  (interactive)
  (ahs-select 'ahs-backward-p))

(defun ahs-forward-definition ()
  "Select highlighted symbols forwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-forward-p t t))
(defalias 'ahs-forward-defined 'ahs-forward-definition)

(defun ahs-backward-definition ()
  "Select highlighted symbols backwardly. only symbol definition."
  (interactive)
  (ahs-select 'ahs-backward-p nil t))
(defalias 'ahs-backward-defined 'ahs-backward-definition)

(defun ahs-back-to-start ()
  "Go back to the highlighting start point."
  (interactive)
  (ahs-select 'ahs-start-point-p))

(defun ahs-change-range (&optional range nomsg)
  "Change range according to plugin's definition."
  (interactive)
  (ahs-clear)
  (let* ((current)
         (error-msg)
         (available (loop for x in ahs-range-plugin-list
                          for plugin = (symbol-value x)
                          for mode = (ahs-get-plugin-prop 'major-mode plugin)
                          when (equal plugin ahs-current-range) do (setq current x)
                          when (or (equal 'none mode)
                                   (and (listp mode)
                                        (memq major-mode mode))
                                   (eq major-mode mode))
                          when (ahs-get-plugin-prop 'condition plugin)
                          collect x))
         (next (cadr (memq current available))))
    (if range
        (if (memq range available)
            (ahs-change-range-internal range)
          (setq error-msg (format "`%s' incorrect major-mode or condition property is nil. nothing to change."
                              (ahs-get-plugin-prop 'name (symbol-value range)))))
      (ahs-change-range-internal (if next next (car available))))
    (unless nomsg
      (if error-msg
          (message error-msg)
        (message "changed to `%s'." (ahs-current-plugin-prop 'name))))
    (when (ahs-called-interactively-p 'any)
      (ahs-idle-function))
    (ahs-set-lighter)))

(defun ahs-set-idle-interval (secs)
  "Set wait until highlighting symbol when emacs is idle."
  (interactive "nSeconds to idle, before highlighting symbol: ")
  (setq ahs-idle-interval secs)
  (ahs-restart-timer))

(defun ahs-highlight-now ()
  "Highlight NOW!!"
  (interactive)
  (ahs-idle-function))

(defun ahs-goto-web ()
  "Go to official? web site."
  (interactive)
  (browse-url ahs-web))

(defun ahs-toggle-search-whole-buffer (&optional force nomsg)
  "Obsolete. please use `ahs-change-range' instead."
  (interactive)
  (ahs-change-range (cond ((or force) 'ahs-range-whole-buffer)
                          ((equal ahs-current-range ahs-range-whole-buffer) 'ahs-range-display)
                          (t 'ahs-range-whole-buffer))
                    nomsg))
(defalias 'toggle-ahs-search-whole-buffer 'ahs-toggle-search-whole-buffer)

;;
;; (@* "Define mode" )
;;
(defun ahs-set-lighter ()
  "Set mode line lighter."
  (setq ahs-mode-line
        (concat " "
                (when ahs-edit-mode-enable (car ahs-edit-mode-lighter-pair))
                (ahs-current-plugin-prop 'lighter)
                (when ahs-edit-mode-enable (cdr ahs-edit-mode-lighter-pair))))
  (force-mode-line-update))

(defun ahs-init ()
  "Initialize"
  (unless ahs-current-range
    (ahs-change-range-internal ahs-default-range))
  (ahs-set-lighter)
  (ahs-start-timer))

(defun ahs-clear ()
  "Clear all highlighted overlay and exit edit mode."
  (if ahs-edit-mode-enable
      (ahs-edit-mode nil nil t)
    (when ahs-highlighted
      (ahs-unhighlight))))

(defun ahs-mode-maybe ()
  "What buffer `auto-highlight-symbol-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ahs-modes))
      (auto-highlight-symbol-mode t)))

(defun ahs-edit-mode (arg &optional temporary force)
  "Turn on edit mode. if call with prefix args , change range to `whole buffer' temporary."
  (interactive
   (if ahs-edit-mode-enable
       (list nil)
     (list t current-prefix-arg)))
  (when (and (or auto-highlight-symbol-mode force)
             (not buffer-read-only)
             ahs-highlighted)
    (if arg
        (ahs-edit-mode-on temporary)
      (ahs-edit-mode-off force))
    (ahs-set-lighter)))

;;;###autoload
(define-global-minor-mode global-auto-highlight-symbol-mode
  auto-highlight-symbol-mode ahs-mode-maybe
  :group 'auto-highlight-symbol)

;;;###autoload
(define-minor-mode auto-highlight-symbol-mode
  "Automatic highlighting current symbol minor mode"
  :group 'auto-highlight-symbol
  :lighter ahs-mode-line
  (if auto-highlight-symbol-mode
      (ahs-init)
    (ahs-clear)))

;;
;; (@* "Revert" )
;;
;; Remove overlay and exit edit mode before revert-buffer
(add-hook 'before-revert-hook 'ahs-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-highlight-symbol)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:

;;
;; $Id: auto-highlight-symbol.el,v 81:389de071b80e 2010-11-06 20:03 +0900 arch320 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-highlight-symbol.el ends here
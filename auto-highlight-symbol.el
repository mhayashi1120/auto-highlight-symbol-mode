;;; auto-highlight-symbol.el --- automatic highlighting symbol minor mode

;; Copyright (C) 2009 2010 Mitsuo Saito
;; Created date 2009-03-03 21:44 +0900

;; Author: Mitsuo Saito <arch320@NOSPAM.gmail.com>
;; Version: 1.02
;; Keywords: face match
;; URL: http://github.com/mitsuo-saito/auto-highlight-symbol-mode/raw/master/auto-highlight-symbol.el
;; Compatibility: GNU Emacs 23.x 24.x later
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

;;; (@* "Index")
;;
;; if you have `linkd.el' turn on `linkd-mode'
;; and (setq linkd-use-icons t ) more easily navigation.
;;
;; (@> "Setup")              basic setup
;; (@> "ScreenCast")         petit screencast
;; (@> "Setting")            some setting example
;;
;; (@> "custom variable")    customizable varible
;; (@> "face")               use face in auto-highlight-symbol-mode
;; (@> "regular expression") symbol include & exclude regular expression
;; (@> "mode map")           key binding
;; (@> "internal variable")  internal variables
;; (@> "timer")              timer function
;; (@> "idle")               idle function
;; (@> "highlight")          highlight function
;; (@> "select")             selective function
;; (@> "edit mode")          edit mode futction
;; (@> "interactive")        interactive function
;; (@> "define mode")        register minor mode
;; (@> "protect overlay")    protect overlay for edit mode
;; (@> "revert")             protect buffer from auto-revert-mode
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
;;; (@* "ScreenCast" )
;;
;;  petit screencast available on YouTube and ScreenToaster
;;  http://www.youtube.com/watch?v=xzJ2r4-s7fo
;;  http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast
;;

;;; (@* "Setting")
;;
;;   * If you want set idle interval before highlighting
;;      (ahs-set-idle-interval 1.5 )
;;
;;      or M-x ahs-set-idle-interval <RET>
;;
;;   * Default mode's behavior(highlighting and editing)
;;     affects display area only   ;; from (window-start) to (window-end)
;;
;;     if you want affects whole-buffer , setting below
;;
;;      all-buffers
;;         (setq-default ahs-search-whole-buffer t )
;;
;;      buffer-local
;;         (add-hook 'emacs-lisp-mode-hook
;;                   (function
;;                     (lambda()
;;                       (ahs-toggle-search-whole-buffer t t))))
;;
;;         or M-x ahs-toggle-search-whole-buffer <RET>
;;
;;      momentary
;;         C-u C-x C-a   ;; call 'ahs-edit-mode with prefix-args
;;
;;      but changing symbol you can't see. so carefully.
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `ahs-forward'
;;    select highlighted symbols forward.
;;  `ahs-backward'
;;    select highlighted symbols backward.
;;  `ahs-forward-defined'
;;    select highlighted symbols forward. only symbol definition.
;;  `ahs-backward-defined'
;;    select highlighted symbols backward. only symbol definition.
;;  `ahs-set-idle-interval'
;;    set wait until highlighting symbol when emacs is idle.
;;  `ahs-toggle-search-whole-buffer'
;;    toggle auto-highlight-symbol search whole-buffer-mode
;;  `ahs-edit-mode'
;;    turn on edit mode. if call with prefix-args , enable whole-buffer-mode momentary.
;;  `auto-highlight-symbol-mode'
;;    automatic highlighting symbol minor mode
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `ahs-modes'
;;    major modes `auto-highlight-symbol-mode' can run on.
;;  `ahs-mode-lighter'
;;    auto-highlight-symbol-mode lighter
;;  `ahs-wmode-lighter'
;;    auto-highlight-symbol search whole-buffer-mode lighter
;;  `ahs-edit-mode-lighter'
;;    auto-highlight-symbol edit mode lighter
;;  `ahs-case-fold-search'
;;    *Non-nil means case-fold-search.
;;  `ahs-search-whole-buffer'
;;    *Non-nil means search whole buffer.
;;  `auto-highlight-symbol-mode-hook'
;;    hook for `auto-highlight-symbol-mode'.
;;  `ahs-edit-mode-on-hook'
;;    Normal hook for run when entering edit mode.
;;  `ahs-edit-mode-off-hook'
;;    Normal hook for run when go out edit mode.
;;  `ahs-idle-interval'
;;    Number of seconds to wait before highlighting symbol.
;;  `ahs-inhibit-face-list'
;;    face list of inhibit highlighting
;;  `ahs-invisible-face-list'
;;    face list of not highlighting. but can move and turn on edit mode.
;;  `ahs-defined-face-list'
;;    face list of symbol definition face
;;  `ahs-include'
;;    include symbol regular expression pattern.
;;  `ahs-exclude'
;;    exclude symbol regular expression pattern.

;;
;; Have fun !!
;;

;;; SCM Log
;;
;;   $Revision: 29:5f15716b0652 tip $
;;   $Commiter: Mitso Saito <arch320@NOSPAM.gmail.com> $
;;   $LastModified: Tue, 26 Oct 2010 23:40:59 +0900 $
;;
;;   $Lastlog: minor fix $
;;

;;; Changelog
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

;;; TODO
;;
;;

;;; Code:

(eval-when-compile
  ;; suppress compile error warning
  (require 'easy-mmode)
  (require 'cl )
  (unless (fboundp 'auto-complete-mode)
    (defun auto-complete-mode(arg)))
  (defvar dropdown-list-overlays nil))

(defconst ahs-mode-vers "$Id: auto-highlight-symbol.el,v 29:5f15716b0652 2010-10-26 23:40 +0900 arch320 $"
  "auto-highlight-symbol-mode version.")

;;
;; (@* "custom variable" )
;;
(defgroup auto-highlight-symbol nil
  "automatic highlighting symbol minor mode"
  :group 'convenience )

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
  "major modes `auto-highlight-symbol-mode' can run on."
  :group 'auto-highlight-symbol
  :type '(list symbol))

(defcustom ahs-mode-lighter " HS"
  "auto-highlight-symbol-mode lighter"
  :group 'auto-highlight-symbol
  :type 'string )

(defcustom ahs-wmode-lighter " HSA"
  "auto-highlight-symbol search whole-buffer-mode lighter"
  :group 'auto-highlight-symbol
  :type 'string )

(defcustom ahs-edit-mode-lighter " *HSE*"
  "auto-highlight-symbol edit mode lighter"
  :group 'auto-highlight-symbol
  :type 'string )

(defcustom ahs-case-fold-search t
  "*Non-nil means case-fold-search."
  :group 'auto-highlight-symbol
  :type 'boolean )

(defcustom ahs-search-whole-buffer nil
  "*Non-nil means search whole buffer."
  :group 'auto-highlight-symbol
  :type 'boolean )
(make-variable-buffer-local 'ahs-search-whole-buffer )

(defcustom auto-highlight-symbol-mode-hook nil
  "hook for `auto-highlight-symbol-mode'."
  :group 'auto-highlight-symbol
  :type 'hook )

(defcustom ahs-edit-mode-on-hook nil
  "Normal hook for run when entering edit mode."
  :group 'auto-highlight-symbol
  :type 'hook )

(defcustom ahs-edit-mode-off-hook nil
  "Normal hook for run when go out edit mode."
  :group 'auto-highlight-symbol
  :type 'hook )

(defvar ahs-idle-timer nil
  "Timer used to highlighting symbol whenever emacs is idle." )

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
;; (@* "face" )
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
  "face list of inhibit highlighting"
  :group 'auto-highlight-symbol
  :type '(list symbol))

(defcustom ahs-invisible-face-list
  '(
    bm-face
    bm-persistent-face
    flymake-errline
    flymake-warnline
    )
  "face list of not highlighting. but can move and turn on edit mode."
  :group 'auto-highlight-symbol
  :type  '(list symbol))

(defcustom ahs-defined-face-list
  '(
    font-lock-function-name-face
    font-lock-variable-name-face
    )
  "face list of symbol definition face"
  :group 'auto-highlight-symbol
  :type  '(list symbol))

(defface ahs-face
  '((t (:foreground "GhostWhite" :background "LightYellow4" )))
  "face of highlighted symbol"
  :group 'auto-highlight-symbol )
(defvar ahs-face 'ahs-face )

(defface ahs-at-point-face
  '((t (:foreground "Black" :background "Orange1" )))
  "face of highlighted symbol at point"
  :group 'auto-highlight-symbol )
(defvar ahs-at-point-face 'ahs-at-point-face )

(defface ahs-defined-face
  '((t (:foreground "moccasin" :background "CadetBlue" :underline t )))
  "face of highlighted symbol definition"
  :group 'auto-highlight-symbol )
(defvar ahs-defined-face 'ahs-defined-face )

;;
;; (@* "regular expression" )
;;
(defconst ahs-default-symbol-regexp "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+$"
  "default symbol regular expression")

(defcustom ahs-include ahs-default-symbol-regexp
  "include symbol regular expression pattern.

has 3 different ways.
  1. `\\(include\\)' regular expression string
  2. `my-include-function' function predicate
  3. `alist'
        '(
          ( emacs-lisp-mode . \"include\" ) ; regular expression in emacs-lisp-mode
          ( php-mode        . my-include-function ) ; function predicate in php-mode
         )
   if major-mode not in list , use ahs-default-symbol-regexp"
  :group 'auto-highlight-symbol
  :type '(choice (string :tag "Regexp" ahs-default-symbol-regexp )
                 (function :tag "Function" (lambda(symbol) t))
                 (alist :tag "alist")))

(defcustom ahs-exclude nil
  "exclude symbol regular expression pattern.

has 3 different ways.
  1. `\\(exclude\\)' regular expression string
  2. `my-exclude-function' function predicate
  3. `alist'
        '(
          ( ruby-mode  . \"end\\|def\\|class\" ) ; regular expression in ruby-mode
          ( dos-mode   . i-hate-wxxxxxs ) ; function predicate in dos-mode
         )
   if major-mode not in list , no symbols exclude."
  :group 'auto-highlight-symbol
  :type '(choice (string :tag "Regexp" "exclude regexp")
                 (function :tag "Function" (lambda(symbol) nil))
                 (alist :tag "alist")))

;;
;; (@* "mode map" )
;;
(defvar auto-highlight-symbol-mode-map nil "Keymap used in auto-highlight-symbol-mode.")

(if auto-highlight-symbol-mode-map
    nil
  (setq auto-highlight-symbol-mode-map (make-sparse-keymap))
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>"    ) 'ahs-backward         )
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>"   ) 'ahs-forward          )
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>"  ) 'ahs-backward-defined )
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>" ) 'ahs-forward-defined  )
  (define-key auto-highlight-symbol-mode-map (kbd "C-x C-a"     ) 'ahs-edit-mode        ))

;;
;; (@* "internal variable" )
;;
(defvar auto-highlight-symbol-mode nil "dummy for suppress bytecompiler warning")

(defconst ahs-modification-hook-list '( ahs-modification-hook-function ))

(defvar ahs-current-overlay  nil )
(defvar ahs-edit-mode-enable nil )
(defvar ahs-highlighted      nil )
(defvar ahs-mode-line        nil )
(defvar ahs-overlay-list     nil )
(make-variable-buffer-local 'ahs-current-overlay  )
(make-variable-buffer-local 'ahs-edit-mode-enable )
(make-variable-buffer-local 'ahs-highlighted      )
(make-variable-buffer-local 'ahs-mode-line        )
(make-variable-buffer-local 'ahs-overlay-list     )

;;
;; (@* "timer" )
;;
(defun ahs-start-timer ()
  "start idle timer"
  (unless ahs-idle-timer
    (setq ahs-idle-timer (run-with-idle-timer ahs-idle-interval t 'ahs-idle-function))))

(defun ahs-restart-timer ()
  "restart idle timer"
  (when (timerp ahs-idle-timer)
    (cancel-timer ahs-idle-timer)
    (setq ahs-idle-timer nil)
    (ahs-start-timer)))

;;
;; (@* "idle" )
;;
(defun ahs-idle-function ()
  "idle function"
  (when auto-highlight-symbol-mode
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (symbol (when bounds
                     (buffer-substring-no-properties start end))))
      (when (and bounds
                 (not ahs-highlighted)
                 (not (ahs-dropdown-list-p))
                 (not (memq (get-text-property (point) 'face) ahs-inhibit-face-list))
                 (not (ahs-symbol-p ahs-exclude symbol t))
                 (ahs-symbol-p ahs-include symbol))
        (ahs-highlight symbol start end)))))

;;
;; (@* "highlight" )
;;
(defun ahs-symbol-p (predicate symbol &optional nodefs)
  "return value *Non-nil means include/exclude symbol"
  (cond ((null predicate) ;; default include/no exclude
         (unless nodefs
           (let ((case-fold-search ahs-case-fold-search))
             (string-match ahs-default-symbol-regexp symbol))))

        ((stringp predicate) ;; standard-regexp
         (let ((case-fold-search ahs-case-fold-search))
           (string-match predicate symbol)))

        ((listp predicate) ;; major-mode decision
         (let ((predicate (cdr (assoc major-mode predicate))))
           (ahs-symbol-p predicate symbol nodefs)))

        ((functionp predicate) ;; function predicate
         (funcall predicate symbol))))

(defun ahs-dropdown-list-p ()
  "disable highlighting when expand dropdown-list"
  (and (featurep 'dropdown-list)
       dropdown-list-overlays ))

(defun ahs-highlight (symbol start end)
  "highlight"
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search))
      (if ahs-search-whole-buffer
          (goto-char (point-min))
        (goto-char (window-start)))
      (while (re-search-forward
              (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>")
              (if ahs-search-whole-buffer
                  (point-max)
                (window-end)) t)
        (let* ((beg (match-beginning 1))
               (pface (get-char-property beg 'face))
               (overlay))
          (unless (memq pface ahs-inhibit-face-list)
            (setq overlay (make-overlay beg (match-end 1) nil nil t))
            (unless (memq pface ahs-invisible-face-list)
              (overlay-put overlay 'face
                           (if (memq pface ahs-defined-face-list)
                               ahs-defined-face
                             ahs-face )))
            (push overlay ahs-overlay-list))))))
  (when ahs-overlay-list
    (ahs-highlight-current-symbol start end)
    (setq ahs-highlighted t)
    (add-hook 'pre-command-hook 'ahs-unhighlight nil t)))

(defun ahs-unhighlight ()
  "unhighlight"
  (unless (memq this-command
                '( universal-argument
                   universal-argument-other-key
                   ahs-edit-mode
                   ahs-forward
                   ahs-backward
                   ahs-forward-defined
                   ahs-backward-defined ))
    (ahs-remove-all-overlay)
    (remove-hook 'pre-command-hook 'ahs-unhighlight t)))

(defun ahs-highlight-current-symbol (beg end)
  "highlight current symbol"
  (let ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'priority 1000 )
    (overlay-put overlay 'face ahs-at-point-face )
    (overlay-put overlay 'modification-hooks    ahs-modification-hook-list  )
    (overlay-put overlay 'insert-in-front-hooks ahs-modification-hook-list  )
    (overlay-put overlay 'insert-behind-hooks   ahs-modification-hook-list  )
    (setq ahs-current-overlay overlay )))

(defun ahs-remove-all-overlay ()
  "remove all overlay"
  (dolist (overlay ahs-overlay-list)
    (delete-overlay overlay))
  (delete-overlay ahs-current-overlay)
  (setq ahs-highlighted     nil
        ahs-current-overlay nil
        ahs-overlay-list    nil ))

;;
;; (@* "select" )
;;
(defun ahs-select-member (predicate source candidate)
  "member with predicate"
  (when candidate
    (if (funcall predicate source (car candidate))
        candidate
      (ahs-select-member predicate source (cdr candidate)))))

(defun ahs-select (predicate candidate)
  "select highlighted symbols"
  (when (and ahs-highlighted
             (> (length candidate ) 0 ))
    (let* ((now (overlay-start ahs-current-overlay))
           (p (or (ahs-select-member predicate now candidate )
                  candidate))
           (beg (overlay-start (car p)))
           (end (overlay-end   (car p))))
      (goto-char (+ beg (- (point) now )))
      (delete-overlay ahs-current-overlay )
      (ahs-highlight-current-symbol beg end))))

(defun ahs-remove-if (predicate candidate)
  "remove-if :D"
  (loop for overlay in candidate
        when (not (funcall predicate overlay))
        collect overlay))

(defun ahs-forward-predicate  (x y) (< x (overlay-start y)))
(defun ahs-backward-predicate (x y) (> x (overlay-start y)))
(defun ahs-defined-predicate  (x)   (eq (overlay-get x 'face) 'ahs-face ))

;;
;; (@* "edit mode" )
;;
(defun ahs-modification-hook-function (overlay after beg end &optional length)
  "overlay modification-hook function"
  (when (and after
             ahs-edit-mode-enable )
    (let ((chw (if (overlay-start overlay)
                   (buffer-substring-no-properties (overlay-start overlay)
                                                   (overlay-end   overlay))
                 "" )))
      (dolist (ov ahs-overlay-list)
        (when (overlay-start ov)
          (let* ((os  (overlay-start ov))
                 (oe  (overlay-end ov))
                 (len (- oe os))
                 (ohw (buffer-substring-no-properties os oe)))
            (unless (equal chw ohw)
              (save-excursion
                (goto-char os)
                (insert chw)
                (delete-char len)))))))))

(defun ahs-edit-post-command-hook-function ()
  "edit mode post-command-hook function"
  (if (or (null (overlay-start ahs-current-overlay))
          (< (point) (overlay-start ahs-current-overlay))
          (> (point) (overlay-end ahs-current-overlay)))
      (ahs-edit-mode nil)))

;;
;; (@* "interactive" )
;;
(defun ahs-forward ()
  "select highlighted symbols forward."
  (interactive)
  (ahs-select 'ahs-forward-predicate (reverse ahs-overlay-list )))

(defun ahs-backward ()
  "select highlighted symbols backward."
  (interactive)
  (ahs-select 'ahs-backward-predicate ahs-overlay-list ))

(defun ahs-forward-defined ()
  "select highlighted symbols forward. only symbol definition."
  (interactive)
  (ahs-select 'ahs-forward-predicate
              (reverse (ahs-remove-if 'ahs-defined-predicate ahs-overlay-list ))))

(defun ahs-backward-defined ()
  "select highlighted symbols backward. only symbol definition."
  (interactive)
  (ahs-select 'ahs-backward-predicate
              (ahs-remove-if 'ahs-defined-predicate ahs-overlay-list )))

(defun ahs-set-idle-interval (secs)
  "set wait until highlighting symbol when emacs is idle."
  (interactive "nSeconds to idle, before highlighting symbol: ")
  (setq ahs-idle-interval secs)
  (ahs-restart-timer))

(defun ahs-toggle-search-whole-buffer (&optional force nomsg)
  "toggle auto-highlight-symbol search whole-buffer-mode

optional below

force 0   turn off search whole-buffer-mode
      t   turn on  search whole-buffer-mode
      nil toggle

nomsg t   suppress mode on-off message"
  (interactive) ;;
  (ahs-clear)
  (cond ((null force) (setq ahs-search-whole-buffer (not ahs-search-whole-buffer)))
        ((equal force 0) (setq ahs-search-whole-buffer nil))
        ((or (equal force t)
             (> force 0))
         (setq ahs-search-whole-buffer t)))
  (unless nomsg
    (message (format "auto-highlight-symbol search whole-buffer-mode %s."
                     (if ahs-search-whole-buffer
                         "on"
                       "off"))))
  (ahs-set-lighter))
(defalias 'toggle-ahs-search-whole-buffer 'ahs-toggle-search-whole-buffer )

;;
;; (@* "define mode" )
;;
(defun ahs-edit-mode (arg &optional fswb force)
  "turn on edit mode. if call with prefix-args , enable whole-buffer-mode momentary."
  (interactive
   (if ahs-edit-mode-enable
       (list nil)
     (list t current-prefix-arg)))

  (when (and (or auto-highlight-symbol-mode force)
             ahs-highlighted
             (not buffer-read-only))
    (if arg
        (progn
          (when fswb
            (let ((ahs-search-whole-buffer t))
              (ahs-remove-all-overlay)
              (ahs-idle-function)))
          (remove-hook 'pre-command-hook 'ahs-unhighlight t )
          (add-hook 'post-command-hook 'ahs-edit-post-command-hook-function nil t )
          (setq ahs-edit-mode-enable t )
          (run-hooks 'ahs-edit-mode-on-hook))
      (progn
        (remove-hook 'post-command-hook 'ahs-edit-post-command-hook-function t)
        (ahs-remove-all-overlay)
        (setq ahs-edit-mode-enable nil )
        (run-hooks 'ahs-edit-mode-off-hook)))
    (ahs-set-lighter)))

(defun ahs-set-lighter ()
  "set modeline lighter"
  (setq ahs-mode-line
        (cond ((or ahs-edit-mode-enable) ahs-edit-mode-lighter)
              ((or ahs-search-whole-buffer) ahs-wmode-lighter)
              ((not ahs-search-whole-buffer) ahs-mode-lighter)))
  (force-mode-line-update))

(defun ahs-clear ()
  "clear all highlighted overlay and exit edit mode."
  (if ahs-edit-mode-enable
      (ahs-edit-mode nil nil t)
    (when ahs-highlighted
      (ahs-unhighlight))))

(defun ahs-mode-maybe ()
  "What buffer `auto-highlight-symbol-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ahs-modes))
      (auto-highlight-symbol-mode t)))

;;;###autoload
(define-global-minor-mode global-auto-highlight-symbol-mode
  auto-highlight-symbol-mode ahs-mode-maybe
  :group 'auto-highlight-symbol )

;;;###autoload
(define-minor-mode auto-highlight-symbol-mode
  "automatic highlighting symbol minor mode"
  :group 'auto-highlight-symbol
  :lighter ahs-mode-line
  (if auto-highlight-symbol-mode
      (progn
        (ahs-set-lighter)
        (ahs-start-timer))
    (ahs-clear)))

;;
;; (@* "protect overlay" )
;;
(defvar ahs-ac-active-flag   nil )
(make-variable-buffer-local 'ahs-ac-active-flag )

(defun ahs-avoid-auto-complete-menu ()
  "avoid auto-complete-mode menu for protect overlay"
  (when (featurep 'auto-complete)
    (setq ahs-ac-active-flag
          (or ahs-ac-active-flag
              (assoc-default 'auto-complete-mode (buffer-local-variables))))
    (when ahs-ac-active-flag
      (auto-complete-mode 0))))

(defun ahs-recover-auto-complete ()
  "recover auto-complete-mode"
  (when (and (featurep 'auto-complete)
             ahs-ac-active-flag)
    (auto-complete-mode t)
    (setq ahs-ac-active-flag nil )))

(add-hook 'ahs-edit-mode-on-hook  'ahs-avoid-auto-complete-menu )
(add-hook 'ahs-edit-mode-off-hook 'ahs-recover-auto-complete )

;;
;; (@* "revert" )
;;
;; remove overlay and exit edit mode before revert-buffer
(add-hook 'before-revert-hook 'ahs-clear )

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-highlight-symbol )

;;
;; $Id: auto-highlight-symbol.el,v 29:5f15716b0652 2010-10-26 23:40 +0900 arch320 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-highlight-symbol.el ends here

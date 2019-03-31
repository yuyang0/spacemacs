;;; personal-utils.el --- some personal utils function
;; Time-stamp: <2013-12-18 16:05:20 Wednesday by Yu Yang>
;;; Author: Yu Yang <yy2012cn@NOSPAM.gmail.com>
;;; URL: https://github.com/yuyang0/
;;; Version: 0.10
;;; Package-Requires: ((package-name "version"))
;;;
;;; Commentary:
;; Usage:
;;
;;; Code:

(unless (boundp 'requires)
  (defun requires (package-lst)
    (if (listp package-lst)
        (mapc 'require package-lst)
      (require package-lst))))

(defmacro with-gensyms (syms &rest body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
(defalias 'when-not 'unless "when-not seems better than unless.")

(defun personal-completing-read (PROMPT COLLECTION &optional PREDICATE
                                        REQUIRE-MATCH INITIAL-INPUT HIST
                                        DEF INHERIT-INPUT-METHOD)
  "if ido-completing-read exists, use ido-completing-read, otherwith use
built-in completing-read."
  (if (boundp 'ido-completing-read)
      (ido-completing-read PROMPT COLLECTION PREDICATE REQUIRE-MATCH
                           INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)
    (completing-read PROMPT COLLECTION PREDICATE REQUIRE-MATCH
                     INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)))

(defun thingatpt-or-region (thing)
  "If region is active, return the string within the region.
otherwith return the `THING' at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point thing)))

(defun read-str-default-thingatpt-or-region (thing &optional prompt-str)
  "If region is active then use string of region as default string.
otherwith use the`THING' at point as default string.The function finally returns
the string read from minibuffer"
  (let ((default (thingatpt-or-region thing))
        (prompt (or prompt-str thing)))
    (read-string (format "%s(default: %s)" prompt default)
                 nil nil default)))

(defun personal-compile-current-buffer (my-command)
  "Compile current buffer with `MY-COMMAND'."
  (let* ((current-file-name (file-name-nondirectory (buffer-file-name)))
         (command (concat my-command " " current-file-name)))
    (setq command (read-from-minibuffer "Compile command: " command))
    (compile command)))

(defun org-insert-wiki-link ()
  "Convert a `WORD'(in org file) to an wikipedia link."
  (interactive)
  (let* ((default-word (thingatpt-or-region 'word))
         (word (read-str-default-thingatpt-or-region 'word))
         (wikipedia-link (format "https://en.wikipedia.org/wiki/%s"
                                 (replace-regexp-in-string " +" "_" word)))
         (default-word-bound (if mark-active
                                 (cons (region-beginning) (region-end))
                               (bounds-of-thing-at-point 'word))))
    (if (equal default-word word)
        (progn
          (delete-region (car default-word-bound)
                         (cdr default-word-bound))
          (goto-char (car default-word-bound)))
      (goto-char (cdr default-word-bound)))
    (insert (format "[[%s][%s]]" wikipedia-link word))))

(defun contain-chinese (ss)
  "Test if `SS' contains chinese characters."
  (string-match-p "\\cc" ss))
;;--------------------------------------------------------------------
;;  some command used to do search engion
;;--------------------------------------------------------------------
(defmacro mk-lookup (func-name search-url browse-func &optional prompt-str)
  `(defun ,func-name (keyword)
     (interactive
      (let ((default (thingatpt-or-region 'word))
            (prompt (or ,prompt-str "Key word")))
        (list (read-string (format "%s(default: %s) " prompt default)
                           nil nil default))))
     (funcall ,browse-func (concat ,search-url (url-hexify-string keyword)))))
(mk-lookup personal-google "https://www.google.com.hk/search?q="
           'w3m-browse-url "Lookup google")
(mk-lookup personal-github "https://github.com/search?q="
           'browse-url "Lookup github")
(mk-lookup personal-baidu "http://www.baidu.com/s?wd="
           'browse-url "Lookup Baidu")
(mk-lookup personal-wikipedia-zh "https://zh.wikipedia.org/w/index.php?search="
           'w3m-browse-url "Lookup wikipedia-ZH")
(mk-lookup personal-wikipedia-en "https://en.wikipedia.org/w/index.php?search="
           'browse-url "Lookup wikipedia-EN")
(defun personal-wikipedia ()
  "Lookup wikipedia. en or zh."
  (interactive)
  (let* ((my-word (replace-regexp-in-string " +" "_"
                                            (read-str-default-thingatpt-or-region 'symbol "Lookup wikipedia"))))
    (if (contain-chinese my-word)
        (personal-wikipedia-zh my-word)
      (personal-wikipedia-en my-word))))

(defun personal-open-file-in-external-app (filename)
    "Open the `FILENAME' in desktop.
Works in Microsoft Windows, Mac OS X, Linux."
    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" filename t t)))
     ((string-equal system-type "darwin")
      (shell-command (format "open \"%s\"" filename)))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" filename)))))

(defun personal-findr (filename base-dir)
  "Find the file named `filename' in `base-dir' recursively."
  (let ((dir-lst (cons base-dir '()))
        (ret-lst '()))
    (while dir-lst
      (let* ((a-dir (car dir-lst))
             (cur-dir-file-lst (directory-files a-dir)))
        (setq dir-lst (cdr dir-lst))
        (dolist (f cur-dir-file-lst)
          (let ((name (expand-file-name f a-dir)))
            (cond
             ((equal (file-name-nondirectory name)
                     (file-name-nondirectory filename))
              (setq ret-lst (cons name ret-lst)))
             ((and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
              (setq dir-lst (append dir-lst (cons name '())))))))))
    ret-lst))

(defun personal-open-with ()
  "Same with `find-file' and `ido-find-file'.
but open the selected file in external app."
  (interactive)
  (let ((filename (if (boundp 'ido-read-file-name)
                      (ido-read-file-name "Find file(open in external app):")
                    (read-file-name "Find file(open in external app):"))))
    (personal-open-file-in-external-app filename)))

(defun personal-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `personal-move-beginning-of-line'
;; (global-set-key [remap move-beginning-of-line]
;;                 'personal-move-beginning-of-line)
;;--------------------------------------------------------------------
;;  C-o: open a newline and indent, like the key 'o' in vi
;;--------------------------------------------------------------------
(defun personal-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun personal-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (personal-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

;; (global-set-key (kbd "C-o") 'personal-smart-open-line)

(defun personal-file-chmod-x ()
  "Make current file executable."
  (interactive)
  (if (shell-command(concat "chmod u+x "
                            (buffer-file-name)))
      (message (concat "Wrote and made executable "
                       (buffer-file-name)))))

;;--------------------------------------------------------------------
;;    bind % to jump to the matched paren of the current paren
;;-------------------------------------------------------------------
(defun personal-goto-matched-paren (arg)
  "Go to the matching paren if on aparen; otherwise insert %."
  (interactive "p")
  (cond
   ((looking-at "\\s\(")
    (forward-list 1))
   ((looking-back "\\s\)")
    (backward-list 1))
   (t (self-insert-command (or arg 1)))))

(defun personal-sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (equal major-mode 'dired-mode)
              (and (buffer-file-name)
                   (not (file-exists-p (file-name-directory (buffer-file-name)))))
              (and (buffer-file-name)
                   (file-writable-p buffer-file-name)))
    (when (yes-or-no-p (format "open %s as root" (buffer-file-name)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

(defun personal-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun personal-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun personal-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun personal-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer)
  (message "Killing other buffers finished."))

(defun personal-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(defun personal-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME. Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun personal-visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (personal-start-or-switch-to (lambda ()
                                (ansi-term (getenv "SHELL")))
                               "*ansi-term*"))

(defun personal-visit-scratch-buffer ()
  "Create or visit a scratch buffer."
  (interactive)
  (let ((name (if (equal major-mode 'emacs-lisp-mode)
                  "*scratch*"
                (format "*%s*"
                        (replace-regexp-in-string "-mode$" "-scratch"
                                                  (symbol-name major-mode))))))
    (personal-start-or-switch-to (lambda ()
                                   (personal-create-scratch-buffer))
                                 name)))

(defun personal-create-scratch-buffer ()
  "Create a new Emacs Lisp scratch buffer."
  (interactive)
  (let ((name (if (equal major-mode 'emacs-lisp-mode)
                  "*scratch*"
                (format "*%s*"
                        (replace-regexp-in-string "-mode$" "-scratch"
                                                  (symbol-name major-mode)))))
        (mode (if (equal major-mode 'emacs-lisp-mode)
                  'lisp-interaction-mode
                major-mode)))
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name name)))
    (funcall mode)))

(defun personal-view-url-raw ()
  "Open a new buffer containing the contents of URL(include http header)."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (nxml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun personal-view-url ()
  "Open a new buffer containing the contents of URL(without http header)."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; delete the http head
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (delete-region (point-min) (point)))
    (cond ((search-forward "<?xml" nil t) (nxml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun personal-enable-proxy ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "127.0.0.1:7777")
                           ("https" . "127.0.0.1:7777"))))

(defun personal-enable-settings ()
  "Enable settings according to major-mode."
  (interactive)
  (let* ((major-feature
         '((c++-mode personal-cc)
           (c-mode personal-cc)
           (textile-mode init-textile)
           (erlang-mode init-erlang)
           (ruby-mode (init-ruby-mode init-rails))))
         (feature (and (assoc major-mode major-feature)
                       (cadr (assoc major-mode major-feature)))))
    (when feature
      (requires feature))))

(defun personal-split-window-for-interp (interp-buffer-name)
  "split window for interpreter buffer."
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer interp-buffer-name)
    (other-window 1))
   ((not (find interp-buffer-name
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer interp-buffer-name)
    (other-window -1))))

;;-------------------------------------------------------------------
;; the two functions below are used to create commands that eval the last
;; expression and print the result in the next line these commands
;; are usually binded to <f7>
;;-------------------------------------------------------------------
(defun delete-previous-output ()
  "delete the output generated by previous execute of the code"
  (save-excursion
    (forward-line)
    (move-beginning-of-line 1)
    (let ((begin (point))
          (end (point))
          (first-line-regex (format "^[ \t]*?%s+ ?=>.*$" (s-trim comment-start)))
          (comment-line-regex (format "^[ \t]*?%s+.*$" (s-trim comment-start)))
          (blank-line-regex "^[ \t]*$")
          (cn 1))
      (when (looking-at-p first-line-regex)
        (while cn
          (if (or (looking-at-p comment-line-regex) ;comment
                  (looking-at-p blank-line-regex))     ;blank line
              (progn
                (forward-line)
                (when (= (point-max) (point))  ;end of buffer
                  (setq cn nil))
                (move-beginning-of-line 1)
                (setq end (point)))
            (setq cn nil))))
      (when (< begin end)
        (delete-region begin end)))))

(defun eval-last-exp-comment-result (process send-exp-command &optional indent)
  "Print the result of last expression at next line and then comment the result str."
  (interactive)
  (end-of-line)
  (funcall send-exp-command)

  (unless (accept-process-output process 4 0 1)
    (error "timeout and no output"))
  (delete-previous-output)
  (save-excursion
    (newline)
    (let (output)
     (with-current-buffer (process-buffer process)
       (goto-char (point-max))
       ;;(search-backward ">")
       (search-backward-regexp comint-prompt-regexp)
       (let (begin end)
         (setq end (- (point) 1))
         (search-backward-regexp comint-prompt-regexp)
         (search-forward-regexp comint-prompt-regexp)
         (setq begin (point))

         (setq output (s-trim (buffer-substring-no-properties begin end)))))
     (let ((output-begin (point)))
      (if (string-match-p "\n" output)
          (insert (format "=>\n%s\n" output))
        (insert (format "=> %s\n" output)))
      (when indent
        (indent-region output-begin (point)))
      (comment-region output-begin (point))))))
;;-------------------------------------------------------------------
;;                   end
;;-------------------------------------------------------------------
(defun personal-insert-license (license-type)
  "insert License Declaration, support GPL, BSD, MIT and Apache License. "
  (interactive
   (list
    (personal-completing-read "License (default: gpl): "
                         '("gpl" "bsd" "mit" "apache")
                         nil nil nil nil "gpl")))
  (let ((gpl-str (format "Copyright (C) %s  %s

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
"
                  (format-time-string "%Y") user-full-name))
        (bsd-str (format "Copyright (C) %s  %s (BSD License)"
                  (format-time-string "%Y") user-full-name))
        (mit-str (format "Copyright (C) %s  %s (MIT License)"
                  (format-time-string "%Y") user-full-name))
        (apache-str (format "Copyright (C) %s  %s (Apache License)"
                  (format-time-string "%Y") user-full-name))
        (begin-point (point)))

    (cond
     ((equal license-type "gpl") (insert gpl-str))
     ((equal license-type "bsd") (insert bsd-str))
     ((equal license-type "mit") (insert mit-str))
     ((equal license-type "apache") (insert apache-str))
     (else (error "Unkown type License(%s)" license-type)))
    (comment-region begin-point (point))))

(provide 'personal-utils)
;;; personal-utils.el ends here

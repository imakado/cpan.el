;;; cpan.el -- 

;; Author: IMAKADO <ken.imakado -at- gmail.com>
;; Keywords: perl
;; Prefix: cpan:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Tested on Emacs 22

;;; you need these commands:
;; 'perl', 'perldoc', 'zcat'
;;
;; to check PATH,
;; M-x getenv PATH

;;; Commands:
;; M-x cpan



(require 'cl)
(require 'rx)
(require 'woman)
(require 'browse-url)
(require 'url-util)

(require 'anything)

(defvar cpan:version 0.01)

(defcustom cpan:perl-command
  (or (executable-find "perl")
      "perl")
  "doc")

(defconst cpan:package-re "[a-zA-Z_][a-zA-Z0-9_:]*")

(defcustom cpan:minicpan-directory nil
  "e.x,If you use minicpan, set this variable to  \"~/minicpan/\"")

(defcustom cpan:cpan-directory "~/.cpan"
  ".cpan directory")

(defcustom cpan:search-url
  "http://search.cpan.org/search?mode=modules&query=%s"
  "If you use minicpan webserver, you maight wanna set this to \"http://localhost:2963/search/?q=\"")

(defcustom cpan:document-buffer-name "*perldoc*"
  "doccuent output buffer name")

(defun cpan:modules-command ()
  (let ((cmd (executable-find "zcat")))
    (unless cmd (error "zcat is not found in PATH"))
    (concat cmd " " (shell-quote-argument (cpan:get-packages-details-path)))))

(defvar cpan:modules-cache nil)

(defun cpan:get-packages-details-path ()
  (cond
   ((and cpan:minicpan-directory
         (stringp cpan:minicpan-directory)
         (file-readable-p (cpan:catdir (expand-file-name cpan:minicpan-directory) "modules/02packages.details.txt.gz")))
    (expand-file-name (cpan:catdir cpan:minicpan-directory "modules/02packages.details.txt.gz")))
   ((file-readable-p (expand-file-name "~/.cpan/sources/modules/02packages.details.txt.gz"))
    (expand-file-name (cpan:catdir cpan:cpan-directory "sources/modules/02packages.details.txt.gz")))
   (t
    (error "cant find 02packages.details.txt.gz. please set `cpan:cpan-directory' or `cpan:minicpan-directory'"))))

;;;; anything-c-source-cpan
(defvar anything-c-source-cpan
  `((name . "CPAN modules")
    (action . (("Document" . cpan:open-document)
               ("CPAN Search" . cpan:search)
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (let ((los (cpan:modules)))
                (with-current-buffer (anything-candidate-buffer 'global)
                  (cpan:insert-each-line los)))))
    (candidates-in-buffer)))

(defun cpan:search (candidate)
  (and (stringp candidate)
       (browse-url (format cpan:search-url
                           (url-hexify-string candidate)))))

(defun cpan:modules ()
  (or cpan:modules-cache
      (let* ((s (shell-command-to-string (cpan:modules-command)))
             (los (cpan:get-modules-from-string s)))
        (message "done.")
        (flet ((success? (los) (> (length los) 10)))
          (when (success? los)
            (setq cpan:modules-cache los))))))

(defun cpan:clear-cache ()
  (interactive)
  (setq cpan:modules-cache nil
        cpan:installed-modules-cache nil))

(defun cpan:get-modules-from-string (str)
  (message "getting modules ...")
  (with-temp-buffer
    (insert str)
    (delete-region (point-min)
                   (progn (goto-char (point-min))
                          (re-search-forward "Database was generated" nil t)
                          (point-at-eol)))
    (cpan:collect-matches (concat "^" cpan:package-re))))

;;;;  anything-c-source-cpan-installed-modules
(defvar anything-c-source-cpan-installed-modules
  `((name . "installed CPAN modules")
    (action . (("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (let ((los (cpan:installed-modules)))
                (with-current-buffer (anything-candidate-buffer 'global)
                  (cpan:insert-each-line los)))))
    (candidates-in-buffer)))

(defvar cpan:installed-modules-cache nil)
(defun* cpan:installed-modules (&optional (filter-fn 'identity))
  (or cpan:installed-modules-cache
      (let ((s (cpan:trim (shell-command-to-string "perldoc -l perllocal"))))
        (when s
          (with-temp-buffer
            (insert-file-contents s)
            (setq cpan:installed-modules-cache
                  (funcall filter-fn
                           (cpan:collect-matches (concat "C<Module> L<\\(" cpan:package-re "\\)|.*?>") 1))))))))

;; too slow.
(defun* cpan:module-files (&optional (regexp ".*"))
  (let* ((los (remove-if-not (lambda (s) (string-match regexp s)) (cpan:installed-modules)))
         (files (mapcar (lambda (m) (cpan:trim (shell-command-to-string (format "perldoc -ml %s" m))))
                        los))
         (files (remove-if-not 'stringp files))
         (files (remove-if-not 'file-readable-p files)))
    files))

(defun* cpan:module-words-for-dabbrev (&optional (regexp "."))
  (lexical-let ((cache nil)
                (regexp regexp))
    (lambda ()
      (or cache
          (let ((files (cpan:module-files regexp)))
            (setq cache
                  (loop for file in files
                        nconc (with-temp-buffer
                                (insert-file-contents file)
                                (message "%s" file)
                                (cpan:collect-matches
                                 (rx (>= 3 (or (syntax word)
                                               (syntax symbol)))))))))))))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Document Browser
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun cpan:perldoc-cmd ()
  (or (executable-find "perldoc")
      (error "perldoc command is not installed")))

(defvar cpan:document-mode-map nil)

(unless cpan:document-mode-map
  (setq cpan:document-mode-map (make-sparse-keymap))
  (set-keymap-parent cpan:document-mode-map woman-mode-map)
  (define-key cpan:document-mode-map [remap woman-follow] 'cpan:folow))

(defun cpan:folow (module-or-topic)
  (interactive (list (Man-default-man-entry)))
  (and (not topic) (string= topic "") (error "No item under point"))
  (cond
   ((cpan:get-module-doc-nroff module-or-topic)
    (cpan:open-document module-or-topic))
   (t
    (call-interactively 'woman-follow))))

(defun cpan:document-mode ()
 (woman-mode)
 (use-local-map cpan:document-mode-map))

(defun cpan:get-module-doc-nroff (module-name)
  "return nroff format doctext or nil if not installed module"
  (let ((module-installed?
         (not (string-match (rx "No" (* space) "module" (* space) "found" (* space) "for")
                            (cpan:trim (shell-command-to-string (concat (cpan:perldoc-cmd) " -ml " module-name)))))))
    (when module-installed?
      (shell-command-to-string (concat (cpan:perldoc-cmd) " -onroff " module-name)))))

(defun cpan:open-document (module-name)
  (interactive)
  (let ((b (get-buffer-create cpan:document-buffer-name)))
    (with-current-buffer b
      (let ((module-doc-nroff (cpan:get-module-doc-nroff module-name)))
        (cond
         ((null module-doc-nroff)
          (error "module [%s] is not installed(perldoc -ml %s)" module-name module-name))
         (t
          (setq buffer-was-readonly nil)
        (erase-buffer)
        (insert module-doc-nroff)
        (woman-decode-region (point-min) (point-max))
        (cpan:document-mode)
        (goto-char (point-min))))))
    (switch-to-buffer b)))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defun cpan ()
  (interactive)
  (anything 'anything-c-source-cpan))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Utilities
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defun* cpan:collect-matches
    (re &optional (count 0) (match-string-fn 'match-string)
        (point-min (point-min)) (point-max (point-max)))
  (save-excursion
    (loop initially (goto-char point-min)
          while (re-search-forward re point-max t)
          collect (funcall match-string-fn count))))

; dotimes-with-progress-reporter
(defun* cpan:collect-matches-with-progress-reporter
    (re &optional (count 0) (match-string-fn 'match-string)
        (point-min (point-min)) (point-max (point-max)))
  (save-excursion
    (let* ((lines (count-lines (point-min) (point-max)))
           (line-count 0)
           (progress-reporter (make-progress-reporter "processing " 0 lines nil 1 2)))
      (loop initially (goto-char point-min)
            while (re-search-forward re point-max t)
            collect (prog1 (funcall match-string-fn count)
                      (progress-reporter-update progress-reporter line-count)
                      (incf line-count)) into ret
            finally return (prog1 ret (progress-reporter-done progress-reporter))))))

(defun cpan:insert-each-line (los)
  (insert (mapconcat 'identity los "\n")))

(defun cpan:trim (s)
  "strip space and newline"
  (replace-regexp-in-string
   "[ \t\n]*$" "" (replace-regexp-in-string "^[ \t\n]*" "" s)))

(defun cpan:catdir (s1 s2)
  (let ((s1 (replace-regexp-in-string (rx "/" eol) "" s1))
        (s2 (replace-regexp-in-string (rx bol "/") "" s2)))
    (concat s1 "/" s2)))


(provide 'cpan)

;;; cpan.el ends here.

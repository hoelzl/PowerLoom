;;; fi-stella.el --- Stella extensions for the Allegro-CL fi-interface

;;; COPYRIGHT (C) UNIVERSITY OF SOUTHERN CALIFORNIA, 1996-2006
;;; University of Southern California, Information Sciences Institute
;;; 4676 Admiralty Way, Marina Del Rey, California 90292

;;; Author: Hans Chalupsky <hans@isi.edu>
;;; Created: 1996
;;; Version: fi-stella.el,v 1.28 2006/05/11 07:06:44 hans Exp

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author or from the Free Software Foundation, Inc.,
;;; 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;; Provides some extensions and fixes to the Allegro-CL fi-interface
;;; that make it easier to work with translated Stella files.  The
;;; main addition is to make `C-c .' (or `M-.') work properly, since
;;; the Lisp compiler records the association between a symbol and the
;;; Lisp file it was defined in, but in most cases we really want to
;;; look at the Stella file from which the Lisp file was derived.

;;; IMPORTANT: These extensions are based on fi version 2.0.19, which
;;; means they probably won't work with older versions of the interface.

;;; Installation:

;;; Put this file somewhere into your X/Emacs `load-path' and byte-compile
;;; it with `M-x byte-compile-file'.  As an alternative to moving the file,
;;; you can simply change your `load-path' variable in your .emacs file
;;; to include the directory this file resides in (let's call it `.../emacs'),
;;; for example, add the following to your .emacs file:
;;;
;;;        (setq load-path (cons ".../emacs" load-path))
;;;
;;; Finally, add `(require 'fi-stella)' to your lisp-mode hook with
;;;
;;;        (add-hook 'fi:lisp-mode-hook
;;;                  '(lambda () (require 'fi-stella)))
;;;
;;; which will make sure this file gets loaded the first time you start Lisp.


(require 'advice)

(defvar fi-stella-lisp-directory-form
  ;; Form to evaluate to determine the directory where Lisp translations of
  ;; STELLA files are stored.  We can't evaluate this at initialization time,
  ;; since STELLA might not yet be up and running.
  ;; Not sure how to do this right, since the Lisp translations could really
  ;; go anywhere dependent on the pathname definitions for a particular system.
  ;; Maybe these should be lists, or should we store the STELLA source file in
  ;;    the Lisp translation instead?
  '(fi:eval-in-lisp
    "(CL:namestring (CL:translate-logical-pathname \"PL:native;lisp;\"))"))

(defvar fi-stella-sources-directory-form
  ;; Form to evaluate to determine the directory where STELLA sources
  ;; are stored.
  '(fi:eval-in-lisp
    "(CL:namestring (CL:translate-logical-pathname \"PL:sources;\"))"))

(defun stella-file-name (lisp-file-name)
  ;; Return the Stella source file of `lisp-file-name' if it exists.
  (let* ((lisp-name-end
          (and (stringp lisp-file-name)
               (or (string-match "\\.lisp$" lisp-file-name)
                   (string-match "\\.slisp$" lisp-file-name)
                   (string-match "\\.vslisp$" lisp-file-name))))
         (lisp-file-directory
          (file-name-directory lisp-file-name))
         (basename-sans-extension
          (file-name-sans-extension (file-name-nondirectory lisp-file-name)))
         (lisp-directory (eval fi-stella-lisp-directory-form))
         (sources-directory (eval fi-stella-sources-directory-form))
         (stella-file-name nil)
         ;; Directories to try to find the STELLA source:
         (directories
          (list lisp-file-directory
                (concat sources-directory
                        (substring lisp-file-directory
                                   (length lisp-directory))))))
    (cond
     (lisp-name-end
      (while directories
        (setq stella-file-name
              (concat (car directories) basename-sans-extension ".ste"))
        (if (file-exists-p stella-file-name)
            (setq directories nil)
          (setq stella-file-name nil)
          (setq directories (cdr directories))))
      stella-file-name))))

(defadvice fi::show-found-definition (around stella-frobber act pre com)
  ;; Offer to find a definition in the source of a translated Stella file.
  (let* ((find-file-existing-other-name t)
         (filename (fi::ensure-translated-pathname (ad-get-arg 1)))
         (name (ad-get-arg 0))
         stella-file-name position)
    (cond ((and (not (equal filename "TOP-LEVEL"))
                (setq stella-file-name (stella-file-name filename))
                (y-or-n-p (format "Lookup Stella source of '%s'? "
                                  (file-name-nondirectory filename))))
           (setq filename stella-file-name) 
           (ad-set-arg 1 filename)
           (save-excursion
             (find-file filename)
             (goto-char (point-min))
             ;; Strip any package information:
             (string-match "^\\(.*::?\\)?\\(.*\\)$" name)
             (setq name (substring name (match-beginning 2) (match-end 2)))
             (setq position
                   (or (re-search-forward
                        (format "^(def\\S +\\s +(?%s[ \t\n]"
                                (regexp-quote name))
                        nil t)
                       0))
             (cond ((> position 0)
                    (beginning-of-defun)
                    (setq position (1- (point)))))
             (ad-set-arg 2 position))))
    ad-do-it))

(defadvice fi::handle-lep-input (before stella-frobber act pre com)
  ;; Convert Emacs's `nil' and `t' into fully qualified versions to avoid
  ;;    some errors caused by the fact that their Lisp equivalents are not
  ;;    imported into the Stella package.
  (ad-set-arg 1 (subst nil 'common-lisp:nil (ad-get-arg 1)))
  (ad-set-arg 1 (subst t 'common-lisp:t (ad-get-arg 1))))


;;; Module support:

(defvar fi:default-stella-module "STELLA"
  "*The name of the STELLA module to use as the default module.
Used if there is no module specification in the mode line or the beginning
of a STELLA file.")

(defvar fi:stella-module nil
  "The name of the module of the STELLA file visited in the current buffer.")
(make-variable-buffer-local 'fi:stella-module)

(defvar fi::stella-in-module-search-boundary 10000)

(defun fi::stella-buffer-p ()
  ;; Return non-nil if the current buffer is a STELLA buffer.
  ;; Should this be a minor mode?
  (let ((file (buffer-file-name)))
    (or (and file
             (string-match "\\.ste$" file))
        (save-excursion
          (goto-char (point-min))
          (re-search-forward
           "^\\s *(in-module\\b\\s *"
           fi::stella-in-module-search-boundary t)))))

(defun fi::parse-stella-module-from-buffer ()
  ;; Try to parse an `in-module' declaration to establish the
  ;; current STELLA module.
  (save-excursion
    (goto-char (point-min))
    (let ((start
           (re-search-forward
            "^\\s *(in-module\\b\\s *" fi::stella-in-module-search-boundary t))
          (module nil))
      (cond (start
             (forward-sexp 1)
             (setq module
                   (car (read-from-string
                         (upcase (buffer-substring start (point))))))
             (if (stringp module)
                 module
               (format "%S" module)))))))

(defadvice fi:parse-mode-line-and-package
  (after fi::parse-stella-module act pre com)
  "If we are in a STELLA buffer, try to parse the module declaration."
  (cond ((fi::stella-buffer-p)
         (setq fi:stella-module
               (fi::parse-mode-line
                "module" fi:default-stella-module t
                'fi::parse-stella-module-from-buffer nil)))))

(defadvice lep::send-request-in-new-session
  (before fi::eval-in-stella-module act pre com)
  "If we are in a STELLA buffer with a known module, wrap a
`lisp-eval/macroexpand-in-module' around the form that is about
to be evaluated or macroexpanded."
  (if fi:stella-module
      (cond ((eq (ad-get-arg 0) 'lep::evaluation-request)
             (let* ((text-cell (cdr (memq ':text (ad-get-arg 2))))
                    (text (car text-cell)))
               (if text
                   (setcar text-cell
                           ;; Use `CL:progn' so we can also handle regions:
                           (format
                            "(STELLA::LISP-EVAL-EMACS-REGION-IN-MODULE %S %S)"
                            fi:stella-module text)))))
            ((eq (ad-get-arg 0) 'lep::macroexpand-session)
             (let* ((expander (car (cdr (memq ':expander (ad-get-arg 2)))))
                    (form-cell (cdr (memq ':form (ad-get-arg 2))))
                    (form (car form-cell)))
               (if form
                   (setcar form-cell
                           (format
                    "(STELLA::LISP-MACROEXPAND-EMACS-REGION-IN-MODULE %s %S %S)"
                            expander fi:stella-module form)))))
            (t nil))))

(defadvice cldebug-eval-in-lisp (before fi::eval-in-stella-module act pre com)
  "If we are in a STELLA buffer with a known module, wrap a
`lisp-eval-in-module' around the form that is about to be evaluated."
  (if (and fi:stella-module
           (null (ad-get-arg 2)))
      (ad-set-arg 3 (format "(STELLA::LISP-EVAL-FORM-IN-MODULE %S '%s)"
                            fi:stella-module (ad-get-arg 3)))))

(if (and (eq major-mode 'fi:common-lisp-mode)
         (fi::stella-buffer-p)
         (null fi:stella-module))
    ;; If this file gets loaded in a mode hook, make sure we establish a
    ;; STELLA module if the first file loaded was a STELLA file:
    (fi:parse-mode-line-and-package))


;;; Indentation settings:

(let ((tag 'fi:common-lisp-indent-hook))
  (put 'mv-bind tag '(like multiple-value-bind))
  (put 'mv-setq tag '(like multiple-value-setq))
  (put 'special tag '(like let))
  (put 'vrlet tag '(like let))
  (put 'only-if tag '(like when))
  (put 'while tag '(like when))
  (put 'verbatim tag '(like when))
  (put 'startup-time-progn tag '(like when))
  (put 'with-input-file tag '(like let))
  (put 'with-output-file tag '(like let))
  (put 'with-transient-objects tag '(like progn))
  (put 'with-permanent-objects tag '(like progn))
  (put 'within-context tag '(like let))
  (put 'within-module tag '(like let))
  (put 'within-world tag '(like let))
  (put 'xml-tag-case tag '(like case))
  (put 'concatenate tag '(like progn))
  (put 'exception-case tag '(like restart-case))
  ;; This is still experimental:
  (put 'defglobal tag
       '((0 t (fi:lisp-indent-keyword-list
	       t			; quotedp
	       t			; keyword-arg-pairs-p
	       t			; keyword-count
	       t			; special-keyword-count
	       3			; special-count
	       nil			; ignore-after-count
	       ;; keywords recognized:
	       ":documentation" ":public?"))))
  (put 'defglobal tag 3)
  (put 'defspecial tag '(like defglobal))
  ;; This redefines the CL:defconstant setting, is this a problem??
  (put 'defconstant tag '(like defglobal))
  ;; Iteration constructs:
  (put 'foreach tag
       '((0 t (fi:lisp-indent-keyword-list
	       nil			; quotedp
	       nil			; keyword-arg-pairs-p
	       t			; keyword-count
	       t			; special-keyword-count
	       1			; special-count
	       nil			; ignore-after-count
	       ;; keywords recognized:
	       "in" "on" "as" "where" "do" "collect" "into"))))
  (put 'exists tag
       '((0 t (fi:lisp-indent-keyword-list
	       nil t t t 1 nil	
	       "in" "on" "as" "where"))))
  (put 'forall tag
       '((0 t (fi:lisp-indent-keyword-list
	       nil t t t 1 nil	
	       "in" "on" "as" "where" "always"))))
  (put 'some tag '(like exists))
  (put 'setof tag '(like exists))
  (put 'set-of tag '(like exists))
  (put 'setofall tag '(like exists))
  (put 'kappa tag '(like exists))
  (put 'assert tag '(like progn))
  ;; copy indentation specs onto upcased symbols also:
  (mapatoms
   (function
    (lambda (sym)
      (let ((indent-info (or (get sym tag)
                             (get sym 'fi:lisp-indent-hook))))
        (if indent-info
            (put (intern (upcase (symbol-name sym))) tag indent-info)))))))

(provide 'fi-stella)

;;; fi-stella.el ends here

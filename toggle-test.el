;;; toggle-test.el --- Toggle between source and test files in various programming languages

;; Copyright (C) 2014 Raghunandan Rao

;; Author: Raghunandan Rao <r.raghunandan@gmail.com>
;; Keywords: tdd test toggle productivity
;; Version: 1.0.2
;; Url: https://github.com/rags/toggle-test

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
;; Toggle test provides test toggle functionality very similar to Jetbrains 
;; IDEs like IntelliJ/Rubymine/Pycharm. It presents
;; the user with choices in case there are multiple macthes (Ex: You have 
;; integration and unit test for the same source file). It creates the file 
;; (test or source), along with the entire directory hierarchy if the file does
;; not exist. 
;; It is language agnostic so you can use it on your rails, django,
;; haskell, scala, node.js or any other project.

;;; Change log:
;; - 1.0 - Initial release
;; - 1.0.1 - autoloads added
;; - 1.0.2 - buffer writers added

;;; Code:

(require 'cl)

(defgroup toggle-test nil
  "IntelliJ like facility to quickly toggle between source and its corresponding test files."
  :group 'convenience
  :prefix "tgt-"
  :link '(emacs-library-link :tag "Lisp File" "toggle-test.el"))

;; A list of projects. Each item in this list is an alist that specifies:
;; 1. The project root directory
;; 2. Source folder(s) (relative to root directory)
;; 3. Test source folder(s) (relative to root directory)
;; 4. prefix/suffix attached to source file names to name the test files
;;    Ex: src/foo.py <-> test/test_foo.py - the prefix is 'test_'
;;        src/controllers/Foo.scala <-> specs/controllers/Foo$Spec.scala 
;;        The suffix here is '$Spec'.
;;    Note: you can specify both prefix and suffix if required.
;;
;; Usage:
;; (add-to-list 'tgt-projects '((:root-dir "~/python-project") 
;;                             (:src-dirs "src") (:test-dirs "tests") 
;;                             (:test-prefixes "test-")))          
;; (add-to-list 'tgt-projects '((:root-dir "~/scala-project") 
;;                             (:src-dirs "src") (:test-dirs "specs") 
;;                             (:test-suffixes "$Spec")))

;;;###autoload
(defcustom tgt-projects '() 
  "Project entries. 
One entry per project that provides naming convention and folder structure"
  :group 'toggle-test
  :type '(repeat (alist)))

;; Indicates if the toggle file should be opened in a new window or
;; replace the buffer in current window. The default behavior is to
;; open it in new window Use (setq tgt-open-in-new-window 'nil) to
;; override default behavior

;;;###autoload
(defcustom tgt-open-in-new-window t 
  "Indicates if the files are opened in new window or current window"
  :group 'toggle-test
  :type 'boolean)

(defun tgt-proj-prop (prop proj) (cdr (assoc prop proj)))
(defun tgt-root-dir (proj) (file-truename (car (tgt-proj-prop :root-dir proj))))

(defun tgt-is-ancestor-p (dir file)
  (if
   (and dir file (> (length file) 0) (> (length dir) 0))
   (let
     ; The directory provided isn't assumed to actually be an encoded abs. directory,
     ; so force it.
     ((abs-dir-path (file-name-as-directory (expand-file-name dir)))
      (abs-file-path (expand-file-name file))
     ) ; Lexically check the parent/child relation.
     (and (>= (length abs-file-path) (length abs-dir-path))
          (string= (substring abs-file-path 0 (length abs-dir-path)) abs-dir-path))
     )
   'nil ; else dir or file arg can't be determined 
   )
  )

; TODO
; Wouldn't this be more efficient with a take-until like function?
; Or do the extra folds just get optimized out anyway? My elisp experience
; is lacking here. -jfeltz
(defun tgt-relative-file-path (file proj dir-type) 
  "Selects the first relative path that is the parent of file in a dir-type's associated list"
  (reduce ; fold f, where f :: current -> next dir -> current
   (lambda (current dir) 
     (or current
         (let*
           ((abs-path (expand-file-name dir (tgt-root-dir proj)))
            (abs-dir (file-name-as-directory abs-path))
           )
           (if (tgt-is-ancestor-p abs-dir file) (subseq file (length abs-dir)))
          )
      )
     )
   (cdr (assoc dir-type proj)) ; if head is ":expr", retrieve directory list
   :initial-value 'nil))

(defun tgt-proj-for (file)
  "Given a file, return its project." 
  (tgt-best-project
    (remove-if-not
     (lambda (proj) (tgt-is-ancestor-p (tgt-root-dir proj) file)) tgt-projects)
    ))

(defun tgt-best-project (projects)
  (if projects 
	  (reduce (lambda (res proj) 
				(if (> (tgt-root-depth proj) (tgt-root-depth res)) proj res)) projects)
	'nil))

(defun tgt-root-depth (proj)
  (length (split-string (file-name-as-directory (tgt-root-dir proj)) "/")))

(defun tgt-find-project-file-in-dirs (file proj)
  (assert (tgt-proj-prop :src-dirs proj) nil "Source directory not configured")
  (assert (tgt-proj-prop :test-dirs proj) nil "Test directory not configured")
  (let ((src-file-rel-path (tgt-relative-file-path file proj :src-dirs)))
    (if src-file-rel-path 
	(values src-file-rel-path 'nil)
      (values 'nil (tgt-relative-file-path file proj :test-dirs)))))

(defun tgt-proj-writer (proj writert path-context)
  "Return a writer tuple if the writer is defined."
  (if
    (tgt-proj-prop writert proj)
    ; My elisp-naive way of getting the function object -jfeltz
    (values (eval (car (tgt-proj-prop writert proj))) path-context)
    )
  )

(defun tgt-find-match (file) 
  (let ((proj (tgt-proj-for file))) ; retrieve project
    (cond
     (proj (multiple-value-bind 
	     (src-file-rel-path test-file-rel-path)
	     (tgt-find-project-file-in-dirs file proj)
	     (cond
	       (src-file-rel-path
           (values
             (tgt-all-toggle-paths 
				       src-file-rel-path proj :test-dirs #'tgt-possible-test-file-names
               )
             (tgt-proj-writer proj :test-writer file) 
            )
         )
	       (test-file-rel-path
           (values
             (tgt-all-toggle-paths 
               test-file-rel-path proj :src-dirs #'tgt-possible-src-file-names
              )
             (tgt-proj-writer proj :src-writer file)
            )
          )
	        (t (message
              "File '%s' in project '%s' is not part src-dirs or test-dirs"
              file (tgt-root-dir proj)
              )
             '(nil nil)
          )
       )
       )
    )
	  (t (message "File '%s' not part of any project. Have you defined a project?" file) 
       '(nil nil))
    )
  )
)

(defun tgt-best-matches (all-matches)
  (let ((exact-matches (remove-if-not #'file-exists-p all-matches)))
	(if exact-matches (values t exact-matches) (values 'nil all-matches))))

(defun tgt-all-toggle-paths (rel-path proj dir-type file-names-generator)
  (tgt-make-full-paths 
   (tgt-possible-dirs proj dir-type (or (file-name-directory rel-path) "")) 
   (funcall file-names-generator 
			(file-name-nondirectory rel-path) 
			(tgt-proj-prop :test-prefixes proj) 
			(tgt-proj-prop :test-suffixes proj))))

(defun tgt-make-full-paths (dirs filenames)
  (tgt-cross-join dirs filenames (lambda (dir file) (expand-file-name file dir))))

;rel-dir-path is com/foo/bar for src in "proj-root/src/com/foo/bar/Blah.java
(defun tgt-possible-dirs (proj dir-type rel-dir-path)
  (let ((root (tgt-root-dir proj)))
	(mapcar (lambda (dir) (expand-file-name rel-dir-path (expand-file-name dir root))) 
			(tgt-proj-prop dir-type proj))))

(defun tgt-remove-file-prefix (prefix file) 
  (if (string-match (concat "^" prefix) file) (replace-match "" t t file) 'nil))

(defun tgt-remove-file-suffix (name suffix ext)
  (if (string-match (concat suffix "$") name) 
	  (concat (replace-match "" t t name) ext) 
	'nil))

(defun tgt-remove-file-preffix-suffix (prefix name suffix ext) 
  (tgt-remove-file-prefix prefix (or (tgt-remove-file-suffix name suffix ext) "")))

(defun tgt-possible-src-file-names (file prefixes suffixes)
  (tgt-possible-file-names file prefixes suffixes 
						   #'tgt-remove-file-prefix
						   #'tgt-remove-file-suffix
						   #'tgt-remove-file-preffix-suffix))

(defun tgt-possible-test-file-names (file prefixes suffixes)
  (tgt-possible-file-names file prefixes suffixes #'concat #'concat #'concat))

(defun tgt-santize-seq (seq)
  ;(append seq 'nil) converts vectors/sequences/list to list
  (delete-dups (remove 'nil (append seq 'nil))))

(defun tgt-possible-file-names (file prefixes suffixes 
									 pref-fn suff-fn
									 pref-suff-fn)
  (or (tgt-santize-seq 
	   (let ((name (file-name-sans-extension file))
			 (ext (file-name-extension file t))
			 (ret-val '()))
		 (setq ret-val (vconcat 
						(mapcar (lambda (prefix) (funcall pref-fn prefix file)) prefixes)
						(mapcar (lambda (suffix) (funcall suff-fn name suffix ext)) suffixes)))
	 
		 (if (and prefixes suffixes) 
			 (setq ret-val (vconcat (tgt-cross-join 
									 prefixes suffixes 
									 (lambda (prefix suffix) 
									   (funcall pref-suff-fn prefix name suffix ext))) 
									ret-val)))
	 
		 ret-val)) (list file)))

(defun tgt-cross-join (list1 list2 &optional fn)
  "Join 2 lists. To make list of tuples by default,fn overrides how 2 elements join"
  (cond ((not list1) list2)
		((not list2) list1)
		(t (let ((ret-val '())) 
			 (dolist (i list1)
			  (dolist (j list2) 
				(add-to-list 'ret-val (if fn (funcall fn i j) (list i j)))))
			 ret-val))))

(defun tgt-open-file (file writer-tuple)
  "Dispatch type of find file function on file"
  (tgt-find-file
    file
    (if tgt-open-in-new-window #'find-file-other-window #'find-file)
    writer-tuple
    )
  )

(defun tgt-writable-p (file)
  "Predicate to check whether a writer should be used on the file buffer."
  (not (or (file-exists-p file) (get-file-buffer file)))
)

(defun tgt-find-file (file find-file-fn writer-tuple)
  "Force open the file by path creation if necessary, and run the
optional writer on its buffer."
  (mkdir (file-name-directory file) t) ;Ensure parent directory exists.
  (let*
    ((writable (and (tgt-writable-p file) writer-tuple))
      (bufobj (funcall find-file-fn file))
    )
    (if writable 
      (let ((f (car writer-tuple)) (other-path (car (cdr writer-tuple))))
        ; Set the buffer for the writer. This is redundant for find-file, but
        ; not others 
        (set-buffer bufobj)
        (funcall f other-path file)
      )
    )
  )
)

(defun tgt-show-matches (matches exact-match-p writer-tuple)
  (with-output-to-temp-buffer "*Toggle Test*"  
	(funcall (if tgt-open-in-new-window 
				 #'switch-to-buffer-other-window #'switch-to-buffer) "*Toggle Test*")
	(princ (if exact-match-p 
			   "Mutiple matching files were found. Choose one to open:\n"
			   "No matching file found. These are the potentials. Pick one to create:\n"))
	(dolist (file matches)
	  (princ "* ")
    ;; On first appearance this probably doesn't make much sense at all.
    ;; I'm basically abusing the button data-structure to carry the information for the
    ;; writer tuple - jfeltz.
	  (insert-button
      file    ; button label
      'action
      (lambda (btn)
        (let ((scoped-writer-tup (button-get btn 'tt-writer)))
          (tgt-find-file (button-label btn) #'find-alternate-file scoped-writer-tup)
        )
      )
      'tt-writer
      writer-tuple
    )
	  (princ "\n"))))

(defun tgt-open (files writer-tuple)
  "Offer a selection of matches, or open best (only) match."
  (if files
	  (multiple-value-bind (exact-match-p matches) (tgt-best-matches files)
		(cond
		 ((= 1 (length matches)) (tgt-open-file (car matches) writer-tuple))
		 (t (tgt-show-matches matches exact-match-p writer-tuple))))))

;;;###autoload
(defun tgt-toggle ()
  (interactive)
  (if buffer-file-truename ; file-truename expands '~/' sub-path etc..
    (let* ((m (tgt-find-match (file-truename buffer-file-truename))))
      (apply 'tgt-open m)))
)

(provide 'toggle-test)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; toggle-test.el ends here

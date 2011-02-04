;;;; slime-project.lisp

(in-package #:slime-project)

(defvar *system-definition-pathname* nil)
(defvar *package-name* nil)
(defvar *update-emacs-p* nil)
(defvar *disable-speedbar-p* nil)

(defgeneric make-project-interactive (pathname name))

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(define-condition system-already-defined-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "~a" (text condition)))))

(defun uninterned-symbolize (name)
  "Return an uninterned symbol named after NAME, which is treated as a
string designator and upcased."
  (make-symbol (string-upcase name)))

(defun write-file-header (file &key (name *package-name*) (system-definition-pathname *system-definition-pathname*) (stream *standard-output*))
  (format stream ";;; -*- lisp -*-~%" (string-upcase name))
  (format stream ";;; $Header: ~a $~%~%" 
          (enough-namestring file 
                             system-definition-pathname))
  (format stream ";;; This file is part of ~a. See LICENSE for licence terms.~%~%" (string-downcase name)))

(defun string-null-or-empty-p (string)
  (string-equal "" (string-trim " " string)))

(defun dependencies-from-string (string)
    "Returns a list of substrings of string
divided by spaces each as symbol list."
    (mapcar #'make-symbol 
            (remove-if #'(lambda (x) (string-equal "" x )) 
                       (loop for i = 0 then (1+ j)
                          as j = (position #\Space string :start i)
                          collect (string-upcase (subseq string i j))
                          while j))))
(defmacro set-if-not-null-or-empty (var value)
  (let ((value-sym (gensym)))
    `(let ((,value-sym ,value))
       (when (not (string-null-or-empty-p ,value-sym))
         (setf ,var (string-trim " " ,value-sym))))))

(defun write-system-form (name &key src-module depends-on version description license author (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize name))
    (when version
      (format stream "  :version \"~a\"~%" version))
    (when license
      (format stream "  :license \"~a\"~%" license))
    (when description
      (format stream "  :description \"~a\"~%" description))
    (when author
      (format stream "  :author \"~a\"~%" author))
    (format stream "  :serial t~%")
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize depends-on)))
    (if src-module
        (progn
          (format stream "  :components ((:module ~a~%" src-module)
          (format stream "                        :components ((:file \"package\")~%")
          (format stream "                                     (:file ~S)))))~%" (string-downcase name)))
        (progn 
          (format stream "  :components ((:file \"package\")~%")
          (format stream "               (:file ~S)))~%" (string-downcase name))))))

(defun pathname-project-name (pathname)
  "Return a project name based on PATHNAME by taking the last element
in the pathname-directory list. E.g. returns \"awesome-project\" for
#p\"src/awesome-project/\"."
  (first (last (pathname-directory pathname))))

(defmacro with-new-file ((stream file) &body body)
  "Like WITH-OPEN-FILE, but specialized for output to a file that must
not already exist."
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error)
     (let ((*print-case* :downcase))
       ,@body)))

(defun write-system-file (name file &key src-module depends-on description version author license)
  (with-new-file (stream file)
    (write-file-header file :stream stream)
    (write-system-form name
                       :depends-on depends-on
                       :src-module src-module
                       :version version
                       :description description
                       :license license
                       :author author
                       :stream stream)
    (terpri stream)))

(defun write-readme-file (name file)
  (with-new-file (stream file)
    (format stream "This is the stub ~A for the ~S project.~%"
            (file-namestring file)
            name)))

(defun write-license-file (author file)
  (with-new-file (os file)
    (format os "Copyright (~d)~@[ ~a~]~%" (nth-value 5 (get-decoded-time)) author)
    (with-open-file (is (merge-pathnames (make-pathname :directory (pathname-directory *this-file*) 
                                                        :name "LICENSE" 
                                                        :type "template")) :direction :input)
       (loop for line = (read-line is nil) 
          while line
          do (format os "~a~%" line)))))

(defun write-package-file (name file)
  (with-new-file (stream file)
    (write-file-header file :stream stream)
    (format stream "(defpackage ~S~%" (uninterned-symbolize name))
    (format stream "  (:use #:cl))~%~%")))

(defun write-application-file (name file)
  (with-new-file (stream file)
    (write-file-header file :stream stream)
    (if name
        (format stream "(in-package ~S)~%~%" (uninterned-symbolize name))
        (format stream ";; (in-package #:a-package)~%~%"))
    (format stream ";;; ~S goes here. Let's go!~%~%" name)))

(defun find-conf (name conf-pathname)
  (loop for i = nil then (or (and i (+ 1 i)) 1)
       for file = (make-pathname :directory (pathname-directory conf-pathname)
                                 :name (format nil "~@[~d-~]~a" i name)
                                 :type "conf")
       when (not (probe-file file))
       return file))

(defun update-emacs (system file)
  (let ((system-namestring (directory-namestring (asdf:system-definition-pathname system)))
        (file-namestring (and file (namestring file))))
    `(progn
       (setq default-directory ,system-namestring)
       (when ,file-namestring
           (find-file-other-window ,file-namestring)
           (setf default-directory ,(directory-namestring file)))
       (unless ,*disable-speedbar-p*
         (speedbar-update-contents)
         (speedbar-refresh)
         (speedbar-get-focus)))))

(defun make-project (pathname &key
                     interactive-p
                     depends-on
                     (version "0.1")
                     (description nil)
                     author
                     (license "Public Domain")
                     src-module
                     (name (pathname-project-name pathname)))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (let ((system (asdf:find-system (uninterned-symbolize name) nil)))
    (when system
      (error 'system-already-defined-error :text (format nil "System ~a is already defined by ~a~%" 
                                                         name 
                                                         (namestring (asdf:system-definition-pathname system))))))
  (if interactive-p
      (make-project-interactive pathname name)
      (let ((*system-definition-pathname* pathname)
            (*package-name* name))
        (labels ((relative (file)
                   (merge-pathnames file 
                                    (or (and src-module
                                             (merge-pathnames (make-pathname :directory (list :relative src-module))
                                                              pathname))
                                        pathname))))
          (ensure-directories-exist pathname)
          (write-readme-file name (merge-pathnames (make-pathname :name "README")
                                                   pathname))
          (write-license-file author (merge-pathnames (make-pathname :name "LICENSE")
                                                      pathname))
          (write-system-file name (merge-pathnames (format nil "~a.asd" name)
                                                   pathname )
                             :version version
                             :description description
                             :author author
                             :license license
                             :src-module src-module
                             :depends-on depends-on)
          (write-package-file name (ensure-directories-exist (relative "package.lisp")))
          (write-application-file name (relative (format nil "~a.lisp" name)))
          (let ((system-symbol (uninterned-symbolize name)))
            (when (not (asdf:find-system system-symbol nil))
              (let ((user-local-system (ensure-directories-exist 
                                        (merge-pathnames (make-pathname :directory
                                                                        '(:relative 
                                                                          ".config"
                                                                          "common-lisp"
                                                                          "source-registry.conf.d"))
                                                         (user-homedir-pathname)))))
                (with-new-file (s (find-conf name user-local-system))
                  (format s "(:directory \"~a\")" (directory-namestring pathname)))
                (asdf:initialize-source-registry)))
            (asdf:load-system system-symbol)
            (when *update-emacs-p*
              (swank:eval-in-emacs (update-emacs system-symbol 
                                                 (merge-pathnames (format nil "~a.asd" name)
                                                                  pathname )) t)))
          name))))

(defun update-system (system &optional interactive-p last-pathname-created default-package)
  (handler-case
      (progn 
        (asdf:load-system 'asdf:load-op system)
        (when *update-emacs-p*
          (swank:eval-in-emacs (update-emacs system last-pathname-created) t)))
    (file-error (ex)
      (let* ((pathname (file-error-pathname ex))
             (*system-definition-pathname* (asdf:system-definition-pathname system))
             (package (or default-package (pathname-project-name *system-definition-pathname*))))
        (if (equal pathname last-pathname-created)
            (signal ex)
            (progn
              (ensure-directories-exist pathname)
              (when (pathname-name pathname)
                (when interactive-p
                  (format t "Enter in-package for file ~a [~a]: " 
                          (enough-namestring pathname *system-definition-pathname*) 
                          package)
                  (set-if-not-null-or-empty package (read-line)))
                (write-application-file package pathname))
              (update-system system interactive-p pathname package)))))))

(defmethod make-project-interactive (pathname name)
  (let ((depends-on nil)
        (version "0.1")
        (author nil)
        (license "Public Domain")
        (project-description nil)
        (src-module "src"))
    (format t "Enter dependencies by spaces []: ")
    (setf depends-on (dependencies-from-string (read-line)))
    (format t "Enter version [~a]: " version)
    (set-if-not-null-or-empty version (read-line))
    (format t "Enter author []: ")
    (set-if-not-null-or-empty author (read-line))
    (format t "Enter license [~a]: " license)
    (set-if-not-null-or-empty license (read-line))
    (format t "Enter project description []: ")
    (set-if-not-null-or-empty project-description (read-line))
    (format t "Enter module for sources [~a]: " src-module)
    (set-if-not-null-or-empty src-module (read-line))
    (format t "Enter project name [~a]: " name)
    (set-if-not-null-or-empty name (read-line))
    (make-project pathname 
                  :depends-on depends-on 
                  :version version
                  :author author
                  :license license
                  :src-module src-module
                  :name name)))

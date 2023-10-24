;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               prototype.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A prototype to test the idea.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-08-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.ERGO.IMPLEMENTATION")

(defun delete-empty-directories (pathname)
  ;; TODO: (implementation specific since there's no conforming delete-directory).
  (declare (ignore pathname))
  nil)

(defvar *verbose* t)
(defun verbose-command (command)
  (when *verbose*
    (format *trace-output* "$ ~A~%" command))
  command)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun normalize-symbol (symbol-designator)
    (intern (string-upcase symbol-designator) "KEYWORD"))

  (defun normalize-designator (designator)
    (mapcar (lambda (element)
              (etypecase element
                (symbol (normalize-symbol element))
                (string element)))
            designator))

  (defun quote-elements (list)
    (mapcar (lambda (element) `(quote ,element)) list)))

;;; --------------------------------------------------------------------------
;;; collection
;;; --------------------------------------------------------------------------

(defclass collection ()
  ((name :initarg :name :accessor collection-name)))

(defmethod print-object ((collection collection) stream)
  (print-parseable-object (collection stream :type t :identity t)
                          name))


(defclass git-collection (collection)
  ((protocol  :initarg :protocol  :accessor collection-git-protocol  :initform :ssh)
   (user      :initarg :user      :accessor collection-git-user      :initform nil)
   (server    :initarg :server    :accessor collection-git-server    :initform nil)
   (root      :initarg :root      :accessor collection-git-root      :initform nil)
   (extension :initarg :extension :accessor collection-git-extension :initform nil)
   (kind      :initarg :kind      :accessor collection-kind          :initform :git)))

(defmethod print-object ((collection git-collection) stream)
  (print-parseable-object (collection stream :type t :identity t)
                          name protocol user server root extension kind))

;; TODO: subclasses for gitlab github gitea etc.
;; example of gitea repo: https://git.mfiano.net/mfiano/cricket

;; TODO: for a given collection class, there may be several url scheme to access the same repository (perhaps with different access rights).  We should manage them.
;;       eg. if we start with a http:// url, for a read-only access, and want to switch to write access with a ssh or git url, that should be possible and easy. (it's just a git remote set-url).



(defgeneric repository-fetch-url (collection relative-path))
(defgeneric repository-fetch-url-format (collection protocol))

(defmethod repository-fetch-url-format ((collection git-collection) (protocol (eql :ssh)))
  "~A@~A:~@[~A/~]~A~@[~A~]")
(defmethod repository-fetch-url-format ((collection git-collection) (protocol (eql :git)))
  "git://~@[~A@~]~A/~@[~A/~]~A~@[~A~]")
(defmethod repository-fetch-url-format ((collection git-collection) (protocol (eql :http)))
  "http://~@[~A@~]~A/~@[~A/~]~A~@[~A~]")

(defmethod repository-fetch-url ((collection git-collection) relative-path)
  (format nil (repository-fetch-url-format collection (collection-git-protocol collection))
          (collection-git-user      collection)
          (collection-git-server    collection)
          (collection-git-root      collection)
          relative-path
          (collection-git-extension collection)))


(defparameter *collections*
  (list
   ;; "https://salsa.debian.org/debian/schroot"
   (make-instance 'git-collection
                  :name "common-lisp.net"
                  :protocol :ssh
                  :user "git"
                  :server "common-lisp.net"
                  :extension ".git"
                  :kind :gitlab)
   (make-instance 'git-collection
                  :name "gitlab.com"
                  :user "git"
                  :server "gitlab.com"
                  :root nil
                  :extension ".git"
                  :kind :gitlab)
   (make-instance 'git-collection
                  :name "github.com"
                  :user "git"
                  :server "github.com"
                  :root nil
                  :extension ".git"
                  :kind :github))
  "Priority ordered list of repository collections.")


;;; --------------------------------------------------------------------------
;;; repository
;;; --------------------------------------------------------------------------

(defclass repository ()
  ((designator   :initarg :designator                 :accessor repository-designator)
   (collection   :initarg :collection                 :accessor repository-collection)
   (local-path   :initarg :local-path   :initform nil :accessor repository-local-path)))

(defmethod print-object ((repository repository) stream)
  (print-parseable-object (repository stream :type t :identity t)
                          designator collection local-path))


(defvar *repositories* (make-hash-table :test 'equal))

(defgeneric key (object))
(defun intern-repository (repository-designator &key collection local-path)
  (or (gethash repository-designator *repositories*)
      (setf (gethash (key repository-designator) *repositories*)
            (make-instance 'repository :designator repository-designator
                                       :collection collection
                                       :local-path local-path))))


(defgeneric probe-repository (designator local-path))
(defgeneric clone-repository (designator local-path))
(defgeneric pull-repository (designator local-path))


(defgeneric update (repository)
  (:documentation "clone or pull the local repository")
  (:method ((repository repository))
    (if (probe-repository (repository-designator repository)
                          (repository-local-path repository))
        (pull  repository)
        (clone repository))))

(defgeneric clone (repository)
  (:documentation "clone the local repository")
  (:method ((repository repository))
    ;; assume the local-path does not exist or is empty
    (clone-repository (repository-designator repository)
                      (repository-local-path repository))))

(defgeneric pull (repository)
  (:documentation "clone or pull the local repository")
  (:method ((repository repository))
    ;; assume the local-path is already cloned
    (pull-repository (repository-designator repository)
                     (repository-local-path repository))))

(defgeneric repository-builder (repository))
(defgeneric build-repository (repository dependency-directories))
(defgeneric build (repository builder dependency-directories))
(defgeneric repository-asdf-system (repository builder))


(defclass builder ()
  ())

(defclass asdf-builder (builder)
  ((asd-file :initarg :asd-file :reader asd-file)))

(defmethod print-object ((builder asdf-builder) stream)
  (print-parseable-object (builder stream :type t :identity t)
                          asd-file))

(defmethod repository-asdf-system ((repository repository) (builder asdf-builder))
  ;; TODO: repository-asdf-system it's not that good to use an infered system to build a repository! The user should choose the root asdf system, and asdf gives us the dependencies systesm.
  ;; Silly Q&D system name from asd file:
  (pathname-name (asd-file builder)))

(defmethod repository-builder ((repository repository))
  (let ((asd-file (first (sort (directory (merge-pathnames "*.asd" (repository-local-path repository)))
                               (function <)
                               :key (lambda (path) (length (namestring path)))))))
    (when (probe-file asd-file)
      (make-instance 'asdf-builder :asd-file asd-file))))

(defmethod build-repository ((repository repository) dependency-directories)
  (build repository (repository-builder repository) dependency-directories))

(defmethod build ((repository repository) (builder asdf-builder) dependency-directories)
  (let ((asdf:*central-registry* dependency-directories))
    (asdf:oos 'asdf:load-op (repository-asdf-system repository builder))))

;;; --------------------------------------------------------------------------
;;; repository designator
;;; --------------------------------------------------------------------------

(defclass repository-designator ()
  ((url       :initarg :url       :reader designator-url)))

(defmethod print-object ((designator repository-designator) stream)
  (print-parseable-object (designator stream :type t :identity t)
                          url))

(defclass git-repository-designator (repository-designator)
  ((kind      :initarg :kind      :reader designator-kind)
   (tip-kind  :initarg :tip-kind  :reader designator-tip-kind)
   (tip-value :initarg :tip-value :reader designator-tip-value)))

(defmethod print-object ((designator git-repository-designator) stream)
  (print-parseable-object (designator  stream :type t)
                          url kind tip-kind tip-value))


(defmethod key ((designator repository-designator))
  (designator-url designator))
(defmethod key ((designator git-repository-designator))
  (append (list 'git-repository (designator-url designator)
                :kind (designator-kind designator))
          (when (designator-tip-kind designator)
            (list (designator-tip-kind designator) (designator-tip-value designator)))))


(defun parse-git-url (url)
  (let ((host (multiple-value-bind (start0 end0 gstart gend) (cl-ppcre:scan "^[a-z0-9]+@([a-z0-9.]+):" url)
                (declare (ignore start0 end0))
                (if (= 1 (length gstart))
                    (subseq url (aref gstart 0) (aref gend 0))
                    nil))))
    (if host
        (values host :ssh)
        (multiple-value-bind (start0 end0 gstart gend) (cl-ppcre:scan "^([a-z0-9]+):/*([a-z0-9]+@)?([-a-z0-9.]+)(:[a-z0-9.]+)?/?" url )
          (declare (ignore start0 end0))
          (if (and (<= 3 (length gstart)) (aref gstart 2) (aref gend 2))
              (values (subseq url (aref gstart 2) (aref gend 2))
                      (normalize-symbol (subseq url (aref gstart 0) (aref gend 0))))
              nil)))))

(defun infer-kind-from-url (url)
  ;; | host of url     | kind   |
  ;; |-----------------+--------|
  ;; | is github.com   | github |
  ;; | contains gitlab | gitlab |
  ;; | contains gitea  | gitea  |
  ;; | else            | plain  |
  (multiple-value-bind (host scheme) (parse-git-url url)
    (declare (ignore scheme))
    (cond
      ((string-equal "github.com" host) :github)
      ((search "gitlab" host)           :gitlab)
      ((search "gitea"  host)           :gitea)
      (t                                :plain))))


(defun parse-git-repository-designator (designator)
  ;; (git <url> [:kind gitlab|github|gitea|plain]
  ;;            [ :branch  <name>
  ;;            | :tag     <name> | :latest ] |
  ;;            | :release <name> | :latest ] |
  ;;            | :commit  <hash> ]
  (multiple-value-bind (url kind branch tag release commit)
      (handler-case
          (destructuring-bind (git url &key kind branch tag release commit) designator
            (declare (ignore git))
            (values url kind branch tag release commit))
        (error (err)
          (error "Invalid git repository designator:~S~%~A" designator err)))
    (let ((kind (if kind
                    (normalize-symbol kind)
                    (infer-kind-from-url url))))
      (unless (member kind '(:gitlab :github :gitea :plain))
        (error "Invalid git repository designator: ~S~%Kind ~S should be one of ~{~S~^, ~}." designator  kind '(:gitlab :github :gitea :plain)))
      (when (< (count nil (list branch tag release commit)) 3)
        (error "Invalid git repository designator: ~S~%At most one of :branch :tag :release or :commit can be given." designator))
      (unless (or (null branch) (stringp branch))
        (error "Invalid git repository designator: ~S~%Branch ~S should be a string, not a ~S" designator branch (type-of branch)))
      (unless (or (null tag) (stringp tag) (eql tag :latest))
        (error "Invalid git repository designator: ~S~%Tag ~S should be a string or :LATEST, not a ~S" designator tag (type-of tag)))
      (unless (or (null release)
                  (and (member kind '(:gitlab :github))
                       (or (stringp release) (eql release :latest))))
        (error "Invalid git repository designator: ~S~%Release ~S should be a string or :LATEST, but only for repositories on gitlab or github (kind = ~S)"
               designator release kind))
      (make-instance 'git-repository-designator
                     :url url :kind kind
                     :tip-kind (cond
                                 (branch  :branch)
                                 (tag     :tag)
                                 (release :release)
                                 (commit  :commit))
                     :tip-value (or branch tag release commit)))))


(defgeneric compute-repository-local-path (collection relative-path))
(defmethod compute-repository-local-path ((collection git-collection) (relative-path string))
  ;; TODO: use the designator to specific tag/branch/release/commit.
  ;; TODO: get the root from configuration (perhaps xdg something, etc) default to (user-homedir-pathnamea)
  (let ((home (user-homedir-pathname)))
    (merge-pathnames (make-pathname
                      :directory (append (list :relative "ergo" "repositories"
                                               (collection-git-server collection))
                                         (split-sequence #\/ relative-path :remove-empty-subseqs t)
                                         ;; TODO: find the default branch name:
                                         (list "master"))
                      :case :local
                      :defaults home)
                     home nil)))

#-(and)
(defmethod compute-repository-local-path ((collection git-collection) (designator git-repository-desig))
  ;; TODO: use the designator to specific tag/branch/release/commit.
  ;; TODO: get the root from configuration (perhaps xdg something, etc) default to (user-homedir-pathnamea)
  (let ((home (user-homedir-pathname)))
    (merge-pathnames (make-pathname
                      :directory (append (list :relative "ergo" "repositories"
                                               (collection-git-server collection))
                                         (split-sequence #\/ relative-path :remove-empty-subseqs t)
                                         ;; TODO: find the default branch name:
                                         (list "master"))
                      :case :local
                      :defaults home)
                     home nil)))

(defun try-collections (relative-path)
  ;; search in collections for a repo with the given relative-path
  (dolist (collection *collections* (error "No such repository ~S" relative-path))
    (let ((repo-url   (repository-fetch-url  collection relative-path))
          (repo-path  (compute-repository-local-path collection relative-path)))
      (ensure-directories-exist (make-pathname :name "probe" :defaults repo-path))
      (flet ((return-designator ()
               (return-from try-collections
                 (make-instance 'git-repository-designator
                                :url repo-url
                                :kind (collection-kind collection)))))
        (if (probe-file (merge-pathnames (make-pathname :directory '(:relative ".git") :name "index" :type nil)
                                         repo-path nil))
            (return-designator)
            ;; try to clone:
            (handler-case (uiop:run-program (verbose-command (format nil "git clone ~S ~S" repo-url (namestring repo-path)))
                                            :output :interactive
                                            :error-output t
                                            :ignore-error-status nil)
              (:no-error () (return-designator))
              (uiop:subprocess-error ()
                (delete-empty-directories repo-path))))))))

(defun parse-repository-designator (designator)
  (typecase designator
    (list
     (case (normalize-symbol (first designator))
       ;; Only git repository designators are supported for now:
       ;; TODO: ask the collections whether the designator url is for them, and set the kind accordingly.
       ((:git) (parse-git-repository-designator designator))
       (otherwise (error "Unsupported (yet) repository designator: ~S" designator))))
    (symbol
     (let ((name (string designator)))
       (if (notany (function lower-case-p) name)
           (parse-repository-designator (string-downcase name))
           (parse-repository-designator name))))
    (string
     (if (cl-ppcre:scan "^[-_a-z0-9]+/[-_a-z0-9]+$" designator)
         ;; relative repository designator
         (try-collections designator)
         (parse-repository-designator (list 'git designator))))
    (t
     (error "Invalid repository designator: ~S" designator))))



;;; --------------------------------------------------------------------------
;;; basic git operations
;;; --------------------------------------------------------------------------


(defmethod probe-repository ((designator git-repository-designator) local-path)
  (declare (ignorable designator))
  (probe-file (merge-pathnames
               (make-pathname :directory '(:relative ".git") :name "index" :type nil)
               local-path nil)))

(defmethod clone-repository ((designator git-repository-designator) local-path)
  (handler-case (uiop:run-program (verbose-command (format nil "git clone ~S ~S"
                                                           (designator-url designator)
                                                           (namestring local-path)))
                                  :output :interactive
                                  :error-output t
                                  :ignore-error-status nil)
    (:no-error () local-path)
    (uiop:subprocess-error ()
      (delete-empty-directories local-path)
      nil)))

(defmethod pull-repository ((designator git-repository-designator) local-path)
  (handler-case (uiop:run-program (verbose-command (format nil "cd ~S && git pull"
                                                           (namestring local-path)))
                                  :force-shell t
                                  :output :interactive
                                  :error-output t
                                  :ignore-error-status nil)
    (:no-error () local-path)
    (uiop:subprocess-error ()
      nil)))

;;; --------------------------------------------------------------------------
;;; project
;;; --------------------------------------------------------------------------

(defclass project ()
  ((repository   :initarg :designator                 :accessor project-repository)
   (dependencies :initarg :dependencies :initform '() :accessor project-dependencies)
   (attributes   :initarg :attributes   :initform '() :accessor project-attributes)))

(defmethod print-object ((project project) stream)
  (print-parseable-object (project stream :type t :identity t)
                          repository dependencies attributes))

(defgeneric project-designator (project)
  (:method ((project project))  (project-repository project)))

(defgeneric project-collection (project)
  (:method ((project project)) (repository-collection (project-repository project))))

(defgeneric project-local-path (project)
  (:method ((project project)) (repository-local-path (project-repository project))))


(defvar *projects* (make-hash-table :test (function equal)))
(defgeneric register-project (project))
(defmethod register-project ((project project))
  ;; TODO: intern project-designators
  (setf (gethash (project-designator project) *projects*) project))



(defvar *current-project* nil)

(defmacro project (&rest attributes &key designator dependencies &allow-other-keys)
  `(setf *current-project*
         (register-project (make-instance
                            'project
                            :designator   (parse-repository-designator ',designator)
                            :dependencies (mapcar (function parse-repository-designator)
                                                  ',dependencies)
                            :attributes (list ,@(quote-elements
                                                 (loop
                                                   :for (key value) :on attributes :by (function cddr)
                                                   :if (not (member key '(:designator :dependencies)))
                                                     :collect key :and :collect value)))))))



(defgeneric resolve-project (project))

(defmethod resolve-project ((project project))
  project)

(defmethod resolve-project ((designator repository-designator))
  (error "not implemented yet")
  designator)

(defmethod resolve-project ((designator list))
  (resolve-project (parse-repository-designator designator)))

(defmethod resolve-project ((designator string))
  (resolve-project (parse-repository-designator designator)))

(defmethod resolve-project ((designator symbol))
  (resolve-project (parse-repository-designator designator)))


(defgeneric project-repositories-local-directories (project)
  (:documentation "List all local directories of the repositories of the project and dependencies."))
(defgeneric update-project (project))
(defgeneric build-project (project))


;; TODO: revise this dichotomy between (project-repository project) and  (project-dependencies project) and the recursions in dependencies.

(defmethod project-repositories-local-directories ((project project))
  (cons (project-local-path project)
        (mapcan (function project-repositories-local-directories)
                (project-dependencies project))))

(defmethod project-repositories-local-directories ((repository repository))
  (copy-list (repository-local-path repository)))


(defmethod update-project ((project project))
  ;; TODO: this is a naive dependency update; make it better.
  (dolist (dependency (cons (project-repository project) (project-dependencies project)))
    (update (intern-repository dependency))))

(defmethod build-project ((project project))
  ;; TODO: Naive dependency builds!
  (let ((dependency-directories (project-repositories-local-directories project)))
    (dolist (dependency (cons (project-repository project) (project-dependencies project)))
      (build-repository (intern-repository dependency) dependency-directories))))


;;; --------------------------------------------------------------------------
;;; user interface
;;; --------------------------------------------------------------------------

(defun select-project (project)
  (when (null project)
    (cerror "Specify a project" "There is no current project yet")
    (format *query-io* "Enter project designator: ")
    (finish-output *query-io*)
    (setf project (read *query-io*)))
  (resolve-project project))

(defun where-from (&optional (project *current-project*))
  (when (setf *current-project* (select-project project))
    (values (designator-url (project-designator *current-project*))
            (project-collection *current-project*))))

(defun where-is (&optional (project *current-project*))
  (when (setf *current-project* (select-project project))
    (project-local-path *current-project*)))

(defun load (&optional (project *current-project*))
  (when (setf *current-project* (select-project project))
    (update-project *current-project*)
    (build-project *current-project*)))

(defun ergo (&optional (project *current-project*))
  (load project))

;;;; THE END ;;;;

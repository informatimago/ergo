(in-package "COM.INFORMATIMAGO.ERGO.IMPLEMENTATION")

#-(and)
(progn
  
  (mapcar (lambda (collection)
            (repository-local-path collection "alexandria/alexandria"))
          *collections*)

  '(#P"/Users/pjb/ergo/repositories/common-lisp.net/alexandria/alexandria/master/"
    #P"/Users/pjb/ergo/repositories/gitlab.com/alexandria/alexandria/master/"
    #P"/Users/pjb/ergo/repositories/github.com/alexandria/alexandria/master/")
  
  ;; ~/ergo/repositories/{git-server}/{relative/path}/{branch-or-tag-or-commit}/.git/


  (mapcar (lambda (collection)
            (repository-fetch-url collection "alexandria/alexandria"))
          *collections*)

  (mapcar (lambda (url) (multiple-value-list (parse-git-url url)))
          '("git://common-lisp.net/projects/alexandria/alexandria.git"
            "git@gitlab.com:alexandria/alexandria.git"
            "git@github.com:alexandria/alexandria.git"))
  
  (mapcar (lambda (url) (multiple-value-list (parse-git-url url)))
          '("git://common-lisp.net/projects/alexandria/alexandria.git"
            "git@gitlab.com:alexandria/alexandria.git"
            "git@github.com:alexandria/alexandria.git"))
  (("common-lisp.net" :git) ("gitlab.com" :ssh) ("github.com" :ssh))

  (setf *print-circle* nil)

  (map nil 'print
       (mapcar (function parse-repository-designator)
               '((git "git://common-lisp.net/projects/alexandria/alexandria.git" :kind :gitlab)
                 (git "git@gitlab.com:alexandria/alexandria.git" :branch "issue-2")
                 (git "git@gitlab.com:alexandria/alexandria.git" :tag "v1.2")
                 (git "git@gitlab.com:alexandria/alexandria.git" :release "v2.9")
                 (git "git@gitlab.com:alexandria/alexandria.git" :tag :latest)
                 (git "git@gitlab.com:alexandria/alexandria.git" :release :latest)
                 (git "git@gitlab.com:alexandria/alexandria.git" :commit "abcdef")
                 (git "git@github.com:alexandria/alexandria.git")
                 "git://common-lisp.net/projects/alexandria/alexandria.git"
                 "git@gitlab.com:alexandria/alexandria.git"
                 "git@github.com:alexandria/alexandria.git"
                 "alexandria/alexandria")))
  
  (git-repository-designator :url "git://common-lisp.net/projects/alexandria/alexandria.git" :kind :gitlab :tip-kind nil :tip-value nil) 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :branch :tip-value "issue-2") 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :tag :tip-value "v1.2") 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :release :tip-value "v2.9") 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :tag :tip-value :latest) 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :release :tip-value :latest) 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind :commit :tip-value "abcdef") 
  (git-repository-designator :url "git@github.com:alexandria/alexandria.git" :kind :github :tip-kind nil :tip-value nil) 
  (git-repository-designator :url "git://common-lisp.net/projects/alexandria/alexandria.git" :kind :plain :tip-kind nil :tip-value nil) 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind nil :tip-value nil) 
  (git-repository-designator :url "git@github.com:alexandria/alexandria.git" :kind :github :tip-kind nil :tip-value nil) 
  (git-repository-designator :url "git@gitlab.com:alexandria/alexandria.git" :kind :gitlab :tip-kind #:unbound :tip-value #:unbound) 
  
  )






;; (repository-url 'alexandria/alexandria)"git@gitlab.com:alexandria/alexandria.git" 
;; "git@gitlab.com:alexandria/alexandria.git"
;; 
;; (repository-url "alexandria/alexandria")
;; "git@gitlab.com:alexandria/alexandria.git"
;; 
;; ;; common-lisp.net "git://common-lisp.net/projects/alexandria/alexandria.git"
;; ;; github.com      "git@github.com:informatimago/ergo.git"
;; ;; gitlab.com      "git@gitlab.com:informatimago/ergo.git"


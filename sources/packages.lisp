;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The packages of the ergo system.
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

(defpackage "COM.INFORMATIMAGO.ERGO.COLLECTION"
  (:documentation "Repository Collections.")
  (:use "COMMON-LISP"))

(defpackage "COM.INFORMATIMAGO.ERGO.PROJECT"
  (:documentation "Project.")
  (:use "COMMON-LISP"))

(defpackage "COM.INFORMATIMAGO.ERGO.GIT"
  (:documentation "Git Operations.")
  (:use "COMMON-LISP"))

(defpackage "COM.INFORMATIMAGO.ERGO.GITLAB"
  (:documentation "GitLab-Specific Operations.")
  (:use "COMMON-LISP"))

(defpackage "COM.INFORMATIMAGO.ERGO.GITHUB"
  (:documentation "GitLab-Specific Operations.")
  (:use "COMMON-LISP"))

(defpackage "COM.INFORMATIMAGO.ERGO.IMPLEMENTATION"
  (:documentation "Implementation.")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                "PRINT-PARSEABLE-OBJECT")
  (:shadow "LOAD")
  (:export "PROJECT" "ERGO" "LOAD" "WHERE-FROM" "WHERE-IS"
           "*PROJECT*"))

(defpackage "COM.INFORMATIMAGO.ERGO"
  (:nicknames "ERGO")
  (:documentation "User Interface Package.")
  (:use)
  (:import-from "COM.INFORMATIMAGO.ERGO.IMPLEMENTATION"
                . #1=("PROJECT" "ERGO" "LOAD" "WHERE-FROM" "WHERE-IS"))
  (:export . #1#))

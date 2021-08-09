;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.ergo.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The ergo system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-08-08 <PJB> Created
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

(asdf:defsystem "com.informatimago.ergo"
  :description "ergo - Project Dependencies Management Program and Project Builder"
  :author "Pascal J. Bourguignon"
  :version "0.0.0"
  :license "AGPL3"
  :depends-on ("uiop"
               "split-sequence" "cl-ppcre"
               "com.informatimago.common-lisp.cesarum")
  :components ((:file "packages"        :depends-on ())
               (:file "prototype"       :depends-on ("packages")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


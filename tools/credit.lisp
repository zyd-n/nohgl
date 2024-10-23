(in-package #:credit-authors)

;;; utils

;; FIXME: This is duplicated from source/utils.lisp.
(defun printn (n thing)
  (format nil "~v@{~A~:*~}" n thing))

(defun last1 (l)
  (first (last l)))

(defun abs-dir (pathname-dir)
  (loop for dir in pathname-dir
        collect (case dir
                  (:absolute "/")
                  (:home (format nil "home/~a/" (last1 (pathname-directory (user-homedir-pathname)))))
                  (t (format nil "~a/" dir)))
        into dirs
        finally (return (format nil "~{~a~}" dirs))))

(defun filename (pathname &optional (part :fullname))
  (case part
    (:fullname
     (file-namestring pathname))
    (:extension
     (pathname-type pathname))
    (:basename
     (pathname-name pathname))
    (:directory
     (last1 (pathname-directory pathname)))
    (:absolute-directory
     (abs-dir (pathname-directory pathname)))
    (:absolute-filename
     (concatenate 'string (abs-dir (pathname-directory pathname)) (file-namestring pathname)))
    (t (error "Not a valid part: ~s" part))))

;;; manually add certain licenses

(defvar *non-lisp-licenses*
  (make-hash-table :test 'equal))

(defvar *lisp-licenses*
  (make-hash-table :test 'equal))

(defun add-license (type system-name license)
  (case type
    (:lisp (setf (gethash system-name *lisp-licenses*) license))
    (:non-lisp (setf (gethash system-name *non-lisp-licenses*) license))))

(defun lisp-license (system-name)
  (gethash system-name *lisp-licenses*))

(add-license
 :lisp
 "3bz"
 "MIT (Bart Botta <00003b at gmail.com>)
")

(add-license
 :lisp
 "parse-float"
 "Public Domain (Sumant Oemrawsingh)
")

(add-license
 :lisp
 "zpb-exif"
 "Created: 2005-12-08 by Zach Beane <xach@xach.com>

Copyright (c) 2005 Zachary Beane, All Rights Reserved

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials
    provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
")

(add-license
 :non-lisp
 "glfw (C)"
 "Copyright (c) 2002-2006 Marcus Geelnard

Copyright (c) 2006-2019 Camilla Löwy

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would
   be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not
   be misrepresented as being the original software.

3. This notice may not be removed or altered from any source
   distribution.
")

;;; main

(defun license? (file)
  (member (string-upcase (filename file :basename))
          '("LICENSE" "LICENCE" "COPYING" "COPY" "COPYRIGHT")
          :test #'string=))

(defun find-license (dir)
  (loop for file in (uiop:directory-files dir)
        do (when (license? file)
             (return (uiop:read-file-string (filename file :absolute-filename))))))

(defun w/licenses (systems)
  (sort
   (mapcar
    (lambda (system)
      (list system
            (or (lisp-license system)
                (find-license (asdf:system-source-directory system)))))
    systems)
   #'string-lessp :key #'first))

(defun wo/licenses (systems)
  (mapcar
   (lambda (system)
     (list (first system)
           (asdf:system-source-directory (first system))))
   (remove-if-not #'null systems :key #'second)))

(defun get-licenses (systems)
  (let* ((systems-with-licenses (w/licenses systems))
         (systems-without-licenses (wo/licenses systems-with-licenses)))
    (values systems-with-licenses
            systems-without-licenses)))

(defun credits-file (system)
  (format nil "~aCREDITS" (filename (asdf:system-source-directory system) :absolute-directory)))

(defun write-section (string credits-file)
  (alexandria:write-string-into-file
   (format nil "~A ~A ~A~%~%" (printn 36 #\=) string (printn 36 #\=))
   credits-file
   :if-exists :append))

(defun write-license (name license credits-file)
  (alexandria:write-string-into-file
   (format nil "-- ~A --~%~A~%" name license)
   credits-file
   :if-exists :append))

(defun update-lisp-credits (systems credits-file)
  (write-section "Lisp" credits-file)
  (loop for (system-name license) in systems
        do (write-license system-name license credits-file)))

(defun update-non-lisp-credits (credits-file)
  (write-section "Other" credits-file)
  (maphash
   (lambda (project-name license)
     (write-license project-name license credits-file))
   *non-lisp-licenses*))

(defun update-credits-file (system)
  (let ((credits-file (credits-file system)))
    (multiple-value-bind (w/licenses wo/licenses) (get-licenses (deptree:deptree system))
      (alexandria:write-string-into-file "" credits-file :if-exists :supersede)
      (update-lisp-credits w/licenses credits-file)
      (update-non-lisp-credits credits-file)
      (values
       (pathname credits-file)
       wo/licenses))))

;; (credit-authors:update-credits-file "nohgl")

;;; projel.el --- Enhanced project -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/projel
;; Version: 0.1.0
;; Keywords: vc
;; Package-Requires: ((emacs "28.1") (project "0.9.8"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with project

;;; Code:



(require 'project)
(defcustom projel-projects-excluded-dirs '("~/Dropbox"
																					 "~/.cache"
																					 "~/.cask")
	"List of directories to omit when finding projects."
	:type '(repeat directory)
	:group 'projel)

(defvar projel-file-name-indicators
  '((("dune-project")
		 . "Dune-Project")
		(("Project.toml")
		 . "Project.Toml")
		(("elm.json")
		 . "Elm.Json")
		(("pubspec.yaml")
		 . "Pubspec.Yaml")
		(("info.rkt")
		 . "Info.Rkt")
		(("Cargo.toml")
		 . "Cargo.Toml")
		(("stack.yaml")
		 . "Stack.Yaml")
		(("DESCRIPTION")
		 . "Description")
		(("Cask")
		 . "Cask")
		(("shard.yml")
		 . "Shard.Yml")
		(("Gemfile" "app" "lib" "db" "config" "spec")
		 . "Gemfile")
		(("Gemfile" "app" "lib" "db" "config" "test")
		 . "Gemfile")
		(("Gemfile" "lib" "test")
		 . "Gemfile")
		(("Gemfile" "lib" "spec")
		 . "Gemfile")
		((".bloop")
		 . ".Bloop")
		(("deps.edn")
		 . "Deps.Edn")
		(("build.boot")
		 . "Build.Boot")
		(("project.clj" ".midje.clj")
		 . "Project.Clj")
		(("project.clj")
		 . "Project.Clj")
		(("build.sc")
		 . "Build.Sc")
		(("build.sbt")
		 . "Build.Sbt")
		(("application.yml" "grails-app")
		 . "Application.Yml")
		(("gradlew")
		 . "Gradlew")
		(("build.gradle")
		 . "Build.Gradle")
		(("pom.xml")
		 . "Pom.Xml")
		(("pyproject.toml")
		 . "Pyproject.Toml")
		(("poetry.lock")
		 . "Poetry.Lock")
		(("Pipfile")
		 . "Pipfile")
		(("tox.ini")
		 . "Tox.Ini")
		(("setup.py")
		 . "Setup.Py")
		(("requirements.txt")
		 . "Requirements.Txt")
		(("manage.py")
		 . "Manage.Py")
		(("angular.json" ".angular-cli.json")
		 . "Angular.Json")
		(("package.json")
		 . "JavaScript")
		(("tsconfig.json")
		 . "TypeScript")
		(("gulpfile.js")
		 . "Gulpfile.Js")
		(("Gruntfile.js")
		 . "Gruntfile.Js")
		(("mix.exs")
		 . "Mix.Exs")
		(("rebar.config")
		 . "Rebar.Config")
		(("composer.json" "app" "src" "vendor")
		 . "Composer.Json")
		(("Taskfile.yml")
		 . "Taskfile.Yml")
		(("CMakeLists.txt")
		 . "Cmakelists.Txt")
		(("GNUMakefile")
		 . "Gnumakefile")
		(("Makefile")
		 . "Makefile")
		(("debian/control")
		 . "Debian/Control")
		(("WORKSPACE")
		 . "Workspace")
		(("flake.nix")
		 . "Flake.Nix")
		(("default.nix")
		 . "Default.Nix")
		(("meson.build")
		 . "Meson.Build")
		(("SConstruct")
		 . "Sconstruct")
		(("src")
		 . "Src")
		(("dune-project")
		 . "Dune-Project")
		(("Project.toml")
		 . "Project.Toml")
		(("elm.json")
		 . "Elm.Json")
		(("pubspec.yaml")
		 . "Pubspec.Yaml")
		(("info.rkt")
		 . "Info.Rkt")
		(("Cargo.toml")
		 . "Cargo.Toml")
		(("stack.yaml")
		 . "Stack.Yaml")
		(("DESCRIPTION")
		 . "Description")
		(("Cask")
		 . "Cask")
		(("shard.yml")
		 . "Shard.Yml")
		(("Gemfile" "app" "lib" "db" "config" "spec")
		 . "Gemfile")
		(("Gemfile" "app" "lib" "db" "config" "test")
		 . "Gemfile")
		(("Gemfile" "lib" "test")
		 . "Gemfile")
		(("Gemfile" "lib" "spec")
		 . "Gemfile")
		((".bloop")
		 . ".Bloop")
		(("deps.edn")
		 . "Clojure")
		(("build.boot")
		 . "Build.Boot")
		(("project.clj" ".midje.clj")
		 . "Clojure")
		(("project.clj")
		 . "Clojure")
		(("build.sc")
		 . "Build.Sc")
		(("build.sbt")
		 . "Build.Sbt")
		(("application.yml" "grails-app")
		 . "Application.Yml")
		(("gradlew")
		 . "Gradlew")
		(("build.gradle")
		 . "Build.Gradle")
		(("pom.xml")
		 . "Pom.Xml")
		(("pyproject.toml")
		 . "Pyproject.Toml")
		(("poetry.lock")
		 . "Poetry.Lock")
		(("Pipfile")
		 . "Pipfile")
		(("tox.ini")
		 . "Tox.Ini")
		(("setup.py")
		 . "Setup.Py")
		(("requirements.txt")
		 . "Requirements.Txt")
		(("manage.py")
		 . "Manage.Py")
		(("angular.json" ".angular-cli.json")
		 . "Angular.Json")
		(("package.json")
		 . "Javascript")
		(("gulpfile.js")
		 . "Gulpfile.Js")
		(("Gruntfile.js")
		 . "Gruntfile.Js")
		(("mix.exs")
		 . "Mix.Exs")
		(("rebar.config")
		 . "Rebar.Config")
		(("composer.json" "app" "src" "vendor")
		 . "Composer.Json")
		(("Taskfile.yml")
		 . "Taskfile.Yml")
		(("CMakeLists.txt")
		 . "Cmakelists.Txt")
		(("GNUMakefile")
		 . "Gnumakefile")
		(("Makefile")
		 . "Makefile")
		(("debian/control")
		 . "Debian/Control")
		(("WORKSPACE")
		 . "Workspace")
		(("flake.nix")
		 . "Flake.Nix")
		(("default.nix")
		 . "Default.Nix")
		(("meson.build")
		 . "Meson.Build")
		(("SConstruct")
		 . "Sconstruct")
		(("src")
		 . "Src")))


(defun projel-minibuffer-get-metadata ()
	"Return current minibuffer completion metadata."
	(completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun projel-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors
                                (projel-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun projel-minibuffer-minibuffer-auto-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (projel-minibuffer-get-metadata) 'category)
       all))))

(defun projel-minibuffer-default-top-minibuffer-completion ()
	"Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete."
	(when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (projel-minibuffer-minibuffer-auto-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar projel-minibuffer-targets-finders
  '(projel-minibuffer-ivy-selected-cand
    projel-minibuffer-default-top-minibuffer-completion))

(defun projel-minibuffer-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'projel-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun projel-restore-completions-wind ()
	"Restore *Completions* window height."
	(when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook #'projel-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun projel-action-no-exit (action)
	"Call ACTION with minibuffer candidate in its original window."
	(pcase-let ((`(,_category . ,current)
               (projel-minibuffer-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook #'projel-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))


(defun projel--exit-with-action (action)
	"Call ACTION with current candidate and exit minibuffer."
	(pcase-let ((`(,_category . ,current)
               (projel-minibuffer-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))

(defvar projel-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projel-project-root`.")

(defun projel-current-minibuffer-file ()
	"Return absolute filename for current completion candidate."
	(require 'project)
	(pcase-let ((`(,_category . ,current)
               (projel-minibuffer-current-candidate)))
    (if (file-name-absolute-p current)
        current
      (expand-file-name current (project-root (project-current))))))

;;;###autoload
(defun projel-find-file-other-window ()
	"Invoke action to find file in other window."
	(interactive)
  (when-let ((file (projel-current-minibuffer-file)))
		(run-with-timer 0 nil 'find-file-other-window file)
    (abort-minibuffers)))

;;;###autoload
(defun projel-preview-file ()
	"Invoke action to find file in other window."
	(interactive)
	(when-let ((file (projel-current-minibuffer-file)))
		(if (minibuffer-selected-window)
        (projel--preview-file file)
      (find-file file))))


(defvar projel-auto-preview-timer nil)

(defvar-local projel-preview-candidate nil)

(defun projel-auto-do-preview ()
	"Try to preview current minibuffer candidate."
	(when (minibufferp)
    (let ((cand (projel-current-minibuffer-file))
          (curr projel-preview-candidate))
      (when (not (equal cand curr))
        (setq projel-preview-candidate cand)
				(projel--preview-file projel-preview-candidate)))))

(defun projel-auto-schedule-preview ()
  "Run preview command after timeout."
  (when (timerp projel-auto-preview-timer)
    (cancel-timer projel-auto-preview-timer))
  (setq projel-auto-preview-timer
        (run-with-timer 0.5 nil
                        'projel-auto-do-preview)))

(defun projel-get-project-top-root-files (dir)
	"Return files and directories DIR at top level depth."
	(mapcan
	 (lambda (it)
		 (let ((full-dir (file-name-as-directory (expand-file-name it dir))))
			 (if (file-directory-p full-dir)
					 (mapcar
						(lambda (f)
							(concat it "/" f))
						(directory-files
						 full-dir nil
						 directory-files-no-dot-files-regexp))
				 (list it))))
	 (delete "node_modules"
					 (delete ".git"
									 (directory-files dir nil
																		directory-files-no-dot-files-regexp)))))
(defun projel--preview-file (file)
	"Preview FILE in other window."
	(with-minibuffer-selected-window
		(let ((buffer (get-buffer-create "*km-project-preview*")))
			(with-current-buffer buffer
				(with-current-buffer-window buffer
						buffer
						(cons 'display-buffer-in-side-window
									'((window-height . fit-window-to-buffer)))
					(lambda (window _value)
						(with-selected-window window
							(setq buffer-read-only t)
							(let ((inhibit-read-only t))
								(unwind-protect
										(read-key-sequence "")
									(quit-restore-window window 'kill)))))
					(if (file-directory-p file)
							(insert
							 (string-join
							  (projel-get-project-top-root-files file)
								"\n"))
						(insert-file-contents file)
						(let ((buffer-file-name file))
							(delay-mode-hooks (set-auto-mode)
																(font-lock-ensure)))
						(setq header-line-format
									(abbreviate-file-name file))))))))

(defun projel-expand-wildcards (pattern dir depth &optional full regexp)
	"Return list of files that matches PATTERN in DIR at max DEPTH.

PATTERN is, by default, a \"glob\"/wildcard string, e.g.,
\"/tmp/*.png\" or \"/*/*/foo.png\", but can also be a regular
expression if the optional REGEXP parameter is non-nil.  In any
case, the matches are applied per sub-directory, so a match can't
span a parent/sub directory, which means that a regexp bit can't
contain the \"/\" character.

If FULL is non-nil, files are absolute."
	(let ((tramp-archive-enabled nil))
		(let ((dir (file-name-as-directory dir)))
			(mapcan (lambda (n)
								(let ((tramp-archive-enabled nil))
									(file-expand-wildcards
									 (concat dir
													 (string-join
														(append (make-vector n "**")
																		(list pattern))
														"/"))
									 full regexp)))
							(number-sequence 0 (1- depth))))))


(defun projel-get-projects (&optional depth dir)
	"Return all git repositories at DEPTH in DIR.
Default value for DEPTH is 3.
Default value for DIR is home directory."
	(unless depth (setq depth 3))
	(unless dir (setq dir "~/"))
	(let ((tramp-archive-enabled nil)
				(projects)
				(excluded (delq nil (append
														 projel-projects-excluded-dirs
														 (when
																 (require 'xdg nil t)
															 (mapcar (lambda (it)
																				 (when (fboundp it)
																					 (funcall it)))
																			 '(xdg-state-home
																				 xdg-data-home
																				 xdg-runtime-dir)))))))
		(dolist (dir (directory-files dir t directory-files-no-dot-files-regexp))
			(when (and (file-directory-p dir)
								 (file-accessible-directory-p dir)
								 (not (seq-find (apply-partially #'file-equal-p dir)
																excluded)))
				(setq projects
							(if (file-exists-p (concat dir "/.git"))
									(push (concat dir "/.git") projects)
								(nconc (projel-expand-wildcards "*/.git" dir
																								(1- depth))
											 projects)))))
		(mapcar (lambda (dir)
							(abbreviate-file-name (file-name-parent-directory dir)))
						projects)))


(defvar magit-repository-directories)
(defvar projel-repos nil)

(defun projel-get-dirs-for-cloning ()
	"Return list of git parents directories."
	(seq-uniq (mapcar #'file-name-parent-directory
										(projel-find-projects))))

(defun projel-find-projects (&optional depth)
	"Search for git projects in home directory at DEPTH."
	(delq nil
				(append
				 (when (fboundp 'xdg-config-home)
					 (list (abbreviate-file-name
									(file-name-as-directory (xdg-config-home)))))
				 (projel-get-projects depth))))



(defvar projel-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'projel-find-file-other-window)
    (define-key map (kbd "C-j") #'projel-preview-file)
    map))


(defun projel-run-js-project (orig-fn arg)
	"In js projects invoke custom function, otherwise just call ORIG-FN with ARG."
	(if (not (locate-dominating-file default-directory "package.json"))
      (funcall orig-fn arg)
    (cond ((locate-dominating-file default-directory "yarn.lock")
           (progn
						 (require 'yarn)
             (when (fboundp 'yarn-run)
               (yarn-run))))
          ((locate-dominating-file default-directory
                                   "package.json")
           (require 'npmjs)
           (when (fboundp 'npmjs-run-script)
             (npmjs-run-script))))))

;;;###autoload
(defun projel-completing-read (prompt choices)
	"Present a project tailored PROMPT with CHOICES."
	(interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (when (active-minibuffer-window)
          (use-local-map
           (make-composed-keymap projel-minibuffer-map
                                 (current-local-map)))))
    (completing-read prompt choices)))

(defun projel--completing-read-node-module (prompt &rest _)
	"Present a project tailored PROMPT."
	(interactive)
  (let* ((directory (or (locate-dominating-file default-directory
                                                "node_modules")))
         (node-modules-files
          (when directory
            (mapcar (apply-partially #'concat "./node_modules/")
                    (directory-files (expand-file-name
                                      "node_modules" directory)
                                     nil
                                     directory-files-no-dot-files-regexp)))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (active-minibuffer-window)
            (use-local-map
             (make-composed-keymap projel-minibuffer-map
                                   (current-local-map)))))
      (completing-read prompt node-modules-files))))


(defun projel-readme-annotation (file)
	"Extract annotation from org or markdown FILE."
	(with-temp-buffer
		(erase-buffer)
		(insert-file-contents file)
		(goto-char (point-min))
		(let* ((heading-re
						(pcase (file-name-extension file)
							("org" "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$")
							((or "md" "mdx" "mkdn")
							 "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)$")
							(_
							 "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)$")))
					 (title
						(save-excursion
							(when (re-search-forward
										 heading-re
										 nil
										 t
										 1)
								(match-string-no-properties 0))))
					 (beg
						(progn (re-search-forward
										"^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)$"
										nil t 1)
									 (when (re-search-forward
													"^[\s]?+[`]?[a-z]"
													nil
													t
													1)
										 (match-beginning 0))))
					 (end
						(when beg
							(goto-char beg)
							(forward-sentence)))
					 (descr
						(when (and beg end)
							(buffer-substring-no-properties
							 beg
							 end))))
			(mapconcat (apply-partially
									#'replace-regexp-in-string
									"[\n\f\r]+" " ")
								 (delq nil (list (or descr title)))
								 " "))))

(defvar projel-file-extensions-indicators
	'(("gradle" . "Java (Gradle)")
		("hs" . "Haskel")
		("cabal" . "Haskel (Cabal)")
		("cpp" . "C")
		("h" . "C")
		("c" . "C")
		("el" . "Emacs lisp")))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(defun projel-read-json (file)
	"Parse FILE with natively compiled function or with json library.
If Emacs has libjansson support, parse it with natively compiled function,
otherwise use the parsing routines from the json library."
	(require 'json nil t)
	(let ((content (with-temp-buffer
									 (insert-file-contents file)
									 (buffer-string))))
		(if (not (and (fboundp 'json-parse-string)
									(fboundp 'json-available-p)
									(json-available-p)))
				(json-parse-string content
													 :object-type 'alist
													 :array-type 'array)
			(when (fboundp 'json-read-from-string)
				(let ((json-array-type 'vector)
							(json-object-type 'alist))
					(json-read-from-string content))))))

(defun projel-get-project-description (project)
	"Return PROJECT descriptions."
	(let* ((files
					(delete ".dir-locals.el"
									(directory-files
									 project nil
									 directory-files-no-dot-files-regexp
									 t)))
				 (intersections
					(seq-find (lambda (pfiles)
											(= (length (seq-intersection (car pfiles) files))
												 (length (car pfiles))))
										projel-file-name-indicators)))
		(string-join
		 (delete-dups (delq nil
												(list
												 (or
													(when intersections
														(propertize
														 (substring-no-properties
															(cdr intersections))
														 'face 'font-lock-keyword-face))
													(when-let ((found
																			(seq-find
																			 (lambda (file)
																				 (assoc-string
																					(file-name-extension
																					 file)
																					projel-file-extensions-indicators))
																			 files)))
														(propertize
														 (substring-no-properties
															(cdr
															 (assoc-string
																(file-name-extension
																 found)
																projel-file-extensions-indicators)))
														 'face 'success)))
												 (when-let ((readme
																		 (car
																			(or (member "README.md"
																									files)
																					(member "README.MD"
																									files)
																					(member "readme.md"
																									files)
																					(member "readme.mkdn"
																									files)
																					(member "README.mkdn"
																									files)
																					(member "README.org"
																									files)
																					(member "readme.org"
																									files)))))
													 (projel-readme-annotation (expand-file-name
																											readme project)))
												 (when-let* ((package-json
																			(when (file-exists-p
																						 (concat
																							project
																							"package.json"))
																				(ignore-errors (projel-read-json
																												(concat
																												 project
																												 "package.json")))))
																		 (description (alist-get
																									 'description
																									 package-json)))
													 (string-join
														(remove nil
																		(list
																		 (if-let ((vers
																							 (alist-get 'version
																													package-json)))
																				 (concat (or
																									(alist-get 'name
																														 package-json)
																									"")
																								 "@"
																								 vers)
																			 (alist-get 'name
																									package-json))
																		 description))
														" ")))))
		 " ")))

(defvar projel-annotations-cache nil)

(defun projel-annotate-project (directory)
	"Annotate project DIRECTORY using `projel-annotations-cache'."
	(unless projel-annotations-cache
		(setq projel-annotations-cache (projel-get-annotated-project-alist)))
	(if-let ((description (cdr (assoc-string
															directory
															projel-annotations-cache))))
			(concat (propertize " " 'display
													(list
													 'space
													 :align-to
													 51))
							description)
		""))

(defun projel-get-annotated-project-alist ()
	"Read project."
	(project--ensure-read-project-list)
	(let* ((projects (delete-dups (append (projel-find-projects)
																				(mapcar #'car project--list))))
				 (annotf (lambda (str)
									 (cons str (projel-get-project-description str)))))
		(setq projel-annotations-cache (mapcar annotf projects))
		(setq project--list
					projel-annotations-cache)))

(defun projel-project--file-completion-table (all-files)
	"Completions table for project. ALL-FILES is collection to complete."
	(lambda (string pred action)
    (cond ((eq action 'metadata)
					 '(metadata . ((category . project-file)
												 (annotation-function . projel-annotate-project))))
					(t
					 (complete-with-action action all-files string pred)))))



(defun projel-completing-read-strict (&rest args)
	"Apply `project--completing-read-strict' with ARGS with preview and keymap.
\\<projel-minibuffer-map>\\{projel-minibuffer-map}."
	(minibuffer-with-setup-hook
			(lambda ()
				(when (minibufferp)
					(add-hook 'pre-command-hook #'projel-auto-schedule-preview nil t)
					(use-local-map
					 (make-composed-keymap projel-minibuffer-map
																 (current-local-map)))))
		(apply #'project--completing-read-strict args)))

(defun projel-project--read-file-cpd-relative (&rest args)
	"Apply `project--read-file-cpd-relative' with ARGS with preview and keymap.
\\<projel-minibuffer-map>\\{projel-minibuffer-map}."
	(minibuffer-with-setup-hook
			(lambda ()
				(when (minibufferp)
					(add-hook 'pre-command-hook #'projel-auto-schedule-preview nil t)
					(use-local-map
					 (make-composed-keymap projel-minibuffer-map
																 (current-local-map)))))
		(apply #'project--read-file-cpd-relative args)))


(defun projel-completing-read-project (&rest _)
	"Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
	(unless projel-annotations-cache
		(projel-get-annotated-project-alist))
	(let* ((dir-choice "... (choose a dir)")
				 (alist (append (list (cons dir-choice "Other Project"))
												projel-annotations-cache))
				 (annotf (lambda (str)
									 (concat (propertize " " 'display
																			 (list 'space :align-to 51))
													 (or (cdr
																(assoc-string str alist))
															 ""))))
				 (cycle-sort-fn (lambda (it) it))
				 (display-sort-fn (lambda (it)
														(seq-sort-by #'length '< it)))
				 (pr-dir ""))
		(while (equal pr-dir "")
      ;; If the user simply pressed RET, do this again until they don't.
      (setq pr-dir (minibuffer-with-setup-hook
											 (lambda ()
												 (when (minibufferp)
													 (add-hook 'pre-command-hook
																		 #'projel-auto-schedule-preview nil t)
													 (use-local-map
														(make-composed-keymap projel-minibuffer-map
																									(current-local-map)))))
										 (completing-read "Candidates: "
																			(lambda (str pred action)
																				(if (eq action 'metadata)
																						`(metadata
																							(annotation-function . ,annotf)
																							(cycle-sort-function .
																																	 ,cycle-sort-fn)
																							(category . project-file)
																							(display-sort-function .
																																		 ,display-sort-fn))
																					(complete-with-action action
																																alist
																																str pred)))
																			nil t))))
		(if (equal pr-dir dir-choice)
        (read-directory-name "Select directory: " default-directory nil t)
      pr-dir)))

;;;###autoload
(defun projel-write-projects ()
	"Save `project--list' in `project-list-file'."
	(interactive)
	(projel-get-annotated-project-alist)
	(project--write-project-list))

;;;###autoload
(defun projel-enable-project-override ()
	"Enable advising project."
	(interactive)
	(setq-default project-read-file-name-function
								#'projel-project--read-file-cpd-relative)
	(advice-add 'project-prompt-project-dir :override
							#'projel-completing-read-project)
	(advice-add 'project--file-completion-table :override
							#'projel-project--file-completion-table))

;;;###autoload
(defun projel-disable-project-override ()
	"Disable advising project."
	(interactive)
	(setq-default project-read-file-name-function
								#'project--read-file-cpd-relative)
	(advice-remove 'project-prompt-project-dir
								 #'projel-completing-read-project)
	(advice-remove 'project--file-completion-table
								 #'projel-project--file-completion-table))


(provide 'projel)
;;; projel.el ends here
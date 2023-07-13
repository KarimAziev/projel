;;; projel.el --- Enhanced project -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/projel
;; Version: 0.1.0
;; Keywords: project, convenience, vc
;; Package-Requires: ((emacs "28.1") (project "0.9.8") (transient "0.4.1"))
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
(require 'transient)

(defcustom projel-projects-excluded-dirs '("~/Dropbox"
                                           "~/.cache"
                                           "~/.cask")
  "List of full directory names to omit when finding projects."
  :type '(repeat directory)
  :group 'projel)

(defcustom projel-projects-ignored-projects nil
  "List of ignored projects."
  :type '(repeat directory)
  :group 'projel)

(defcustom projel-allow-magit-repository-directories-sync t
  "Whether to add projects parents directory to `magit-repository-directories'."
  :type '(repeat directory)
  :group 'projel)


(defcustom projel-projects-exlcuded-non-directory-names	'(".cache"
                                                          ".config"
                                                          ".local"
                                                          ".Trash"
                                                          ".cask"
                                                          "node_modules")
  "Names of directories to omit when finding projects."
  :type '(repeat directory)
  :group 'projel)


(defcustom projel-auto-preview-delay nil
  "Number of seconds to wait before show file preview in minibuffer.
If nil, disable auto preview."
  :type '(radio (const :tag "Not set" nil)
                (number :tag "Seconds"))
  :group 'projel)

;; currenlty not implemented
(defcustom projel-explore-dirs-settings '(("~/"
                                           (:max-depth . 3)
                                           (:non-visit-dirs . ("node_modules"
                                                               ".cache"
                                                               ".cask"))
                                           (:exclude-regexps . ("^\\."))
                                           (:include-regexps . ("^\\.emacs\\.d"))))
  "Settings per parent directories. Currenlty it is not implemented."
  :type
  '(alist
    :key-type (directory :tag "Directory")
    :value-type
    (alist :options
           ((:max-depth (integer :tag "Maximum depth" 1))
            (:non-visit-dirs (repeat :tag
                                     "Excluded directory"
                                     directory)
                             '("node_modules"
                               ".cache"
                               ".cask"))
            (:exclude-regexps . ("^\\."))
            (:include-regexps . ("^\\.emacs\\.d")))))
  :group 'projel)

(defface projel-project-type-annotation '((t :inherit font-lock-keyword-face))
  "Face used by for project type annotations."
  :group 'projel)

(defface projel-project-description-annotation '((t :inherit
                                                    completions-annotations))
  "Face used by for project descriptions annotations."
  :group 'projel)

(defcustom projel-annotation-project-type-max-width 10
  "The maximum length of the project type to display in minibuffer.

If the string exceeds this limit, it will be truncated to fit."
  :type 'integer
  :group 'projel)

(defcustom projel-annotation-project-description-max-width 80
  "The maximum length of the project description to display in minibuffer.

If the string exceeds this limit, it will be truncated to fit."
  :type 'integer
  :group 'projel)

(defcustom projel-explore-project-depth 3
  "Maximum depth for searching projects in home directory."
  :type 'integer
  :group 'projel)

(defcustom projel-auto-rescan-on-init t
  "Whether to rescan projects on first projects read."
  :type 'boolean
  :group 'projel)


(defcustom projel-projects-actions-alist '(("*Rescan all projects*" .
                                            projel-rescan-all-projects)
                                           ("*Find projects in directory*" .
                                            projel-rescan-directory)
                                           ("*Add project*"
                                            . projel-add-project-directory)
                                           ("*Remove project*"
                                            . projel-remove-project))
  "A text to show in minibuffer completions to rescan projects."
  :group 'projel
  :type '(alist
          :key-type (string :tag "Label")
          :value-type (choice (function-item projel-add-project-directory)
                              (function-item projel-rescan-directory)
                              (function-item projel-rescan-all-projects)
                              (function-item projel-remove-project)
                              (function :tag "Custom function"))) )

(defcustom projel-preview-buffer-name "*projel-preview*"
  "Buffer name to use for preview file."
  :group 'projel
  :type 'string)

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
     . "Leiningen")
    (("project.clj" "package.json")
     . "Leiningen")
    (("project.clj" "package.json")
     . "Leiningen")
    (("project.clj")
     . "Leiningen")
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
     . "ClojureScript")
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
     . "Src"))
  "Alist of project files and corresponding type annotations.")

(defmacro projel--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro projel--always (value)
  "Return a function that always return VALUE.
This function accepts any number of arguments but ignores them."
  (declare (pure t)
           (side-effect-free error-free))
  `(lambda (&rest _) ,value))

(defmacro projel--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (indent defun)
           (side-effect-free t))
  `(projel--pipe ,@(reverse functions)))

(defmacro projel--or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (or
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro projel--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (and
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro projel--partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

(defmacro projel--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

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
  "Return the currently selected minibuffer item in Ivy."
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
  "Return cons with category and current completion candidate in minibuffer."
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
  (when (memq this-command '(minibuffer-next-completion
                             minibuffer-previous-completion))
    (remove-hook 'post-command-hook #'projel-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun projel-hide-completions-wind ()
  "Temporarly minimize *Completions* window."
  (when-let ((win (get-buffer-window "*Completions*" 0)))
    (minimize-window win)
    (add-hook 'post-command-hook #'projel-restore-completions-wind)))

(defun projel-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window.
Also `minimize-window' *Completions* window if any and restore at
after `minibuffer-previous-completion' or `minibuffer-next-completion'
commands."
  (pcase-let ((`(,_category . ,current)
               (projel-minibuffer-current-candidate)))
    (projel-hide-completions-wind)
    (with-minibuffer-selected-window
      (funcall action current))))

(defun projel--exit-with-action (action)
  "Call ACTION with current minibuffer candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (projel-minibuffer-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))

(defun projel-current-minibuffer-file ()
  "Return absolute filename for current minibuffer candidate or nil."
  (pcase-let* ((`(,_category . ,current)
                (projel-minibuffer-current-candidate)))
    (cond ((or (projel-project-action-candidate-p current)
               (string-empty-p current))
           nil)
          ((file-name-absolute-p current)
           (when (file-exists-p current)
             current))
          (t
           (when-let* ((project (project-current))
                       (root (if (fboundp 'project-root)
                                 (project-root project)
                               (with-no-warnings
                                 (car (project-roots project)))))
                       (file (expand-file-name current root)))
             (when (file-exists-p file)
               file))))))


;;;###autoload
(defun projel-find-file-other-window ()
  "Invoke action to find selected file in minibuffer in other window."
  (interactive)
  (when-let ((file (projel-current-minibuffer-file)))
    (run-with-timer 0 nil #'find-file-other-window file)
    (abort-minibuffers)))

;;;###autoload
(defun projel-preview-file ()
  "Invoke action to preview current file in minibuffer in other window."
  (interactive)
  (when-let ((file (projel-current-minibuffer-file)))
    (if (minibuffer-selected-window)
        (projel--preview-file file)
      (find-file file))))

(defvar projel-auto-preview-timer nil)

(defvar-local projel-preview-candidate nil)

(defun projel-auto-do-preview ()
  "Try to preview current minibuffer completion canidate as file.
This function use the temporarly buffer `projel-preview-buffer-name' with file
content instead of visiting it to avoid unwanted side effects, such
as running find file hooks, starting lsp or eglot servers and so on."
  (when (minibufferp)
    (let ((cand (projel-current-minibuffer-file))
          (curr projel-preview-candidate))
      (when (and cand (not (equal cand curr)))
        (setq projel-preview-candidate cand)
        (projel--preview-file projel-preview-candidate)))))

(defun projel-auto-schedule-preview ()
  "Schedule minibuffer preview if `projel-auto-preview-delay' is a number."
  (when (timerp projel-auto-preview-timer)
    (cancel-timer projel-auto-preview-timer))
  (when projel-auto-preview-delay
    (setq projel-auto-preview-timer
          (run-with-timer projel-auto-preview-delay nil
                          #'projel-auto-do-preview))))

(defun projel-get-project-top-root-files (dir)
  "Return files and directories of DIR at top level depth."
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

(defun projel--kill-preview-buffer ()
  "Kill the buffer with the name `projel-preview-buffer-name'."
  (when-let ((buff (get-buffer projel-preview-buffer-name)))
    (when (buffer-live-p buff)
      (kill-buffer buff))))

(defun projel--preview-file (file)
  "Preview FILE in other window.
This function doesn't really visit FILE to avoid unwanted side effects, such
as running find file hooks, starting lsp or eglot servers and so on.
Instead it uses a temporarly buffer `projel-preview-buffer-name'."
  (when (and file (file-exists-p file))
    (with-minibuffer-selected-window
      (let ((buffer (get-buffer-create projel-preview-buffer-name)))
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
                    (abbreviate-file-name file)))))))))

(defun projel-git-top-level-non-git-dir ()
  "Traverse up to non-git directory."
  (let ((dir)
        (curr default-directory))
    (while (setq dir
                 (when curr
                   (locate-dominating-file curr ".git")))
      (setq curr (projel-file-name-parent-directory dir)))
    curr))

(defun projel-expand-pattern (curr pattern)
  "Recoursively expand PATTERN for string CURR.
PATTERN can be either string or function, or list of strings and functions."
  (pcase pattern
    ((pred functionp)
     (funcall pattern curr))
    ((pred stringp)
     (string= curr pattern))
    (_ (seq-find
        (apply-partially #'projel-expand-pattern curr)
        pattern))))

(defun projel-find-in-dir (dir &optional pattern non-visit-pattern max-depth
                               transform-fn current-depth)
  "Return list of files that matches PATTERN in DIR at MAX-DEPTH.

Both PATTERN and NON-VISIT-PATTERN, if non nil,
will be tested against the directory to visit.

It should be either:
- string that will be tested against the current file name of the directory.
- function (will be called with one argument local directory name)
- list of patterns, that will be tested until first non nil result.

If PATTERN matches, it will be added to result, and not be visited.

If NON-VISIT-PATTERN matches, directory will not be visited.

If TRANSFORM-FN is non nil, it should be a function that will be called with one
argument - full directory name.

CURRENT-DEPTH is used for recoursive purposes."
  (setq current-depth (1+ (or current-depth 0)))
  (unless max-depth (setq max-depth 1))
  (when (>= max-depth current-depth)
    (let ((tramp-archive-enabled nil))
      (let ((found-dirs))
        (let
            ((default-directory (expand-file-name (file-name-as-directory dir))))
          (dolist
              (curr
               (directory-files default-directory nil
                                directory-files-no-dot-files-regexp t))
            (let ((full-dir (expand-file-name curr))
                  (tramp-archive-enabled nil))
              (cond ((not (file-directory-p full-dir)))
                    ((and non-visit-pattern
                          (projel-expand-pattern curr non-visit-pattern)))
                    ((and pattern
                          (projel-expand-pattern curr pattern))
                     (setq found-dirs
                           (push (if transform-fn
                                     (funcall transform-fn full-dir)
                                   full-dir)
                                 found-dirs)))
                    (t
                     (unless pattern
                       (setq found-dirs
                             (push (if transform-fn
                                       (funcall transform-fn full-dir)
                                     full-dir)
                                   found-dirs)))
                     (when-let ((subdirs (projel-find-in-dir full-dir
                                                             pattern
                                                             non-visit-pattern
                                                             max-depth
                                                             transform-fn
                                                             current-depth)))
                       (setq found-dirs
                             (if found-dirs
                                 (nconc
                                  found-dirs
                                  subdirs)
                               subdirs))))))))
        found-dirs))))


(defun projel-find-projects-in-dir (dir &optional max-depth write)
  "Explore projects in DIR at max depth MAX-DEPTH.
If WRITE is non nil, write found projects.
Return alist of added projects."
  (let ((results))
    (projel-find-in-dir dir #'project--find-in-directory
                        (append
                         projel-projects-exlcuded-non-directory-names
                         (mapcar
                          (apply-partially #'apply-partially 'file-equal-p)
                          projel-projects-excluded-dirs))
                        max-depth
                        (lambda (dir)
                          (let ((proj
                                 (file-name-as-directory (abbreviate-file-name
                                                          dir))))
                            (unless (assoc-string proj
                                                  project--list)
                              (setq results (push (list proj) results)))
                            proj)))
    (when results
      (setq project--list (nconc results project--list))
      (when write
        (project--write-project-list)))
    results))

;;;###autoload
(defun projel-add-projects-to-magit-repositories-dirs ()
  "Add parents directories of current projects to `magit-repository-directories'."
  (interactive)
  (let ((added))
    (dolist (dir (projel-get-projects-parent-dirs))
      (when (and projel-allow-magit-repository-directories-sync
                 (boundp 'magit-repository-directories)
                 (not (assoc-string dir magit-repository-directories)))
        (add-to-list 'magit-repository-directories (cons dir 1))
        (push dir added)))
    (message "Projel: added %d directories to magit-repository-directories"
             (length
              added))))

(defun projel-rescan-all (&optional depth)
  "Explore projects in DIR at max depth DEPTH.
If CHECK-EXISTING is non nil, also remove dead projects."
  (let ((existing-projects
         (seq-filter (projel--compose file-exists-p car)
                     project--list))
        (dead-projects))
    (setq dead-projects
          (when (> (length project--list)
                   (length existing-projects))
            (-
             (length project--list)
             (length existing-projects))))
    (when dead-projects
      (setq project--list existing-projects))
    (let ((dirs (projel-get-projects-parent-dirs))
          (results))
      (dolist (dir (or dirs (list "~/")))
        (message "Rescanning %s" dir)
        (when-let ((res
                    (when (file-exists-p dir)
                      (projel-find-projects-in-dir
                       dir
                       (or depth
                           (if dirs 1
                             projel-explore-project-depth))
                       t))))
          (setq results
                (nconc results res))
          (when (and projel-allow-magit-repository-directories-sync
                     (boundp 'magit-repository-directories))
            (add-to-list 'magit-repository-directories (list dir 1)))))
      (when (or dead-projects results)
        (project--write-project-list)
        (message
         "Projel: %s"
         (string-join
          (delq nil
                (list
                 (when results
                   (format "Added %d projects. " (length results)))
                 (when dead-projects
                   (format "Removed %d projects. " dead-projects))))
          ""))))))

(defun projel-get-projects ()
  "Init `project--list'.
If `projel-auto-rescan-on-init' is enabled and `project--list' is not initted,
rescan parent directories of projects.
Also check and remove unexisting projects."
  (if (or
       (and projel-auto-rescan-on-init
            (eq project--list 'unset)))
      (progn
        (project--ensure-read-project-list)
        (projel-rescan-all (if project--list
                               1
                             projel-explore-project-depth)))
    (project--ensure-read-project-list)
    (if (proper-list-p project--list)
        (when (seq-find (projel--compose
                          not
                          file-exists-p
                          car)
                        project--list)
          (setq project--list (seq-filter (projel--compose
                                            file-exists-p
                                            car)
                                          project--list))
          (project--write-project-list))
      (message "Resetting invalid project--list")
      (setq project--list nil)
      (projel-rescan-all)))
  project--list)


(defun projel-file-name-parent-directory (filename)
  "Return the directory name of the parent directory of FILENAME.
If FILENAME is at the root of the filesystem, return nil.
If FILENAME is relative, it is interpreted to be relative
to `default-directory', and the result will also be relative."
  (let* ((expanded-filename (expand-file-name filename))
         (parent (file-name-directory (directory-file-name expanded-filename))))
    (cond ;; filename is at top-level, therefore no parent
     ((or (null parent)
          ;; `equal' is enough, we don't need to resolve symlinks here
          ;; with `file-equal-p', also for performance
          (equal parent expanded-filename))
      nil)
     ;; filename is relative, return relative parent
     ((not (file-name-absolute-p filename))
      (file-relative-name parent))
     (t
      parent))))

(defun projel-get-projects-parent-dirs ()
  "Return list of git parents directories."
  (project--ensure-read-project-list)
  (delete-dups (mapcar #'projel-file-name-parent-directory
                       (project-known-project-roots))))

(defvar projel-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'projel-find-file-other-window)
    (define-key map (kbd "C-j") #'projel-preview-file)
    map)
  "Keymap to use in minibuffer when reading projects and files.")


;;;###autoload
(defun projel-remove-project (&optional project-dir)
  "Dispatch or perform action to remove PROJECT-DIR from the project list.

If active minibuffer window exists it will throw list with
`project--remove-from-project-list' and currently selected minibuffer project.

If no active minibuffer window exists, it will remove directory PROJECT-DIR
of a missing project from the project list. If PROJECT-DIR is nil,
it will read project in minibuffer."
  (interactive)
  (unless project-dir
    (setq project-dir (if (active-minibuffer-window)
                          (pcase-let ((`(,_category . ,current)
                                       (projel-minibuffer-current-candidate)))
                            current)
                        (projel-completing-read-project))))
  (if (active-minibuffer-window)
      (throw 'action (list 'project--remove-from-project-list project-dir
                           "Project `%s' removed from known projects"))
    (project--remove-from-project-list
     project-dir "Project `%s' removed from known projects")))

;;;###autoload
(defun projel-add-project-directory ()
  "Either dispatch or add directory PROJECT-DIR from the project list.

If active minibuffer window exists, it will throw corresponding action and
its arguments.

If no active minibuffer window exists, it will prompt directory, and add
it to list of projects."
  (interactive)
  (let ((fn (lambda ()
              (if-let ((pr (project--find-in-directory (read-directory-name
                                                        "Select directory: "
                                                        (if-let
                                                            ((proj
                                                              (projel-current-project-root)))
                                                            (projel-file-name-parent-directory
                                                             proj)
                                                          default-directory)
                                                        nil t))))
                  (project-remember-project pr)
                (message "could add project")))))
    (if (active-minibuffer-window)
        (throw 'action (list fn))
      (funcall fn))))

;;;###autoload
(defun projel-rescan-directory (&optional dir depth)
  "Dispatch or perform action to add projects in DIR at max DEPTH.
If DIR or DEPTH is nil it will read DIR and DEPTH in minibuffer.

If active minibuffer window exists throw the action
`projel-find-projects-in-dir' with DIR and DEPTH, otherwise invoke this action
with DIR and DEPTH."
  (interactive (list (read-directory-name "Directory")
                     (read-number "Max depth: ")))
  (unless dir (setq dir (read-directory-name "Directory")))
  (unless depth (setq depth (read-number "Max depth: ")))
  (if (active-minibuffer-window)
      (throw 'action (list #'projel-find-projects-in-dir dir depth))
    (projel-find-projects-in-dir dir depth)))


;;;###autoload
(defun projel-rescan-all-projects ()
  "Rescan parent directories of current projects."
  (interactive)
  (if (active-minibuffer-window)
      (throw 'action (list 'projel-rescan-all ))
    (projel-rescan-all)))

(defvar projel-minibuffer-project-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map projel-minibuffer-map)
    (define-key map (kbd "C-c C-u") #'projel-rescan-all-projects)
    (define-key map (kbd "C-c M-D") #'projel-remove-project)
    (define-key map (kbd "C-c !") #'projel-rescan-directory)
    (define-key map (kbd "C-c C-M-o") #'projel-add-project-directory)
    (define-key map (kbd "C-h ?") #'projel-projects-minibuffer-help)
    map)
  "Keymap to use in minibuffer when reading projects.")

;;;###autoload
(defun projel-projects-minibuffer-help ()
  "Show keybindings for `projel-minibuffer-project-map'."
  (interactive)
  (let ((overlay))
    (unwind-protect
        (progn (setq overlay (make-overlay (point)
                                           (1+ (point))))
               (overlay-put overlay 'after-string
                            (substitute-command-keys
                                     "\\<projel-minibuffer-project-map>\\{projel-minibuffer-project-map}"))
               (read-key-sequence ""))
      (setq unread-command-events
            (append (this-single-command-raw-keys)
                    unread-command-events))
      (when (overlayp overlay)
        (delete-overlay overlay)))))


(defun projel-run-js-project (orig-fn arg)
  "In js projects invoke custom command, otherwise just call ORIG-FN with ARG."
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

(defun projel--completing-read-node-modules (prompt &rest _)
  "Read closest to current directory files in node_modules with PROMPT.
During minibuffer completion the following next commands are available:
\\<projel-minibuffer-map>\\{projel-minibuffer-map}."
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

(defun projel-find-readme (dir)
  "Return list of readme files in DIR."
  (let* ((tramp-archive-enabled nil)
         (score-alist '(("org" . 3)
                        ("mdx" . 2)
                        ("md" . 1)))
         (readme-files (or (directory-files dir t "README\\.[a-z]+" t)
                           (directory-files dir t "readme\\.[a-z]+" t))))
    (seq-sort-by (lambda (it)
                   (or (cdr (assoc-string (file-name-extension (downcase it))
                                          score-alist))
                       -1))
                 #'>
                 readme-files)))

(defun projel-remove-link (str)
  "Remove markdown links from string STR."
  (with-temp-buffer
    (insert str)
    (while (re-search-backward
            "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:[^]^][^]]*\\|\\)\\(?4:\\]\\)[ ]?\\(?5:\\[\\)\\(?6:[^]]*?\\)\\(?7:\\]\\)"
            nil t 1)
      (let ((label (match-string-no-properties 3)))
        (if label
            (replace-match label nil nil nil 0))))
    (buffer-string)))

(defun projel-md-description ()
  "Extract annotation from FILE."
  (let ((found nil))
    (while (and
            (not found)
            (= 0 (forward-line)))
      (cond ((looking-at
              "\\(\s-*\\|^\\)[`][`][`]\\([^`]+\\)[`][`][`]")
             (re-search-forward
              "\\(\s-*\\|^\\)[`][`][`]\\([^`]+\\)[`][`][`]"
              nil t 1))
            ((looking-at "\\(\s-*\\|^\\)#\\+begin_")
             (re-search-forward "\\(\s-*\\|^\\)#\\+end_" nil t 1))
            ((let ((case-fold-search nil))
               (looking-at "^\\(\s-*\\|^\\)\\([A-Z`][a-z][^.]+\\)[.]"))
             (let ((case-fold-search nil))
               (let* ((beg (point))
                      (end
                       (or
                        (re-search-forward "[.]\n" nil t 1)
                        (re-search-forward
                         "^\\(\s-*\\|^\\)\\([A-Z`][a-z][^.]+\\)[.]" nil t 1)))
                      (annotation (buffer-substring-no-properties
                                   beg end)))
                 (setq found
                       annotation))))
            ((looking-at "\\(\s-*\\|^\\)[a-z]")
             (let* ((beg (point))
                    (end (progn
                           (forward-sentence 1)
                           (point)))
                    (annotation (buffer-substring-no-properties
                                 beg end)))
               (setq found
                     (when (string-match-p
                            "\\([a-z]+[\s\t]\\)+"
                            annotation)
                       annotation))))))
    (when found
      (string-join (split-string (projel-remove-link found)
                                 "[\n\r\f]" t)
                   "\s"))))

(defun projel-readme-annotation (files)
  "Extract annotation from FILES."
  (let ((result))
    (while (and files (not result))
      (let ((file (pop files)))
        (setq result
              (with-temp-buffer
                (erase-buffer)
                (insert-file-contents file)
                (goto-char (point-min))
                (projel-md-description)))))
    result))

(defvar projel-file-extensions-indicators
  '(("gradle" . "Java (Gradle)")
    ("hs" . "Haskel")
    ("cabal" . "Haskel (Cabal)")
    ("cpp" . "C")
    ("h" . "C")
    ("c" . "C")
    ("el" . "Emacs Lisp")))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(defun projel-read-json-string (str &optional object-type array-type null-object
                                    false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (when (progn
            (require 'json nil t)
            (fboundp 'json-read-from-string))
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null (or null-object :null))
            (json-false (or false-object :false)))
        (json-read-from-string str)))) )

(defun projel-read-json-buffer (&optional object-type array-type null-object
                                          false-object)
  "Parse current buffer with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and
       (fboundp 'json-available-p)
       (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (when (progn
            (require 'json nil t)
            (fboundp 'json-read))
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null (or null-object :null))
            (json-false (or false-object :false)))
        (json-read)))) )

(defun projel-read-json (file)
  "Parse FILE with natively compiled function or with json library.
If Emacs has libjansson support, parse it with natively compiled function,
otherwise use the parsing routines from the json library."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (projel-read-json-buffer)))

(defun projel-find-project-type-by-files ()
  "Return type of project by `projel-file-name-indicators'.
It checks whether current directory files includes first car in
`projel-file-name-indicators'."
  (let* ((dirs (directory-files default-directory nil
                                directory-files-no-dot-files-regexp
                                t))
         (found (cdr (seq-find (lambda (pfiles)
                                 (= (length (seq-intersection
                                             (car pfiles)
                                             dirs))
                                    (length (car pfiles))))
                               projel-file-name-indicators))))
    (when found
      (substring-no-properties found))))

(defun projel-get-project-by-files-extensions ()
  "Guess project type by file extension."
  (when-let ((found
              (seq-find
               (lambda (file)
                 (assoc-string
                  (file-name-extension
                   file)
                  projel-file-extensions-indicators))
               (delq nil (nconc (file-expand-wildcards
                                 "[a-z]+/[a-z-0-9]+\\.[a-z]+"
                                 nil
                                 t)
                                (file-expand-wildcards "[a-z-0-9]+\\.[a-z]+" nil
                                                       t))))))
    (substring-no-properties
     (cdr
      (assoc-string
       (file-name-extension
        found)
       projel-file-extensions-indicators)))))

(defvar projel-project-type-finders '(projel-find-project-type-by-files
                                      projel-get-project-by-files-extensions))

(defun projel-get-project-languages ()
  "Return PROJECT descriptions."
  (when (fboundp 'github-linguist-lookup)
    (github-linguist-lookup default-directory)))

(defun projel-get-project-type (project)
  "Return PROJECT descriptions."
  (let (target)
    (run-hook-wrapped
     'projel-project-type-finders
     (lambda (fun dir)
       (let ((default-directory dir))
         (setq target (ignore-errors (funcall fun)))))
     (file-name-as-directory (expand-file-name project)))
    target))

(defun projel-get-readme-annotation ()
  "Try to find annotation from readme file in current directory."
  (when-let ((readmes (projel-find-readme default-directory)))
    (projel-readme-annotation readmes)))

(defvar projel-project-descriptions-finders
  '(projel-get-readme-annotation
    projel-get-package-json-description))

(defun projel-get-from-package-json (file &rest keys)
  "Read json FILE and return list of values for KEYS."
  (when-let ((obj (ignore-errors (projel-read-json
                                  file))))
    (mapcar (lambda (key)
              (alist-get key obj))
            keys)))

(defun projel-get-package-json-description ()
  "Return description and name from package.json in current directory."
  (car (projel-get-from-package-json (concat
                                      default-directory
                                      "package.json")
                                     'description)))

(defun projel-get-project-descriptions (project)
  "Return PROJECT descriptions."
  (let (target)
    (run-hook-wrapped
     'projel-project-descriptions-finders
     (lambda (fun dir)
       (let ((default-directory dir))
         (setq target (ignore-errors (funcall fun)))))
     (file-name-as-directory (expand-file-name project)))
    target))

;;;###autoload
(defun projel-remove-all-projects ()
  "Remove all."
  (interactive)
  (setq project--list nil)
  (when (and (file-exists-p project-list-file)
             (yes-or-no-p (format "Remove %s?" project-list-file)))
    (delete-file project-list-file)))

(defun projel--setup-minibuffer (&optional keymap)
  "Setup minibuffer preview if value of `projel-auto-preview-delay' is a number.
If KEYMAP is non nil, also setup KEYMAP."
  (when (minibufferp)
    (remove-hook 'pre-command-hook
                 #'projel-auto-schedule-preview t)
    (remove-hook 'post-command-hook #'projel-restore-completions-wind)
    (add-hook 'minibuffer-exit-hook #'projel--kill-preview-buffer nil t)
    (when projel-auto-preview-delay
      (add-hook 'pre-command-hook
                #'projel-auto-schedule-preview nil t))
    (when keymap
      (use-local-map
       (make-composed-keymap keymap
                             (current-local-map))))))

(defun projel-setup-minibuffer ()
  "Setup `projel-minibuffer-map' in minibuffer and possible allow file preview.
See `projel-auto-preview-delay'."
  (projel--setup-minibuffer projel-minibuffer-map))

(defun projel-setup-projects-minibuffer ()
  "Setup minibuffer to use `projel-minibuffer-project-map' in minibuffer.
Also setup auto preview if value of `projel-auto-preview-delay' is a number."
  (projel--setup-minibuffer projel-minibuffer-project-map))

(defun projel-sort-files-alist (files)
  "Sort alist of FILES."
  (let ((sorted-files (seq-sort-by
                       (projel--or car-safe identity)
                       #'file-newer-than-file-p
                       files)))
    sorted-files))

(defun projel-sort-project (files)
  "Sort alist of FILES."
  (let* ((meta (projel-projects-action-candidates))
         (sorted-files (seq-sort
                        (lambda (a b)
                          (if (seq-find (apply-partially #'string= a) meta)
                              nil
                            (file-newer-than-file-p a b)))
                        files)))
    sorted-files))

(defun projel-group-fn (str &optional transform)
  "A function for grouping projects during the minibuffer completion.
STR is a completion candidate, and TRANSFORM is a boolean flag.
If TRANSFORM is nil, return the group title of the group to which the candidate
 belongs.  The returned title can also be nil.  Otherwise the function must
return the transformed candidate."
  (pcase str
    ((guard (projel-project-action-candidate-p str))
     (when transform str))
    ((guard (not transform))
     (propertize (abbreviate-file-name
                  (projel-file-name-parent-directory
                   str))
                 'face
                 'font-lock-constant-face))
    (_
     (file-name-nondirectory (directory-file-name str)))))

(defun projel-projects-action-candidates (&optional transform-fn)
  "Return completion candidates that indicates some action.
If TRANSFORM-FN is non nil, it wil be called with each candidate."
  (let ((strings (seq-filter #'stringp
                             (mapcar #'car projel-projects-actions-alist))))
    (if transform-fn
        (mapcar
         transform-fn
         strings)
      strings)))

(defun projel--read-file-cpd-relative (prompt all-files &optional predicate hist
                                              mb-default)
  "Apply `project--read-file-cpd-relative' with preview and keymap setup.
\\<projel-minibuffer-map>\\{projel-minibuffer-map}.

ALL-FILES is a list of possible file name completions.

PROMPT, PREDICATE and HIST have the same meaning as in `completing-read'.

MB-DEFAULT is used as part of \"future history\", to be inserted
by the user at will."
  (minibuffer-with-setup-hook
      #'projel-setup-minibuffer
    (project--read-file-cpd-relative prompt
                                     all-files
                                     predicate hist
                                     mb-default)))

(defun projel-project-action-candidate-p (completion)
  "Check whether COMPLETION is not real project directory, but an action."
  (seq-find (apply-partially #'string= completion)
            (projel-projects-action-candidates)))

(defun projel-current-project-root ()
  "Return project root directory."
  (when-let ((project (project-current)))
    (if (fboundp 'project-root)
        (project-root project)
      (with-no-warnings
        (car (project-roots project))))))

(defun projel-annotate-project-type (project-directory)
  "Return propertized string with project type for PROJECT-DIRECTORY or nil."
  (when-let ((str
              (projel-get-project-type
               project-directory)))
    (propertize (truncate-string-to-width
                 str
                 projel-annotation-project-type-max-width
                 nil ?\s "...")
                'face
                'projel-project-type-annotation)))

(defun projel-annotate-project-description (project-directory)
  "Return propertized string with description for PROJECT-DIRECTORY or nil."
  (when-let ((description (projel-get-project-descriptions project-directory)))
    (propertize (truncate-string-to-width
                 description
                 projel-annotation-project-description-max-width
                 nil nil "...")
                'face
                'projel-project-description-annotation)))


(defun projel-make-project-annotation-fn (alist)
  "Create annotation function for projects ALIST.
FMT is a format control string with 2 %s placeholders.
CURRENT-PROJECT is a current project directory or nil.
META-CANDIDATES is a list of strings that shouldn't be annotated."
  (let ((project-default-type
         (make-string projel-annotation-project-type-max-width ?\s))
        (meta-candidates (projel-projects-action-candidates))
        (fmt
         (let ((longest (if alist
                            (+ 5
                               (apply #'max (mapcar
                                             (projel--compose
                                               length
                                               car)
                                             alist)))
                          10)))
           (concat
            (propertize " " 'display
                        `(space
                          :align-to
                          ,longest))
            " %s"
            (propertize " " 'display
                        `(space
                          :align-to
                          ,(+ longest
                              projel-annotation-project-type-max-width)))
            " %s"))))
    (lambda (it)
      (cond ((seq-find
              (apply-partially #'string= it) meta-candidates)
             "")
            (t
             (let ((proj-type (projel-annotate-project-type it))
                   (descr (projel-annotate-project-description it)))
               (if (and (not proj-type)
                        (not descr))
                   ""
                 (format fmt
                         (or (projel-annotate-project-type it)
                             project-default-type)
                         (or (projel-annotate-project-description it)
                             "")))))))))

(defun projel-project--file-completion-table (alist &optional annotate-fn)
  "Completion table for project ALIST of projects with annotations ANNOTATE-FN."
  (let* ((annotfn (or annotate-fn
                      (projel-make-project-annotation-fn alist)))
         (meta (projel-projects-action-candidates))
         (sort-fn (lambda (files)
                    (sort
                     files
                     (lambda (a b)
                       (cond ((seq-find (apply-partially #'string= a)
                                        meta)
                              nil)
                             ((seq-find (apply-partially #'string= b) meta)
                              t)
                             (t (file-newer-than-file-p a b))))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotfn)
            (group-function . projel-group-fn)
            (display-sort-function . ,sort-fn)
            (category . project-root))
        (complete-with-action action alist str pred)))))



(defun projel-completing-read-project (&rest _)
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (let* ((pr-dir "")
         (action)
         (done))
    (projel-get-projects)
    (while (setq action
                 (unless done (catch 'action
                                (minibuffer-with-setup-hook
                                    #'projel-setup-projects-minibuffer
                                  (completing-read "Select project: "
                                                   (projel-project--file-completion-table
                                                    (append
                                                     (remove
                                                      (list
                                                       (projel-current-project-root))
                                                      project--list)
                                                     (projel-projects-action-candidates
                                                      (projel--compose
                                                        list
                                                        substring-no-properties))))
                                                   nil t)))))
      (cond ((or (listp action))
             (apply (car action)
                    (cdr action)))
            ((cdr (assoc action projel-projects-actions-alist))
             (funcall (cdr (assoc action projel-projects-actions-alist))))
            (t (setq pr-dir action)))
      (setq done (not (string-empty-p pr-dir))))
    pr-dir))

(defmacro projel-csetq (variable value)
  "Set the value of VARIABLE to VALUE using the appropriate set function."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defun projel-get-custom-value (sym)
  "Get saved or standard value for SYM."
  (or (car (get sym 'saved-value))
      (eval (car (get sym 'standard-value)))))



;;;###autoload (autoload 'projel-menu "projel.el" nil t)
(transient-define-prefix projel-menu ()
  "Menu for project commands."
  [:description
   (lambda ()
     (or (ignore-errors (project-name (project-current nil)))
         ""))
   [("p" "Switch Project" project-switch-project)
    ("D" "Dired" project-dired)
    ("f" "Find File" project-find-file)
    ("F" "Find File external roots" project-or-external-find-file)
    ("G" "Find Regexp" project-or-external-find-regexp)
    ("d" "Find Dir" project-find-dir)
    ("g" "Find Regexp" project-find-regexp)
    ("r" "Query Replace Regexp" project-query-replace-regexp)
    ("b" "Switch To Buffer" project-switch-to-buffer)
    ("C-b" "List Buffers" project-list-buffers)]
   [("c" "Compile" project-compile)
    ("e" "Eshell" project-eshell)
    ("k" "Kill Buffers" project-kill-buffers)
    ("v" "Run VC-Dir" project-vc-dir)
    ("s" "Shell" project-shell)
    ("!" "Shell Command" project-shell-command)
    ("&" "Async Shell Command" project-async-shell-command)
    ("x" "Execute Extended Command"
     project-execute-extended-command)]])

;;;###autoload (define-key ctl-x-map "p" 'projel-menu)

;;;###autoload
(defun projel-enable-project-override ()
  "Enable annotations in project list and additional keymap.

It will set next custom variables:
- `project-read-file-name-function' - to enable additional keymap:
\\<projel-minibuffer-map>\\{projel-minibuffer-map}.
- `project-prompter' - add annotations in project list and keymap:
 \\<projel-minibuffer-project-map>\\{projel-minibuffer-project-map}

If the variable `project-prompter' is not bound, it will advice
`project-prompt-project-dir' instead."
  (interactive)
  (projel-csetq project-read-file-name-function #'projel--read-file-cpd-relative)
  (if (not (boundp 'project-prompter))
      (advice-add 'project-prompt-project-dir :override
                  #'projel-completing-read-project)
    (projel-csetq project-prompter
                  #'projel-completing-read-project)))

;;;###autoload
(defun projel-disable-project-override ()
  "Disable project advising and reset changed values."
  (interactive)
  (projel-csetq project-read-file-name-function
                (projel-get-custom-value 'project-read-file-name-function))
  (if (not (boundp 'project-prompter))
      (advice-remove 'project-prompt-project-dir
                     #'projel-completing-read-project)
    (projel-csetq project-prompter (projel-get-custom-value 'project-prompter))))

;;;###autoload
(define-minor-mode projel-mode
  "Enable additinal setup for project when this mode is on.

Enable annotations to project list, and enable additional keymap.

This mode will set or reset next custom variables:
- `project-read-file-name-function' - to enable additional keymap:
\\<projel-minibuffer-map>\\{projel-minibuffer-map}.
- `project-prompter' - add annotations in project list and keymap:
 \\<projel-minibuffer-project-map>\\{projel-minibuffer-project-map}

If the variable `project-prompter' is not bound, it will advice
`project-prompt-project-dir' instead."
  :lighter " prjl"
  :global t
  :group 'project
  (if projel-mode
      (projel-enable-project-override)
    (projel-disable-project-override)))

(provide 'projel)
;;; projel.el ends here
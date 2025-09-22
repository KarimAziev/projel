;;; projel.el --- Enhanced project -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/projel
;; Version: 0.1.0
;; Keywords: project, convenience, vc
;; Package-Requires: ((emacs "29.1") (project "0.10.0") (transient "0.6.0"))
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

;; Minor mode that enhance Emacs builtin project.el with annotations, sorting,
;; grouping and additional keymaps. Support various completion systems -
;; default, ivy, fido, ido, icomplete etc.

;;; Code:

(require 'subr-x)
(require 'project)
(require 'transient)

(defcustom projel-projects-excluded-dirs '("~/Dropbox"
                                           "~/.cache"
                                           "~/.cask")
  "List of full directory names to omit when finding projects."
  :type '(repeat directory)
  :group 'projel)

(defcustom projel-allow-magit-repository-directories-sync t
  "Whether to add projects parents directory to `magit-repository-directories'."
  :type '(repeat directory)
  :group 'projel)


(defcustom projel-projects-exlcuded-non-directory-names '(".cache"
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


(defface projel-project-type-annotation '((t :inherit font-lock-keyword-face))
  "Face used by for project type annotations in minibuffer."
  :group 'projel)

(defface projel-project-description-annotation '((t :inherit
                                                    completions-annotations))
  "Face used by for project descriptions annotations in minibuffer."
  :group 'projel)

(defface projel-selected-project '((t :inherit underline))
  "Face used for the selected project in minibuffer.

It has effect only if this face is the value of
`projel-selected-project-minibuffer-indicator'."
  :group 'projel)

(defcustom projel-selected-project-minibuffer-indicator 'projel-selected-project
  "Indicator for the selected project in the minibuffer.

A string used as an indicator for the currently selected project in
the minibuffer. When set, this string will be appended to the name
of the current project to visually distinguish it from other
projects.

Possible values include:

- nil: No indicator will be shown.

- A string: The specified string will be used as the indicator.

This can help quickly identify the active project when switching
between multiple projects."
  :group 'projel
  :type '(radio
          (const :tag "None" nil)
          (string :tag "Suffix")
          (face :tag "Face")))

(defcustom projel-annotation-project-type-max-width 'full
  "Maximum width for displaying project type annotations.

Defines the maximum width for displaying project types in the
annotations.

Possible values are:

- `full': Automatically align to the longest project type.

- An integer: Specify a fixed width for the project type."
  :type '(radio
          (const :tag "Auto (align to the longest project type)" full)
          (integer :tag "Width"))
  :group 'projel)

(defcustom projel-annotation-project-description-max-width 120
  "The maximum length of the project description to display in minibuffer.

If the string exceeds this limit, it will be truncated to fit."
  :type '(radio
          (const :tag "Auto (align to the longest project type)" auto)
          (integer :tag "Width"))
  :group 'projel)

(defcustom projel-explore-project-depth 3
  "Maximum depth for searching projects in home directory."
  :type 'integer
  :group 'projel)

(defcustom projel-auto-rescan-on-init t
  "Whether to rescan projects on first projects read."
  :type 'boolean
  :group 'projel)

(defcustom projel-display-modified-time 'projel-format-time-readable
  "Whether to display modified time for projects.

If it is a nil, don't display modified time.

If it is a function, it will be called with one argument -
the modification time in the style of `current-time' and should return a string.

If it is a string, it should be the format-string for `format-time-string'."
  :type '(radio
          (const :tag "Don't display time" nil)
          (function-item projel-format-time-readable)
          (function-item projel-format-time-readable-long)
          (string
           :tag "Time Format"
           :value "%b %d %Y")
          (function :tag "Custom formatter"))
  :group 'projel)


(defcustom projel-metadata-file (locate-user-emacs-file
                                 "var/projel-projects-metadata")
  "File path for storing project metadata.

If set to nil, metadata will not be saved to a file.

The file should be readable and writable to ensure proper
functionality.

This variable is used by functions that read and write project
metadata to persist project information across sessions."
  :group 'projel
  :type '(choice (const :tag "Don't save" nil)
          file))


(defcustom projel-projects-actions-alist '(("*Rescan all projects*" . projel-rescan-all-projects)
                                           ("*Find projects in directory*" . projel-rescan-directory)
                                           ("*Add project*" . projel-add-project-directory)
                                           ("*Remove project*" . projel-remove-project)
                                           ("*Edit description*" . projel-edit-project-description)
                                           ("*Edit project type" . projel-edit-project-type))
  "Alist of project action labels and their corresponding functions.

An alist mapping action labels to their corresponding functions for
project management.

Each key is a string representing the label of the action, and each
value is a function symbol that performs the action.


To add a custom action, append a new pair to the list:

 \\=(add-to-list \\='projel-projects-actions-alist
               \\='(\"*Custom action*\" . my-custom-function))"
  :group 'projel
  :type '(alist
          :key-type (string :tag "Label")
          :value-type (choice (function-item projel-add-project-directory)
                       (function-item projel-rescan-directory)
                       (function-item projel-rescan-all-projects)
                       (function-item projel-remove-project)
                       (function :tag "Custom function"))))

(defcustom projel-preview-buffer-name "*projel-preview*"
  "Buffer name to use for preview file."
  :group 'projel
  :type 'string)

(defcustom projel-ensure-prompt-spaced t
  "Whether to ensure a space at the end of prompts.

Determines whether to automatically append a space to the end of prompts when
they do not already end with one.

When non-nil, any prompt lacking a trailing space will have one added. If nil,
prompts will be used as provided without modification.

The default value is t, which enables the addition of a space.

To change this behavior, set the value to nil."
  :group 'projel
  :type 'boolean)

(defcustom projel-projects-annotations-columns '((:type auto "%s" face
                                                  projel-project-type-annotation)
                                                 (:modified-time auto
                                                  projel-format-time-readable)
                                                 (:description auto "%s" face
                                                  projel-project-description-annotation))
  "A list of columns to display annotations for projects.

Each element in the list should be a plist specifying the column
type, width, format, and optionally text properties.

The structure of each column specification is as follows:

- type - The type of the column. This can be one of the following:
  - :type - Represents the project type.
  - :modified-time - Represents the last modified time of the project.
  - :description - Represents the project description.

- width - The width of the column. This can be:
  - auto - Automatically adjust the width based on the content.
  - An integer - A fixed width for the column.

- format - The format of the column content. This can be:
  - A string - A format string (e.g., \"%s\").
  - A function - A function to format the content. The function should accept a
    single argument, which is the value to be formatted, and return a formatted
    string.

- text properties."
  :group 'projel
  :type
  '(repeat
    (choice
     :tag "Column"
     :value
     (:description auto "%s" face projel-project-description-annotation)
     (list :tag "Project Type"
      (const :type)
      (choice
       :tag "Column Width"
       :value auto
       (const :tag "Auto" auto)
       (integer :tag "Fixed Width" 11))
      (radio :value "%s"
       (string :tag "Format" "%s")
       (function :tag "Formatter"))
      (set :inline t
       (list
        :format "%v"
        :inline t
        (const
         :format "" face)
        (face :tag "Face" projel-project-type-annotation)))
      (plist
       :tag "Text properties"
       :inline t))
     (list :tag "Modified Time"
      (const :modified-time)
      (choice
       :tag "Column Width"
       :value auto
       (const :tag "Auto" auto)
       (integer :tag "Fixed Width" 11))
      (radio :value projel-format-time-readable
       (string :tag "Format" "%b %d %Y")
       (function-item projel-format-time-readable)
       (function-item projel-format-time-readable-long)
       (function :tag "Formatter"))
      (set :inline t
       (list
        :format "%v"
        :inline t
        (const
         :format "" face)
        (face :tag "Face" projel-project-description-annotation)))
      (plist
       :tag "Text properties"
       :inline t))
     (list :tag "Description"
      (const :description)
      (choice
       :tag "Column Width"
       :value auto
       (const :tag "Auto" auto)
       (integer :tag "Fixed Width" 11))
      (radio :value "%s"
       (string :tag "Format" "%s")
       (function :tag "Formatter"))
      (set :inline t
       (list
        :format "%v"
        :inline t
        (const
         :format "" face)
        (face :tag "Face"
         projel-project-description-annotation)))
      (plist
       :tag "Text properties"
       :inline t)))))



(defcustom projel-file-name-indicators '((("dune-project") . "Dune-Project")
                                         (("elm.json") . "Elm")
                                         (("Cargo.toml") . "Rust")
                                         (("Gemfile") . "Ruby")
                                         ((".bloop") . "Scala")
                                         (("project.clj" ".midje.clj") . "Leiningen")
                                         (("project.clj" "package.json") . "Leiningen")
                                         (("project.clj") . "Leiningen")
                                         (("gradlew") . "Gradlew")
                                         (("Pipfile") . "Pipfile")
                                         (("early-init.el") . "Emacs Config")
                                         (("init.el") . "Emacs Config")
                                         (("Cask") . "Elisp")
                                         (("tsconfig.json") . "TypeScript")
                                         (("package.json") . "JavaScript")
                                         (("Makefile") . "Makefile")
                                         (("deps.edn") . "Clojure")
                                         (("project.clj" ".midje.clj") . "Clojure")
                                         (("project.clj") . "Clojure")
                                         (("poetry.lock") . "Python Poetry")
                                         (("pyproject.toml") . "Python Pyproject")
                                         (("Pipfile") . "Python Pipenv")
                                         (("setup.py") . "Python")
                                         (("requirements.txt") . "Python Pip")
                                         (("pubspec.yaml") . "Dart")
                                         (("info.rkt") . "Racket")
                                         (("stack.yaml") . "Haskell")
                                         (("shard.yml") . "Crystal")
                                         (("deps.edn") . "Clojure")
                                         (("build.boot") . "Clojure")
                                         (("build.sc") . "Scala")
                                         (("build.sbt") . "Scala")
                                         (("application.yml" "grails-app") . "Groovy")
                                         (("build.gradle") . "Groovy")
                                         (("pom.xml") . "Java")
                                         (("pyproject.toml") . "Python")
                                         (("poetry.lock") . "Python")
                                         (("tox.ini") . "Python")
                                         (("manage.py") . "Python Django")
                                         (("angular.json" ".angular-cli.json") . "JavaScript")
                                         (("gulpfile.js") . "JavaScript")
                                         (("Gruntfile.js") . "JavaScript")
                                         (("mix.exs") . "Elixir")
                                         (("rebar.config") . "Erlang")
                                         (("composer.json" "app" "src" "vendor") . "PHP")
                                         (("Taskfile.yml") . "Go")
                                         (("CMakeLists.txt") . "C/C++")
                                         (("flake.nix") . "Nix")
                                         (("default.nix") . "Nix")
                                         (("meson.build") . "C")
                                         (("Project.toml") . "Julia")
                                         (("elm.json") . "Elm")
                                         (("GNUMakefile") . "Makefile")
                                         (("debian/control") . "Debian Package")
                                         (("WORKSPACE") . "Bazel")
                                         (("SConstruct") . "SCons")
                                         (("build.xml") . "Ant")
                                         (("build.kts") . "Kotlin")
                                         (("build.gradle.kts") . "Kotlin")
                                         (("Gemfile.lock") . "Ruby")
                                         (("Rakefile") . "Ruby")
                                         (("Berksfile") . "Chef")
                                         (("Cheffile") . "Chef")
                                         (("metadata.rb") . "Chef")
                                         (("Vagrantfile") . "Vagrant")
                                         (("Dockerfile") . "Docker")
                                         (("docker-compose.yml") . "Docker Compose")
                                         (("Jenkinsfile") . "Jenkins")
                                         (("fastlane/Fastfile") . "Fastlane")
                                         (("Podfile") . "CocoaPods")
                                         (("Cartfile") . "Carthage")
                                         (("Brewfile") . "Homebrew")
                                         (("Conanfile.txt") . "Conan")
                                         (("conanfile.py") . "Conan")
                                         (("vcpkg.json") . "Vcpkg")
                                         (("vcpkg-configuration.json") . "Vcpkg")
                                         (("vcpkg-configuration.cmake") . "Vcpkg")
                                         (("vcpkg.json") . "Vcpkg")
                                         (("vcpkg-configuration.json") . "Vcpkg")
                                         (("vcpkg-configuration.cmake") . "Vcpkg"))
  "Alist mapping file names to project types.

A list of file name patterns and their corresponding project types.

Each element is a cons cell where the car is a list of file names
that indicate a specific project type, and the cdr is a string
representing the project type.

For example, if a directory contains a file named \"Cargo.toml\",
the project type will be identified as \"Rust\".

This list is used to determine the type of a project based on the
presence of specific files in the project's root directory."
  :group 'projel
  :type '(alist
          :key-type
          (repeat string)
          :value-type string))

(defcustom projel-file-extensions-indicators '(("gradle" . "Java (Gradle)")
                                               ("hs" . "Haskel")
                                               ("cabal" . "Haskel (Cabal)")
                                               ("cpp" . "C")
                                               ("h" . "C")
                                               ("c" . "C"))
  "Alist mapping file extensions to their corresponding project indicators.

A list of file extensions and their corresponding project type
indicators. Each element is a cons cell where the car is a file
extension (string) and the cdr is a string describing the project
type.

This list is used to determine the type of project based on the
file extensions present in the project directory. For example,
a file with the extension \"gradle\" will be identified as a \"Java
\\=(Gradle)\" project."
  :group 'projel
  :type '(alist
          :key-type string
          :value-type string))

(defcustom projel-project-type-finders '(projel-get-project-by-files-extensions
                                         projel-find-project-type-by-files)
  "List of functions to determine the project type by examining project files.

A list of functions used to determine the type of a project.

Each function in the list should accept a single argument, the
project directory, and return a symbol representing the project
type if it can determine it, or nil otherwise.

The functions are executed in order until one returns a non-nil
value, which is then used as the project type."
  :group 'projel
  :type 'hook)

(defcustom projel-minibuffer-group-fn 'projel-group-by-parent-directory
  "Function to group project files in the minibuffer.

Function to group project candidates in the minibuffer.

The value can be one of the following:

- `projel-group-by-parent-directory': Group projects by their parent directory.

- `projel-group-by-project-type': Group projects by their type.

- nil: Do not group projects.

- A custom function: Provide a custom grouping function.

The custom function should accept a list of project candidates and return a list
of groups. Each group should be a cons cell where the car is the group name and
the cdr is a list of project candidates belonging to that group."
  :group 'projel
  :type '(radio
          (function-item projel-group-by-parent-directory)
          (function-item projel-group-by-project-type)
          (const :tag "Don't group" nil)
          (function "Custom function")))

(defcustom projel-file-sorting-threshold 5000
  "Threshold for enabling sorting in project files.

If the number of project files exceeds this threshold, sorting will be
inhibited.

Set this to nil to always allow sorting, or to a numeric value to
specify the threshold."
  :group 'projel
  :type '(radio
          (natnum :tag "Threshold")
          (const :tag "Always allow sorting" nil)))

(defcustom projel-file-annotation-threshold 5000
  "Threshold for enabling annotations in project files.

If the number of project files exceeds this threshold, annotations will be
inhibited.

Set this to nil to always allow annotations or to a numeric value to specify the
threshold."
  :group 'projel
  :type '(radio
          (natnum :tag "Threshold")
          (const :tag "Always allow annotations" nil)))

(defvar projel-projects-cache (make-hash-table :test #'equal))
(defvar projel-projects-metadata-cache (make-hash-table :test #'equal))

(defvar projel--reading-project nil)
(defvar projel--metadata-loaded nil)

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

(defmacro projel--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (indent defun)
           (side-effect-free t))
  `(projel--pipe ,@(reverse functions)))

(eval-and-compile
  (defun projel--expand (init-fn)
    "If INIT-FN is a non-quoted symbol, add a sharp quote.
Otherwise, return it as is."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defmacro projel--use-with (combine-fn &rest functions)
  "Return a function with the arity of length FUNCTIONS.
Call every branching function with an argument at the same index,
and finally, COMBINE-FN will be applied to the supplied values.

Example:

\(funcall (projel--use-with concat [upcase downcase]) \"hello \" \"world\")


If first element of FUNCTIONS is vector, it will be used instead:

\(funcall (projel--use-with + [(fp-partial 1+) identity]) 2 2)
=> Result: 5

\(funcall (projel--use-with + (fp-partial 1+) identity) 2 2)
=> Result: 5

=> Result: \"HELLO world\"."
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       (apply
        ,@(projel--expand combine-fn)
        (list
         ,@(seq-map-indexed (lambda (v idx)
                              `(funcall ,@(projel--expand v)
                                (nth ,idx ,args)))
            (if (vectorp (car functions))
                (append (car functions) nil)
              functions)))))))

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

(defmacro projel-cond (&rest pairs)
  "Return a function that expands a list of PAIRS to cond clauses.
Every pair should be either:
- a vector of [predicate transformer],
- a list of (predicate transformer).

The predicate can also be t.

All of the arguments to function are applied to each of the predicates in turn
until one returns a \"truthy\" value, at which point fn returns the result of
applying its arguments to the corresponding transformer."
  (declare (pure t)
           (indent defun)
           (side-effect-free error-free))
  (let ((args (make-symbol "args")))
    `(lambda (&rest ,args)
       (cond ,@(mapcar (lambda (v)
                         (list (if (eq (aref v 0) t) t
                                `(apply ,@(projel--expand (aref v 0)) ,args))
                          `(apply ,@(projel--expand (aref v 1)) ,args)))
                pairs)))))

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
    projel--vertico-selected
    projel-minibuffer-default-top-minibuffer-completion))

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--update "ext:vertico")

(defun projel--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when (bound-and-true-p vertico--input)
    (vertico--update)
    (cons (completion-metadata-get (projel-minibuffer-get-metadata) 'category)
          (vertico--candidate))))


(defun projel-minibuffer-current-candidate ()
  "Return cons with category and current completion candidate in minibuffer."
  (let (target)
    (run-hook-wrapped
     'projel-minibuffer-targets-finders
     (lambda (fun)
       (when-let* ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr result))
                    (not (string-empty-p (cdr result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun projel-restore-completions-wind ()
  "Restore *Completions* window height."
  (when (memq this-command '(minibuffer-next-completion
                             minibuffer-previous-completion))
    (remove-hook 'post-command-hook #'projel-restore-completions-wind)
    (when-let* ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun projel-current-minibuffer-file ()
  "Return absolute filename for current minibuffer candidate or nil."
  (pcase-let* ((`(,_category . ,current)
                (projel-minibuffer-current-candidate)))
    (cond ((or (projel-project-action-candidate-p current)
               (string-empty-p current))
           nil)
          ((file-exists-p current)
           (expand-file-name current))
          (t
           (when-let* ((root (or
                              (with-minibuffer-selected-window
                                project-current-directory-override)
                              project-current-directory-override
                              (projel-current-project-root)))
                       (file (expand-file-name current root)))
             (when (file-exists-p file)
               file))))))


;;;###autoload
(defun projel-find-file-other-window ()
  "Invoke action to find selected file in minibuffer in other window."
  (interactive)
  (when-let* ((file (projel-current-minibuffer-file)))
    (run-with-timer 0 nil #'find-file-other-window file)
    (abort-minibuffers)))

;;;###autoload
(defun projel-preview-file ()
  "Invoke action to preview current file in minibuffer in other window."
  (interactive)
  (when-let* ((file (projel-current-minibuffer-file)))
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
  (when-let* ((buff (get-buffer projel-preview-buffer-name)))
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
    (let ((non-essential t))
      (let ((found-dirs))
        (let
            ((default-directory (expand-file-name (file-name-as-directory dir))))
          (dolist
              (curr
               (directory-files default-directory nil
                                directory-files-no-dot-files-regexp t))
            (let ((full-dir (expand-file-name curr))
                  (tramp-archive-enabled nil))
              (cond ((not (file-accessible-directory-p full-dir)))
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
                     (when-let* ((subdirs (projel-find-in-dir full-dir
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
                         (list (apply-partially #'string-match-p
                                                vc-ignore-dir-regexp))
                         vc-directory-exclusion-list
                         projel-projects-exlcuded-non-directory-names
                         (mapcar
                          (apply-partially #'apply-partially 'file-equal-p)
                          projel-projects-excluded-dirs))
                        max-depth
                        (lambda (dir)
                          (let ((proj
                                 (file-name-as-directory (abbreviate-file-name
                                                          dir))))
                            (unless (or (assoc-string proj
                                                      project--list)
                                        (assoc-string proj results))
                              (setq results (push (list proj) results)))
                            proj)))
    (when results
      (setq project--list (append results project--list))
      (when write
        (projel--write-project-list)))
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


(defun projel--maybe-format-list (results max-count &optional face)
  "Return formatted string from RESULTS if its length is within MAX-COUNT.

Argument RESULTS is a list whose elements are to be formatted.

Argument MAX-COUNT is the maximum number of items permitted in RESULTS
for processing to occur.

Optional argument FACE is used to apply a face property to the
formatted string if provided."
  (when (<= (length results) max-count)
    (let ((str (mapconcat (apply-partially #'format "%s") results ", ")))
      (if (not face)
          str
        (propertize str 'face face)))))

(defun projel-rescan-all (&optional depth no-write)
  "Rescan directories, update project list, and sync with Magit if enabled.

Optional argument DEPTH is an integer specifying the maximum depth to search for
projects. It defaults to the value of `projel-explore-project-depth' if not
provided.

Optional argument NO-WRITE is a boolean that, when non-nil, prevents the
function from writing the updated project list to disk. It defaults to nil."
  (let ((non-essential t)
        (existing-projects
         (seq-filter (projel--compose file-exists-p car)
                     project--list))
        (dead-projects))
    (setq dead-projects
          (when (> (length project--list)
                   (length existing-projects))
            (seq-remove (pcase-lambda (`(,k . ,_))
                          (assoc k existing-projects))
                        project--list)))
    (when dead-projects
      (setq project--list existing-projects))
    (let ((dirs (projel-get-projects-parent-dirs))
          (results))
      (dolist (dir (or dirs (list "~/")))
        (message "Rescanning %s" dir)
        (when-let* ((res
                     (when (file-exists-p dir)
                       (projel-find-projects-in-dir
                        dir
                        (or depth
                            (if dirs
                                1
                              projel-explore-project-depth))
                        nil))))
          (setq results
                (append results res))
          (when (and projel-allow-magit-repository-directories-sync
                     (boundp 'magit-repository-directories))
            (add-to-list 'magit-repository-directories (cons dir 1)))))
      (when (or dead-projects results)
        (unless no-write
          (projel--write-project-list))
        (when-let* ((msg
                     (delq nil
                           (list
                            (when results
                              (let ((suffix
                                     (projel--maybe-format-list
                                      (mapcar #'car results) 3
                                      'font-lock-constant-face))
                                    (len (length results)))
                                (concat (format "Added %d project%s"
                                                len
                                                (if (= len 1)
                                                    ""
                                                  "s"))
                                        (when suffix ": ") suffix)))
                            (when dead-projects
                              (let ((suffix
                                     (projel--maybe-format-list
                                      (mapcar #'car dead-projects) 3
                                      'font-lock-constant-face))
                                    (len (length dead-projects)))
                                (concat (format "Removed %d project%s"
                                                (length dead-projects)
                                                (if (= len 1)
                                                    ""
                                                  "s"))
                                        (when suffix ": ") suffix)))))))
          (setq msg (concat "Projel: " (string-join msg ", ")))
          (message msg))))
    (clrhash projel-projects-cache)))

(defun projel--write-project-list ()
  "Save `project--list' in `project-list-file'."
  (let ((filename project-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (insert (prin1-to-string (mapcar (pcase-lambda (`(,k . ,v))
                                           (cons (substring-no-properties k) v))
                                         project--list))))
      (write-region nil nil filename nil 'silent))))

(defun projel--ensure-read-project-list ()
  "Initialize `project--list' if it isn't already initialized."
  (when (eq project--list 'unset)
    (project--read-project-list)))

(defun projel-get-projects ()
  "Init `project--list'.
If `projel-auto-rescan-on-init' is enabled and `project--list' is not initted,
rescan parent directories of projects.
Also check and remove unexisting projects."
  (unless projel--metadata-loaded
    (setq projel--metadata-loaded t)
    (projel--read-projects-metadata))
  (if (or
       (and projel-auto-rescan-on-init
            (eq project--list 'unset)))
      (progn
        (projel--ensure-read-project-list)
        (projel-rescan-all (if project--list
                               1
                             projel-explore-project-depth)))
    (projel--ensure-read-project-list)
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
          (projel--write-project-list))
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
  "Return a list of unique parent directories of current projects."
  (projel--ensure-read-project-list)
  (let ((parents))
    (pcase-dolist (`(,proj . ,_v) project--list)
      (when-let* ((project (ignore-errors (project-current
                                           nil proj)))
                  (root (if (fboundp 'project-root)
                            (project-root project)
                          (with-no-warnings
                            (car (project-roots project))))))
        (when (equal proj root)
          (when-let* ((parent (projel-file-name-parent-directory root)))
            (while
                (let ((default-directory parent))
                  (projel-current-project-root))
              (setq parent (projel-file-name-parent-directory parent)))
            (unless (member parent parents)
              (push parent parents))))))
    parents))

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
  (interactive (list (projel--read-project-for-action)))
  (if projel--reading-project
      (throw 'action (list 'project--remove-from-project-list
                           (or project-dir
                               (projel--read-project-for-action))
                           "Project `%s' removed from known projects"))
    (project--remove-from-project-list
     (or project-dir
         (projel--read-project-for-action))
     "Project `%s' removed from known projects")))


;;;###autoload
(defun projel-edit-project-description (&optional project-dir)
  "Edit the description of a project specified by PROJECT-DIR.

Optional argument PROJECT-DIR is the directory of the project to edit."
  (interactive (list (projel--read-project-for-action)))
  (if projel--reading-project
      (throw 'action (list (apply-partially #'projel--edit-project-field
                                            :description)
                           project-dir
                           "Project `%s' edited"))
    (projel--edit-project-field :description
                                (or project-dir
                                    (projel--read-project-for-action))
                                "Project `%s' edited")))

;;;###autoload
(defun projel-edit-project-type (&optional project-dir)
  "Edit the project type for a given project directory.

Optional argument PROJECT-DIR is the directory of the project to edit."
  (interactive (list (projel--read-project-for-action)))
  (if projel--reading-project
      (throw 'action (list (apply-partially #'projel--edit-project-field
                                            :type)
                           project-dir
                           "Project `%s' edited"))
    (projel--edit-project-field :type (or project-dir
                                          (projel--read-project-for-action))
                                "Project `%s' edited")))

;;;###autoload
(defun projel-add-project-directory ()
  "Either dispatch or add directory PROJECT-DIR from the project list.

If active minibuffer window exists, it will throw corresponding action and
its arguments.

If no active minibuffer window exists, it will prompt directory, and add
it to list of projects."
  (interactive)
  (if projel--reading-project
      (throw 'action
             (list (projel--compose
                     #'projel--add-project
                     (apply-partially #'projel--read-new-project-directory
                                      "Project directory: "))))
    (projel--add-project (projel--read-new-project-directory
                          "Project directory: "))))

;;;###autoload
(defun projel-rescan-directory (dir depth)
  "Dispatch or perform action to add projects in DIR at max DEPTH.
If DIR or DEPTH is nil it will read DIR and DEPTH in minibuffer.

If active minibuffer window exists throw the action
`projel-find-projects-in-dir' with DIR and DEPTH, otherwise invoke this action
with DIR and DEPTH."
  (interactive (list (read-directory-name "Directory: ")
                     (read-number "Max depth: ")))
  (if projel--reading-project
      (throw 'action (list #'projel-find-projects-in-dir dir depth))
    (projel-find-projects-in-dir dir depth)))


;;;###autoload
(defun projel-rescan-all-projects ()
  "Rescan parent directories of current projects."
  (interactive)
  (if projel--reading-project
      (throw 'action (list 'projel-rescan-all))
    (projel-rescan-all)))

(defvar projel-minibuffer-project-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map projel-minibuffer-map)
    (define-key map (kbd "C-c C-u") #'projel-rescan-all-projects)
    (define-key map (kbd "C-c M-D") #'projel-remove-project)
    (define-key map (kbd "C-c M-d") #'projel-remove-project)
    (define-key map (kbd "C-c C-e d") #'projel-edit-project-description)
    (define-key map (kbd "C-c C-e t") #'projel-edit-project-type)
    (define-key map (kbd "C-c !") #'projel-rescan-directory)
    (define-key map (kbd "C-c C-M-o") #'projel-add-project-directory)
    (define-key map (kbd "C-h ?") #'projel-projects-minibuffer-help)
    map)
  "Keymap to use in minibuffer when reading projects.")


(defun projel--get-command-key (sym)
  "Return the key description for the command symbol SYM.

Argument SYM is the symbol representing the command to find the key binding for."
  (let ((k (where-is-internal sym
                              nil
                              t)))
    (when (and k (eq sym (key-binding
                          k)))
      (let ((i (cl-search [?\C-x
                           ?6]
                          k)))
        (when i
          (let ((dup
                 (vconcat (substring
                           k 0
                           i)
                          [f2]
                          (substring
                           k
                           (+
                            i
                            2))))
                (map
                 (current-global-map)))
            (when (equal (lookup-key
                          map k)
                         (lookup-key
                          map dup))
              (setq k dup)))))
      (key-description
       k))))

(defun projel--format-keymap (map)
  "Format a keymap into a string with aligned keys and command names.

Argument MAP is a keymap or a list of keymaps to be formatted."
  (let ((alist)
        (col-len 0))
    (dolist (sym (delete-dups
                  (seq-filter #'commandp
                              (flatten-list
                               map))))
      (when-let* ((key (projel--get-command-key sym)))
        (setq col-len (max (length key) col-len))
        (push (list key sym) alist)))
    (mapconcat
     (pcase-lambda (`(,k ,v))
       (concat k (make-string (- col-len (length k)) ?\s) " " (format
                                                               "%s" v)))
     alist
     "\n")))

(defun projel-projects-minibuffer-help ()
  "Show keybindings for `projel-minibuffer-project-map'."
  (interactive)
  (let ((overlay))
    (unwind-protect
        (progn (setq overlay (make-overlay (window-start)
                                           (1+ (window-start))))
               (overlay-put overlay 'before-string
                            (concat "\n"
                                    (projel--format-keymap
                                     projel-minibuffer-project-map)
                                    "\n"))
               (read-key-sequence ""))
      (setq unread-command-events
            (append (this-single-command-raw-keys)
                    unread-command-events))
      (when (overlayp overlay)
        (delete-overlay overlay)))))


(defun projel--edit-project-field (field proj &optional report-message)
  "Edit the specified FIELD of the project PROJ and optionally report a message.

Argument FIELD is the project field to be edited, which can be a keyword or a
string.

Argument PROJ is the project identifier, typically a string.

Optional argument REPORT-MESSAGE is a message to be displayed after editing the
field."
  (let* ((pl (gethash proj projel-projects-metadata-cache))
         (new-descr (read-string (format "Edit %s in `%s': " (if
                                                                 (keywordp field)
                                                                 (substring-no-properties
                                                                  (symbol-name
                                                                   field)
                                                                  1)
                                                               field)
                                         proj)
                                 (plist-get
                                  (gethash proj
                                           projel-projects-metadata-cache)
                                  field)
                                 (plist-get
                                  (gethash proj
                                           projel-projects-cache)
                                  field))))
    (setq pl (plist-put pl field new-descr))
    (puthash proj pl projel-projects-metadata-cache)
    (projel--write-metadata)
    (when report-message
      (apply #'message (if (string-match-p "%s" report-message)
                           (list report-message proj)
                         (list report-message))))))

(defun projel--read-project-for-action ()
  "Prompt the user to select a project from a list, with completion."
  (if projel--reading-project
      (pcase-let
          ((`(,_category . ,current)
            (projel-minibuffer-current-candidate)))
        (if (and current
                 (file-exists-p current))
            current
          (user-error "A project %s doesn't exists" current)))
    (projel-get-projects)
    (completing-read "Select project: "
                     (projel--projects-completion-table
                      project--list)
                     nil
                     t)))

(defun projel--add-project (dir)
  "Add a project from DIR to the project list if it exists.

Argument DIR is the directory to search for a project."
  (if-let* ((pr (project--find-in-directory dir)))
      (project-remember-project pr)
    (message "Project is not found in `%s'" (abbreviate-file-name dir))))

(defun projel--get-guess-new-project-dirs ()
  "Return a list of new project directories guessed from known project roots."
  (let* ((non-essential t)
         (projects-dirs (project-known-project-roots))
         (parents)
         (dirs))
    (dolist (proj-dir projects-dirs)
      (when-let* ((parent (file-name-parent-directory proj-dir)))
        (unless (member parent parents)
          (push parent parents))))
    (dolist (parent parents)
      (dolist (dir (directory-files
                    parent t
                    directory-files-no-dot-files-regexp
                    t))
        (when (file-directory-p dir)
          (setq dir (file-name-as-directory
                     dir))
          (when
              (let ((default-directory dir))
                (projel-current-project-root))
            (let ((abbreviated (abbreviate-file-name dir)))
              (unless (or
                       (member abbreviated dirs)
                       (member abbreviated projects-dirs)
                       (not (file-accessible-directory-p abbreviated))
                       (not (project--find-in-directory abbreviated)))
                (push abbreviated dirs)))))))
    dirs))

(defvar projel-minibuffer-directory-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map projel-minibuffer-map)
    (define-key map (kbd "C->") #'projel-throw-next-source)
    (define-key map (kbd "C-<") #'projel-throw-prev-source)
    map)
  "Keymap to use in minibuffer when reading new projects.")

(defun projel-throw-next-source ()
  "Throw the symbol `next-source' using the tag `action'."
  (interactive)
  (throw 'action 'next-source))

(defun projel-throw-prev-source ()
  "Throw the `prev-source' action."
  (interactive)
  (throw 'action 'prev-source))

(defun projel--read-new-project-directory (prompt &optional dir default-dirname
                                                  initial)
  "PROMPT for a new project directory, with optional INITIAL directory and name.

Argument PROMPT is the string to prompt the user with.

Optional argument DIR is the directory to start in.

Optional argument DEFAULT-DIRNAME is the default directory name.

Optional argument INITIAL is the initial input."
  (unless dir
    (setq dir (if-let* ((proj
                        (projel-current-project-root)))
                  (projel-file-name-parent-directory proj)
                default-directory)))
  (let*
      ((guessed-dirs)
       (sources
        (list
         (apply-partially #'read-directory-name prompt dir
                          default-dirname t initial)
         (lambda ()
           (completing-read prompt
                            (or guessed-dirs
                                (setq guessed-dirs
                                      (projel--get-guess-new-project-dirs))))))))
    (let ((action)
          (done)
          (pr-dir)
          (idx 0))
      (while
          (setq action
                (unless done (catch 'action
                               (minibuffer-with-setup-hook
                                   (lambda ()
                                     (use-local-map
                                      (make-composed-keymap projel-minibuffer-directory-map
                                                            (current-local-map))))
                                 (funcall (nth idx sources))))))
        (pcase action
          ('next-source (setq idx (if (nth (1+ idx) sources)
                                      (1+ idx)
                                    0)))
          ('prev-source (setq idx (if (zerop idx)
                                      (1- (length sources))
                                    (1- idx))))
          (_ (setq pr-dir action)
             (setq done (not (string-empty-p pr-dir))))))
      pr-dir)))


(defun projel-add-current-project-dir-to-projects-maybe ()
  "Add the current project directory to the project list if not already present."
  (when-let* ((pr (project--find-in-directory default-directory)))
    (project--ensure-read-project-list)
    (unless (member (project-root pr) project--list)
      (project-remember-project pr)
      (message "Added new project `%s'" pr))))

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
  (let* ((non-essential t)
         (tramp-archive-enabled nil)
         (score-alist '(("org" . 1)
                        ("mdx" . 2)
                        ("md" . 3)))
         (re (concat "\\`readme\\(\\."
                     (regexp-opt (mapcar #'car score-alist))
                     "\\)?\\'"))
         (files (directory-files dir nil
                                 directory-files-no-dot-files-regexp
                                 t))
         (case-fold-search t)
         (readme-files
          (seq-filter (apply-partially #'string-match-p re)
                      files)))
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
            "\\[\\([^]]+\\)\\]([^)]+)"
            nil t 1)
      (let ((label (match-string-no-properties 1)))
        (when label
          (replace-match label nil nil nil 0))))
    (goto-char (point-max))
    (while (re-search-backward "\\[\\[[^]]+]\\[\\([^]]+\\)]]" nil t
                               1)
      (let ((label (match-string-no-properties 1)))
        (when label
          (replace-match label nil nil nil 0))))
    ;; (while (re-search-backward
    ;;         "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:[^]^][^]]*\\|\\)\\(?4:\\]\\)[ ]?\\(?5:\\[\\)\\(?6:[^]]*?\\)\\(?7:\\]\\)"
    ;;         nil t 1)
    ;;   (let ((label (match-string-no-properties 3)))
    ;;     (if label
    ;;         (replace-match label nil nil nil 0))))
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

(defun projel--serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projel--unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (insert (prin1-to-string data))))))

(defun projel--unserialize (filename)
  "Read data serialized by `projel--serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defun projel--read-projects-metadata ()
  "Load the data from the file."
  (let ((alist (and projel-metadata-file
                    (file-readable-p projel-metadata-file)
                    (projel--unserialize projel-metadata-file))))
    (pcase-dolist (`(,k . ,v) alist)
      (puthash k v projel-projects-metadata-cache))))

(defun projel--write-metadata ()
  "Write the data to the file."
  (when (and projel-metadata-file
             (not (hash-table-empty-p projel-projects-metadata-cache)))
    (let ((parent-dir (file-name-directory projel-metadata-file)))
      (unless (file-exists-p parent-dir)
        (make-directory parent-dir t))
      (projel--serialize
       (let ((alist nil))
         (maphash (lambda (k v)
                    (push (cons (substring-no-properties k)
                                (mapcar (lambda (it)
                                          (if (stringp it)
                                              (substring-no-properties it)
                                            it))
                                        v))
                          alist))
                  projel-projects-metadata-cache)
         alist)
       projel-metadata-file))))


(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

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
       :null-object null-object
       :false-object false-object)
    (when (progn
            (require 'json nil t)
            (fboundp 'json-read))
      (let ((json-object-type (or object-type 'alist))
            (json-array-type
             (pcase array-type
               ('list 'list)
               ('array 'vector)
               (_ 'vector)))
            (json-null null-object)
            (json-false false-object))
        (json-read)))))

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
  (when-let* ((found
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


(defun projel-get-project-languages ()
  "Retrieve programming languages used in the current project directory."
  (require 'github-linguist nil t)
  (when (fboundp 'github-linguist-lookup)
    (when-let* ((langs (github-linguist-lookup default-directory)))
      (mapconcat #'car (seq-filter (pcase-lambda (`(,_k . ,v))
                                     (>= v 15))
                                   langs)
                 "/"))))

(defun projel-get-project-type (project)
  "Determine PROJECT type by running hooks on project directory.

Argument PROJECT is the directory or file path for which to determine the
PROJECT type."
  (let ((default-directory (file-name-as-directory (expand-file-name project))))
    (run-hook-with-args-until-success 'projel-project-type-finders)))

(defun projel-get-readme-annotation ()
  "Try to find annotation from readme file in current directory."
  (when-let* ((readmes (projel-find-readme default-directory)))
    (projel-readme-annotation readmes)))

(defvar projel-project-descriptions-finders
  '(projel-get-package-json-description
    projel-get-readme-annotation))

(defun projel-get-from-package-json (file &rest keys)
  "Read json FILE and return list of values for KEYS."
  (when-let* ((obj (ignore-errors (projel-read-json
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
  "Retrieve PROJECT descriptions using registered finder functions.

Argument PROJECT is a string representing the path to the project directory."
  (let ((default-directory (file-name-as-directory (expand-file-name project))))
    (run-hook-with-args-until-success
     'projel-project-descriptions-finders)))

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


(defun projel-group-by-parent-directory (str &optional transform)
  "A function for grouping projects during the minibuffer completion.

STR is a completion candidate, and TRANSFORM is a boolean flag.

If TRANSFORM is nil, return the group title of the group to which the candidate
 belongs.  The returned title can also be nil.  Otherwise the function must
return the transformed candidate."
  (pcase str
    ((guard (projel-project-action-candidate-p str))
     (if transform str "Actions"))
    ((guard (not transform))
     (propertize (abbreviate-file-name
                  (projel-file-name-parent-directory
                   str))
                 'face
                 'font-lock-constant-face))
    (_
     (file-name-nondirectory (directory-file-name str)))))

(defun projel-group-by-project-type (str &optional transform)
  "A function for grouping projects by type during minibuffer completion.

STR is a completion candidate, and TRANSFORM is a boolean flag.

If TRANSFORM is nil, return the group title of the group to which the candidate
 belongs.  The returned title can also be nil.  Otherwise the function must
return the transformed candidate."
  (pcase str
    ((guard (projel-project-action-candidate-p str))
     (if transform str "Actions"))
    ((guard (not transform))
     (or
      (plist-get (gethash str projel-projects-metadata-cache) :type)
      (plist-get (gethash str projel-projects-cache) :type)
      "Unknown"))
    (_
     str)))

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

(defun projel--format-prompt (prompt)
  "Ensure PROMPT ends with a space, unless configured otherwise.

Argument PROMPT is a string used as the prompt text."
  (if (or (not projel-ensure-prompt-spaced)
          (string-suffix-p " " prompt))
      prompt
    (concat prompt " ")))


(defvar org-directory)

;;;###autoload
(defun projel-find-org-file (&optional arg)
  "Open an org file from the `org-directory'.

With optional argument ARG is non nil, open file in another window."
  (interactive "P")
  (require 'org)
  (let*
      ((non-essential t)
       (buff-file buffer-file-name)
       (file
        (projel--read-files-in-dir-relative "File: " org-directory
                                            (directory-files-recursively
                                             org-directory
                                             "\\.org\\'")
                                            (when buff-file
                                              (lambda (name)
                                                (not
                                                 (file-equal-p name buff-file)))))))
    (funcall (if arg #'find-file-other-window
               #'find-file)
             file)))

(defun projel--read-files-in-dir-relative (prompt proj-root all-files &optional
                                                  predicate hist mb-default)
  "Read a file name in PROJ-ROOT, prompting with PROMPT.

ALL-FILES is a list of possible file name completions.

PREDICATE and HIST have the same meaning as in `completing-read'.

MB-DEFAULT is used as part of \"future history\", to be inserted
by the user at will.
\\<projel-minibuffer-map>\\{projel-minibuffer-map}."
  (let*
      ((non-essential t)
       (mb-default
        (mapcar
         (lambda (mb-default)
           (cond ((not mb-default) nil)
                 ((and
                   mb-default
                   (file-name-absolute-p mb-default)
                   (file-exists-p mb-default))
                  mb-default)
                 ((file-exists-p (expand-file-name mb-default
                                                   default-directory))
                  (expand-file-name mb-default default-directory))
                 ((and proj-root (file-exists-p (expand-file-name
                                                 mb-default proj-root)))
                  (expand-file-name mb-default proj-root))))
         (if (listp mb-default) mb-default (list mb-default))))
       (project-current-directory-override (file-name-as-directory
                                            proj-root))
       (table
        (projel--project-files-completion-table all-files
                                                project-current-directory-override)))
    (expand-file-name
     (minibuffer-with-setup-hook
         #'projel-setup-minibuffer
       (completing-read (projel--format-prompt prompt)
                        table
                        predicate 'confirm
                        nil
                        hist
                        mb-default))
     project-current-directory-override)))

(defun projel--completing-read-strict (prompt collection &optional predicate
                                              hist mb-default)
  "Read a string in the minibuffer with completion, enforcing strict selection.

Argument PROMPT is the string to prompt the user.

Argument COLLECTION is the list of strings to complete against.

Optional argument PREDICATE is a function to filter COLLECTION.

Optional argument HIST is the history list to use.

Optional argument MB-DEFAULT is the default value for the minibuffer."
  (let ((table (projel--project-files-completion-table collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (projel-setup-minibuffer)
          (setq-local minibuffer-default-add-function
                      (lambda ()
                        (let ((minibuffer-default mb-default))
                          (minibuffer-default-add-completions)))))
      (completing-read (format "%s: " prompt)
                       table
                       predicate 'confirm
                       nil
                       hist))))

(defun projel--read-file-cpd-relative (prompt all-files &optional predicate hist
                                              mb-default)
  "Read a file name, prompting with PROMPT.

ALL-FILES is a list of possible file name completions.

PREDICATE and HIST have the same meaning as in `completing-read'.

MB-DEFAULT is used as part of \"future history\", to be inserted
by the user at will.
\\<projel-minibuffer-map>\\{projel-minibuffer-map}."
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" all-files)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (common-parent-directory (if
                                      (and (car all-files)
                                           (file-name-absolute-p (car all-files)))
                                      common-parent-directory
                                    (concat default-directory
                                            common-parent-directory)))
         (prompt (if (and (zerop cpd-length)
                          all-files
                          (file-name-absolute-p (car all-files)))
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (included-cpd
          (when (member common-parent-directory all-files)
            (setq all-files
                  (delete common-parent-directory all-files))
            t))
         (mb-default
          (mapcar (lambda (mb-default)
                    (if (and common-parent-directory
                             mb-default
                             (file-name-absolute-p mb-default))
                        (file-relative-name
                         mb-default common-parent-directory)
                      mb-default))
                  (if (listp mb-default) mb-default (list mb-default))))
         (substrings (mapcar (lambda (s)
                               (substring s cpd-length))
                             all-files))
         (_
          (when included-cpd
            (setq substrings (cons "./" substrings))))
         (abs-cpd (expand-file-name common-parent-directory))
         (abs-cpd-length (length abs-cpd))
         (relname (cl-letf* ((non-essential t) ; Avoid new Tramp connections.
                             ((symbol-value hist)
                              (mapcan
                               (lambda (s)
                                 (setq s (expand-file-name s))
                                 (and (string-prefix-p abs-cpd s)
                                      (not (eq abs-cpd-length (length s)))
                                      (list (substring s abs-cpd-length))))
                               (symbol-value hist))))
                    (projel--completing-read-strict prompt
                                                    substrings
                                                    (if (functionp predicate)
                                                        (projel--and
                                                         file-exists-p
                                                         (car (list predicate)))
                                                      #'file-exists-p)
                                                    hist mb-default)))
         (absname (expand-file-name relname common-parent-directory)))
    absname))



(defun projel-project-action-candidate-p (completion)
  "Check whether COMPLETION is not real project directory, but an action."
  (seq-find (apply-partially #'string= completion)
            (projel-projects-action-candidates)))

(defun projel-current-project-root ()
  "Return project root directory."
  (when-let* ((project (ignore-errors (project-current))))
    (if (fboundp 'project-root)
        (project-root project)
      (with-no-warnings
        (car (project-roots project))))))


(defun projel--cache-projects-metadata (&optional transducer)
  "Cache project metadata by updating project type and description if missing.

Optional argument TRANSDUCER is a function that transforms the reducing
function."
  (let ((rf
         (lambda (result proj)
           (let* ((cache (gethash proj
                                  projel-projects-cache))
                  (saved-cache (gethash proj
                                        projel-projects-metadata-cache))
                  (proj-type (or
                              (plist-get saved-cache :type)
                              (plist-get cache :type)))
                  (description (or (plist-get saved-cache :description)
                                   (plist-get cache :description))))
             (let ((new-type)
                   (new-descr))
               (unless proj-type
                 (setq new-type (projel-get-project-type proj)))
               (unless description
                 (setq new-descr (projel-get-project-descriptions proj)))
               (when (or new-type new-descr)
                 (when new-type
                   (setq cache (plist-put cache :type new-type)))
                 (when new-descr
                   (setq cache (plist-put cache :description new-descr)))
                 (puthash proj cache projel-projects-cache))
               (setq result
                     (push (list
                            :type proj-type
                            :description description)
                           result)))))))
    (let ((final-rf (if transducer
                        (funcall transducer rf)
                      rf)))
      (pcase-dolist (`(,proj . _) project--list)
        (funcall final-rf nil proj)))))






(defun projel--cache-projects-annotations ()
  "Cache project annotations with metadata and formatting details."
  (let ((longest-proj-name-len
         (if-let* ((actions (mapcar #'length
                                    (projel-projects-action-candidates))))
             (apply #'max actions)
           40))
        (longest-time-str)
        (time-formatter
         (caddr (assq :modified-time
                      projel-projects-annotations-columns)))
        (timehash (make-hash-table :test #'equal))
        (annotations-cache (make-hash-table :test #'equal))
        (longest-type-len)
        (longest-descr-len)
        (minibuff-wind-width
         (or (ignore-errors
               (with-selected-window (minibuffer-window)
                 (- (window-width)
                    (length
                     projel-projects-annotations-columns))))
             120))
        (widths))
    (projel--cache-projects-metadata
     (lambda (rf)
       (lambda (result proj)
         (let ((plist (car (funcall rf result proj))))
           (when-let* ((type (plist-get plist :type)))
             (setq longest-type-len (max (or longest-type-len 0)
                                         (length type))))
           (when-let* ((type (plist-get plist :description)))
             (setq longest-descr-len (max (or longest-descr-len 0)
                                          (length type)))))
         (setq longest-proj-name-len (max
                                      longest-proj-name-len
                                      (length proj)))
         (when time-formatter
           (let ((time-str))
             (setq time-str
                   (funcall time-formatter
                            (projel--file-modification-time proj)))
             (setq longest-time-str (max (or longest-time-str
                                             1)
                                         (length time-str)))
             (puthash proj time-str timehash))))))
    (setq longest-proj-name-len (1+ longest-proj-name-len))
    (setq minibuff-wind-width (- minibuff-wind-width longest-proj-name-len))
    (setq widths (mapcar
                  (pcase-lambda (`(,k ,width _))
                    (min minibuff-wind-width
                         (pcase width
                           ('auto
                            (or (cond ((eq k :modified-time)
                                       longest-time-str)
                                      ((eq k :type)
                                       longest-type-len)
                                      ((eq k :description)
                                       longest-descr-len)
                                      (t 1))
                                40))
                           (_ width))))
                  projel-projects-annotations-columns))
    (let ((total (apply #'+ widths)))
      (when (> total minibuff-wind-width)
        (setq widths (mapcar
                      (lambda (width)
                        (let ((percentage (/ (* width 100.0) total)))
                          (floor (/ (* percentage minibuff-wind-width) 100.0))))
                      widths))))
    (pcase-dolist (`(,proj . _) project--list)
      (let ((pl (gethash proj projel-projects-cache))
            (edited-pl (gethash proj projel-projects-metadata-cache))
            (prefix (propertize " " 'display
                                `(space :align-to ,longest-proj-name-len)))
            (label)
            (count 0))
        (pcase-dolist (`(,k ,_width ,formatter . ,props)
                       projel-projects-annotations-columns)
          (let ((value (or (if (eq k :modified-time)
                               (gethash proj timehash)
                             (or (plist-get edited-pl k)
                                 (plist-get pl k)))
                           ""))
                (width
                 (nth count widths))
                (formatted-val)
                (col-label))
            (setq formatted-val
                  (truncate-string-to-width
                   (if (eq k :modified-time)
                       value
                     (cond ((functionp formatter)
                            (funcall formatter value))
                           ((stringp formatter)
                            (apply  #'format
                                    formatter (list value)))
                           (t (if (stringp value)
                                  value
                                ""))))
                   (max width 1)
                   nil ?\s t))
            (setq col-label
                  (if props
                      (apply #'propertize
                             (substring-no-properties formatted-val)
                             props)
                    formatted-val))
            (setq label (concat label col-label " ")))
          (setq count (1+ count)))
        (puthash proj (concat prefix label)
                 annotations-cache)))
    annotations-cache))


(defun projel-make-project-annotation-fn (_alist)
  "Create annotation function for projects ALIST.

FMT is a format control string with 2 %s placeholders.

CURRENT-PROJECT is a current project directory or nil.

META-CANDIDATES is a list of strings that shouldn't be annotated."
  (let ((anotations (projel--cache-projects-annotations))
        (curr-proj (projel-current-project-root)))
    (lambda (it)
      (let ((label (gethash it anotations)))
        (if (and curr-proj
                 projel-selected-project-minibuffer-indicator
                 (string= curr-proj it))
            (cond ((facep projel-selected-project-minibuffer-indicator)
                   (add-face-text-property 0 (length it)
                                           projel-selected-project-minibuffer-indicator
                                           t it)
                   label)
                  (t (concat
                      label
                      projel-selected-project-minibuffer-indicator)))
          label)))))


(defun projel--projects-completion-table (alist &optional annotate-fn)
  "Completion table for project ALIST of projects with annotations ANNOTATE-FN."
  (unless annotate-fn
    (setq annotate-fn (projel-make-project-annotation-fn alist)))
  (let* ((meta (projel-projects-action-candidates))
         (sort-fn (lambda (files)
                    (sort
                     files
                     (lambda (a b)
                       (cond ((seq-find (apply-partially #'string= a)
                                        meta)
                              nil)
                             ((seq-find (apply-partially #'string= b) meta)
                              t)
                             (t (file-newer-than-file-p a b)))))))
         (cands (mapcar (projel-cond
                          [stringp identity]
                          [t (projel--compose
                               substring-no-properties
                               car)])
                        alist)))
    (unless (eq completing-read-function 'completing-read-default)
      (setq cands (funcall sort-fn cands)))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotate-fn)
            (group-function . ,projel-minibuffer-group-fn)
            (display-sort-function . ,sort-fn)
            (category . project-file))
        (complete-with-action action cands str pred)))))


(defvar projel--projects-dir-history nil)

(defun projel-completing-read-project (&optional prompt &rest _)
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.

When PROMPT is non-nil, use it as the prompt string.

It's also possible to enter an arbitrary directory not in the list."
  (let* ((pr-dir "")
         (action)
         (done))
    (projel-get-projects)
    (while
        (setq action
              (unless done
                (catch 'action
                  (unwind-protect
                      (progn
                        (setq projel--reading-project t)
                        (minibuffer-with-setup-hook
                            #'projel-setup-projects-minibuffer
                          (completing-read
                           (if prompt
                               (format "%s: " (substitute-command-keys prompt))
                             "Select project: ")
                           (projel--projects-completion-table
                            (append
                             project--list
                             (projel-projects-action-candidates
                              (lambda (it) (list (substring-no-properties it))))))
                           nil
                           t
                           nil
                           'projel--projects-dir-history)))
                    (setq projel--reading-project nil)))))
      (cond ((or (listp action))
             (apply (car action)
                    (cdr action)))
            ((commandp (cdr (assoc action projel-projects-actions-alist)))
             (call-interactively (cdr (assoc action
                                             projel-projects-actions-alist))))
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

(defun projel--format-plural (count singular-str)
  "Format COUNT with SINGULAR-STR, adding \"s\" for plural.

Argument COUNT is an integer representing the quantity to consider for
pluralization.

Argument SINGULAR-STR is a string representing the singular form of the word to
be potentially pluralized."
  (concat (format "%d " count)
          (concat singular-str
                  (if (= count 1) "" "s"))))

(defun projel-format-time-readable (time)
  "Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-secs (-
                    (float-time (current-time))
                    (float-time time))))
    (pcase-let ((`(,format-str . ,value)
                 (cond ((< diff-secs 60)
                        (cons "%d second" (truncate diff-secs)))
                       ((< diff-secs 3600)
                        (cons "%d minute" (truncate (/ diff-secs 60))))
                       ((< diff-secs 86400)
                        (cons "%d hour" (truncate (/ diff-secs 3600))))
                       ((< diff-secs 2592000)
                        (cons "%d day" (truncate (/ diff-secs 86400))))
                       (t
                        (cons "%d month" (truncate (/ diff-secs 2592000)))))))
      (format (concat format-str (if (= value 1) " ago" "s ago")) value))))

(defun projel-format-time-readable-long (time)
  "Format a human-readable string representing TIME difference.

Argument TIME is a time value representing the number of seconds since the epoch
\\=(January 1, 1970, 00:00:00 GMT)."
  (let ((diff-secs
         (- (float-time (encode-time (append (list 0)
                                             (cdr (decode-time
                                                   (current-time))))))
            (float-time
             (encode-time (append (list 0)
                                  (cdr (decode-time time))))))))
    (if (zerop (round diff-secs))
        "Now"
      (let* ((past (> diff-secs 0))
             (diff-secs-int (if past diff-secs (- diff-secs)))
             (suffix (if past "ago" "from now"))
             (minutes-secs 60)
             (hours-secs (* 60 minutes-secs))
             (day-secs (* 24 hours-secs))
             (month-secs (* 30 day-secs))
             (year-secs (* 365 day-secs))
             (res
              (cond ((< diff-secs-int minutes-secs)
                     (projel--format-plural (truncate diff-secs-int) "second"))
                    ((< diff-secs-int hours-secs)
                     (projel--format-plural (truncate (/ diff-secs-int
                                                         minutes-secs))
                                            "minute"))
                    ((< diff-secs-int day-secs)
                     (projel--format-plural (truncate
                                             (/ diff-secs-int hours-secs))
                                            "hour"))
                    ((< diff-secs-int month-secs)
                     (projel--format-plural
                      (truncate (/ diff-secs-int day-secs))
                      "day"))
                    ((< diff-secs-int year-secs)
                     (projel--format-plural (truncate
                                             (/ diff-secs-int month-secs))
                                            "month"))
                    (t
                     (let* ((months (truncate (/ diff-secs-int month-secs)))
                            (years (/ months 12))
                            (remaining-months (% months 12)))
                       (string-join
                        (delq nil
                              (list
                               (when (> years 0)
                                 (projel--format-plural years "year"))
                               (when (> remaining-months 0)
                                 (projel--format-plural remaining-months "month"))))
                        " "))))))
        (concat res " " suffix)))))



(defun projel-project-root-files (proj-root)
  "List all non-directory files in a given project root PROJ-ROOT."
  (let ((files))
    (projel-find-in-dir
     proj-root
     nil
     (let ((default-directory
            (expand-file-name proj-root)))
       (delq nil
             (mapcar (lambda (it)
                       (when (or
                              (member it
                                      projel-projects-exlcuded-non-directory-names)
                              (member it
                                      vc-directory-exclusion-list))
                         it))
                     (directory-files proj-root nil
                                      directory-files-no-dot-files-regexp
                                      t))))
     most-positive-fixnum
     (lambda (dir)
       (setq files
             (nconc files
                    (seq-remove
                     #'file-directory-p
                     (directory-files dir t
                                      directory-files-no-dot-files-regexp))))))))


(defun projel--sort-files-alist (alist files)
  "Sort FILES based on their timestamps in ALIST.

Argument ALIST is an association list where keys are filenames and values are
timestamps.

Argument FILES is a list of filenames to be sorted based on their timestamps in
ALIST."
  (sort files
        (lambda (a b)
          (not (time-less-p (cdr (assoc-string a alist))
                            (cdr (assoc-string b alist)))))))

(defun projel--file-modification-time (file)
  "Return the modification time of FILE.

Argument FILE is the path to the file whose modification time is needed."
  (file-attribute-modification-time (file-attributes file)))

(defun projel--project-files-completion-table (files &optional proj-root)
  "Create a completion table for FILES with optional annotations and sorting.

Argument FILES is a list of project file paths to be processed.

Optional argument PROJ-ROOT is the root directory of the project. If specified,
file paths will be abbreviated relative to this directory.

The behavior of the function is influenced by two customizable variables:

- `projel-file-sorting-threshold': Specifies a threshold for sorting the file
  list. If the number of files exceeds this threshold, sorting will be disabled.

- `projel-file-annotation-threshold': Specifies a threshold for adding
  annotations to the file list. If the number of files exceeds this threshold,
  annotations will be disabled."
  (when proj-root (setq proj-root (file-name-directory (abbreviate-file-name
                                                        proj-root))))
  (let ((len (and proj-root (length proj-root)))
        (filtered-files)
        (unexisting-files)
        (max-len 0)
        (cands)
        (files-len (length files))
        (allow-sorting)
        (allow-annotations))
    (setq allow-annotations
          (or
           (not projel-file-annotation-threshold)
           (> projel-file-annotation-threshold files-len)))
    (setq allow-sorting
          (or (not projel-file-sorting-threshold)
              (> projel-file-sorting-threshold files-len)))
    (if (and (not allow-annotations)
             (not allow-sorting))
        (lambda (str pred action)
          (if (eq action 'metadata)
              `(metadata
                (category . project-file))
            (complete-with-action action files str pred)))
      (while files
        (let ((file (car files)))
          (cond ((not (file-exists-p file))
                 (push file unexisting-files)
                 (push file cands)
                 (setq max-len (max max-len (length file))))
                (t
                 (let ((filename (or
                                  (and (file-name-absolute-p file)
                                       (or
                                        (and proj-root (string-prefix-p
                                                        proj-root
                                                        file)
                                             (substring-no-properties file len))
                                        (let ((abbr-file
                                               (abbreviate-file-name file)))
                                          (and proj-root
                                               (string-prefix-p proj-root
                                                                abbr-file)
                                               (substring-no-properties
                                                abbr-file
                                                len)))))
                                  file)))
                   (setq max-len (max max-len (1+ (length filename))))
                   (push filename cands)
                   (push (cons filename (projel--file-modification-time file))
                         filtered-files)))))
        (setq files (cdr files)))
      (let ((annotfn (and allow-annotations
                          (lambda (str)
                            (or
                             (if (member str unexisting-files)
                                 (concat
                                  (propertize " " 'display
                                              `(space :align-to ,max-len))
                                  "Unknown")
                               (when-let*
                                   ((time (cdr (assoc str filtered-files))))
                                 (concat
                                  (propertize " " 'display
                                              `(space :align-to ,max-len))
                                  (projel-format-time-readable time))))
                             ""))))
            (sort-fn (and allow-sorting
                          (apply-partially #'projel--sort-files-alist
                                           filtered-files))))
        (unless (or (not sort-fn)
                    (eq completing-read-function 'completing-read-default))
          (setq cands (funcall sort-fn cands)))
        (lambda (str pred action)
          (if (eq action 'metadata)
              `(metadata
                (annotation-function . ,annotfn)
                (display-sort-function . ,sort-fn)
                (category . project-file))
            (complete-with-action action cands str pred)))))))



;;;###autoload (autoload 'projel-menu "projel" nil t)
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
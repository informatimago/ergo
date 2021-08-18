# ergo - Project Dependencies Management Program and Project Builder.

The purpose of `ergo` is:

1- fetch from `git` repositories a project and its dependencies.
2- build the dependencies and the project.
3- load, test or run the project.

Building a project will depend on the kind of project, and may require
some help from the user, specifying the commands to use.  However, it
is expected that some common classes of projects be recognized and
built automatically by `ergo`.  (The initial class of projects is that
of Common Lisp systems defined `asd` files.)

The loading, testing or running of the project will depend on the kind
of project and environment, and may require some help from the user,
specifying the commands to use.  However, it is expected that useful
classes of projects can be recognized and handled automatically by
`ergo`. (The initial class of projects is that of Common Lisp systems
defined with `asd` files, that can be loaded in the current lisp
image, and tested thru `asdf:test-op` (or some other expression
specified by the user in the ergo file).  Running a Common Lisp system
depends on the kind of system (libraries vs. stand alone programs
vs. functions that can be called at the REPL), and would probably
require some user input.


In the most automatic and simple use case, typing the following expression:

```
(ergo:load :informatimago/lisp)
```

will automatically:

1- `git clone git@github.com:informatimago/lisp.git` and any
   dependency specified in the asd file found in this repository,

2- `asdf:compile-op` the asd system present in this cloned repository,
   and:

3- `asdf:load-op` said asd system.

## IRC

There's an irc channel for discussion about this project: irc://irc.libera.chat/#clergo

## Similar systems

### common-lisp-repo

Common interface for version control systems.

Repo allows you to use source repositories directly as ASDF-installable packages and keep them synced with upstream for development purposes.

Each repo is installed in a subdirectory. Github repositories are installed in the user subdirectory.

https://github.com/common-lisp-repo/repo

### Common Lisp Package Manager

A package manager for Common Lisp that strives to cleanly separate the
package manager process itself from the client image that uses it.

cf. https://gitlab.common-lisp.net/clpm/clpm/-/blob/master/docs/papers/ELS-21/main.pdf

Sophisticated; tries to manage multiple versions (uses a groveler and
a worker process to avoid collisions between dependency versions).
Seems a little brittle.

https://gitlab.common-lisp.net/clpm/clpm

### Google repo

Repo is a tool built on top of Git. Repo helps manage many Git
repositories, does the uploads to revision control systems, and
automates parts of the development workflow. Repo is not meant to
replace Git, only to make it easier to work with Git. The repo command
is an executable Python script that you can put anywhere in your path.

(uses git submodules).

https://android.googlesource.com/tools/repo

### cl-git

https://github.com/russell/cl-git/

### multi-repo vs. monorepo
https://css-tricks.com/from-a-single-repo-to-multi-repos-to-monorepo-to-multi-monorepo/

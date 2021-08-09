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

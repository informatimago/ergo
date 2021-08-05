# ergo - Project Dependencies Management Program.

Very early notes about a project idea:

Well, asdf doesn't assume any provenance for the systems. It could be tarballs, ftp, git, mercurial, whatever.

quicklisp itself doesn't assume any provenance for the systems either.

there is a (approximately montly) quicklisp distribution production process run by Xach, in which he takes updates on the quicklisp-projects repository, and use the data in the source.txt files, to update copies of the sources, test their compilations on sbcl (the current version Xach is running), and if it compiles successfully there, those sources are tared and  stored for download by the quicklisp client.

The quicklisp client uses data in ~/quicklisp/dists/quicklisp/releases.txt to fetch the tarball.

eg.: 1am http://beta.quicklisp.org/archive/1am/2014-11-06/1am-20141106-git.tgz 3490 c5e83c329157518e3ebfeef63e4ac269 83dfee1159cc630cc2453681a7caaf745f987d41 1am-20141106-git 1am.asd

so (quickload :1am) will fetch http://beta.quicklisp.org/archive/1am/2014-11-06/1am-20141106-git.tgz, untar it in 1am-20141106-git, and will asdf load 1am-20141106-git/1am.asd

To implement quick-where-from, I have to clone (or pull) the quicklisp-project, and search the source.txt files there.

I posted this feature request: https://github.com/quicklisp/quicklisp-client/issues/210 (I don't count on it to be acted upon any time soon either).

Note that some systems in quicklisp don't come from github/gitlab or git at all. There are some mercurial, there are tarballs, etc.

Also while asdf has some provision to deal with versions, it's not complete yet AFAIK. Quicklisp has no provision at all: you get the "current" version of the ql distribution.

(remember also that you can use other quicklisp distributions, such as the https://ultralisp.org)

But for projects we have to deal with versions (branches, tags, and sometimes even specific commit).

For example, Google repo command (used for android sources) gathers git repositories and lists specific commits to integrate.

A similar way with git submodules too.

So we could specify a new tool, like quicklisp, but for example that would use only github and gitlab references, where instead of having a specific set of systems would target (potentially) all the repos. You would write a project definition file that would list the url and commit (or tag or branch) of all your dependencies, and the build systems to use to build them (asd, make, cmake, ant, whatever).  So a command would fetch or

update them, and build them, including non CL (asd) dependencies.

Actually, it seems that it would be more useful than quicklisp, and even simplier to implement (and there would be no management (no curation) of the system list).  For simple 1-system projects, we could imply the project definition file>  (project-load :informatimago/lisp) would find github.com/informatimago/lisp alone and would find the asd file inside and load it.  B

but users could write project-definition files as mentionned for more complex projects requiring more depndencies.

And the advantage is that we could easily implement more advanced features using github/gitlab such as dealing with issues, merge request/pull requests, wiki, etc.

I think we have a project here!

```
(ergo :my-project)  loads my-project.ergo and downloads and compile stuff.
```

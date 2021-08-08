# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

NAME=documentation
PDFS=$(NAME).pdf
all:documents

documents pdfs :$(PDFS)
	open $(PDFS)

.PHONY:: documents pdfs

$(NAME)-todo.org $(NAME)-todo.sha256:$(NAME).org
	@ name=$(NAME) ;\
      ( ( [ -r $${name}-todo.org -a -r $${name}-todo.sha256 ] \
	      && [ "$$(sha256sum $${name}-todo.org)" = "$$(cat $${name}-todo.sha256)" ] ) \
	   || [ ! -r $${name}-todo.org -o ! -r $${name}-todo.sha256 ] ) \
	&& ( printf '// Regenerating %s\n' "$${name}-todo.org" ;\
	     ../tools/extract-trace-items --todo $${name}.org  > $${name}-todo.org \
	     && sha256sum $${name}-todo.org > $${name}-todo.sha256 ) \
	|| ( printf '!! %s has been modified. I will not override it!\n' "$${name}-todo.org" ; exit 1 ; )

# include ../tools/common.make

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill

clean::
	- rm -f $(NAME).tex missfont.log ./--version.lock


.PHONY::  questions tbd

questions:
	grep -nH -e '\<\(QUESTIONS\?\|HOW\)\>' *.{txt,org}

tbd:
	grep -nH -e '\<\(TBD\|GENERAL_DESCRIPTION\|DESCRIPTION_OF_THE_RESPONSIBILITIES_OF_THE_ENTITY\|LIST_OF_ATTRIBUTES\|LIST_OF_ASSOCIATIONS_WITH_OTHER_ENTITIES\)\>' *.txt

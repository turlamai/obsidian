#lang info

(define collection "obsidian")
(define version "0.1")
(define pkg-desc
  "Obsidian Flavored Markdown for Racket: frontmatter, wikilinks, embeds, callouts, tags, highlights and task lists as an AST layer over the markdown package")
(define license '(MIT))

(define deps '("base"
               "markdown"
               "rackunit-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define scribblings '(("scribblings/obsidian.scrbl" ())))

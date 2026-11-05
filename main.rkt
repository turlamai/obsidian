#lang racket/base

(require "frontmatter.rkt"
         "transform.rkt"
         "render.rkt")

(provide (all-from-out "frontmatter.rkt")
         (all-from-out "transform.rkt")
         (all-from-out "render.rkt"))

;; Command-line pipe: read Obsidian-flavored Markdown from stdin, write HTML.
(module+ main
  (require racket/port)
  (display (obsidian->html (port->string (current-input-port)))))

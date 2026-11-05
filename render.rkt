#lang racket/base

;; Composition: source -> AST -> dialect rewrites -> xexprs/HTML.
;;
;; Two parameters control link and embed resolution so the host application
;; (a static-site engine, a preview server) decides what [[links]] and
;; ![[embeds]] mean. Everything degrades gracefully: unresolvable wikilinks
;; render as plain label text, unresolvable non-image embeds stay literal.

(require markdown
         racket/list
         racket/string
         "frontmatter.rkt"
         "transform.rkt")

(provide obsidian->xexprs
         obsidian->html
         document-title
         document-tags
         current-resolve-wikilink
         current-resolve-embed)

;; current-resolve-wikilink : (parameter/c (-> string? (or/c string? #f)))
;; Maps a trimmed [[wikilink]] target to a URL; #f degrades to label text.
(define current-resolve-wikilink (make-parameter (lambda (target) #f)))

;; current-resolve-embed : (parameter/c (-> string? (or/c (listof any/c) #f)))
;; Maps a trimmed ![[embed]] target to elements to splice; #f falls back to
;; the default (images become <img src>, everything else stays literal).
(define current-resolve-embed (make-parameter (lambda (target) #f)))

;; obsidian->xexprs : string? -> (listof any/c)
(define (obsidian->xexprs src)
  ;; callouts come last: a top-level collector over marker paragraphs
  (rewrite-callouts
   (rewrite-task-lists
    (rewrite-tags
     (rewrite-highlights
      (rewrite-wikilinks
       (rewrite-embeds
        (parse-markdown (mark-dashes (mark-callouts (mark-embeds (strip-frontmatter src)))))
        (current-resolve-embed))
       (current-resolve-wikilink)))))))

;; obsidian->html : string? -> string?
(define (obsidian->html src)
  (apply string-append
         (for/list ((x (in-list (obsidian->xexprs src))))
           (xexpr->string x))))

;; element-text : any/c -> string?
(define (element-text x)
  (cond
    ((string? x) x)
    ((pair? x) (apply string-append (map element-text (cddr x))))
    (else "")))

;; document-title : string? -> (or/c string? #f)
;; The first H1's text, or #f when the document has no H1. This is the
;; editor-neutral replacement for frontmatter titles: the title lives in the
;; body, not in metadata.
(define (document-title src)
  (for*/first ((x (in-list (obsidian->xexprs src)))
               #:when (and (pair? x) (eq? (car x) 'h1))
               (t (in-value (string-trim (element-text x))))
               #:unless (string=? t ""))
    t))

;; document-tags : string? -> (listof string?)
(define (document-tags src)
  (collect-tags (parse-markdown (mark-dashes (strip-frontmatter src)))))

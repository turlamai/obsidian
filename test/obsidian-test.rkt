#lang racket/base

(require rackunit
         markdown
         racket/list
         racket/string
         "../frontmatter.rkt"
         "../transform.rkt"
         "../render.rkt")

;; ---------------------------------------------------------------------------
;; frontmatter
;; ---------------------------------------------------------------------------

(let-values (((meta body) (split-frontmatter "---\ntitle: Hello\ntags:\n  - a\n---\n\nBody here")))
  ;; tolerant parser: `tags:` is captured with an empty raw value; nested
  ;; list items are skipped — frontmatter is a hint, never load-bearing
  (check-equal? meta '(("title" . "Hello") ("tags" . "")))
  (check-true (string-contains? body "Body here")))

(let-values (((meta body) (split-frontmatter "---\ntitle: x\n...\nBody")))
  (check-equal? meta '(("title" . "x")))
  (check-equal? (string-trim body) "Body"))

(let-values (((meta body) (split-frontmatter "---\ntitle: never closed\nBody")))
  (check-false meta))

(let-values (((meta body) (split-frontmatter "no frontmatter at all")))
  (check-false meta)
  (check-equal? body "no frontmatter at all"))

;; frontmatter immediately followed by another fence is still fine
(check-equal? (strip-frontmatter "---\ndraft: true\n---\n---\ncode fence in body\n---\n")
              "---\ncode fence in body\n---\n")

;; ---------------------------------------------------------------------------
;; embeds
;; ---------------------------------------------------------------------------

(check-equal? (rewrite-embeds (parse-markdown (mark-embeds "![[pic.png]]")))
              '((p () (img ((loading "lazy") (src "pic.png") (alt "pic.png"))))))

(check-equal? (rewrite-embeds (parse-markdown (mark-embeds "![[pic.png|alt text]]")))
              '((p () (img ((loading "lazy") (src "pic.png") (alt "alt text"))))))

;; non-image embed without resolver degrades to its plain wikilink form
(check-equal? (rewrite-embeds (parse-markdown (mark-embeds "before ![[some-note]] after")))
              '((p () "before [[some-note]] after")))

;; resolver output is spliced in
(check-equal?
 (rewrite-embeds (parse-markdown (mark-embeds "![[some-note]]"))
                 (lambda (t) (list `(em () ,(string-append "embed:" t)))))
 '((p () (em () "embed:some-note"))))

;; image embed lets the resolver win over the default
(check-equal?
 (rewrite-embeds (parse-markdown (mark-embeds "![[pic.png]]")) (lambda (t) (list "resolved")))
 '((p () "resolved")))

;; the source pass never touches embeds inside code
(check-equal? (mark-embeds "```racket\n![[raw]]\n```")
              "```racket\n![[raw]]\n```")
(check-equal? (mark-embeds "text ![[x]] `code ![[y]]` tail")
              "text OBEMBEDMARK[[x]] `code ![[y]]` tail")

;; ---------------------------------------------------------------------------
;; wikilinks
;; ---------------------------------------------------------------------------

(check-equal? (rewrite-wikilinks (parse-markdown "[[Some Note]]")
                                 (lambda (t) (format "/blog/~a/" t)))
              '((p () (a ((href "/blog/Some Note/") (class "wikilink")) "Some Note"))))

(check-equal? (rewrite-wikilinks (parse-markdown "[[target|label]]")
                                 (lambda (t) "/x"))
              '((p () (a ((href "/x") (class "wikilink")) "label"))))

;; unresolvable wikilinks degrade to plain label text (no dead links)
(check-equal? (rewrite-wikilinks (parse-markdown "[[target|label]]"))
              '((p () "label")))

(check-equal? (rewrite-wikilinks (parse-markdown "[[plain]]"))
              '((p () "plain")))

;; code blocks are immune
(check-equal? (rewrite-wikilinks (parse-markdown "```racket\n[[not-a-link]]\n```"))
              '((pre ((class "brush: racket")) (code () "[[not-a-link]]"))))

;; inline code is immune too
(check-true (string-contains? (xexpr->string (car (rewrite-wikilinks (parse-markdown "`[[x]]`"))))
                              "[[x]]"))

;; ---------------------------------------------------------------------------
;; highlights
;; ---------------------------------------------------------------------------

(check-equal? (rewrite-highlights (parse-markdown "==important=="))
              '((p () (mark () "important"))))

(check-equal? (rewrite-highlights (parse-markdown "a ==b== c ==d== e"))
              '((p () "a " (mark () "b") " c " (mark () "d") " e")))

(check-equal? (rewrite-highlights (parse-markdown "`==code==`"))
              '((p () (code () "==code=="))))

;; ---------------------------------------------------------------------------
;; tags
;; ---------------------------------------------------------------------------

(check-equal? (rewrite-tags (parse-markdown "saw a #racket tip"))
              '((p () "saw a " (span ((class "tag")) "#racket") " tip")))

(check-equal? (rewrite-tags (parse-markdown "#heading is not a tag"))
              '((h1 ((id "heading-is-not-a-tag")) "heading is not a tag")))

(check-equal? (rewrite-tags (parse-markdown "42#not-tag"))
              '((p () "42#not-tag")))

(check-equal? (rewrite-tags (parse-markdown "url https://x.com/page#anchor stays"))
              '((p () "url https://x.com/page#anchor stays")))

(check-equal? (rewrite-tags (parse-markdown "中文 #嵌套/子标签 works"))
              '((p () "中文 " (span ((class "tag")) "#嵌套/子标签") " works")))

(check-equal? (collect-tags (parse-markdown "a #one b #two c #one d `#code`"))
              '("one" "two"))

(check-equal? (document-tags "body #tag1\n\nmore #tag-2")
              '("tag1" "tag-2"))

;; ---------------------------------------------------------------------------
;; callouts (source-level markers + top-level collection)
;; ---------------------------------------------------------------------------

(define (callout src) (car (rewrite-callouts (parse-markdown (mark-callouts src)))))

(define note-co (callout "> [!note] Watch out\n> body line"))
(check-equal? (first note-co) 'div)
(check-equal? (second (assq 'class (second note-co))) "callout callout-note")
(check-equal? (second (assq 'data-callout (second note-co))) "note")
(check-true (string-contains? (xexpr->string note-co) "Watch out"))
(check-true (string-contains? (xexpr->string note-co) "body line"))

;; default title = capitalized kind; content below the marker is content
(check-true (string-contains? (xexpr->string (callout "> [!tip]\n> just body")) "Tip"))
(check-true (string-contains? (xexpr->string (callout "> [!tip]\n> just body")) "just body"))

;; fold markers become classes
(check-equal?
 (second (assq 'class (second (callout "> [!warn]- folded\n> x"))))
 "callout callout-warn is-collapsed")

;; multi-line content keeps full markdown
(check-true
 (string-contains?
  (xexpr->string (callout "> [!info] multi\n> **bold** and more\n> - a list"))
  "<strong>bold</strong>"))
(check-true
 (string-contains?
  (xexpr->string (callout "> [!info] multi\n> **bold** and more\n> - a list"))
  "a list"))

;; ordinary quotes are untouched
(check-equal? (rewrite-callouts (parse-markdown (mark-callouts "> plain quote")))
              '((blockquote () (p () "plain quote"))))

;; code blocks are immune to the source pass
(check-equal? (mark-callouts "```\n> [!note] not a callout\n> ```\n")
              "```\n> [!note] not a callout\n> ```\n")

;; ---------------------------------------------------------------------------
;; task lists
;; ---------------------------------------------------------------------------

;; the parser wraps consecutive items in one ul; the second item is checked
(define tasks (rewrite-task-lists (parse-markdown "- [ ] open item\n- [x] done item")))
(define task-lis (cddr (car tasks)))
(check-false (string-contains? (xexpr->string (first task-lis)) "checked"))
(check-true (string-contains? (xexpr->string (second task-lis)) "checked"))

;; ---------------------------------------------------------------------------
;; title extraction
;; ---------------------------------------------------------------------------

(check-equal? (document-title "# My Title\n\nbody") "My Title")
(check-equal? (document-title "no title here") #f)
(check-equal? (document-title "") #f)

;; ---------------------------------------------------------------------------
;; composition: everything at once, with code immunity
;; ---------------------------------------------------------------------------

(define src
  (string-append
   "---\ntitle: ignored\n---\n"
   "# Composed\n\n"
   "Link [[target|lbl]], mark ==hi==, tag #demo.\n\n"
   "> [!note] Title\n> content\n\n"
   "- [x] task\n\n"
   "```racket\n[[raw]] ==raw== #raw ![[raw]]\n```\n"))

(define html (obsidian->html src))
(check-true (string-contains? html "Composed"))
;; unresolvable wikilink renders as plain label text
(check-true (string-contains? html "lbl"))
(check-true (string-contains? html "<mark>hi</mark>"))
(check-true (string-contains? html "class=\"tag\""))
(check-true (string-contains? html "callout-note"))
(check-true (string-contains? html "checked"))
(for ((raw (in-list '("[[raw]]" "==raw==" "#raw" "![[raw]]"))))
  (check-true (string-contains? html raw) (format "code block kept literal: ~a" raw)))

(parameterize ((current-resolve-wikilink (lambda (t) (string-append "/p/" t))))
  (check-true (string-contains? (obsidian->html src) "href=\"/p/target\"")))

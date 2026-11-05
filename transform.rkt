#lang racket/base

;; AST-level transforms for the Obsidian Markdown dialect.
;;
;; Everything here operates on the xexpr AST produced by the `markdown`
;; package, never on raw source text. Rewrites are skipped inside
;; code-bearing elements, so `[[links]]`, `==marks==` and `#tags` inside code
;; blocks always stay literal.

(require racket/format
         racket/list
         racket/match
         racket/string)

(provide map-strings
         map-nodes
         map-source-text
         mark-embeds
         mark-callouts
         mark-dashes
         rewrite-embeds
         rewrite-wikilinks
         rewrite-highlights
         rewrite-tags
         rewrite-callouts
         rewrite-task-lists
         tidy
         collect-tags)

;; Elements whose string children are code and must never be rewritten.
(define protected-tags '(code pre samp kbd style script))

;; map-strings : (-> string? (listof any/c)) (listof any/c) -> (listof any/c)
;;
;; Applies f to every string element, splicing the result list in place of
;; the string. f receives one string and returns a list of elements (strings
;; and/or xexprs) that replace it.
(define (map-strings f elements)
  (append-map
   (lambda (x)
     (cond
       ((string? x) (f x))
       ((pair? x)
        (if (memq (car x) protected-tags)
            (list x)
            (match x
              ((list tag attrs elems ...)
               (list (list* tag attrs (map-strings f elems)))))))
       (else (list x))))
   elements))

;; map-nodes : (-> any/c (listof any/c)) (listof any/c) -> (listof any/c)
;;
;; Depth-first: children first, then f on each element. f returns a LIST of
;; elements that replaces the element (a single-element list keeps it).
;; Elements are themselves lists, so the contract must be list-in/list-out —
;; returning a bare element would be indistinguishable from a list to splice.
(define (map-nodes f elements)
  (append-map
   (lambda (x)
     (cond
       ((string? x) (list x))
       ((pair? x)
        (if (memq (car x) protected-tags)
            (list x)
            (match x
              ((list tag attrs elems ...)
               (f (list* tag attrs (map-nodes f elems)))))))
       (else (list x))))
   elements))

;; tidy : (listof any/c) -> (listof any/c)
;;
;; Drops empty strings and merges adjacent string siblings left behind by
;; splicing, so the output looks hand-written. Protected elements are kept
;; byte-for-byte.
(define (tidy elements)
  (let loop ((es elements) (acc '()))
    (cond
      ((null? es) (reverse acc))
      ((and (string? (car es)) (string=? (car es) ""))
       (loop (cdr es) acc))
      ((and (pair? acc) (string? (car acc)) (string? (car es)))
       (loop (cdr es) (cons (string-append (car acc) (car es)) (cdr acc))))
      ((pair? (car es)) (loop (cdr es) (cons (tidy-element (car es)) acc)))
      (else (loop (cdr es) (cons (car es) acc))))))

(define (tidy-element x)
  (match x
    ((list tag attrs elems ...)
     (if (memq tag protected-tags)
         x
         (list* tag attrs (tidy elems))))
    (_ x)))

;; splice-matches : regexp? string? (-> (listof (or/c string? #f))
;;                                      (or/c #f (listof any/c)))
;;                -> (listof any/c)
;;
;; Splits str around matches of rx. For each match, (on-match groups) returns
;; the list of elements to insert, or #f to keep the match as literal text.
;; Unmatched text passes through as strings.
(define (splice-matches rx str on-match)
  (let loop ((start 0) (acc '()))
    (define m (regexp-match rx str start))
    (if (not m)
        (if (< start (string-length str))
            (reverse (cons (substring str start) acc))
            (reverse acc))
        (let* ((bounds (car (regexp-match-positions rx str start)))
               (mstart (car bounds))
               (mend (cdr bounds))
               (replacement (on-match (cdr m))))
          (if (not replacement)
              (loop mend (cons (substring str mstart mend)
                               (cons (substring str start mstart) acc)))
              (loop mend (append (reverse replacement)
                                 (cons (substring str start mstart) acc))))))))

;; ---------------------------------------------------------------------------
;; Source-level text transforms with code protection
;; ---------------------------------------------------------------------------

;; The upstream parser eats the `!` of `![[embeds]]` (image syntax), so
;; embeds must be rewritten at the source level — but only outside code.
;; map-source-text applies fn to the ordinary text of src while fenced code
;; blocks and inline code spans pass through untouched. (Indented code blocks
;; are not detected; Obsidian-authored content uses fenced blocks.)

;; line-fence : string? -> (or/c #f string?)
;; The fence marker (three or more backticks or tildes) when the line OPENS
;; a fenced code block, #f otherwise.
(define (line-fence line)
  (match (regexp-match #px"^[[:space:]]{0,3}(`{3,}|~{3,})" line)
    ((list _ marker) marker)
    (_ #f)))

;; fence-closes? : string? string? -> boolean?
;; A closing fence is a line of the same fence character (at least as long
;; as the opener) followed only by spaces.
(define (fence-closes? line marker)
  (define ch (string-ref marker 0))
  (regexp-match?
   (regexp (string-append "^[[:space:]]{0,3}"
                          (make-string (string-length marker) ch)
                          "+[[:space:]]*$"))
   line))

;; tick-run-length : string? exact-positive-integer? -> exact-positive-integer?
(define (tick-run-length s i)
  (let loop ((j i) (n 0))
    (if (and (< j (string-length s)) (char=? (string-ref s j) #\`))
        (loop (add1 j) (add1 n))
        n)))

;; find-tick-run : string? exact-positive-integer? exact-positive-integer?
;;               -> (or/c exact-positive-integer? #f)
;; Index of the first run of exactly n backticks at or after `from`
;; (a longer run would open a longer span in CommonMark, not close this one).
(define (find-tick-run s from n)
  (let loop ((j from))
    (cond
      ((>= j (string-length s)) #f)
      ((char=? (string-ref s j) #\`)
       (define run (tick-run-length s j))
       (if (= run n) j (loop (+ j run))))
      (else (loop (add1 j))))))

;; split-inline-code : string? -> (listof (cons/c boolean? string?))
;; Splits one line around inline code spans; segments concatenated reproduce
;; the line exactly.
(define (split-inline-code line)
  (define n-len (string-length line))
  (let loop ((i 0) (seg-start 0) (acc '()))
    (cond
      ((>= i n-len)
       (reverse (cons (cons #f (substring line seg-start)) acc)))
      ((char=? (string-ref line i) #\`)
       (define run (tick-run-length line i))
       (define close (find-tick-run line (+ i run) run))
       (if (not close)
           (reverse (cons (cons #f (substring line seg-start)) acc))
           (let* ((plain (substring line seg-start i))
                  (code (substring line i (+ close run)))
                  (acc2 (if (string=? plain "")
                            acc
                            (cons (cons #f plain) acc))))
             (loop (+ close run) (+ close run) (cons (cons #t code) acc2)))))
      (else (loop (add1 i) seg-start acc)))))

;; split-code-segments : string? -> (listof (cons/c boolean? string?))
;; Segments in order; #t marks protected (code) segments. Concatenating the
;; segment texts reproduces src exactly.
(define (split-code-segments src)
  (define lines (string-split src "\n" #:trim? #f))
  (define n-lines (length lines))
  (let loop ((idx 0) (fence #f) (rev '()))
    (if (>= idx n-lines)
        (merge-adjacent (reverse rev))
        (let* ((line (list-ref lines idx))
               (eol (if (= idx (sub1 n-lines)) "" "\n")))
          (cond
            (fence
             (loop (add1 idx)
                   (if (fence-closes? line fence) #f fence)
                   (cons (cons #t (string-append line eol)) rev)))
            ((line-fence line)
             => (lambda (marker)
                  (loop (add1 idx) marker
                        (cons (cons #t (string-append line eol)) rev))))
            (else
             ;; ordinary line: mark embeds are the only source-level edit and
             ;; they never span lines, so split on inline code and push the
             ;; pieces with the newline attached to the last piece
             (define pieces (split-inline-code line))
             (define with-eol
               (append (drop-right pieces 1)
                       (list (cons (car (last pieces))
                                   (string-append (cdr (last pieces)) eol)))))
             (loop (add1 idx) #f (append (reverse with-eol) rev))))))))

(define (merge-adjacent segs)
  (let loop ((s segs) (acc '()))
    (match s
      ('() (reverse acc))
      ((cons (cons f t) rest)
       (match acc
         ((cons (cons f2 t2) acc2)
          #:when (and (eq? f f2) (not f))
          (loop rest (cons (cons f (string-append t2 t)) acc2)))
         (_ (loop rest (cons (cons f t) acc))))))))

;; map-source-text : string? (-> string? string?) -> string?
;; Applies fn to ordinary text segments; code segments pass through verbatim.
(define (map-source-text src fn)
  (string-append*
   (for/list ((seg (in-list (split-code-segments src))))
     (if (car seg) (cdr seg) (fn (cdr seg))))))

;; ---------------------------------------------------------------------------
;; Embeds:  ![[target]]  or  ![[target|label]]
;; ---------------------------------------------------------------------------

;; Source pass: replace the `!` (which the parser would eat) with a marker
;; token that survives parsing. Code segments are never touched.
(define embed-marker "OBEMBEDMARK")

(define (mark-embeds src)
  (map-source-text
   src
   (lambda (s)
     (regexp-replace*
      #px"!\\[\\[([^\\[\\]|]+?)(?:\\|([^\\[\\]]+))?\\]\\]"
      s
      (lambda (all target label)
        (string-append embed-marker "[[" target
                       (if label (string-append "|" label) "")
                       "]]"))))))

;; The upstream parser applies smart dashes: a hyphen followed by a digit
;; becomes an en-dash element, which corrupts tags and slugs like #tag-2
;; (Obsidian keeps hyphens literal). Source pass: escape exactly that trigger
;; as \- so the parser emits a literal hyphen. Code segments untouched.
(define (mark-dashes src)
  (map-source-text
   src
   (lambda (s)
     (regexp-replace* #px"-([0-9])" s "\\\\-\\1"))))

;; AST pass: recognize the marker form.
(define embed-rx
  #px"OBEMBEDMARK\\[\\[([^\\[\\]|]+?)(?:\\|([^\\[\\]]+))?\\]\\]")

(define image-extensions
  '(".png" ".jpg" ".jpeg" ".gif" ".webp" ".svg" ".avif" ".bmp"))

(define (image-path? s)
  (define low (string-downcase s))
  (for/or ((ext (in-list image-extensions)))
    (string-suffix? low ext)))

;; rewrite-embeds : (listof any/c)
;;                [(-> string? (or/c #f (listof any/c)))]
;;                -> (listof any/c)
;;
;; resolve-embed receives the trimmed embed target. When it returns a list of
;; elements that list is spliced in; when it returns #f, image targets get a
;; default <img> (src = target) and everything else degrades to its plain
;; wikilink form, so an unresolvable embed is never silently dropped.
(define (rewrite-embeds elements (resolve-embed (lambda (target) #f)))
  (tidy
   (map-strings
   (lambda (s)
     (splice-matches embed-rx s
       (lambda (groups)
         (match groups
           ((list target label)
            (define t (string-trim target))
            (or (resolve-embed t)
                (default-embed t (and label (string-trim label)))))))))
   elements)))

(define (default-embed target label)
  (if (image-path? target)
      (list
       (list 'img
             (append '((loading "lazy"))
                     (list (list 'src target))
                     (list (list 'alt (or label
                                          (last (string-split target "/"))))))))
      (list (string-append "[[" target
                           (if label (string-append "|" label) "")
                           "]]"))))

;; ---------------------------------------------------------------------------
;; Wikilinks:  [[target]]  or  [[target|label]]
;; ---------------------------------------------------------------------------

(define wikilink-rx #px"\\[\\[([^\\[\\]|]+?)(?:\\|([^\\[\\]]+))?\\]\\]")

;; rewrite-wikilinks : (listof any/c)
;;                   [(-> string? (or/c string? #f))]
;;                   -> (listof any/c)
;;
;; resolve-wikilink maps the trimmed target to a URL. Unresolvable targets
;; degrade to their plain label text: no dead links are ever rendered.
(define (rewrite-wikilinks elements (resolve-wikilink (lambda (target) #f)))
  (tidy
   (map-strings
   (lambda (s)
     (splice-matches wikilink-rx s
       (lambda (groups)
         (match groups
           ((list target label)
            (define t (string-trim target))
            (define lbl (if label (string-trim label) t))
            (define url (resolve-wikilink t))
            (if url
                (list `(a ((href ,url) (class "wikilink")) ,lbl))
                (list lbl)))))))
   elements)))

;; ---------------------------------------------------------------------------
;; Highlights:  ==text==  ->  <mark>
;; ---------------------------------------------------------------------------

;; Built with `regexp` so the class holds a real newline character:
;; alphabetic escapes are illegal inside character classes.
(define highlight-rx (regexp "==([^=\n]+)=="))

(define (rewrite-highlights elements)
  (tidy
   (map-strings
   (lambda (s)
     (splice-matches highlight-rx s
       (lambda (groups)
         (match groups
           ((list inner) (list `(mark () ,inner)))))))
   elements)))

;; ---------------------------------------------------------------------------
;; Inline tags:  #tag  #nested/sub-tag  #中文标签
;; ---------------------------------------------------------------------------

;; The first character must be a letter or underscore (all-digit tags are not
;; tags in Obsidian); continuation characters add digits, hyphen and slash.
;; A tag must be at string start or follow whitespace or opening punctuation,
;; so URLs like ...#anchor never match: the optional prefix is matched and
;; re-emitted verbatim. Racket quirk: Unicode property classes (\p{...}) and
;; alphabetic escapes are illegal inside character classes, so the prefix is
;; a three-way alternation and the tag body is built from top-level
;; alternatives instead of one class.
(define tag-rx
  #px"(?:(^)|([[:space:]()\\[{>\"'，。；：！？])|(\\p{Zs}))#((?:\\p{L}|_)(?:\\p{L}|\\p{N}|[-_/])*)")

(define (rewrite-tags elements)
  (tidy
   (map-strings
   (lambda (s)
     (splice-matches tag-rx s
       (lambda (groups)
         (match groups
           ((list bol p1 p2 tag)
            (define pre (cond (bol "") (else (or p1 p2 ""))))
            (list pre `(span ((class "tag")) ,(string-append "#" tag))))))))
   elements)))

;; collect-tags : (listof any/c) -> (listof string?)
;; Same recognition rules as rewrite-tags, but only observes.
(define (collect-tags elements)
  (define tags '())
  (map-strings
   (lambda (s)
     (for ((m (in-list (regexp-match* tag-rx s #:match-select cdr))))
       (match m
         ((list bol p1 p2 tag)
          (set! tags (cons tag tags)))))
     (list s))
   elements)
  (remove-duplicates (reverse tags) string=?))

;; ---------------------------------------------------------------------------
;; Callouts:  > [!kind] Title  /  > [!kind]- folded  /  > [!kind]+ expanded
;; ---------------------------------------------------------------------------

;; The parser merges blockquote lines into single strings, losing the line
;; structure callouts need (title on line one, content below). So callouts
;; are handled in two passes: a source pass (mark-callouts, code-safe like
;; mark-embeds) rewrites the blockquote into marker-delimited plain markdown,
;; then this top-level AST pass gathers the elements between the markers into
;; a callout div. Content keeps full Markdown support.

(define callout-open-line-rx
  #px"^[[:space:]]{0,3}>[[:space:]]*\\[!([A-Za-z][A-Za-z0-9-]*)\\]([+-])?[[:space:]]*(.*)$")

(define callout-quote-line-rx #px"^[[:space:]]{0,3}>[[:space:]]?(.*)$")

(define callout-open-marker-rx
  #px"^OBCALLOUTOPEN:([^:]+):([+-])?:?(.*)$")

(define callout-close-marker "OBCALLOUTCLOSE")

;; mark-callouts : string? -> string?
;; Source-level pass. Only ordinary text segments are rewritten; code blocks
;; are untouched.
(define (mark-callouts src)
  (map-source-text
   src
   (lambda (seg)
     (define lines (string-split seg "\n"))
     (string-join
      (let loop ((ls lines) (acc '()) (state 'out))
        (match ls
          ('()
           (if (eq? state 'in)
               (reverse (cons callout-close-marker acc))
               (reverse acc)))
          ((cons line rest)
           (cond
             ((eq? state 'out)
              (match (regexp-match callout-open-line-rx line)
                ((list _ kind fold title)
                 (loop rest
                       (cons "" (cons (open-marker-line kind fold title) acc))
                       'in))
                (_ (loop rest (cons line acc) 'out))))
             (else
              (match (regexp-match callout-quote-line-rx line)
                ((list _ content)
                 (loop rest (cons content acc) 'in))
                (_ (loop (cons "" ls)
                         (cons callout-close-marker (cons "" acc))
                         'out))))))))
      "\n"))))

;; open-marker-line : string? string? string? -> string?
(define (open-marker-line kind fold title)
  (string-append "OBCALLOUTOPEN:" kind ":" (or fold "") ":" title))

;; rewrite-callouts : (listof any/c) -> (listof any/c)
(define (rewrite-callouts elements)
  (tidy
   (let loop ((es elements) (acc '()))
     (match es
       ('() (reverse acc))
       ((cons x rest)
        (define marker (element-callout-marker x))
        (cond
          (marker
           (match-let (((list _ kind fold title) marker))
             (let collect ((ys rest) (content '()))
               (match ys
                 ('()
                  (loop '()
                        (cons (build-callout kind fold title (reverse content))
                              acc)))
                 ((cons y rest2)
                  (if (close-marker? y)
                      (loop rest2
                            (cons (build-callout kind fold title
                                                 (reverse content))
                                  acc))
                      (collect rest2 (cons y content))))))))
          ((close-marker? x) (loop rest acc))
          (else (loop rest (cons x acc)))))))))

;; element-callout-marker : any/c -> (or/c #f (listof (or/c string? #f)))
(define (element-callout-marker x)
  (define s (and (pair? x)
                 (eq? (car x) 'p)
                 (pair? (cddr x))
                 (string? (third x))
                 (string-trim (third x))))
  (and s (regexp-match callout-open-marker-rx s)))

;; close-marker? : any/c -> boolean?
(define (close-marker? x)
  (and (pair? x)
       (eq? (car x) 'p)
       (pair? (cddr x))
       (string? (third x))
       (equal? (string-trim (third x)) callout-close-marker)))

;; build-callout : string? string? string? (listof any/c) -> any/c
(define (build-callout kind fold title content)
  (define kind-lower (string-downcase kind))
  (define fold-class
    (match fold
      ("-" " is-collapsed")
      ("+" " is-expanded")
      (_ "")))
  (define title-text
    (let ((t (string-trim title)))
      (if (string=? t "")
          (string-titlecase kind-lower)
          t)))
  (list 'div
        (list (list 'class
                    (string-append "callout callout-" kind-lower fold-class))
              (list 'data-callout kind-lower))
        (list 'div '((class "callout-title"))
              (list 'p '() title-text))
        (list* 'div '((class "callout-content"))
               content)))

;; ---------------------------------------------------------------------------
;; Task lists:  - [ ] open  /  - [x] done
;; ---------------------------------------------------------------------------

(define task-rx #px"^\\[([ xX])\\][[:space:]]*")

(define (rewrite-task-lists elements)
  (tidy
   (map-nodes
   (lambda (x)
     (match x
       ((list 'li attrs children ...)
        (match children
          ((cons (? string? first) rest)
           (define m (regexp-match task-rx first))
           (if (not m)
               (list x)
               (match-let (((list _ check) m)
                           (end (cdr (car (regexp-match-positions task-rx first)))))
                 (list (list* 'li attrs
                              (list 'input (append '((type "checkbox")
                                                     (class "task")
                                                     (disabled ""))
                                                   (if (string-ci=? check "x")
                                                       '((checked ""))
                                                       '())))
                              (substring first end)
                              rest)))))
          (_ (list x))))
       (_ (list x))))
   elements)))

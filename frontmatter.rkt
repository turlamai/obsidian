#lang racket/base

;; YAML frontmatter detection and splitting.
;;
;; Deliberately tolerant: this package never *requires* frontmatter (the
;; publishing pipeline it serves is editor-neutral), so everything here is
;; best-effort extraction of top-level `key: value` pairs. A document without
;; frontmatter parses cleanly as pure body; a document with frontmatter has it
;; stripped without the fields ever being load-bearing.

(require racket/list
         racket/string)

(provide split-frontmatter
         strip-frontmatter
         frontmatter->alist)

;; split-frontmatter : string? -> (values (or/c #f (alist/c string? string?)) string?)
;;
;; Recognizes the Obsidian convention: an optional leading fence of `---`
;; lines closed by a `---` or `...` line. The opening fence must be the very
;; first line; an unclosed fence is ordinary content, not frontmatter.
;; Returns the (possibly #f) metadata alist and the body.
(define (split-frontmatter src)
  (define lines (string-split src "\n"))
  (if (and (not (null? lines))
           (equal? (string-trim (car lines)) "---"))
      (let loop ((rest (cdr lines)) (acc '()))
        (cond
          ((null? rest) (values #f src)) ; unclosed fence: not frontmatter
          ((member (string-trim (car rest)) '("---" "..."))
           (values (frontmatter->alist (reverse acc))
                   (let ((body (string-join (cdr rest) "\n")))
                     (if (string-suffix? src "\n")
                         (string-append body "\n")
                         body))))
          (else (loop (cdr rest) (cons (car rest) acc)))))
      (values #f src)))

;; strip-frontmatter : string? -> string?
;;
;; Returns the body without the frontmatter block. Documents without
;; frontmatter come back unchanged.
(define (strip-frontmatter src)
  (let-values (((meta body) (split-frontmatter src)))
    (if meta body src)))

;; frontmatter->alist : (listof string?) -> (or/c #f (alist/c string? string?))
;;
;; Naive top-level `key: value` extraction. List items, nested maps and
;; multi-line values are skipped on purpose: the pipeline treats frontmatter
;; as optional hints, never as structure it depends on. Returns #f for an
;; empty block so callers can distinguish "no metadata" from "empty".
(define (frontmatter->alist lines)
  (define pairs
    (for/list ((line (in-list lines))
               #:unless (string-prefix? (string-trim line) "-")
               #:unless (string-prefix? line " ")
               (m (in-value (regexp-match #px"^([A-Za-z0-9_-]+):[[:space:]]*(.*)$" line)))
               #:when m)
      (cons (second m) (third m))))
  (if (null? pairs) #f pairs))

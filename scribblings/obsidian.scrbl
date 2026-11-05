#lang scribble/doc

@(require scribble/manual
          (for-label racket)
          (for-label markdown)
          (for-label obsidian))

@title{Obsidian: Obsidian Flavored Markdown for Racket}
@author{@(author+email "jrtxio" "jrtxio@users.noreply.github.com")}

@defmodule[obsidian]

Parses and renders @hyperlink["https://help.obsidian.md/syntax" "Obsidian Flavored
Markdown"]: YAML frontmatter, @tt{[[wikilinks]]}, @tt{![[embeds]]}, callouts,
inline tags, highlights and task lists.

@bold{Why} this package exists: Obsidian's dialect is the de-facto note format
of a large ecosystem, yet every renderer for it lives in JavaScript. This
package is a Racket implementation built @bold{on top of} the
@racketmodname[markdown] package — CommonMark parsing stays there; everything
here is the dialect layer. It is editor-neutral by design: frontmatter is
@emph{tolerated, never required}, and no dialect feature is load-bearing, so
plain Markdown renders identically.

@bold{Invariants}:

@itemlist[
  @item{Code never lies about content: @tt{[[links]]}, @tt{==marks==} and
  @tt{#tags} inside code blocks and inline code are always left literal.}
  @item{Unresolvable @tt{[[wikilinks]]} degrade to their label text — no dead
  links are ever rendered.}
  @item{Unresolvable non-image @tt{![[embeds]]} degrade to their plain
  wikilink form — nothing is silently dropped.}
]

@section{Frontmatter}

@defproc[(split-frontmatter [src string?])
         (values (or/c #f (alist/c string? string?)) string?)]{
  Splits an optional leading @tt{---} fence into naive top-level
  @tt{key: value} pairs and the body. The fence must start on line one; an
  unclosed fence is ordinary content.
}

@defproc[(strip-frontmatter [src string?]) string?]{
  Returns the body without the frontmatter block. Documents without
  frontmatter pass through unchanged.
}

@defproc[(frontmatter->alist [lines (listof string?)])
         (or/c #f (alist/c string? string?))]{
  Naive top-level @tt{key: value} extraction. Nested structures are skipped
  on purpose — frontmatter is a hint, never structure the pipeline depends on.
}

@section{Rendering}

@defparam[current-resolve-wikilink resolve-wikilink
          (-> string? (or/c string? #f))]{
  Maps a trimmed @tt{[[wikilink]]} target to a URL. @racket[#f] degrades the
  link to its label text.
}

@defparam[current-resolve-embed resolve-embed
          (-> string? (or/c (listof any/c) #f))]{
  Maps a trimmed @tt{![[embed]]} target to elements to splice in. When it
  returns @racket[#f], image targets get a default @tt{<img>} and everything
  else degrades to the plain wikilink form.
}

@defproc[(obsidian->xexprs [src string?]) (listof any/c)]{
  Parses source (stripping frontmatter) and applies every dialect rewrite,
  returning xexpr elements — the same AST shape the @racketmodname[markdown]
  package produces.
}

@defproc[(obsidian->html [src string?]) string?]{
  Convenience wrapper rendering @racket[obsidian->xexprs] to an HTML string.
}

@defproc[(document-title [src string?]) (or/c string? #f)]{
  The first H1's text, or @racket[#f]. This is the editor-neutral replacement
  for frontmatter titles: the title lives in the body, not in metadata.
}

@defproc[(document-tags [src string?]) (listof string?)]{
  All inline tags in document order, deduplicated.
}

@section{AST transforms}

Each @tt{rewrite-*} function takes the element list produced by
@racket[parse-markdown] (or another rewrite) and can be used à la carte:
@defproc[(rewrite-embeds [elements (listof any/c)]
                         [resolve-embed (-> string? (or/c #f (listof any/c)))])
         (listof any/c)]
@defproc[(rewrite-wikilinks [elements (listof any/c)]
                            [resolve-wikilink (-> string? (or/c string? #f))])
         (listof any/c)]
@defproc[(rewrite-highlights [elements (listof any/c)]) (listof any/c)]
@defproc[(rewrite-tags [elements (listof any/c)]) (listof any/c)]
@defproc[(rewrite-callouts [elements (listof any/c)]) (listof any/c)]
@defproc[(rewrite-task-lists [elements (listof any/c)]) (listof any/c)]

Source-level passes (needed because the underlying parser eats the
@tt{!} of embeds and merges blockquote lines, both fatal to embeds and
callouts) are exposed too.

@defproc[(map-source-text [src string?] [fn (-> string? string?)]) string?]{
  Applies @racket[fn] to ordinary text of @racket[src]; fenced code blocks
  and inline code spans pass through byte-for-byte.
}

@defproc[(mark-embeds [src string?]) string?]{
  Code-safe source pass replacing @tt{![[target]]} with a marker form that
  survives the upstream parser.
}

@defproc[(mark-callouts [src string?]) string?]{
  Code-safe source pass rewriting callout blockquotes into marker-delimited
  plain Markdown that @racket[rewrite-callouts] collects.
}

@defproc[(mark-dashes [src string?]) string?]{
  Code-safe source pass escaping hyphen-digit sequences so the upstream
  parser's smart dashes cannot corrupt tags and slugs.
}

@section{Known limitations}

@itemlist[
  @item{Callout titles are plain text; inline markup inside a title is not
  rendered.}
  @item{Indented (four-space) code blocks are not protected by source passes;
  Obsidian-authored content uses fenced blocks.}
  @item{Smart typography of the underlying parser is partially neutralized:
  hyphen-digit sequences (@tt{#tag-2}) are un-escaped to literal hyphens so
  tags and slugs survive; quote styling is left as the parser produces it.}
]

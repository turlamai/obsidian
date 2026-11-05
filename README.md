# obsidian

[English](README.md) · [简体中文](README.zh-CN.md)

Obsidian Flavored Markdown for Racket: frontmatter, `[[wikilinks]]`,
`![[embeds]]`, callouts, inline tags, highlights and task lists — as an AST
layer over the [`markdown`](https://pkgs.racket-lang.org/pkg/markdown)
package.

```racket
(require obsidian)

(obsidian->html
  (parameterize ((current-resolve-wikilink (lambda (t) (format "/p/~a" t))))
    "> [!tip] Pro tip\n> See [[other-note]] — written ==today== #racket"))
```

## Design

- **Editor-neutral.** The package never requires frontmatter and treats no
  dialect feature as load-bearing: plain Markdown renders identically, and
  documents authored in Obsidian, Typora or a plain text editor all work.
- **Code never lies about content.** `[[links]]`, `==marks==` and `#tags`
  inside code blocks and inline code are always left literal.
- **Graceful degradation.** Unresolvable `[[wikilinks]]` render as plain
  label text; unresolvable non-image `![[embeds]]` degrade to their wikilink
  form. Nothing is silently dropped, no dead links are rendered.
- **Layered.** CommonMark parsing belongs to the `markdown` package; this
  package is only the dialect.

## Install

```sh
raco pkg install obsidian
```

(From source: `raco pkg install --link <this-directory>`.)

## License

MIT — see `LICENSE`.

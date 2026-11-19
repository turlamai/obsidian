# obsidian

[English](README.md) · 简体中文

Racket 版 Obsidian 方言 Markdown：frontmatter、`[[wikilinks]]`、`![[embeds]]`、callout、行内标签、高亮与任务列表——构建在 [`markdown`](https://pkgs.racket-lang.org/pkg/markdown) 包之上的方言层（AST 级）。

```racket
(require obsidian)

(obsidian->html
  (parameterize ((current-resolve-wikilink (lambda (t) (format "/p/~a" t))))
    "> [!tip] Pro tip\n> See [[other-note]] — written ==today== #racket"))
```

## 设计

- **编辑器中立。** 不要求 frontmatter，任何方言特性都不是必需的：纯 Markdown 渲染结果不变，Obsidian、Typora 或纯文本编辑器写的内容都能工作。
- **代码块内容永不被篡改。** 代码块与行内代码中的 `[[links]]`、`==marks==`、`#tags` 始终保持字面。
- **优雅降级。** 无法解析的 `[[wikilinks]]` 渲染为纯标签文本；无法解析的非图片 `![[embeds]]` 退化为 wikilink 形式。不静默丢弃内容，不渲染死链。
- **分层清晰。** CommonMark 解析属于 `markdown` 包，本包只做方言。

## 安装

```sh
raco pkg install obsidian
```

（从源码：`raco pkg install --link <本目录>`。）

## 许可

MIT，见 `LICENSE`。

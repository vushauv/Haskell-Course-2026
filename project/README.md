# mdethodology

A static site generator: turns a folder of Markdown into a linked HTML site.

## Build a site

```
stack run -- build site/ -o out/
```

Reads pages from `site/`, writes HTML to `out/`.

## View it

```
cd out && python3 -m http.server 8000
```

Open http://localhost:8000/.

> Open through the server, not by double-clicking files — links are root-absolute and need an HTTP root.

## Writing pages

Each page is a `.md` file under `site/`. Start it with a YAML frontmatter block, then write Markdown:

```markdown
---
title: About
---
# About

A page with **bold**, *italic*, `code`, and a [link home](/).
```

- The file path becomes the URL: `site/about.md` → `/about`, `site/index.md` → `/`.
- Supported Markdown: headings, paragraphs, bold/italic, inline code, fenced code blocks, bullet lists, links.
- Internal links use the target route directly, e.g. `[about](/about)`. A link to a page that doesn't exist **fails the build**.
- External links (`https://…`) are left as-is.

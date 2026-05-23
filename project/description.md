# mdethodology: A Static Site Generator from Markdown and YAML

## Motivation

The idea comes from my intention to set up a personal website while writing as little code as possible for its future modifications. Due to my laziness, existing CMSs do not work, because I would need to dig into their docs, and I do not want to set up Ghost or, even worse, WordPress. In addition, I want to be able to easily add content or modify my website using Claude Code. From these three points the idea of a **Markdown-and-YAML-driven CMS** follows directly. However, for the scope of this project it is reduced to a static site generator (i.e. a subset of Hugo or Jekyll) as an essential step on the way to the full CMS of this type. I believe this idea makes real sense in the current world, where humanity has learned about the Markdown format and would like to create a blog simply by asking an AI agent that writes mostly `.md` and `.yml` files — which sounds much easier, and more human-readable on average, than dragging and dropping things in WordPress, setting up MCP server or vibecoding for hours. 

## Project Overview

mdethodology is a small static site generator that turns a directory of Markdown and YAML files into a linked HTML website.

The user writes pages as `.md` files with `YAML frontmatter`, a top-level `config.yml` for directory-wide settings, and `HTML` templates under `templates/`; the engine walks the source tree, derives a route for each page from its file path, resolves internal links against the resulting site model, applies the chosen template, and writes the rendered HTML to an output directory.

## Key Goals

1. **Markdown + YAML Parser**: Parse a useful subset of Markdown (headings, paragraphs, emphasis, inline code, lists, links, fenced code blocks) and a YAML subset rich enough for real metadata — scalars, lists, and nested maps. Both parsers written from scratch.
2. **Site Builder & Renderer**: Walk the source directory, build a queryable site model (pages, routes, metadata), resolve internal links between pages, apply templates with variable substitution, and write HTML to an output directory — wired together as a composable list of pipeline passes over the site model.
3. **Test Suite**: Cover the parsers, the route/link resolver, individual pipeline passes, and a handful of end-to-end builds against expected output.
4. **Dev Server (stretch)**: A small HTTP server that serves the built site on `localhost:8000` by running the same render pipeline on request, optionally rebuilding when source files change.

## Suggested Core Data Types

A starting point (to be adapted).

```haskell
-- A parsed source file: YAML frontmatter + Markdown body AST.
data SourceFile = SourceFile
  { srcPath        :: FilePath
  , srcFrontmatter :: YamlValue        
  , srcBody        :: [Block]
  }

-- YAML subset: scalars, lists, nested maps.
data YamlValue
  = YString String
  | YNumber Double
  | YBool   Bool
  | YNull

-- Markdown AST: enough to render real pages, not a full CommonMark clone.
data Block
  = Heading Int [Inline]
  | Paragraph [Inline]
  | BulletList [[Block]]
  | ...

data Inline
  = Text String
  | Emph [Inline]
  | Link { linkText :: [Inline], linkTarget :: LinkTarget }
  | ...

data LinkTarget = External String | Internal String   -- internal resolved later

-- The site model: every page, indexed by route. Open enough to query.
data Site = Site
  { sitePages  :: [(Route, Page)]
  , siteConfig :: YamlValue
  }

data Page = Page
  { pageSource   :: SourceFile
  , pageRoute    :: Route
  , pageTemplate :: TemplateName
  }

-- The pipeline: a build is a fold of passes over Site.
type Pass = Site -> IO Site

defaultPipeline :: [Pass]
defaultPipeline =
  [ loadSources
  , resolveLinks
  , applyTemplates
  , writeOutput
  ]
```

## Example

Input directory:

```
site/
  index.md
  templates/
    default.html
```

`site/index.md`:

```markdown
---
title: Home
layout: default
---

# Hello

Welcome to my site.
```

`site/templates/default.html`:

```html
<!doctype html>
<html>
  <head><title>{{ title }}</title></head>
  <body>{{ content }}</body>
</html>
```

Running:

```
$ mdethodology build site/ -o out/
parsed 1 page
wrote out/index.html
```

The resulting `out/index.html` contains the rendered Markdown body substituted into `default.html` at `{{ content}}` and `{{ title }}` is filled from the frontmatter.

## Implementation Components

### 1. Markdown + YAML Parser

- Parse a Markdown subset (headings, paragraphs, inline emphasis/strong/code, links, fenced code blocks, bullet lists) into an AST.
- Parse a YAML subset rich enough for real metadata — scalars, lists, and nested maps — both as the frontmatter block at the top of a `.md` file and as standalone `.yml` files.
- Report syntax errors with useful location information.

### 2. Site Builder & Renderer

- Walk the source directory and build a site model: every page, its route, its metadata, its parsed body.
- Derive each page's route from its file path; allow an explicit `slug` in frontmatter to override it.
- Resolve internal links against the site model; reject any link whose target does not correspond to a known page.
- Apply templates to render each page to HTML, substituting frontmatter variables and the page body, and write the result to the output directory.
- Wire the steps together as a composable pipeline of passes over the site model, so further passes (tag indexes, RSS, a dev server) can be added without rewriting the core.

### 3. Test Suite

- **Unit tests**: parser correctness on individual Markdown and YAML constructs; route derivation from file paths; template substitution on a fixed `(template, scope) → output` table.
- **End-to-end tests**: a handful of complete source directories with their expected output trees checked in. Build, then compare. Includes a fixture with a deliberately broken internal link to verify the error path.
- **Property-based tests**: parsing then re-serialising a YAML value is the identity on the supported subset (round-trip); for any successfully-built site, every internal link in the rendered HTML corresponds to an existing output file.

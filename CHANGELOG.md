# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Fediverse site validation and signature support (2026-01-07)
- Flag `--random` for `raco tr next` command (2025-12-29)
- TOC entry construction with taxon hint (2025-12-14)

### Changed
- Apply suggestions from linter (2026-01-07)
- Remove body in link (2026-01-07)
- Apply suggestions from language server (2025-12-29)
- Extract common part of TOC entry construction (2025-12-14)

### Removed
- Take back the `next-agda` command (2025-11-29)

## [1.1.0] - 2025-11-27

### Added
- Command `raco tr next-agda` for literate Agda files
- Default module content for literate Agda files

## [1.0] - 2025-11-20

### Added
- Immediate card `tr/card` feature
- External link functionality
- Meta/text, meta/link, and ORCID support
- `raco tr meta addr` command to show metadata JSON
- Metadata DOI and generation in card
- BibTeX for reference card
- RSS description for feed readers with full article content
- User-configurable LaTeX imports
- User-assignable asset directories in site.json
- Configurable `toc/depth`
- Viewport meta tag
- RSS item descriptions
- SVG output support
- `raco tr init` tool
- Nested TOC (at most 2-level)
- Transitive reference at one-level
- External link provider
- `pubDate` for RSS
- Transclude with expand/collapse control
- Keyboard event handling (Cmd/Ctrl + K) for search bar toggle
- Search functionality with minisearch
- Metadata JSON for each address
- Mention link functionality
- `@date` and `@author` metadata
- Context and backlinks features
- TOC generation
- KaTeX math support (inline and block)
- TikZ/tikzcd support for diagrams

### Changed
- `mention[#:title "text"]{addr}` now is `mention["addr"]{text}` (Breaking Change)
- Config setup for `watch` command
- Allow user to assign build mode in site.json
- Accept different config file from CLI
- `_build` directory cannot be configured
- Improved build process with metadata caching
- HTML escape improvements
- Fallback to address if no title provided
- LaTeX build excludes files with unchanged source
- Use at-exp reader for metadata building (performance improvement)
- Use set operations for metadata comparison
- Various CSS improvements (colors, padding, margins, layout)
- Moved various functionalities to Racket itself

### Fixed
- Undeclared dependency
- Details content location for proper closing
- Various bugs in transclude and mention logic
- Exclude check for metadata JSON
- SVG output issues
- Grammar fixes
- Build process for nested structures

### Removed
- Cache functionality (#49)
- iframe usage (#31)
- Dead code and unused features
- Legacy formulas meta
- Dependencies section in README

## [0.0.1] - 2025-05-31

### Added
- Initial project setup
- Basic Scribble-based card system
- HTML generation
- Basic CSS styling
- Asset management
- Tree function implementation

[Unreleased]: https://github.com/dannypsnl/tr-notes/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/dannypsnl/tr-notes/compare/v1.0...v1.1.0
[1.0]: https://github.com/dannypsnl/tr-notes/compare/v0.0.1...v1.0
[0.0.1]: https://github.com/dannypsnl/tr-notes/releases/tag/v0.0.1

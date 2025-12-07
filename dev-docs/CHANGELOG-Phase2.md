# CHANGELOG - Phase 2: Code Editor

**Status:** Complete
**Date:** December 6, 2024

## Overview
Implemented the core text editing functionality with syntax highlighting, parenthesis matching, and tabbed interface.

## Details

### Editor Widget (`lithp/editor/`)
- **`code_editor.py`**: Created `CodeEditor` class (subclass of `QPlainTextEdit`).
  - Implemented custom `LineNumberArea` widget for gutter line numbers.
  - Added highlighting for the current line.
  - Configured font rendering for Monospace fonts (Source Code Pro).
- **`highlighter.py`**: Created `LispHighlighter` class (subclass of `QSyntaxHighlighter`).
  - Defined regex rules for Lisp keywords, built-in functions, strings, comments, and numbers.
  - Applied color formatting to matched patterns.
- **`paren_matcher.py`**: Created `ParenMatcher` class.
  - Implemented logic to find match for parenthesis under cursor.
  - Highlights matching pair in teal/yellow.
  - Highlights mismatched/unmatched parenthesis in red.

### Tabbed Interface
- **`tab_widget.py`**: Created `EditorTabWidget` class (subclass of `QTabWidget`).
  - Handles multiple open documents.
  - Tracks modification state (adds `*` to tab title).
  - Implemented `new_file`, `open_file`, and `close_tab` logic.

### Integration
- Updated `MainWindow` to use `EditorTabWidget` in the center panel.
- Implemented file I/O operations in `MainWindow`:
  - `New`: Creates blank tab.
  - `Open`: standard file dialog to load content.
  - `Save` / `Save As`: Writes content to disk.

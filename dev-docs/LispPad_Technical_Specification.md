# LITHP IDE - Technical Specification

**Version 1.0 | December 2024**

*A Beginner-Friendly Common Lisp Development Environment*

---

## 1. Executive Summary

LITHP is a lightweight, educational integrated development environment designed specifically for students and beginning programmers learning Common Lisp. The application prioritizes simplicity, clarity, and immediate feedback while providing essential professional tools.

The IDE features a syntax-highlighted editor with parenthesis matching (critical for Lisp languages), an integrated file browser, and a full terminal emulator running the CLISP interpreter. Built with Python and PyQt6, the application targets Linux initially with cross-platform compatibility as a secondary goal.

---

## 2. Project Goals

### 2.1 Primary Objectives

- Provide a distraction-free environment for learning Lisp fundamentals
- Eliminate the complexity barrier of command-line tooling for beginners
- Offer visual feedback for parenthesis matching—essential for Lisp syntax comprehension
- Enable immediate code execution with full interactive I/O support
- Support standard file management workflows (open, save, organize projects)

### 2.2 Design Philosophy

The design philosophy emphasizes capability-first learning over enterprise patterns. Students should understand what their code does before learning industrial best practices. The interface should feel approachable and reduce cognitive load, allowing focus on the language itself.

---

## 3. Technical Architecture

### 3.1 Technology Stack

| Component | Technology |
|-----------|------------|
| **Language** | Python 3.10+ |
| **GUI Framework** | PyQt6 (Qt 6.x) |
| **Terminal** | PTY-based (ptyprocess) with pyte for VT100 emulation |
| **Lisp Runtime** | CLISP (GNU Common Lisp implementation) |
| **Syntax Highlighting** | QSyntaxHighlighter subclass with Lisp grammar |
| **Configuration** | JSON files (~/.config/lithp/) |

### 3.2 Application Architecture

The application follows a modular MVC-inspired architecture with clear separation between UI components and business logic. Each major feature is encapsulated in its own module for maintainability and testability.

#### Module Structure

```
lithp/
├── main.py                 # Application entry point
├── app.py                  # QApplication setup, main window
├── editor/
│   ├── code_editor.py      # QPlainTextEdit subclass
│   ├── highlighter.py      # Lisp syntax highlighter
│   ├── paren_matcher.py    # Parenthesis matching logic
│   └── line_numbers.py     # Line number widget
├── terminal/
│   ├── pty_terminal.py     # PTY management
│   └── terminal_widget.py  # Terminal display widget
├── browser/
│   └── file_browser.py     # File tree widget
├── config/
│   ├── settings.py         # Configuration management
│   └── themes.py           # Color scheme definitions
└── utils/                  # Shared utilities
```

---

## 4. Feature Specifications

### 4.1 Code Editor

The editor is the primary workspace and must provide a comfortable, responsive writing experience with Lisp-specific enhancements.

#### Core Requirements

1. Full Unicode support with UTF-8 encoding
2. Line numbers with current line highlighting
3. Configurable monospace font (default: Source Code Pro or similar)
4. Tab width configuration (default: 2 spaces for Lisp conventions)
5. Standard editing operations: cut, copy, paste, undo, redo
6. Find and replace functionality
7. Multiple file tabs with modified indicator

#### Syntax Highlighting

The highlighter must recognize and color-code the following Lisp elements:

- **Keywords:** defun, defvar, defparameter, let, let*, lambda, if, cond, case, when, unless, progn, loop, do, dotimes, dolist
- **Built-in functions:** car, cdr, cons, list, append, mapcar, reduce, format, print, read, etc.
- **Strings:** Double-quoted text with escape sequence support
- **Comments:** Semicolon to end-of-line, #| block comments |#
- **Numbers:** Integers, floats, rationals, complex numbers
- **Special forms:** quote, backquote, comma, splice

#### Parenthesis Matching

This feature is critical for Lisp development. The system must:

1. Highlight matching parenthesis when cursor is adjacent to one
2. Support (), [], and {} bracket types
3. Indicate unmatched/mismatched brackets with error highlighting
4. Optionally show scope depth via subtle background shading or margin indicators
5. Rainbow parentheses option (color-code by nesting depth)

### 4.2 Integrated Terminal

The terminal provides a full REPL experience running CLISP within the IDE. Using a PTY (pseudo-terminal) ensures proper interactive behavior including cursor control, color output, and bidirectional I/O.

#### Terminal Requirements

- VT100/ANSI escape sequence support for colors and cursor movement
- Full keyboard input including Ctrl+C, Ctrl+D, arrow keys
- Scrollback buffer (configurable, default 10,000 lines)
- Copy/paste support
- Configurable font and colors matching editor theme
- Process management: start, restart, interrupt CLISP session

#### REPL Integration

- "Send to REPL" command from editor (send selection or current expression)
- "Load File" command to load current file into REPL
- Clear REPL output command

### 4.3 File Browser

A tree-view file browser enables project navigation without leaving the IDE.

- Tree view with expand/collapse folders
- Double-click or Enter to open files in editor
- Right-click context menu: New File, New Folder, Rename, Delete
- Filter to show only .lisp, .lsp, .cl files (toggleable)
- Refresh command
- Set working directory command

---

## 5. User Interface Design

### 5.1 Layout

The main window uses a three-panel layout with splitters for user-adjustable sizing:

```
┌─────────────────────────────────────────────────────────────┐
│  Menu Bar  |  Toolbar                                       │
├──────────────┬──────────────────────────────────────────────┤
│              │                                              │
│    File      │           Code Editor (Tabbed)               │
│   Browser    │                                              │
│              │                                              │
│              ├──────────────────────────────────────────────┤
│              │                                              │
│              │           Terminal (CLISP REPL)              │
│              │                                              │
└──────────────┴──────────────────────────────────────────────┘
│              Status Bar                                     │
```

### 5.2 Color Themes

Ship with at least two themes, with the dark theme as default for reduced eye strain during extended coding sessions.

| Element | Dark Theme | Light Theme |
|---------|------------|-------------|
| Background | #1e1e2e (dark charcoal) | #fafafa (off-white) |
| Foreground | #cdd6f4 (light gray) | #2e2e2e (dark gray) |
| Keywords | #cba6f7 (mauve) | #8839ef (purple) |
| Strings | #a6e3a1 (green) | #40a02b (green) |
| Comments | #6c7086 (gray) | #8c8fa1 (gray) |
| Matched Paren | #f9e2af (yellow) bg | #df8e1d (orange) bg |

---

## 6. Dependencies & Requirements

### 6.1 Python Dependencies

```
PyQt6>=6.4.0          # GUI framework
ptyprocess>=0.7.0     # PTY management
pyte>=0.8.0           # Terminal emulation
```

### 6.2 System Requirements

- **Linux:** Ubuntu 22.04+ or equivalent (primary target)
- **CLISP:** Version 2.49+ (install via apt: `sudo apt install clisp`)
- **Python:** 3.10 or higher
- **Memory:** 512MB minimum, 1GB recommended
- **Display:** 1024x768 minimum resolution

### 6.3 Cross-Platform Considerations

For future macOS/Windows support:

- **macOS:** CLISP available via Homebrew; PTY support native
- **Windows:** Requires ConPTY (Windows 10+) or winpty fallback; CLISP via installer
- Abstract PTY interface to allow platform-specific implementations

---

## 7. Configuration System

User preferences are stored in JSON format under `~/.config/lithp/`.

### settings.json Example

```json
{
  "editor": {
    "font_family": "Source Code Pro",
    "font_size": 14,
    "tab_width": 2,
    "show_line_numbers": true,
    "rainbow_parens": true
  },
  "terminal": {
    "scrollback_lines": 10000,
    "clisp_path": "/usr/bin/clisp"
  },
  "theme": "dark"
}
```

---

## 8. Success Criteria

The project is considered complete when the following criteria are met:

- User can open, edit, and save .lisp files
- Syntax highlighting correctly identifies all major Lisp constructs
- Parenthesis matching visually indicates matched pairs and errors
- Terminal runs CLISP with full interactive capabilities
- Code can be sent from editor to REPL for execution
- File browser allows navigation and file management
- Application launches and remains stable during typical use
- Settings persist between sessions

---

## 9. Appendix: Why CLISP?

CLISP was chosen over SBCL and other implementations for the following reasons:

- **Interpreted mode:** Provides immediate feedback without compilation overhead
- **Friendly errors:** Error messages are more beginner-accessible than SBCL's
- **Lightweight:** Smaller memory footprint and faster startup
- **Cross-platform:** Excellent support across Linux, macOS, and Windows
- **ANSI compliance:** Full Common Lisp standard implementation

Students can later transition to SBCL when they need compiled performance, but CLISP provides the gentler learning curve appropriate for an educational tool.

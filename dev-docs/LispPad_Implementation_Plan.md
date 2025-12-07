# LITHP IDE - Implementation Plan

**Step-by-Step Development Guide for AI Agents**

**Version 1.0 | December 2024**

---

## Implementation Overview

This document provides a phased implementation plan for building LITHP IDE. Each phase builds upon the previous, allowing for incremental testing and validation. The plan is designed to be AI-agent friendly, with clear checkpoints and verification steps.

### Phase Summary

| Phase | Focus | Deliverable | Est. Effort |
|-------|-------|-------------|-------------|
| 1 | Project Foundation | Empty window + structure | 1-2 hours |
| 2 | Code Editor | Editor with highlighting | 3-4 hours |
| 3 | Terminal Integration | PTY terminal + CLISP | 4-6 hours |
| 4 | File Browser | Tree view + operations | 2-3 hours |
| 5 | Integration & Polish | Complete application | 3-4 hours |

---

## Phase 1: Project Foundation

**Goal:** Establish project structure, dependencies, and a basic PyQt6 window.

### Tasks

1. **Create project directory structure**

```
lithp/
├── __init__.py
├── main.py
├── app.py
├── editor/__init__.py
├── terminal/__init__.py
├── browser/__init__.py
└── config/__init__.py
```

2. **Create requirements.txt**

```
PyQt6>=6.4.0
ptyprocess>=0.7.0
pyte>=0.8.0
```

3. **Implement main.py entry point**

Create minimal entry point that initializes QApplication and shows main window.

4. **Implement app.py with MainWindow class**

Create QMainWindow subclass with basic menu bar (File menu with Exit), status bar, and placeholder central widget using QSplitter layout.

5. **Create config/settings.py**

Implement Settings class with JSON load/save to `~/.config/lithp/settings.json`. Include default values for all configurable options.

### Verification Checkpoint

- [ ] Running `python -m lithp` opens a window with menu bar and status bar
- [ ] File > Exit closes the application
- [ ] Settings file is created on first run
- [ ] Window can be resized and closed

---

## Phase 2: Code Editor

**Goal:** Implement the syntax-highlighted editor with parenthesis matching.

### Tasks

1. **Create editor/code_editor.py**

Subclass QPlainTextEdit. Implement line number area widget, current line highlighting, and custom paint event for line numbers. Set monospace font from settings.

2. **Create editor/highlighter.py**

Subclass QSyntaxHighlighter. Define highlighting rules for:
- Keywords (defun, defvar, let, lambda, if, cond, etc.)
- Built-in functions (car, cdr, cons, list, format, print, etc.)
- Strings (double-quoted with escape support)
- Comments (; to EOL and #| block |#)
- Numbers (integers, floats, rationals)
- Special characters (quote ', backquote `, comma ,)

3. **Create editor/paren_matcher.py**

Implement ParenMatcher class that:
- Connects to editor's cursorPositionChanged signal
- Finds matching bracket when cursor is adjacent to (, ), [, ], {, }
- Uses QTextEdit.ExtraSelection for highlighting
- Indicates mismatched brackets with error color

4. **Implement rainbow parentheses (optional enhancement)**

Color-code parentheses by nesting depth. Use a palette of 6-8 distinct colors that cycle.

5. **Create tabbed editor interface**

Wrap CodeEditor in QTabWidget. Implement tab management: add, close, modified indicator (*), close button per tab.

6. **Implement File operations**

Add to MainWindow: New File, Open File, Save, Save As menu items with standard keyboard shortcuts (Ctrl+N, Ctrl+O, Ctrl+S, Ctrl+Shift+S).

### Verification Checkpoint

- [ ] Can open, edit, and save .lisp files
- [ ] Syntax highlighting displays correctly for all Lisp constructs
- [ ] Placing cursor next to '(' highlights matching ')'
- [ ] Mismatched parentheses show error highlighting
- [ ] Multiple files can be open in tabs
- [ ] Modified files show * in tab title

---

## Phase 3: Terminal Integration

**Goal:** Implement a full PTY-based terminal running CLISP.

### Tasks

1. **Create terminal/pty_process.py**

Implement PTYProcess class that:
- Uses ptyprocess.PtyProcess to spawn CLISP
- Runs read loop in QThread to prevent UI blocking
- Emits signals for output data (dataReceived)
- Provides write() method for sending input

```python
class PTYProcess(QThread):
    data_received = pyqtSignal(bytes)
    process_exited = pyqtSignal(int)
    
    def run(self):
        self.process = ptyprocess.PtyProcess.spawn(['clisp'])
        while self.running and self.process.isalive():
            try:
                data = self.process.read(1024)
                if data:
                    self.data_received.emit(data)
            except EOFError:
                break
```

2. **Create terminal/terminal_widget.py**

Implement TerminalWidget (QWidget subclass) that:
- Uses pyte.Screen and pyte.Stream for VT100 emulation
- Renders terminal buffer with custom paintEvent
- Handles keyboard input including special keys
- Supports ANSI colors and text attributes
- Implements scrollback buffer

3. **Implement REPL integration commands**

Add menu items and keyboard shortcuts:
- Send Selection to REPL (Ctrl+Enter)
- Send Current Expression to REPL
- Load File in REPL (Ctrl+L)
- Clear REPL Output

4. **Implement terminal management**

Add Terminal menu with: Restart REPL, Interrupt (Ctrl+C to process), Clear Screen. Handle process termination gracefully.

### Verification Checkpoint

- [ ] CLISP REPL starts automatically when application opens
- [ ] Can type Lisp expressions and see results
- [ ] (read) and other input functions work correctly
- [ ] Ctrl+C interrupts running code
- [ ] Can send code from editor to REPL
- [ ] Colors and formatting display correctly

---

## Phase 4: File Browser

**Goal:** Implement the file/folder navigation panel.

### Tasks

1. **Create browser/file_browser.py**

Implement FileBrowser widget using QTreeView with QFileSystemModel:
- Set root path to user's home or last opened directory
- Configure to show only relevant columns (name, size)
- Enable sorting by name

2. **Implement file filtering**

Add toggle button/checkbox to show only Lisp files (*.lisp, *.lsp, *.cl). Use QSortFilterProxyModel for filtering.

3. **Implement context menu**

Right-click menu with:
- New File (creates .lisp file)
- New Folder
- Rename
- Delete (with confirmation)
- Open in System File Manager

4. **Connect to editor**

Double-click or Enter on a file opens it in the editor. Emit signal for MainWindow to handle.

5. **Add working directory controls**

Toolbar button or menu item to change root directory. Show current path in browser header. Remember last directory in settings.

### Verification Checkpoint

- [ ] File tree displays folders and files correctly
- [ ] Can expand/collapse folders
- [ ] Double-clicking file opens in editor
- [ ] Can create, rename, and delete files/folders
- [ ] Filter toggle works correctly

---

## Phase 5: Integration & Polish

**Goal:** Finalize the application with themes, settings UI, and polish.

### Tasks

1. **Create config/themes.py**

Define Theme dataclass with colors for: background, foreground, keyword, string, comment, number, function, matched_paren, error. Implement dark and light theme presets.

2. **Apply theming throughout application**

Create stylesheet from theme colors. Apply to editor, terminal, file browser, and all UI elements. Add View > Theme submenu for switching.

3. **Create Settings dialog**

QDialog with tabs for:
- Editor: Font family, size, tab width, line numbers toggle
- Terminal: Scrollback lines, CLISP path
- Appearance: Theme selection, rainbow parens toggle

4. **Implement Find/Replace**

Add Edit > Find (Ctrl+F) and Edit > Replace (Ctrl+H). Create find bar widget that appears below editor with previous/next buttons, case sensitivity, and whole word options.

5. **Add toolbar**

Create toolbar with icons for: New, Open, Save, Run (send to REPL), Stop. Use standard icons from QStyle or include icon set.

6. **Implement status bar information**

Show in status bar: current line:column, file encoding, CLISP status (running/stopped).

7. **Add Help menu**

About dialog with version info. Link to CLISP documentation. Keyboard shortcuts reference.

8. **Final testing and bug fixes**

Test all features end-to-end. Fix any issues. Ensure graceful handling of errors (file permissions, CLISP not installed, etc.).

### Final Verification

- [ ] Application launches with saved settings
- [ ] Theme switching works throughout UI
- [ ] Settings dialog saves and applies changes
- [ ] Find/Replace works correctly
- [ ] All keyboard shortcuts function
- [ ] Error conditions are handled gracefully
- [ ] Complete workflow: open project, edit code, run in REPL, debug, save

---

## Testing Guide

Use this sample Lisp code to verify all editor features:

```lisp
;;; Sample Lisp code for testing LITHP

(defun factorial (n)
  "Calculate the factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defvar *greeting* "Hello, Lisp!")

#|
  Block comment example
  Spans multiple lines
|#

(let ((x 10)
      (y 3.14)
      (ratio 22/7))
  (format t "Values: ~A, ~A, ~A~%" x y ratio))

;; Test nested parens
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
```

### Test Checklist

- [ ] 'defun', 'defvar', 'if', 'let' highlighted as keywords
- [ ] String content shows in string color
- [ ] Both comment styles display in comment color
- [ ] Numbers (10, 3.14, 22/7) highlighted correctly
- [ ] Cursor next to '(' highlights matching ')'
- [ ] Deeply nested parens have rainbow colors (if enabled)
- [ ] Code executes correctly when sent to REPL
- [ ] (read) prompts for input and receives it

---

## Key Technical Decisions

1. **PTY over pipes:** Enables proper interactive I/O for (read) and other input functions
2. **pyte for terminal:** VT100 emulation handles ANSI escape sequences correctly
3. **QThread for PTY read:** Prevents UI blocking during terminal output
4. **QSyntaxHighlighter:** Qt's built-in highlighter integrates smoothly with QPlainTextEdit
5. **JSON config:** Human-readable, easy to edit, consistent with Just Code approach

---

## Error Handling Guide

| Condition | Detection | Response |
|-----------|-----------|----------|
| CLISP not found | `which clisp` returns empty | Show installation dialog |
| PTY spawn failure | Exception in PtyProcess | Show error, offer retry |
| File read error | IOError on open | Show error dialog |
| File write error | IOError on save | Show error, keep buffer |
| Invalid JSON config | JSONDecodeError | Use defaults, log warning |
| Terminal crash | process_exited signal | Show restart button |

---

## Cross-Platform Notes (Future)

### macOS
- CLISP via Homebrew: `brew install clisp`
- PTY works natively

### Windows
- Requires Windows 10+ for ConPTY
- Or use `winpty` as fallback
- CLISP has Windows installer
- Abstract PTY interface to swap implementations

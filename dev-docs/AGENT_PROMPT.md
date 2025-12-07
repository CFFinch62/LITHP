# LITHP IDE - AI Agent Project Prompt

## Project Identity

**Name:** LITHP  
**Type:** Educational Lisp IDE  
**Target Users:** Students and beginning programmers learning Common Lisp  
**Platform:** Linux (primary), cross-platform (future)  
**Tech Stack:** Python 3.10+, PyQt6, CLISP

---

## Project Summary

Build a lightweight, beginner-friendly IDE for Common Lisp development featuring:

1. **Syntax-highlighted code editor** with parenthesis matching
2. **File/folder browser** for project navigation  
3. **Integrated PTY terminal** running CLISP REPL with full I/O capabilities

The design philosophy prioritizes clarity and immediate feedback over industrial complexity. Students should understand their code before learning enterprise patterns.

---

## Directory Structure

```
lithp/
├── __init__.py
├── main.py                 # Entry point
├── app.py                  # MainWindow, application setup
├── editor/
│   ├── __init__.py
│   ├── code_editor.py      # QPlainTextEdit subclass with line numbers
│   ├── highlighter.py      # Lisp syntax highlighter
│   ├── paren_matcher.py    # Parenthesis matching logic
│   └── tab_widget.py       # Tabbed editor interface
├── terminal/
│   ├── __init__.py
│   ├── pty_process.py      # PTY process management (QThread)
│   └── terminal_widget.py  # Terminal display with pyte
├── browser/
│   ├── __init__.py
│   └── file_browser.py     # QTreeView file browser
├── config/
│   ├── __init__.py
│   ├── settings.py         # JSON settings management
│   └── themes.py           # Color theme definitions
└── resources/
    └── icons/              # Toolbar icons
```

---

## Dependencies

```
# requirements.txt
PyQt6>=6.4.0
ptyprocess>=0.7.0
pyte>=0.8.0
```

**System requirement:** CLISP (`sudo apt install clisp`)

---

## Core Components Specification

### 1. Code Editor (`editor/code_editor.py`)

Subclass `QPlainTextEdit` with:

- **Line number area:** Custom widget painted in `paintEvent`
- **Current line highlighting:** Use `QTextEdit.ExtraSelection`
- **Monospace font:** Default to "Source Code Pro" or "DejaVu Sans Mono"
- **Tab handling:** Convert to spaces (default 2)

```python
class CodeEditor(QPlainTextEdit):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.line_number_area = LineNumberArea(self)
        self.highlighter = LispHighlighter(self.document())
        self.paren_matcher = ParenMatcher(self)
        # ... setup font, connect signals
```

### 2. Syntax Highlighter (`editor/highlighter.py`)

Subclass `QSyntaxHighlighter`. Define rules as list of (pattern, format) tuples:

**Keywords (bold, keyword_color):**
```
defun|defvar|defparameter|defmacro|defclass|defmethod|defgeneric
let|let\*|flet|labels|lambda|setq|setf
if|when|unless|cond|case|typecase
loop|do|do\*|dolist|dotimes|progn|block|return|return-from
and|or|not|nil|t
```

**Built-in functions (function_color):**
```
car|cdr|cons|list|append|reverse|length|nth|first|rest|last
mapcar|mapc|maplist|reduce|remove|remove-if|find|find-if|member
format|print|princ|prin1|terpri|read|read-line
eq|eql|equal|equalp|null|atom|listp|numberp|stringp|symbolp
\+|\-|\*|/|<|>|<=|>=|=|/=|1\+|1\-|mod|floor|ceiling|round|abs
```

**Patterns:**
- Strings: `"([^"\\]|\\.)*"` (green)
- Line comments: `;.*$` (gray)
- Block comments: `#\|.*?\|#` (gray, multiline)
- Numbers: `\b-?[0-9]+(/[0-9]+)?(\.[0-9]+)?\b` (number_color)
- Special: `'|`|,@|,` (special_color)

### 3. Parenthesis Matcher (`editor/paren_matcher.py`)

Connect to `cursorPositionChanged` signal. When cursor is adjacent to bracket:

1. Find matching bracket using stack-based algorithm
2. Create `QTextEdit.ExtraSelection` for both brackets
3. Use `matched_color` background for valid match
4. Use `error_color` background for mismatch/unmatched

**Optional rainbow parens:** Track nesting depth during highlighting, assign colors from rotating palette (6-8 colors).

### 4. PTY Terminal (`terminal/pty_process.py`)

```python
class PTYProcess(QThread):
    data_received = pyqtSignal(bytes)
    process_exited = pyqtSignal(int)
    
    def __init__(self, command=['clisp']):
        super().__init__()
        self.process = None
        self.command = command
        self.running = True
    
    def run(self):
        self.process = ptyprocess.PtyProcess.spawn(self.command)
        while self.running and self.process.isalive():
            try:
                data = self.process.read(1024)
                if data:
                    self.data_received.emit(data)
            except EOFError:
                break
        self.process_exited.emit(self.process.exitstatus or 0)
    
    def write(self, data: bytes):
        if self.process and self.process.isalive():
            self.process.write(data)
    
    def terminate(self):
        self.running = False
        if self.process:
            self.process.terminate(force=True)
```

### 5. Terminal Widget (`terminal/terminal_widget.py`)

Use `pyte` for VT100 emulation:

```python
class TerminalWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.screen = pyte.Screen(80, 24)
        self.stream = pyte.Stream(self.screen)
        self.pty = PTYProcess()
        self.pty.data_received.connect(self.on_data)
        # ... setup font, focus policy
    
    def on_data(self, data: bytes):
        self.stream.feed(data.decode('utf-8', errors='replace'))
        self.update()  # Trigger repaint
    
    def paintEvent(self, event):
        painter = QPainter(self)
        # Render self.screen.display line by line
        # Apply colors from self.screen.buffer
    
    def keyPressEvent(self, event):
        # Map Qt keys to terminal sequences
        # Send to self.pty.write()
```

### 6. File Browser (`browser/file_browser.py`)

```python
class FileBrowser(QWidget):
    file_selected = pyqtSignal(str)  # Emits file path
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.model = QFileSystemModel()
        self.tree = QTreeView()
        self.tree.setModel(self.model)
        self.tree.doubleClicked.connect(self.on_double_click)
        # ... setup filter, context menu
```

### 7. Settings (`config/settings.py`)

Store in `~/.config/lithp/settings.json`:

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
  "browser": {
    "last_directory": "~",
    "show_hidden": false,
    "lisp_filter": true
  },
  "theme": "dark"
}
```

### 8. Themes (`config/themes.py`)

```python
@dataclass
class Theme:
    name: str
    background: str
    foreground: str
    keyword: str
    function: str
    string: str
    comment: str
    number: str
    matched_paren: str
    error: str
    current_line: str

DARK_THEME = Theme(
    name="dark",
    background="#1e1e2e",
    foreground="#cdd6f4",
    keyword="#cba6f7",
    function="#89b4fa",
    string="#a6e3a1",
    comment="#6c7086",
    number="#fab387",
    matched_paren="#f9e2af",
    error="#f38ba8",
    current_line="#313244"
)

LIGHT_THEME = Theme(
    name="light",
    background="#fafafa",
    foreground="#2e2e2e",
    keyword="#8839ef",
    function="#1e66f5",
    string="#40a02b",
    comment="#8c8fa1",
    number="#fe640b",
    matched_paren="#df8e1d",
    error="#d20f39",
    current_line="#e6e9ef"
)
```

---

## Main Window Layout

```
┌─────────────────────────────────────────────────────────────┐
│  File  Edit  View  Terminal  Help                           │
├─────────────────────────────────────────────────────────────┤
│  [New] [Open] [Save] │ [Run] [Stop]                         │
├──────────────┬──────────────────────────────────────────────┤
│              │  [file1.lisp] [file2.lisp] [+]               │
│   File       │ ─────────────────────────────────────────────│
│   Browser    │                                              │
│              │         Code Editor                          │
│  [▼ project] │                                              │
│    file1.lisp│                                              │
│    file2.lisp│ ─────────────────────────────────────────────│
│  [▼ lib]     │                                              │
│    utils.lisp│         Terminal (CLISP REPL)                │
│              │                                              │
├──────────────┴──────────────────────────────────────────────┤
│  Ln 42, Col 15  │  UTF-8  │  CLISP running                  │
└─────────────────────────────────────────────────────────────┘
```

Use `QSplitter` for resizable panels:
- Horizontal splitter: File Browser | Editor+Terminal
- Vertical splitter: Editor | Terminal

---

## Menu Structure

**File:**
- New (Ctrl+N)
- Open (Ctrl+O)
- Save (Ctrl+S)
- Save As (Ctrl+Shift+S)
- ---
- Close Tab (Ctrl+W)
- ---
- Exit (Ctrl+Q)

**Edit:**
- Undo (Ctrl+Z)
- Redo (Ctrl+Shift+Z)
- ---
- Cut (Ctrl+X)
- Copy (Ctrl+C)
- Paste (Ctrl+V)
- ---
- Find (Ctrl+F)
- Replace (Ctrl+H)
- ---
- Preferences

**View:**
- Theme > Dark / Light
- Toggle File Browser
- Toggle Line Numbers
- Toggle Rainbow Parens

**Terminal:**
- Send Selection (Ctrl+Enter)
- Load File (Ctrl+L)
- ---
- Clear Output
- Restart REPL
- Interrupt (sends Ctrl+C to process)

**Help:**
- Keyboard Shortcuts
- CLISP Documentation (opens URL)
- ---
- About LITHP

---

## Implementation Order

1. **main.py + app.py** - Get a window showing
2. **code_editor.py** - Basic text editing with line numbers
3. **highlighter.py** - Syntax colors
4. **paren_matcher.py** - Bracket matching
5. **pty_process.py** - CLISP subprocess
6. **terminal_widget.py** - Terminal display
7. **file_browser.py** - File navigation
8. **settings.py + themes.py** - Configuration
9. **Polish** - Menus, toolbar, find/replace, settings dialog

---

## Testing Checklist

Use this Lisp code to verify features:

```lisp
;;; Test file for LITHP

(defun factorial (n)
  "Calculate factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defvar *greeting* "Hello, Lisp!")

#|
  Block comment test
  Multiple lines
|#

(let ((x 10)
      (y 3.14)
      (ratio 22/7))
  (format t "x=~A, y=~A, ratio=~A~%" x y ratio))

;; Test nested parens
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
```

**Verify:**
- [ ] Keywords highlighted (defun, defvar, if, let, lambda)
- [ ] Strings in string color
- [ ] Both comment styles highlighted
- [ ] Numbers highlighted (10, 3.14, 22/7)
- [ ] Matching parens highlight when cursor adjacent
- [ ] Mismatched paren shows error
- [ ] Code runs in REPL
- [ ] (read) works for input

---

## Error Handling

- **CLISP not found:** Show dialog with installation instructions
- **File permission denied:** Show error, don't crash
- **PTY spawn failure:** Fall back to message, offer retry
- **Invalid settings JSON:** Use defaults, log warning

---

## Cross-Platform Notes (Future)

**macOS:**
- CLISP via Homebrew: `brew install clisp`
- PTY works natively

**Windows:**
- Requires Windows 10+ for ConPTY
- Or use `winpty` as fallback
- CLISP has Windows installer
- Abstract PTY interface to swap implementations

---

## Agent Instructions

When implementing this project:

1. **Start with Phase 1** - Get the window structure working first
2. **Test incrementally** - Verify each component before moving on
3. **Use the verification checkpoints** in the Implementation Plan
4. **Follow PyQt6 patterns** - Signal/slot connections, proper parent hierarchies
5. **Keep it simple** - This is educational software, clarity over cleverness
6. **Handle errors gracefully** - Students will make mistakes, don't crash

The goal is a working IDE that helps people learn Lisp, not a feature-complete professional tool.

# LITHP Architecture Reference

## Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              MainWindow (app.py)                            │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                           QMenuBar                                      ││
│  │  [File] [Edit] [View] [Terminal] [Help]                                ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                           QToolBar                                      ││
│  │  [New] [Open] [Save] [|] [Run] [Stop] [|] [Settings]                   ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                      QSplitter (Horizontal)                             ││
│  │  ┌──────────────┐  ┌────────────────────────────────────────────────┐  ││
│  │  │              │  │           QSplitter (Vertical)                 │  ││
│  │  │  FileBrowser │  │  ┌──────────────────────────────────────────┐  │  ││
│  │  │              │  │  │            QTabWidget                    │  │  ││
│  │  │  QTreeView   │  │  │  ┌────────────────────────────────────┐  │  │  ││
│  │  │      +       │  │  │  │          CodeEditor               │  │  │  ││
│  │  │  QFileSystem │  │  │  │  QPlainTextEdit + LispHighlighter │  │  │  ││
│  │  │    Model     │  │  │  │  + ParenMatcher + LineNumbers     │  │  │  ││
│  │  │              │  │  │  └────────────────────────────────────┘  │  │  ││
│  │  │              │  │  └──────────────────────────────────────────┘  │  ││
│  │  │              │  │  ┌──────────────────────────────────────────┐  │  ││
│  │  │              │  │  │          TerminalWidget                 │  │  ││
│  │  │              │  │  │  pyte.Screen + PTYProcess (QThread)    │  │  ││
│  │  │              │  │  │         ↕ ptyprocess ↕                  │  │  ││
│  │  │              │  │  │            [CLISP]                      │  │  ││
│  │  │              │  │  └──────────────────────────────────────────┘  │  ││
│  │  └──────────────┘  └────────────────────────────────────────────────┘  ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                           QStatusBar                                    ││
│  │  [Ln 42, Col 15]  [UTF-8]  [CLISP: running]                            ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Flow

```
User Input                    System                         Output
─────────────────────────────────────────────────────────────────────

[Keyboard] ──────────────────> CodeEditor
                                   │
                                   ├──> LispHighlighter ──> Colored text
                                   │
                                   └──> ParenMatcher ──────> Bracket highlights

[Keyboard in Terminal] ──────> TerminalWidget
                                   │
                                   └──> PTYProcess.write() ──> CLISP stdin
                                              │
                                              └──> CLISP process

CLISP stdout ────────────────> PTYProcess.data_received signal
                                   │
                                   └──> TerminalWidget.on_data()
                                              │
                                              └──> pyte.Stream.feed()
                                                        │
                                                        └──> Screen buffer
                                                                  │
                                                                  └──> paintEvent()

[File Browser Double-Click] ──> file_selected signal
                                   │
                                   └──> MainWindow.open_file()
                                              │
                                              └──> New tab with CodeEditor

[Send to REPL Command] ───────> MainWindow
                                   │
                                   └──> Get selection from CodeEditor
                                              │
                                              └──> PTYProcess.write(selection + "\n")
```

## Class Relationships

```
QMainWindow
    └── MainWindow (app.py)
            ├── Settings (config/settings.py)
            ├── Theme (config/themes.py)
            │
            ├── FileBrowser (browser/file_browser.py)
            │       ├── QTreeView
            │       └── QFileSystemModel
            │
            ├── QTabWidget
            │       └── CodeEditor[] (editor/code_editor.py)
            │               ├── QPlainTextEdit (base)
            │               ├── LineNumberArea (internal widget)
            │               ├── LispHighlighter (editor/highlighter.py)
            │               │       └── QSyntaxHighlighter (base)
            │               └── ParenMatcher (editor/paren_matcher.py)
            │
            └── TerminalWidget (terminal/terminal_widget.py)
                    ├── QWidget (base)
                    ├── pyte.Screen
                    ├── pyte.Stream
                    └── PTYProcess (terminal/pty_process.py)
                            ├── QThread (base)
                            └── ptyprocess.PtyProcess
```

## Signal/Slot Connections

```python
# File Browser
file_browser.file_selected.connect(main_window.open_file)
file_browser.directory_changed.connect(main_window.update_title)

# Editor
code_editor.cursorPositionChanged.connect(paren_matcher.highlight_matching)
code_editor.textChanged.connect(main_window.mark_modified)
code_editor.modificationChanged.connect(tab_widget.update_tab_title)

# Terminal
pty_process.data_received.connect(terminal_widget.on_data)
pty_process.process_exited.connect(terminal_widget.on_exit)

# Actions
action_send_to_repl.triggered.connect(main_window.send_selection_to_repl)
action_load_file.triggered.connect(main_window.load_file_in_repl)
action_restart_repl.triggered.connect(terminal_widget.restart)
action_interrupt.triggered.connect(terminal_widget.interrupt)

# Settings
settings_dialog.accepted.connect(main_window.apply_settings)
theme_action.triggered.connect(main_window.change_theme)
```

## File Format: settings.json

```json
{
  "version": 1,
  "editor": {
    "font_family": "Source Code Pro",
    "font_size": 14,
    "tab_width": 2,
    "show_line_numbers": true,
    "highlight_current_line": true,
    "rainbow_parens": true,
    "auto_indent": true
  },
  "terminal": {
    "scrollback_lines": 10000,
    "clisp_path": "/usr/bin/clisp",
    "font_family": "Source Code Pro",
    "font_size": 14
  },
  "browser": {
    "last_directory": "/home/user/lisp-projects",
    "show_hidden": false,
    "lisp_filter": true
  },
  "window": {
    "geometry": "AdnQywADAAAAAAMgAAABkAAAB38AAAQ...",
    "splitter_state": "AAAA/wAAAAEAAAACAA..."
  },
  "theme": "dark",
  "recent_files": [
    "/home/user/project/main.lisp",
    "/home/user/project/utils.lisp"
  ]
}
```

## Keyboard Shortcuts

| Action | Shortcut | Context |
|--------|----------|---------|
| New File | Ctrl+N | Global |
| Open File | Ctrl+O | Global |
| Save | Ctrl+S | Editor |
| Save As | Ctrl+Shift+S | Editor |
| Close Tab | Ctrl+W | Editor |
| Undo | Ctrl+Z | Editor |
| Redo | Ctrl+Shift+Z | Editor |
| Find | Ctrl+F | Editor |
| Replace | Ctrl+H | Editor |
| Send to REPL | Ctrl+Enter | Editor |
| Load File | Ctrl+L | Editor |
| Interrupt | Ctrl+C | Terminal |
| Quit | Ctrl+Q | Global |

## Error States

```
State                  Detection                    Response
─────────────────────────────────────────────────────────────────
CLISP not found       which clisp returns empty    Show install dialog
PTY spawn fail        PtyProcess exception         Show error, offer retry  
File read error       IOError on open              Show error dialog
File write error      IOError on save              Show error, keep buffer
Invalid JSON config   JSONDecodeError              Use defaults, log warning
Terminal crash        process_exited signal        Show restart button
```

## Theme Color Map

```
Element              Dark Theme      Light Theme     CSS Property
──────────────────────────────────────────────────────────────────
Background           #1e1e2e         #fafafa         background-color
Foreground           #cdd6f4         #2e2e2e         color
Keyword              #cba6f7         #8839ef         (highlighter)
Function             #89b4fa         #1e66f5         (highlighter)
String               #a6e3a1         #40a02b         (highlighter)
Comment              #6c7086         #8c8fa1         (highlighter)
Number               #fab387         #fe640b         (highlighter)
Matched Paren BG     #f9e2af         #df8e1d         (ExtraSelection)
Error BG             #f38ba8         #d20f39         (ExtraSelection)
Current Line BG      #313244         #e6e9ef         (ExtraSelection)
Selection BG         #45475a         #ccd0da         selection-background
Line Number FG       #6c7086         #8c8fa1         color
Line Number BG       #181825         #f2f4f8         background-color
```

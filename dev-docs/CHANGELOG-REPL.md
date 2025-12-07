# CHANGELOG - REPL Enhancement: Multi-Interpreter Support

**Status:** Complete
**Date:** December 7, 2024

## Overview
Migrated default Lisp interpreter from CLISP to SBCL and added automatic detection and selection of installed Lisp interpreters. Users can now easily switch between different Common Lisp implementations.

## Details

### Interpreter Migration
- **Default Interpreter**: Changed from CLISP to SBCL (Steel Bank Common Lisp)
- Updated default paths in `lithp/config/settings.py`
- Updated fallback commands in `lithp/terminal/pty_process.py` and `terminal_widget.py`
- Renamed setting key from `clisp_path` to `lisp_path` for generic interpreter support

### New: Interpreter Detection (`lithp/config/lisp_detector.py`)
Created new module to automatically detect installed Lisp interpreters:
- **Supported Interpreters**: SBCL, CLISP, ECL, CCL (Clozure CL), ABCL, Allegro CL, LispWorks
- **Version Detection**: Retrieves version information where available
- **Validation**: Provides path validation for custom interpreter paths

### Enhanced Settings Dialog (`lithp/config/settings_dialog.py`)
Redesigned Terminal settings tab with:
- **Interpreter Dropdown**: Shows all detected interpreters with version info
- **Refresh Button**: Re-scans system for installed interpreters
- **Custom Path Option**: Checkbox to enable manual path entry
- **Browse Button**: File dialog for locating interpreter executables
- **Path Validation**: Warns if specified path is not a valid executable

### Restart REPL Feature
- Added `restart_repl_requested` signal to settings dialog
- When interpreter is changed, prompts user: "Restart the REPL now?"
- If confirmed, REPL restarts immediately with new interpreter
- Status bar confirms: "REPL restarted with /path/to/interpreter"

### Terminal Widget Enhancement (`lithp/terminal/terminal_widget.py`)
- Enhanced `restart()` method to accept optional new command
- Reloads settings on restart to pick up interpreter changes
- Creates fresh PTY process with new interpreter

### Settings Fix (`lithp/config/settings.py`)
- Fixed `set()` method to support top-level settings (e.g., `theme`)
- Now supports both `set("section", "key", value)` and `set("key", value)` signatures

### Documentation Updates
- Updated README.md with SBCL references
- Updated USER-GUIDE.md with new interpreter selection instructions

## Files Changed

| File | Change |
|------|--------|
| `lithp/config/settings.py` | Changed default to SBCL, fixed `set()` method |
| `lithp/config/settings_dialog.py` | New interpreter selection UI, restart signal |
| `lithp/config/lisp_detector.py` | **New file** - Interpreter detection module |
| `lithp/terminal/pty_process.py` | Default command changed to SBCL |
| `lithp/terminal/terminal_widget.py` | Enhanced restart with new interpreter support |
| `lithp/app.py` | Connected restart signal, added handler |
| `README.md` | Updated for SBCL |
| `docs/USER-GUIDE.md` | Updated terminal/interpreter documentation |


# CHANGELOG - Phase 4: File Browser

**Status:** Complete
**Date:** December 6, 2024

## Overview
Implemented the side panel file browser for navigating the project directory structure.

## Details

### Browser Widget (`lithp/browser/`)
- **`file_browser.py`**: Created `FileBrowser` class.
  - Uses `QTreeView` backed by `QFileSystemModel`.
  - Implemented `LispFileFilterProxy` (`QSortFilterProxyModel`) to show only folders and Lisp source files (`.lisp`, `.lsp`, `.cl`).
  - Hid unnecessary columns (Size, Type, Date) for a cleaner view.

### Interactions
- **Context Menu**: Added right-click menu options:
  - `New File`: Creates new file in selected directory.
  - `New Folder`: Creates new directory.
  - `Rename`: Renames selected item.
  - `Delete`: Deletes item with confirmation.
- **Navigation**: Double-clicking a file emits `file_selected` signal.

### Integration
- Replaced placeholder left panel in `MainWindow` with `FileBrowser`.
- Connected `file_selected` signal to `MainWindow.open_file_from_browser`, allowing users to open files directly from the tree view.

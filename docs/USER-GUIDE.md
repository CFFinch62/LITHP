# LITHP IDE User Guide

Welcome to LITHP, a beginner-friendly Integrated Development Environment (IDE) for GNU Common Lisp. This guide will help you install, configure, and use the key features of the application.

## 1. Installation and Running

### Prerequisites
- **Python 3.10+**: Ensure Python is installed on your system.
- **CLISP**: The IDE relies on `clisp` for its REPL. Install it via your package manager (e.g., `sudo apt install clisp`).

### Starting LITHP
LITHP comes with automated scripts for easy setup and launching:

1.  **Open a terminal** in the LITHP project directory.
2.  **Run the launch script**:
    ```bash
    ./run.sh
    ```
    - The script will automatically create a virtual environment (`venv`) if it doesn't exist.
    - It will install all necessary dependencies (`PyQt6`, etc.).
    - It will launch the application.

## 2. Interface Overview

The main window is divided into three sections:

1.  **File Browser (Left)**: Navigate your project directories.
2.  **Code Editor (Center)**: The main area for writing Lisp code. Supports multiple tabs.
3.  **Terminal (Bottom)**: Integrated CLISP REPL for immediate code execution.

### Menu Bar
- **File**: Create New files, Open existing ones, Save, or Exit.
- **Edit**: access Preferences and Find/Replace.
- **Terminal**: Control the REPL (Restart, Clear, Interrupt) and send code to it.
- **View**: (Reserved for future toggle options).
- **Help**: About information.

## 3. Key Features

### Code Editor
- **Syntax Highlighting**: Keywords, numbers, strings, and comments are colored for readability.
- **Parenthesis Matching**: Placing the cursor next to a parenthesis highlights its matching partner. Mismatched parentheses are highlighted in red.
- **Line Numbers**: Displayed in the left gutter.
- **Tabbed Editing**: Work on multiple files simultaneously.

### Integrated Terminal (REPL)
The bottom panel runs a real `clisp` process. You can type specialized Lisp commands directly here.
- **Interactive**: Type `(+ 1 2)` and press Enter to see `3`.
- **History**: Use Up/Down arrows to cycle through command history.
- **Integration**:
    - **Ctrl+Enter**: Sends the currently selected text (or the current line) from the Editor to the REPL.
    - **Ctrl+L**: Loads the current file into the REPL `(load "filename")`.
    - **Ctrl+C**: Interrupts the current computation.

### File Browser
- **Navigation**: Double-click folders to expand/collapse.
- **Opening Files**: Double-click a file (`.lisp`, `.lsp`, `.cl`) to open it in the editor.
- **Context Menu**: Right-click to Create New File/Folder, Rename, or Delete items.

## 4. Configuration

Access the settings via **Edit -> Preferences**.

### Editor Settings
- **Font**: Change the font family (e.g., "Monospace", "Courier New") and size.
- **Tab Width**: Set the indentation space count (default: 4).
- **Line Numbers**: Toggle visibility.

### Terminal Settings
- **CLISP Path**: If `clisp` is not in your system PATH, specify the absolute path here.
- **Scrollback**: Limit the number of lines retained in the terminal history.

### Appearance
- **Theme**: Switch between **Dark** and **Light** themes to suit your preference.

## 5. Troubleshooting

- **"CLISP not found"**: Ensure `clisp` is installed. If installed in a custom location, update the path in **Preferences -> Terminal**.
- **PyQt6 Errors**: If you encounter display issues, ensure your graphics drivers are up to date.

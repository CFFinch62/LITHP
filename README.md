![LITHP Banner](images/lithp_banner.png)

# LITHP (Lisp IDE To Help Programmers)

**LITHP** is a beginner-friendly Integrated Development Environment (IDE) designed specifically for students and new programmers learning **Common Lisp**. Built with Python and PyQt6, it provides a modern, accessible interface for the powerful `clisp` environment.

## ✨ Features

*   **Syntax Highlighting:** Clear color-coding for standard Lisp keywords, functions, and data types.
*   **Parenthesis Matching:** Visual cues for matching parentheses to help navigate nested structures.
*   **Integrated REPL:** A built-in Terminal running `clisp`, allowing for immediate code execution to see results instantly.
*   **Editor-to-REPL Workflow:** seamlessly send code selections or files directly to the REPL with shortcuts (Ctrl+Enter, Ctrl+L).
*   **Project Navigation:** integrated File Browser for managing your Lisp source files.
*   **Customization:** Switch between Dark and Light themes and configure editor preferences.

## 🚀 Quick Start

### Prerequisites
*   **Linux, macOS, or Windows**
*   **Python 3.10+**
*   **GNU CLISP** (`sudo apt install clisp` or `brew install clisp`)

### Installation & Running

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/yourusername/LITHP.git
    cd LITHP
    ```

2.  **Launch the Application:**
    Simply run the included script. It will automatically set up the virtual environment and dependencies for you.
    ```bash
    ./run.sh
    ```

### Building for Distribution

To create a standalone executable for your platform:

```bash
python3 build.py
```
The executable will be located in the `dist/LITHP/` folder.

## 📖 Documentation

*   [**User Guide**](docs/USER-GUIDE.md): Detailed usage instructions.
*   [**Developer Changelog**](dev-docs/CHANGELOG-Master.md): History of changes and phases.

## 🛠️ Technology Stack

*   **Frontend:** Python 3.12, PyQt6
*   **Terminal Emulation:** `pyte`, `ptyprocess`
*   **Backend:** GNU CLISP

## 📜 License

(c) 2025 Chuck Finch - Fragillidae Software

---
*Created with the assistance of Google DeepMind Agents.*

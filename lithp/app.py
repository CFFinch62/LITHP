
from PyQt6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QSplitter, 
                            QMenuBar, QStatusBar, QMenu, QLabel)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction

from PyQt6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QSplitter, 
                            QMenuBar, QStatusBar, QMenu, QLabel, QFileDialog, QMessageBox,
                            QToolBar, QApplication) # Added QToolBar, QApplication
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction, QIcon
from pathlib import Path

from lithp.config.settings import Settings
from lithp.editor.tab_widget import EditorTabWidget
from lithp.terminal.terminal_widget import TerminalWidget
from lithp.browser.file_browser import FileBrowser
from lithp.config.themes import get_theme, apply_theme_to_app
from lithp.config.settings_dialog import SettingsDialog
from lithp.editor.find_replace import FindReplaceDialog

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.settings = Settings()
        self.setWindowTitle("LITHP - Lisp IDE To Help Programmers")
        self.resize(1024, 768)
        
        self.setup_ui()
        self.create_toolbar() # Added toolbar
        self.load_settings()
        self.apply_theme() # Apply theme on startup

    def setup_ui(self):
        # Central widget and layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        layout = QVBoxLayout(central_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Main splitter (Horizontal)
        self.main_splitter = QSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(self.main_splitter)
        
        # Left Panel (File Browser)
        self.file_browser = FileBrowser()
        self.file_browser.file_selected.connect(self.open_file_from_browser)
        self.main_splitter.addWidget(self.file_browser)
        
        # Right Panel (Vertical Splitter for Editor and Terminal)
        self.right_splitter = QSplitter(Qt.Orientation.Vertical)
        self.main_splitter.addWidget(self.right_splitter)
        
        # Editor Panel (Tab Widget)
        self.editor_tabs = EditorTabWidget()
        self.right_splitter.addWidget(self.editor_tabs)
        
        # Terminal Panel
        self.terminal = TerminalWidget()
        self.right_splitter.addWidget(self.terminal)
        
        # Set initial splitter sizes
        self.main_splitter.setSizes([200, 800])
        self.right_splitter.setSizes([500, 200])

        # Menu Bar
        self.create_menus()
        
        # Status Bar
        self.setup_status_bar()

    def create_menus(self):
        menu_bar = self.menuBar()
        
        # File Menu
        file_menu = menu_bar.addMenu("&File")
        
        self.new_action = QAction("&New", self)
        self.new_action.setShortcut("Ctrl+N")
        self.new_action.triggered.connect(self.new_file)
        file_menu.addAction(self.new_action)
        
        self.open_action = QAction("&Open...", self)
        self.open_action.setShortcut("Ctrl+O")
        self.open_action.triggered.connect(self.open_file)
        file_menu.addAction(self.open_action)
        
        self.save_action = QAction("&Save", self)
        self.save_action.setShortcut("Ctrl+S")
        self.save_action.triggered.connect(self.save_file)
        file_menu.addAction(self.save_action)
        
        save_as_action = QAction("Save &As...", self)
        save_as_action.setShortcut("Ctrl+Shift+S")
        save_as_action.triggered.connect(self.save_file_as)
        file_menu.addAction(save_as_action)
        
        file_menu.addSeparator()
        
        exit_action = QAction("E&xit", self)
        exit_action.setShortcut("Ctrl+Q")
        exit_action.setStatusTip("Exit application")
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)
        
        # Edit Menu
        edit_menu = menu_bar.addMenu("&Edit")
        
        find_action = QAction("&Find/Replace", self)
        find_action.setShortcut("Ctrl+F")
        find_action.triggered.connect(self.show_find_replace)
        edit_menu.addAction(find_action)
        
        edit_menu.addSeparator()
        
        # Adding Preferences here
        prefs_action = QAction("&Preferences", self)
        prefs_action.triggered.connect(self.open_settings)
        edit_menu.addAction(prefs_action)

        # View Menu
        view_menu = menu_bar.addMenu("&View")
        
        self.toggle_browser_action = QAction("Show &File Browser", self)
        self.toggle_browser_action.setCheckable(True)
        self.toggle_browser_action.setChecked(True)
        self.toggle_browser_action.triggered.connect(self.toggle_browser)
        view_menu.addAction(self.toggle_browser_action)
        
        self.toggle_terminal_action = QAction("Show &Terminal", self)
        self.toggle_terminal_action.setCheckable(True)
        self.toggle_terminal_action.setChecked(True)
        self.toggle_terminal_action.triggered.connect(self.toggle_terminal)
        view_menu.addAction(self.toggle_terminal_action)
        
        # Terminal Menu
        terminal_menu = menu_bar.addMenu("&Terminal")
        
        self.send_sel_action = QAction("Send &Selection to REPL", self)
        self.send_sel_action.setShortcut("Ctrl+Enter")
        self.send_sel_action.triggered.connect(self.send_selection_to_repl)
        terminal_menu.addAction(self.send_sel_action)
        
        load_file_action = QAction("&Load File in REPL", self)
        load_file_action.setShortcut("Ctrl+L")
        load_file_action.triggered.connect(self.load_file_in_repl)
        terminal_menu.addAction(load_file_action)
        
        terminal_menu.addSeparator()
        
        clear_action = QAction("&Clear Output", self)
        clear_action.triggered.connect(self.terminal.clear)
        terminal_menu.addAction(clear_action)
        
        restart_action = QAction("&Restart REPL", self)
        restart_action.triggered.connect(self.terminal.restart)
        terminal_menu.addAction(restart_action)
        
        interrupt_action = QAction("&Interrupt", self)
        interrupt_action.triggered.connect(self.terminal.interrupt)
        terminal_menu.addAction(interrupt_action)
        
        # Help Menu
        help_menu = menu_bar.addMenu("&Help")
        about_action = QAction("&About", self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)

    def create_toolbar(self):
        toolbar = QToolBar("Main Toolbar")
        self.addToolBar(toolbar)
        
        toolbar.addAction(self.new_action)
        toolbar.addAction(self.open_action)
        toolbar.addAction(self.save_action)
        toolbar.addSeparator()
        toolbar.addAction(self.send_sel_action)

    def setup_status_bar(self):
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)
        
        self.cursor_label = QLabel("Ln 1, Col 1")
        self.cursor_label.setMinimumWidth(100)
        self.status_bar.addPermanentWidget(self.cursor_label)
        
        self.status_bar.showMessage("Ready")
        
        # Connect tab changes to update connection
        self.editor_tabs.currentChanged.connect(self.on_tab_changed)
        # Initialize for first tab if exists?
        self.on_tab_changed(0) 

    def on_tab_changed(self, index):
        editor = self.editor_tabs.get_current_editor()
        if editor:
            # Disconnect previous if needed? 
            # Simplified: just connect. Pass check to avoid dupe connections in a real robust app,
            # or use unique connection type. PyQt handles multiple connects by calling multiple times,
            # so we should be careful. 
            # A cleaner way is to disconnect all from signal, but we don't have reference to old.
            # We'll just try/except disconnect or manage in TabWidget.
            # For this MVP, we will rely on connecting single slot that checks sender() or just update.
            # Actually, simplest is:
            try:
                editor.cursorPositionChanged.disconnect(self.update_cursor_position)
            except:
                pass
            editor.cursorPositionChanged.connect(self.update_cursor_position)
            self.update_cursor_position()
        else:
            self.cursor_label.setText("")

    def update_cursor_position(self):
        editor = self.editor_tabs.get_current_editor()
        if editor:
            cursor = editor.textCursor()
            line = cursor.blockNumber() + 1
            col = cursor.columnNumber() + 1
            self.cursor_label.setText(f"Ln {line}, Col {col}")

    def toggle_browser(self, checked):
        self.file_browser.setVisible(checked)

    def toggle_terminal(self, checked):
        self.terminal.setVisible(checked)

    def open_settings(self):
        dialog = SettingsDialog(self, self.settings)
        if dialog.exec():
            # Settings saved by dialog on accept
            self.apply_theme()
            pass

    def apply_theme(self):
        theme_name = self.settings.get("theme")
        theme = get_theme(theme_name)
        apply_theme_to_app(QApplication.instance(), theme)

    def show_find_replace(self):
        editor = self.editor_tabs.get_current_editor()
        if editor:
            dialog = FindReplaceDialog(editor, self)
            dialog.show()

    def show_about(self):
        QMessageBox.about(self, "About LITHP", 
                          "LITHP IDE - Lisp IDE To Help Programmers v1.0\n\nA student/beginner-friendly GNU CLisp environment.\n\n(c) 2025 Chuck Finch - Fragillidae Software")

    def new_file(self):
        self.editor_tabs.new_file()

    def open_file(self):
        path, _ = QFileDialog.getOpenFileName(self, "Open File", "", "Lisp Files (*.lisp *.lsp *.cl);;All Files (*)")
        if path:
            try:
                p = Path(path)
                with open(p, 'r', encoding='utf-8') as f:
                    content = f.read()
                self.editor_tabs.open_file(p, content)
                self.status_bar.showMessage(f"Opened {p.name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Could not open file: {e}")

    def open_file_from_browser(self, path):
        try:
            p = Path(path)
            with open(p, 'r', encoding='utf-8') as f:
                content = f.read()
            self.editor_tabs.open_file(p, content)
            self.status_bar.showMessage(f"Opened {p.name}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Could not open file: {e}")

    def save_file(self):
        editor = self.editor_tabs.get_current_editor()
        if not editor:
            return
            
        file_path = editor.property("file_path")
        if not file_path:
            self.save_file_as()
        else:
            self.save_file_to_path(editor, file_path)

    def save_file_as(self):
        editor = self.editor_tabs.get_current_editor()
        if not editor:
            return
            
        path, _ = QFileDialog.getSaveFileName(self, "Save File", "", "Lisp Files (*.lisp);;All Files (*)")
        if path:
            self.save_file_to_path(editor, path)

    def save_file_to_path(self, editor, path):
        try:
            with open(path, 'w', encoding='utf-8') as f:
                f.write(editor.toPlainText())
            editor.setProperty("file_path", str(path))
            editor.document().setModified(False)
            idx = self.editor_tabs.indexOf(editor)
            self.editor_tabs.setTabText(idx, Path(path).name)
            self.status_bar.showMessage(f"Saved {Path(path).name}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Could not save file: {e}")

    def send_selection_to_repl(self):
        editor = self.editor_tabs.get_current_editor()
        if not editor:
            return
        
        cursor = editor.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
        else:
            # Send current line or block?
            # For now, just current line if no selection
            # Or better: send paragraph/block
            cursor.select(cursor.SelectionType.BlockUnderCursor)
            text = cursor.selectedText()
            
        # Replace unicode paragraph separator if any
        text = text.replace('\u2029', '\n')
        
        if text:
            self.terminal.write(text.encode('utf-8') + b'\n')

    def load_file_in_repl(self):
        editor = self.editor_tabs.get_current_editor()
        if not editor:
            return
            
        file_path = editor.property("file_path")
        if file_path:
            # Escape path for Lisp?
            # (load "path")
            cmd = f'(load "{file_path}")\n'
            self.terminal.write(cmd.encode('utf-8'))
        else:
            QMessageBox.warning(self, "Warning", "Save file first before loading.")

    def load_settings(self):
        # Load geometry if available
        geometry = self.settings.get("window", "geometry")
        if geometry:
            self.restoreGeometry(bytes.fromhex(geometry))
            
        splitter_state = self.settings.get("window", "splitter_state")
        if splitter_state:
            self.main_splitter.restoreState(bytes.fromhex(splitter_state))

    def closeEvent(self, event):
        # Save settings on close
        self.settings.set("window", "geometry", self.saveGeometry().toHex().data().decode())
        self.settings.set("window", "splitter_state", self.main_splitter.saveState().toHex().data().decode())
        self.settings.save()
        
        # Terminate PTY
        self.terminal.pty.terminate_process()
        self.terminal.pty.wait()
        
        event.accept()

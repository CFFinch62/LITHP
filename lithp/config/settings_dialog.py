
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                            QWidget, QLabel, QSpinBox, QCheckBox, QComboBox,
                            QDialogButtonBox, QLineEdit, QFileDialog, QPushButton,
                            QGroupBox, QMessageBox)
from PyQt6.QtCore import Qt, pyqtSignal

from lithp.config.lisp_detector import detect_lisp_interpreters, is_valid_interpreter


class SettingsDialog(QDialog):
    # Signal emitted when user wants to restart REPL with new interpreter
    restart_repl_requested = pyqtSignal(str)  # Emits the new interpreter path
    def __init__(self, parent, settings):
        super().__init__(parent)
        self.settings = settings
        self.setWindowTitle("Preferences")
        self.resize(500, 400)
        
        self.layout = QVBoxLayout(self)
        
        self.tabs = QTabWidget()
        self.layout.addWidget(self.tabs)
        
        self.create_editor_tab()
        self.create_terminal_tab()
        self.create_appearance_tab()
        
        # Buttons
        self.button_box = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        self.button_box.accepted.connect(self.accept)
        self.button_box.rejected.connect(self.reject)
        self.layout.addWidget(self.button_box)
        
        self.load_values()

    def create_editor_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Font Family
        h = QHBoxLayout()
        h.addWidget(QLabel("Font Family:"))
        self.font_family_edit = QLineEdit()
        h.addWidget(self.font_family_edit)
        layout.addLayout(h)
        
        # Font Size
        h = QHBoxLayout()
        h.addWidget(QLabel("Font Size:"))
        self.font_size_spin = QSpinBox()
        self.font_size_spin.setRange(8, 72)
        h.addWidget(self.font_size_spin)
        layout.addLayout(h)
        
        # Tab Width
        h = QHBoxLayout()
        h.addWidget(QLabel("Tab Width:"))
        self.tab_width_spin = QSpinBox()
        self.tab_width_spin.setRange(1, 16)
        h.addWidget(self.tab_width_spin)
        layout.addLayout(h)
        
        # Line Numbers
        self.line_numbers_check = QCheckBox("Show Line Numbers")
        layout.addWidget(self.line_numbers_check)
        
        # Rainbow Parens (Placeholder for now)
        self.rainbow_check = QCheckBox("Enable Rainbow Parentheses")
        layout.addWidget(self.rainbow_check)
        
        layout.addStretch()
        self.tabs.addTab(tab, "Editor")

    def create_terminal_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Lisp Interpreter Selection Group
        interpreter_group = QGroupBox("Lisp Interpreter")
        interpreter_layout = QVBoxLayout(interpreter_group)

        # Detected interpreters dropdown
        h = QHBoxLayout()
        h.addWidget(QLabel("Interpreter:"))
        self.interpreter_combo = QComboBox()
        self.interpreter_combo.setMinimumWidth(250)
        self.interpreter_combo.currentIndexChanged.connect(self.on_interpreter_changed)
        h.addWidget(self.interpreter_combo)

        refresh_btn = QPushButton("Refresh")
        refresh_btn.setToolTip("Re-scan for installed Lisp interpreters")
        refresh_btn.clicked.connect(self.refresh_interpreters)
        h.addWidget(refresh_btn)
        interpreter_layout.addLayout(h)

        # Custom path option
        h = QHBoxLayout()
        self.custom_path_check = QCheckBox("Use custom path:")
        self.custom_path_check.toggled.connect(self.on_custom_path_toggled)
        h.addWidget(self.custom_path_check)

        self.lisp_path_edit = QLineEdit()
        self.lisp_path_edit.setEnabled(False)
        self.lisp_path_edit.setPlaceholderText("Path to Lisp interpreter executable")
        h.addWidget(self.lisp_path_edit)

        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self.browse_lisp)
        h.addWidget(browse_btn)
        interpreter_layout.addLayout(h)

        layout.addWidget(interpreter_group)

        # Scrollback
        h = QHBoxLayout()
        h.addWidget(QLabel("Scrollback Lines:"))
        self.scrollback_spin = QSpinBox()
        self.scrollback_spin.setRange(100, 1000000)
        self.scrollback_spin.setSingleStep(100)
        h.addWidget(self.scrollback_spin)
        layout.addLayout(h)

        # Note about restart
        note_label = QLabel("<i>Note: Changes to interpreter require restarting the REPL.</i>")
        note_label.setStyleSheet("color: gray;")
        layout.addWidget(note_label)

        layout.addStretch()
        self.tabs.addTab(tab, "Terminal")

        # Populate interpreters
        self.refresh_interpreters()

    def create_appearance_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Theme
        h = QHBoxLayout()
        h.addWidget(QLabel("Theme:"))
        self.theme_combo = QComboBox()
        self.theme_combo.addItems(["dark", "light"])
        h.addWidget(self.theme_combo)
        layout.addLayout(h)
        
        layout.addStretch()
        self.tabs.addTab(tab, "Appearance")

    def refresh_interpreters(self):
        """Detect and populate the interpreter combo box."""
        self.interpreter_combo.clear()
        self.detected_interpreters = detect_lisp_interpreters()

        if not self.detected_interpreters:
            self.interpreter_combo.addItem("No interpreters found", "")
            self.interpreter_combo.setEnabled(False)
            # Force custom path mode
            self.custom_path_check.setChecked(True)
            self.custom_path_check.setEnabled(False)
        else:
            self.interpreter_combo.setEnabled(True)
            self.custom_path_check.setEnabled(True)
            for interp in self.detected_interpreters:
                self.interpreter_combo.addItem(interp.display_name(), interp.path)

    def on_interpreter_changed(self, index):
        """Handle interpreter selection change."""
        if not self.custom_path_check.isChecked() and index >= 0:
            path = self.interpreter_combo.currentData()
            if path:
                self.lisp_path_edit.setText(path)

    def on_custom_path_toggled(self, checked):
        """Handle custom path checkbox toggle."""
        self.lisp_path_edit.setEnabled(checked)
        self.interpreter_combo.setEnabled(not checked)

        if not checked and self.interpreter_combo.currentData():
            self.lisp_path_edit.setText(self.interpreter_combo.currentData())

    def browse_lisp(self):
        """Browse for a Lisp interpreter executable."""
        path, _ = QFileDialog.getOpenFileName(self, "Select Lisp Interpreter Executable")
        if path:
            self.lisp_path_edit.setText(path)
            self.custom_path_check.setChecked(True)

    def load_values(self):
        # Editor
        self.font_family_edit.setText(self.settings.get("editor", "font_family"))
        self.font_size_spin.setValue(self.settings.get("editor", "font_size"))
        self.tab_width_spin.setValue(self.settings.get("editor", "tab_width"))
        self.line_numbers_check.setChecked(self.settings.get("editor", "show_line_numbers"))
        self.rainbow_check.setChecked(self.settings.get("editor", "rainbow_parens"))

        # Terminal - Lisp interpreter
        saved_path = self.settings.get("terminal", "lisp_path")
        if saved_path:
            self.lisp_path_edit.setText(saved_path)
            # Check if saved path matches a detected interpreter
            found_match = False
            for i in range(self.interpreter_combo.count()):
                if self.interpreter_combo.itemData(i) == saved_path:
                    self.interpreter_combo.setCurrentIndex(i)
                    found_match = True
                    break

            if not found_match and saved_path:
                # Custom path is set
                self.custom_path_check.setChecked(True)

        self.scrollback_spin.setValue(self.settings.get("terminal", "scrollback_lines"))

        # Appearance
        current_theme = self.settings.get("theme")
        index = self.theme_combo.findText(current_theme)
        if index >= 0:
            self.theme_combo.setCurrentIndex(index)

    def save_values(self):
        # Editor
        self.settings.set("editor", "font_family", self.font_family_edit.text())
        self.settings.set("editor", "font_size", self.font_size_spin.value())
        self.settings.set("editor", "tab_width", self.tab_width_spin.value())
        self.settings.set("editor", "show_line_numbers", self.line_numbers_check.isChecked())
        self.settings.set("editor", "rainbow_parens", self.rainbow_check.isChecked())

        # Terminal - Lisp interpreter
        lisp_path = self.lisp_path_edit.text()
        self.settings.set("terminal", "lisp_path", lisp_path)
        self.settings.set("terminal", "scrollback_lines", self.scrollback_spin.value())

        # Appearance
        self.settings.set("theme", self.theme_combo.currentText())

        self.settings.save()

    def accept(self):
        # Validate the interpreter path
        lisp_path = self.lisp_path_edit.text()
        if lisp_path and not is_valid_interpreter(lisp_path):
            result = QMessageBox.warning(
                self,
                "Invalid Interpreter",
                f"The path '{lisp_path}' does not appear to be a valid executable.\n\nSave anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )
            if result == QMessageBox.StandardButton.No:
                return

        # Check if interpreter changed
        old_path = self.settings.get("terminal", "lisp_path")
        interpreter_changed = (lisp_path != old_path)

        self.save_values()

        # Offer to restart REPL if interpreter changed
        if interpreter_changed and lisp_path:
            result = QMessageBox.question(
                self,
                "Restart REPL?",
                f"The Lisp interpreter has been changed.\n\nRestart the REPL now with the new interpreter?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )
            if result == QMessageBox.StandardButton.Yes:
                self.restart_repl_requested.emit(lisp_path)

        super().accept()

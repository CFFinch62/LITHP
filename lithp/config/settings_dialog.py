
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, 
                            QWidget, QLabel, QSpinBox, QCheckBox, QComboBox, 
                            QDialogButtonBox, QLineEdit, QFileDialog, QPushButton)
from PyQt6.QtCore import Qt

class SettingsDialog(QDialog):
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
        
        # CLISP Path
        h = QHBoxLayout()
        h.addWidget(QLabel("CLISP Path:"))
        self.clisp_path_edit = QLineEdit()
        h.addWidget(self.clisp_path_edit)
        btn = QPushButton("Browse...")
        btn.clicked.connect(self.browse_clisp)
        h.addWidget(btn)
        layout.addLayout(h)
        
        # Scrollback
        h = QHBoxLayout()
        h.addWidget(QLabel("Scrollback Lines:"))
        self.scrollback_spin = QSpinBox()
        self.scrollback_spin.setRange(100, 1000000)
        self.scrollback_spin.setSingleStep(100)
        h.addWidget(self.scrollback_spin)
        layout.addLayout(h)
        
        layout.addStretch()
        self.tabs.addTab(tab, "Terminal")

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

    def browse_clisp(self):
        path, _ = QFileDialog.getOpenFileName(self, "Select CLISP Executable")
        if path:
            self.clisp_path_edit.setText(path)

    def load_values(self):
        # Editor
        self.font_family_edit.setText(self.settings.get("editor", "font_family"))
        self.font_size_spin.setValue(self.settings.get("editor", "font_size"))
        self.tab_width_spin.setValue(self.settings.get("editor", "tab_width"))
        self.line_numbers_check.setChecked(self.settings.get("editor", "show_line_numbers"))
        self.rainbow_check.setChecked(self.settings.get("editor", "rainbow_parens"))
        
        # Terminal
        self.clisp_path_edit.setText(self.settings.get("terminal", "clisp_path"))
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
        
        # Terminal
        self.settings.set("terminal", "clisp_path", self.clisp_path_edit.text())
        self.settings.set("terminal", "scrollback_lines", self.scrollback_spin.value())
        
        # Appearance
        self.settings.set("theme", self.theme_combo.currentText())
        
        self.settings.save()
        
    def accept(self):
        self.save_values()
        super().accept()

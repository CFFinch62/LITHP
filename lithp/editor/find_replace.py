
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel, 
                            QLineEdit, QPushButton, QCheckBox, QMessageBox)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QTextCursor, QTextDocument

class FindReplaceDialog(QDialog):
    def __init__(self, editor, parent=None):
        super().__init__(parent)
        self.editor = editor
        self.setWindowTitle("Find/Replace")
        self.setFixedWidth(400)
        
        layout = QVBoxLayout(self)
        
        # Find
        h_find = QHBoxLayout()
        h_find.addWidget(QLabel("Find:"))
        self.find_input = QLineEdit()
        h_find.addWidget(self.find_input)
        layout.addLayout(h_find)
        
        # Replace
        h_replace = QHBoxLayout()
        h_replace.addWidget(QLabel("Replace:"))
        self.replace_input = QLineEdit()
        h_replace.addWidget(self.replace_input)
        layout.addLayout(h_replace)
        
        # Options
        self.case_check = QCheckBox("Case Sensitive")
        layout.addWidget(self.case_check)
        
        # Buttons
        btn_layout = QHBoxLayout()
        find_next_btn = QPushButton("Find Next")
        find_next_btn.clicked.connect(self.find_next)
        btn_layout.addWidget(find_next_btn)
        
        replace_btn = QPushButton("Replace")
        replace_btn.clicked.connect(self.replace)
        btn_layout.addWidget(replace_btn)
        
        replace_all_btn = QPushButton("Replace All")
        replace_all_btn.clicked.connect(self.replace_all)
        btn_layout.addWidget(replace_all_btn)
        
        layout.addLayout(btn_layout)

    def find_next(self):
        text = self.find_input.text()
        if not text:
            return
            
        flags = QTextDocument.FindFlag(0)
        if self.case_check.isChecked():
            flags |= QTextDocument.FindFlag.FindCaseSensitively
            
        found = self.editor.find(text, flags)
        
        if not found:
            # Wrap around?
            cursor = self.editor.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.Start)
            self.editor.setTextCursor(cursor)
            found = self.editor.find(text, flags)
            
            if not found:
                QMessageBox.information(self, "Find", "Text not found.")

    def replace(self):
        cursor = self.editor.textCursor()
        if cursor.hasSelection() and cursor.selectedText() == self.find_input.text():
            cursor.insertText(self.replace_input.text())
            self.find_next()
        else:
            self.find_next()

    def replace_all(self):
        text = self.find_input.text()
        if not text:
            return
            
        replacement = self.replace_input.text()
        count = 0
        
        cursor = self.editor.textCursor()
        cursor.beginEditBlock()
        
        cursor.movePosition(QTextCursor.MoveOperation.Start)
        self.editor.setTextCursor(cursor)
        
        flags = QTextDocument.FindFlag(0)
        if self.case_check.isChecked():
            flags |= QTextDocument.FindFlag.FindCaseSensitively
            
        while self.editor.find(text, flags):
            cursor = self.editor.textCursor()
            cursor.insertText(replacement)
            count += 1
            
        cursor.endEditBlock()
        QMessageBox.information(self, "Replace All", f"Replaced {count} occurrences.")

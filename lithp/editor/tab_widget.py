
from PyQt6.QtWidgets import QTabWidget, QMessageBox
from PyQt6.QtCore import pyqtSignal
from lithp.editor.code_editor import CodeEditor

class EditorTabWidget(QTabWidget):
    tab_changed = pyqtSignal(int)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setTabsClosable(True)
        self.setMovable(True)
        self.tabCloseRequested.connect(self.close_tab)
        self.currentChanged.connect(self.on_tab_changed)

    def new_file(self):
        editor = CodeEditor()
        index = self.addTab(editor, "Untitled")
        self.setCurrentIndex(index)
        editor.modificationChanged.connect(lambda m: self.update_tab_title(editor, m))
        editor.setFocus()
        return editor

    def open_file(self, path, content):
        editor = CodeEditor()
        editor.setPlainText(content)
        editor.document().setModified(False)
        editor.setProperty("file_path", str(path))
        
        index = self.addTab(editor, path.name)
        self.setCurrentIndex(index)
        
        editor.modificationChanged.connect(lambda m: self.update_tab_title(editor, m))
        editor.setFocus()
        return editor

    def close_tab(self, index):
        editor = self.widget(index)
        if editor.document().isModified():
            reply = QMessageBox.question(
                self, "Unsaved Changes",
                f"Save changes to {self.tabText(index)}?",
                QMessageBox.StandardButton.Save | 
                QMessageBox.StandardButton.Discard | 
                QMessageBox.StandardButton.Cancel
            )
            
            if reply == QMessageBox.StandardButton.Cancel:
                return
            elif reply == QMessageBox.StandardButton.Save:
                # Signal main window or handle save here? 
                # Better to let main window handle I/O or have a callback
                # For now, just return False to indicate not closed if main handles it
                # But here we are inside the widget.
                pass 
        
        self.removeTab(index)

    def update_tab_title(self, editor, modified):
        index = self.indexOf(editor)
        if index != -1:
            title = self.tabText(index)
            if modified and not title.endswith("*"):
                self.setTabText(index, title + "*")
            elif not modified and title.endswith("*"):
                self.setTabText(index, title[:-1])

    def get_current_editor(self):
        return self.currentWidget()

    def on_tab_changed(self, index):
        self.tab_changed.emit(index)


from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QTreeView, 
                            QHeaderView, QMenu, QMessageBox)
from PyQt6.QtCore import Qt, pyqtSignal, QDir, QSortFilterProxyModel
from PyQt6.QtGui import QAction, QFileSystemModel
from pathlib import Path
import os
import sys

from lithp.config.settings import Settings

class LispFileFilterProxy(QSortFilterProxyModel):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.enabled = False
        
    def filterAcceptsRow(self, source_row, source_parent):
        if not self.enabled:
            return True
            
        model = self.sourceModel()
        index = model.index(source_row, 0, source_parent)
        
        # Always accept directories
        if model.isDir(index):
            return True
            
        name = model.fileName(index)
        return name.endswith('.lisp') or name.endswith('.lsp') or name.endswith('.cl')

class FileBrowser(QWidget):
    file_selected = pyqtSignal(str) # Emits absolute path
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()
        
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        
        # Model
        self.fs_model = QFileSystemModel()
        self.fs_model.setRootPath(QDir.rootPath())
        
        # Proxy for filtering (if needed)
        self.proxy_model = LispFileFilterProxy(self)
        self.proxy_model.setSourceModel(self.fs_model)
        
        # Tree View
        self.tree = QTreeView()
        self.tree.setModel(self.proxy_model)
        self.tree.setRootIndex(self.proxy_model.mapFromSource(self.fs_model.index(QDir.homePath())))
        
        # Hide columns except Name
        self.tree.setColumnHidden(1, True) # Size
        self.tree.setColumnHidden(2, True) # Type
        self.tree.setColumnHidden(3, True) # Date
        self.tree.setHeaderHidden(True)
        
        self.tree.doubleClicked.connect(self.on_double_click)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        
        self.layout.addWidget(self.tree)
        
        # Apply settings
        self.load_settings()

    def on_double_click(self, index):
        source_index = self.proxy_model.mapToSource(index)
        path = self.fs_model.filePath(source_index)
        if not self.fs_model.isDir(source_index):
            self.file_selected.emit(path)

    def show_context_menu(self, point):
        index = self.tree.indexAt(point)
        # Even if valid index, we might want to interact with background?
        # Usually file managers allow clicking empty space for "New" in current dir.
        # For now, require checking index
        
        menu = QMenu(self)
        
        if index.isValid():
            source_index = self.proxy_model.mapToSource(index)
            path = self.fs_model.filePath(source_index)
            is_dir = self.fs_model.isDir(source_index)
            
            if is_dir:
                pass # Directory actions
            else:
                pass # File actions
                
            # Common actions
            new_file_action = QAction("New File", self)
            new_file_action.triggered.connect(lambda: self.create_new_file(path if is_dir else os.path.dirname(path)))
            menu.addAction(new_file_action)
            
            new_folder_action = QAction("New Folder", self)
            new_folder_action.triggered.connect(lambda: self.create_new_folder(path if is_dir else os.path.dirname(path)))
            menu.addAction(new_folder_action)
            
            menu.addSeparator()
            
            rename_action = QAction("Rename", self)
            rename_action.triggered.connect(lambda: self.rename_item(index))
            menu.addAction(rename_action)
            
            delete_action = QAction("Delete", self)
            delete_action.triggered.connect(lambda: self.delete_item(index))
            menu.addAction(delete_action)
            
        else:
            # Clicked on whitespace, maybe use current root?
            # QFileSystemView logic is tricky for "current dir" from whitespace.
            # Using root path of the view.
            pass
            
        menu.exec(self.tree.mapToGlobal(point))

    def create_new_file(self, dir_path):
        # Implement logic to ask for name and create
        # For MVP, skipping dialogs here to keep it simple, or implement simple input dialog
        # This requires QInputDialog which is blocking.
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(self, "New File", "Filename:")
        if ok and name:
            full_path = os.path.join(dir_path, name)
            if not name.endswith('.lisp'): 
                 pass # Enforce? No.
            try:
                with open(full_path, 'w') as f:
                    pass
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create file: {e}")

    def create_new_folder(self, dir_path):
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(self, "New Folder", "Folder Name:")
        if ok and name:
            full_path = os.path.join(dir_path, name)
            try:
                os.mkdir(full_path)
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder: {e}")

    def rename_item(self, index):
        # QFileSystemModel is read-only by default?
        # It has setReadOnly(False) but renaming via edits might be easier
        # Or just use fs operations.
        # "Rename" usually triggers inline edit or dialog.
        # self.tree.edit(index) if model supports it.
        # QFileSystemModel supports it if not read only
        self.fs_model.setReadOnly(False)
        self.tree.edit(index)
        self.fs_model.setReadOnly(True) 

    def delete_item(self, index):
        source_index = self.proxy_model.mapToSource(index)
        path = self.fs_model.filePath(source_index)
        
        reply = QMessageBox.question(
            self, "Delete",
            f"Are you sure you want to delete {os.path.basename(path)}?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            try:
                if self.fs_model.isDir(source_index):
                    # QFileSystemModel.remove is for rows, use fs function preferably or model remove
                    self.fs_model.remove(source_index)
                else:
                    self.fs_model.remove(source_index)
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Could not delete: {e}")

    def load_settings(self):
        last_dir = self.settings.get("browser", "last_directory")
        if last_dir and os.path.exists(last_dir):
            self.tree.setRootIndex(self.proxy_model.mapFromSource(self.fs_model.index(last_dir)))

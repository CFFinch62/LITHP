
from PyQt6.QtWidgets import QPlainTextEdit, QWidget, QTextEdit
from PyQt6.QtCore import Qt, QRect, QSize, pyqtSignal
from PyQt6.QtGui import QColor, QPainter, QTextFormat, QFont, QFontDatabase

from lithp.editor.highlighter import LispHighlighter
from lithp.editor.paren_matcher import ParenMatcher
from lithp.config.settings import Settings

class LineNumberArea(QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.codeEditor = editor

    def sizeHint(self):
        return QSize(self.codeEditor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.codeEditor.lineNumberAreaPaintEvent(event)

class CodeEditor(QPlainTextEdit):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()
        
        self.line_number_area = LineNumberArea(self)
        self.highlighter = LispHighlighter(self.document())
        self.paren_matcher = ParenMatcher(self)
        
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        self.cursorPositionChanged.connect(self.highlight_current_line)
        self.cursorPositionChanged.connect(self.paren_matcher.highlight_matching)
        
        self.update_line_number_area_width(0)
        self.highlight_current_line()
        
        self.setup_font()
        self.setup_editor()

    def setup_font(self):
        font_family = self.settings.get("editor", "font_family")
        font_size = self.settings.get("editor", "font_size")
        
        font = QFont(font_family, font_size)
        font.setStyleHint(QFont.StyleHint.Monospace)
        font.setFixedPitch(True)
        self.setFont(font)
        
        # Fallback if font not found
        if not QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont):
             pass # Use what we set or default

    def setup_editor(self):
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        tab_width = self.settings.get("editor", "tab_width")
        self.setTabStopDistance(self.fontMetrics().horizontalAdvance(' ') * tab_width)

    def line_number_area_width(self):
        digits = 1
        max_val = max(1, self.blockCount())
        while max_val >= 10:
            max_val //= 10
            digits += 1
        space = 3 + self.fontMetrics().horizontalAdvance('9') * digits
        return space

    def update_line_number_area_width(self, _):
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)

    def update_line_number_area(self, rect, dy):
        if dy:
            self.line_number_area.scroll(0, dy)
        else:
            self.line_number_area.update(0, rect.y(), self.line_number_area.width(), rect.height())

        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width(0)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        cr = self.contentsRect()
        self.line_number_area.setGeometry(QRect(cr.left(), cr.top(), self.line_number_area_width(), cr.height()))

    def highlight_current_line(self):
        extra_selections = []
        
        if not self.isReadOnly():
            selection = QTextEdit.ExtraSelection()
            line_color = QColor("#313244") # Dark theme default, move to themes later
            # In a real app, query current theme
            
            selection.format.setBackground(line_color)
            selection.format.setProperty(QTextFormat.Property.FullWidthSelection, True)
            selection.cursor = self.textCursor()
            selection.cursor.clearSelection()
            extra_selections.append(selection)

        # Add parent matching selections
        extra_selections.extend(self.paren_matcher.get_selections())
        
        self.setExtraSelections(extra_selections)

    def lineNumberAreaPaintEvent(self, event):
        painter = QPainter(self.line_number_area)
        painter.fillRect(event.rect(), QColor("#181825")) # Dark theme gutter

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        top =  round(self.blockBoundingGeometry(block).translated(self.contentOffset()).top())
        bottom = top + round(self.blockBoundingRect(block).height())

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                painter.setPen(QColor("#6c7086")) # Dark theme line number
                painter.drawText(0, top, self.line_number_area.width(), self.fontMetrics().height(),
                               Qt.AlignmentFlag.AlignRight, number)

            block = block.next()
            top = bottom
            bottom = top + round(self.blockBoundingRect(block).height())
            block_number += 1

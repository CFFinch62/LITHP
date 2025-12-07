
from PyQt6.QtWidgets import QTextEdit
from PyQt6.QtGui import QTextCursor, QColor

class ParenMatcher:
    def __init__(self, editor):
        self.editor = editor
        self.current_selections = []

    def highlight_matching(self):
        self.current_selections = []
        cursor = self.editor.textCursor()
        pos = cursor.position()
        document = self.editor.document()
        text = document.toPlainText()
        
        if pos == 0 and not text:
            return

        # Check character before and after cursor
        # Priority: character before cursor (closing paren logic?) or after?
        # Standard behavior: if after is '(', match forward. If before is ')', match backward.
        
        char_after = text[pos] if pos < len(text) else None
        char_before = text[pos-1] if pos > 0 else None
        
        match_pos = -1
        start_pos = -1
        
        if char_before == ')':
            start_pos = pos - 1
            match_pos = self.find_match(text, start_pos, -1)
        elif char_after == '(':
            start_pos = pos
            match_pos = self.find_match(text, start_pos, 1)
            
        if match_pos != -1:
            self.create_selection(start_pos, "#f9e2af") # Match color
            self.create_selection(match_pos, "#f9e2af")
        elif (char_before == ')' or char_after == '('):
            # Mismatch found
            if start_pos != -1:
                 self.create_selection(start_pos, "#f38ba8") # Error color

    def find_match(self, text, start_pos, direction):
        target = text[start_pos]
        match_char = '(' if target == ')' else ')'
        
        depth = 0
        limit = len(text)
        i = start_pos
        
        while 0 <= i < limit:
            c = text[i]
            if c == target:
                depth += 1
            elif c == match_char:
                depth -= 1
                if depth == 0:
                    return i
            i += direction
            
        return -1

    def create_selection(self, pos, color_str):
        selection = QTextEdit.ExtraSelection()
        selection.format.setBackground(QColor(color_str))

        cursor = self.editor.textCursor()
        cursor.setPosition(pos)
        cursor.movePosition(QTextCursor.MoveOperation.NextCharacter, QTextCursor.MoveMode.KeepAnchor)
        selection.cursor = cursor
        self.current_selections.append(selection)

    def get_selections(self):
        return self.current_selections

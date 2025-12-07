
import pyte
from PyQt6.QtWidgets import QWidget
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import QPainter, QFont, QColor, QFontMetrics, QKeyEvent, QBrush

from lithp.terminal.pty_process import PTYProcess
from lithp.config.settings import Settings

class TerminalWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()
        
        # Terminal state
        self.cols = 80
        self.rows = 24
        
        # Pyte setup
        self.screen = pyte.Screen(self.cols, self.rows)
        self.stream = pyte.Stream(self.screen)
        
        # PTY Process
        # Check settings for custom clisp path
        clisp_path = self.settings.get("terminal", "clisp_path")
        cmd = [clisp_path] if clisp_path else ['clisp']
        
        self.pty = PTYProcess(command=cmd)
        self.pty.data_received.connect(self.on_data_received)
        self.pty.process_exited.connect(self.on_process_exited)
        
        # Font setup
        self.setup_font()
        
        # Cursor blinking
        self.cursor_visible = True
        self.cursor_timer = QTimer(self)
        self.cursor_timer.timeout.connect(self.toggle_cursor)
        self.cursor_timer.start(500)
        
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        
        # Start the PTY
        self.pty.start()

    def setup_font(self):
        font_family = self.settings.get("terminal", "font_family") or \
                      self.settings.get("editor", "font_family") or "Monospace"
        font_size = self.settings.get("terminal", "font_size") or \
                    self.settings.get("editor", "font_size") or 14
                    
        self.font = QFont(font_family, font_size)
        self.font.setStyleHint(QFont.StyleHint.Monospace)
        self.fm = QFontMetrics(self.font)
        
        self.char_width = self.fm.horizontalAdvance('W')
        self.char_height = self.fm.height()
        
        # Calculate initial size
        self.resize_terminal()

    def resizeEvent(self, event):
        self.resize_terminal()
        super().resizeEvent(event)

    def resize_terminal(self):
        w = self.width()
        h = self.height()
        
        new_cols = max(1, w // self.char_width)
        new_rows = max(1, h // self.char_height)
        
        if new_cols != self.cols or new_rows != self.rows:
            self.cols = new_cols
            self.rows = new_rows
            self.screen.resize(self.rows, self.cols)
            self.pty.resize(self.rows, self.cols)
            self.update()

    def on_data_received(self, data):
        # pyte expects unicode string
        try:
            text = data.decode('utf-8', errors='replace')
            self.stream.feed(text)
            self.update()
        except Exception:
            pass

    def on_process_exited(self, exit_code):
        self.stream.feed(f"\r\n[Process exited with code {exit_code}]\r\n")
        self.update()
        
    def write(self, data: bytes):
        self.pty.write(data)

    def keyPressEvent(self, event: QKeyEvent):
        # Map Qt keys to VT100 sequences
        text = event.text()
        key = event.key()
        modifiers = event.modifiers()
        
        # Basic mapping - can be expanded
        if key == Qt.Key.Key_Return:
            self.write(b'\r')
        elif key == Qt.Key.Key_Backspace:
            self.write(b'\x7f')
        elif key == Qt.Key.Key_Tab:
            self.write(b'\t')
        elif key == Qt.Key.Key_Up:
            self.write(b'\x1b[A')
        elif key == Qt.Key.Key_Down:
            self.write(b'\x1b[B')
        elif key == Qt.Key.Key_Right:
            self.write(b'\x1b[C')
        elif key == Qt.Key.Key_Left:
            self.write(b'\x1b[D')
        elif modifiers & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_C:
            self.write(b'\x03') # Ctrl+C
        elif modifiers & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_D:
            self.write(b'\x04') # Ctrl+D
        elif text:
            self.write(text.encode('utf-8'))
            
        # Scroll to bottom on input?
        # self.update() already handled by data echo

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setFont(self.font)
        
        # Background
        # Use theme colors eventually
        bg_color = QColor("#0f0f0f") # Terminal BG
        fg_color = QColor("#cccccc") # Terminal FG
        
        painter.fillRect(self.rect(), bg_color)
        
        # Draw characters
        # self.screen.display is a list of strings (lines) - incorrect, it's a dict mapping line num to chars in pyte usually?
        # Actually pyte.Screen builds a 'buffer'. self.screen.display is a property returning list of lines.
        
        for y, line in enumerate(self.screen.display):
            # line is a string of characters for that row
            # To handle colors correctly, we need self.screen.buffer[y] which contains Char objects with attributes
            # For MVP, let's try simply drawing text first to verify, then attributes.
            
            # Using buffer for attributes:
            row_chars = self.screen.buffer[y]
            
            for x, char in enumerate(row_chars):
                char_data = row_chars[x] # pyte.screens.Char namedtuple likely
                # char_data.data (char), char_data.fg, char_data.bg, char_data.bold, etc.
                
                # Draw character
                # Determine colors (mapping pyte colors to QColor)
                # Default for now
                painter.setPen(fg_color)
                
                rect_x = x * self.char_width
                rect_y = y * self.char_height
                
                painter.drawText(rect_x, rect_y + self.fm.ascent(), char_data.data)

        # Draw Cursor
        if self.cursor_visible and self.pty.running:
            cx = self.screen.cursor.x
            cy = self.screen.cursor.y
            
            cursor_rect_x = cx * self.char_width
            cursor_rect_y = cy * self.char_height
            
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(QColor(200, 200, 200, 100))) # Semi-transparent block
            painter.drawRect(cursor_rect_x, cursor_rect_y, self.char_width, self.char_height)

    def toggle_cursor(self):
        self.cursor_visible = not self.cursor_visible
        # Only update the cursor area to save resources
        cx = self.screen.cursor.x
        cy = self.screen.cursor.y
        self.update(cx * self.char_width, cy * self.char_height, self.char_width, self.char_height)
        
    def restart(self):
        self.pty.terminate_process()
        self.pty.wait()
        self.screen.reset()
        self.pty.start()

    def clear(self):
        self.screen.reset()
        self.update()
        
    def interrupt(self):
        self.write(b'\x03')

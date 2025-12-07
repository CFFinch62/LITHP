
from dataclasses import dataclass
from PyQt6.QtGui import QColor

@dataclass
class Theme:
    name: str
    background: str
    foreground: str
    keyword: str
    function: str
    string: str
    comment: str
    number: str
    matched_paren: str
    error: str
    current_line: str
    selection: str
    line_number_fg: str
    line_number_bg: str

DARK_THEME = Theme(
    name="dark",
    background="#1e1e2e",
    foreground="#cdd6f4",
    keyword="#cba6f7",
    function="#89b4fa",
    string="#a6e3a1",
    comment="#6c7086",
    number="#fab387",
    matched_paren="#f9e2af",
    error="#f38ba8",
    current_line="#313244",
    selection="#45475a",
    line_number_fg="#6c7086",
    line_number_bg="#181825"
)

LIGHT_THEME = Theme(
    name="light",
    background="#fafafa",
    foreground="#2e2e2e",
    keyword="#8839ef",
    function="#1e66f5",
    string="#40a02b",
    comment="#8c8fa1",
    number="#fe640b",
    matched_paren="#df8e1d",
    error="#d20f39",
    current_line="#e6e9ef",
    selection="#ccd0da",
    line_number_fg="#8c8fa1",
    line_number_bg="#f2f4f8"
)

THEMES = {
    "dark": DARK_THEME,
    "light": LIGHT_THEME
}

def get_theme(name):
    return THEMES.get(name, DARK_THEME)

def apply_theme_to_app(app, theme: Theme):
    # Basic QSS for main UI elements
    qss = f"""
    QMainWindow, QDialog {{
        background-color: {theme.background};
        color: {theme.foreground};
    }}
    QPlainTextEdit, QTextEdit {{
        background-color: {theme.background};
        color: {theme.foreground};
        selection-background-color: {theme.selection};
    }}
    QTreeView {{
        background-color: {theme.line_number_bg}; /* Slightly different for sidebar */
        color: {theme.foreground};
        border: none;
    }}
    QTreeView::item:selected {{
        background-color: {theme.selection};
    }}
    QSplitter::handle {{
        background-color: {theme.current_line};
    }}
    QMenuBar {{
        background-color: {theme.background};
        color: {theme.foreground};
    }}
    QMenuBar::item:selected {{
        background-color: {theme.selection};
    }}
    QMenu {{
        background-color: {theme.background};
        color: {theme.foreground};
        border: 1px solid {theme.comment};
    }}
    QMenu::item:selected {{
        background-color: {theme.selection};
    }}
    QTabWidget::pane {{
        border: 1px solid {theme.current_line};
    }}
    QTabBar::tab {{
        background-color: {theme.line_number_bg};
        color: {theme.line_number_fg};
        padding: 5px;
    }}
    QTabBar::tab:selected {{
        background-color: {theme.background};
        color: {theme.foreground};
    }}
    QStatusBar {{
        background-color: {theme.line_number_bg};
        color: {theme.line_number_fg};
    }}
    """
    app.setStyleSheet(qss)

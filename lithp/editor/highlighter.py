
from PyQt6.QtGui import QSyntaxHighlighter, QTextCharFormat, QColor, QFont
from PyQt6.QtCore import QRegularExpression

class LispHighlighter(QSyntaxHighlighter):
    def __init__(self, document):
        super().__init__(document)
        self.highlighting_rules = []

        # Colors (Hardcoded for now, should use Theme)
        keyword_format = QTextCharFormat()
        keyword_format.setForeground(QColor("#cba6f7"))
        keyword_format.setFontWeight(QFont.Weight.Bold)
        keywords = [
            "defun", "defvar", "defparameter", "defmacro", "defclass", "defmethod", "defgeneric",
            "let", "let*", "flet", "labels", "lambda", "setq", "setf",
            "if", "when", "unless", "cond", "case", "typecase",
            "loop", "do", "do*", "dolist", "dotimes", "progn", "block", "return", "return-from",
            "and", "or", "not", "nil", "t"
        ]
        for word in keywords:
            pattern = QRegularExpression(f"\\b{word}\\b")
            self.highlighting_rules.append((pattern, keyword_format))

        function_format = QTextCharFormat()
        function_format.setForeground(QColor("#89b4fa"))
        functions = [
            "car", "cdr", "cons", "list", "append", "reverse", "length", "nth", "first", "rest", "last",
            "mapcar", "mapc", "maplist", "reduce", "remove", "remove-if", "find", "find-if", "member",
            "format", "print", "princ", "prin1", "terpri", "read", "read-line",
            "eq", "eql", "equal", "equalp", "null", "atom", "listp", "numberp", "stringp", "symbolp",
            "mod", "floor", "ceiling", "round", "abs"
        ]
        # Needs careful regex for symbols like + - * / 
        for word in functions:
            pattern = QRegularExpression(f"\\b{word}\\b")
            self.highlighting_rules.append((pattern, function_format))
        
        # Operators / Special symbols
        op_format = QTextCharFormat()
        op_format.setForeground(QColor("#89b4fa")) 
        ops = [r"\+", r"\-", r"\*", "/", "<", ">", "<=", ">=", "=", "/="]
        for op in ops:
             pattern = QRegularExpression(f"[{op}]")
             self.highlighting_rules.append((pattern, op_format))


        # Strings
        string_format = QTextCharFormat()
        string_format.setForeground(QColor("#a6e3a1"))
        self.highlighting_rules.append((QRegularExpression(r'"[^"\\]*(\\.[^"\\]*)*"'), string_format))

        # Numbers
        number_format = QTextCharFormat()
        number_format.setForeground(QColor("#fab387"))
        self.highlighting_rules.append((QRegularExpression(r"\b-?[0-9]+(/[0-9]+)?(\.[0-9]+)?\b"), number_format))
        
        # Comments
        comment_format = QTextCharFormat()
        comment_format.setForeground(QColor("#6c7086"))
        self.highlighting_rules.append((QRegularExpression(r";.*"), comment_format))
        
        # Block comments handled in highlightBlock (simple version) or specialized rule

    def highlightBlock(self, text):
        for pattern, format in self.highlighting_rules:
            match_iterator = pattern.globalMatch(text)
            while match_iterator.hasNext():
                match = match_iterator.next()
                self.setFormat(match.capturedStart(), match.capturedLength(), format)

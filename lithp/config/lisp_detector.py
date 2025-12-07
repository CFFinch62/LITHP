"""
Lisp interpreter detection module.

Detects common Lisp implementations installed on the system.
"""

import shutil
import subprocess
from dataclasses import dataclass
from typing import Optional


@dataclass
class LispInterpreter:
    """Represents a detected Lisp interpreter."""
    name: str
    path: str
    version: Optional[str] = None
    
    def display_name(self) -> str:
        """Return a user-friendly display name."""
        if self.version:
            return f"{self.name} ({self.version})"
        return self.name


# Common Lisp interpreters to search for
# Format: (command_name, display_name, version_args, version_parser)
KNOWN_INTERPRETERS = [
    ("sbcl", "SBCL", ["--version"], lambda out: out.strip()),
    ("clisp", "CLISP", ["--version"], lambda out: out.split('\n')[0].strip() if out else None),
    ("ecl", "ECL", ["--version"], lambda out: out.strip()),
    ("ccl", "Clozure CL", ["--version"], lambda out: out.strip()),
    ("ccl64", "Clozure CL (64-bit)", ["--version"], lambda out: out.strip()),
    ("abcl", "ABCL", ["--version"], lambda out: out.strip()),
    ("alisp", "Allegro CL", None, None),  # Commercial, no easy version check
    ("lispworks", "LispWorks", None, None),  # Commercial
]


def get_interpreter_version(path: str, version_args: list, version_parser) -> Optional[str]:
    """Try to get the version string for an interpreter."""
    if not version_args or not version_parser:
        return None
    
    try:
        result = subprocess.run(
            [path] + version_args,
            capture_output=True,
            text=True,
            timeout=5
        )
        output = result.stdout or result.stderr
        if output:
            return version_parser(output)
    except (subprocess.TimeoutExpired, subprocess.SubprocessError, OSError):
        pass
    
    return None


def detect_lisp_interpreters() -> list[LispInterpreter]:
    """
    Detect all installed Lisp interpreters on the system.
    
    Returns:
        A list of LispInterpreter objects for each detected interpreter.
    """
    detected = []
    
    for cmd, display_name, version_args, version_parser in KNOWN_INTERPRETERS:
        path = shutil.which(cmd)
        if path:
            version = get_interpreter_version(path, version_args, version_parser)
            detected.append(LispInterpreter(
                name=display_name,
                path=path,
                version=version
            ))
    
    return detected


def get_default_interpreter() -> Optional[LispInterpreter]:
    """
    Get the default/preferred Lisp interpreter.
    
    Preference order: SBCL > CLISP > ECL > CCL > others
    
    Returns:
        The preferred LispInterpreter, or None if none are installed.
    """
    interpreters = detect_lisp_interpreters()
    
    if not interpreters:
        return None
    
    # Return first match (list is already in preference order)
    return interpreters[0]


def is_valid_interpreter(path: str) -> bool:
    """
    Check if a given path is a valid, executable Lisp interpreter.
    
    Args:
        path: Path to the interpreter executable.
        
    Returns:
        True if the path exists and is executable.
    """
    if not path:
        return False
    
    # Use shutil.which to verify it's executable
    # If it's an absolute path, check directly
    import os
    if os.path.isabs(path):
        return os.path.isfile(path) and os.access(path, os.X_OK)
    
    return shutil.which(path) is not None


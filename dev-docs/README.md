# LITHP IDE - Project Documentation

A comprehensive documentation set for building an educational Common Lisp IDE.

## Documents Included

### 1. Technical Specification (`LITHP_Technical_Specification.docx`)
Complete technical specification covering:
- Project goals and design philosophy
- Technology stack (Python, PyQt6, CLISP)
- Module architecture and directory structure
- Feature specifications (editor, terminal, file browser)
- User interface design with layout diagrams
- Color themes (dark/light)
- Dependencies and system requirements
- Configuration system
- Success criteria

### 2. Implementation Plan (`LITHP_Implementation_Plan.docx`)
Phased development guide:
- **Phase 1:** Foundation (1-2 hours) - Project structure, basic window
- **Phase 2:** Code Editor (3-4 hours) - Highlighting, paren matching
- **Phase 3:** Terminal (4-6 hours) - PTY integration, CLISP REPL
- **Phase 4:** File Browser (2-3 hours) - Navigation, context menus
- **Phase 5:** Polish (3-4 hours) - Themes, settings, final touches

Each phase includes verification checkpoints.

### 3. AI Agent Project Prompt (`AGENT_PROMPT.md`)
**This is the primary document for AI coding agents.** Contains:
- Complete directory structure
- Code templates for all major components
- Detailed class specifications with code examples
- Signal/slot connection patterns
- Menu structure and keyboard shortcuts
- Testing checklist with sample Lisp code
- Error handling guidelines
- Cross-platform notes

### 4. Architecture Reference (`ARCHITECTURE.md`)
Visual reference including:
- ASCII component diagrams
- Data flow diagrams
- Class hierarchy
- Signal/slot connection map
- Configuration file format
- Keyboard shortcuts table
- Theme color mapping

## Quick Start for AI Agents

1. Read `AGENT_PROMPT.md` first - it has everything needed to start coding
2. Follow the implementation order in the document
3. Use the verification checkpoints to test each phase
4. Reference `ARCHITECTURE.md` for visual context

## Project Overview

**LITHP** is a lightweight IDE designed for students learning Common Lisp. It features:

- Syntax-highlighted editor with parenthesis matching (essential for Lisp)
- Integrated file browser for project navigation
- Full PTY terminal running CLISP with complete I/O support

The design philosophy emphasizes clarity over complexity - students should understand their code before learning enterprise patterns.

## Technology Choices

| Component | Technology | Rationale |
|-----------|------------|-----------|
| Language | Python 3.10+ | Readable, good Qt bindings |
| GUI | PyQt6 | Cross-platform, mature, well-documented |
| Terminal | PTY + pyte | Real terminal emulation with full I/O |
| Lisp | CLISP | Beginner-friendly errors, fast startup |
| Config | JSON | Human-readable, easy to edit |

## Estimated Total Effort

| Phase | Estimated Time |
|-------|---------------|
| Foundation | 1-2 hours |
| Code Editor | 3-4 hours |
| Terminal | 4-6 hours |
| File Browser | 2-3 hours |
| Polish | 3-4 hours |
| **Total** | **13-19 hours** |

## Notes for Human Developers

If you're working on this manually rather than with an AI agent:

1. The `AGENT_PROMPT.md` is still your best starting point
2. The code templates are designed to be copy-paste ready
3. Test incrementally - get each component working before integration
4. The PTY terminal is the trickiest part; budget extra time

## License

These documents are provided for project development use.

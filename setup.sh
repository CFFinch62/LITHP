#!/bin/bash

# Configuration
VENV_DIR="venv"
REQUIREMENTS_FILE="requirements.txt"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}LITHP Setup${NC}"

# Check if venv exists
if [ ! -d "$VENV_DIR" ]; then
    echo -e "Creating virtual environment in ${GREEN}$VENV_DIR${NC}..."
    python3 -m venv "$VENV_DIR"
    if [ $? -ne 0 ]; then
        echo "Error: Failed to create virtual environment."
        exit 1
    fi
else
    echo -e "Virtual environment found in ${GREEN}$VENV_DIR${NC}."
fi

# Activate venv
source "$VENV_DIR/bin/activate"

# Update pip
echo "Updating pip..."
pip install --upgrade pip

# Install dependencies
if [ -f "$REQUIREMENTS_FILE" ]; then
    echo -e "Installing dependencies from ${GREEN}$REQUIREMENTS_FILE${NC}..."
    pip install -r "$REQUIREMENTS_FILE"
    if [ $? -ne 0 ]; then
        echo "Error: Failed to install dependencies."
        exit 1
    fi
else
    echo "Warning: $REQUIREMENTS_FILE not found."
fi

echo -e "${GREEN}Setup complete!${NC}"
echo "You can now run the application using ./run.sh"

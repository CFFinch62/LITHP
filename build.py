
import PyInstaller.__main__
import platform
import os
import shutil
import sys

# Define Project Info
APP_NAME = "LITHP"
ENTRY_POINT = "lithp/main.py"

def clean_build_dirs():
    print("Cleaning build directories...")
    for d in ['build', 'dist']:
        if os.path.exists(d):
            shutil.rmtree(d)
    if os.path.exists(f"{APP_NAME}.spec"):
        os.remove(f"{APP_NAME}.spec")

def get_os_specific_args():
    system = platform.system()
    args = []
    
    if system == "Linux":
        args = [
            '--name', APP_NAME,
            '--clean',
            '--windowed', # GUI application
            '--hidden-import', 'PyQt6',
            '--hidden-import', 'ptyprocess',
            '--hidden-import', 'pyte',
            '--add-data', 'images:images', # Include images folder
        ]
    elif system == "Darwin": # macOS
        args = [
            '--name', APP_NAME,
            '--clean',
            '--windowed',
            '--target-architecture', 'universal2', 
            '--icon', 'images/lithp_icon.icns',
            '--add-data', 'images:images',
        ]
    elif system == "Windows":
        args = [
            '--name', APP_NAME,
            '--clean',
            '--windowed',
            '--icon', 'images/lithp_icon.ico',
            '--add-data', 'images;images', # Windows uses ; separator

        ]
    return args

def build():
    clean_build_dirs()
    
    args =  get_os_specific_args()
    
    # Common Args
    args.extend([
        '--noconfirm',
        ENTRY_POINT
    ]) 
    
    print(f"Building {APP_NAME} for {platform.system()}...")
    
    try:
        PyInstaller.__main__.run(args)
        print("Build complete!")
        print(f"Executable located in: dist/{APP_NAME}/")
    except Exception as e:
        print(f"Build failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    build()

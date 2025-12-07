
import os
import ptyprocess
from PyQt6.QtCore import QThread, pyqtSignal

class PTYProcess(QThread):
    data_received = pyqtSignal(bytes)
    process_exited = pyqtSignal(int)
    
    def __init__(self, command=None, env=None):
        super().__init__()
        self.command = command or ['sbcl']
        self.env = env or os.environ.copy()
        self.process = None
        self.running = False

    def run(self):
        try:
            self.process = ptyprocess.PtyProcess.spawn(
                self.command,
                env=self.env,
                echo=True
            )
            self.running = True
            
            # Read loop
            while self.running and self.process.isalive():
                try:
                    # Non-blocking read or small timeout behavior depends on implementation
                    # ptyprocess read is blocking by default, which is fine in a thread
                    data = self.process.read(1024)
                    if data:
                        self.data_received.emit(data)
                except EOFError:
                    break
                except Exception as e:
                    print(f"PTY Read Error: {e}")
                    break
                    
            exit_status = self.process.exitstatus if self.process else 0
            self.process_exited.emit(exit_status or 0)
            
        except Exception as e:
            print(f"PTY Spawn Error: {e}")
            self.process_exited.emit(1)
        finally:
            self.running = False

    def write(self, data: bytes):
        if self.process and self.process.isalive():
            self.process.write(data)

    def terminate_process(self):
        self.running = False
        if self.process:
            self.process.terminate(force=True)

    def resize(self, rows, cols):
        if self.process:
            self.process.setwinsize(rows, cols)

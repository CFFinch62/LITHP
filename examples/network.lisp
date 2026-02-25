
;; (Simplified) Server side
(defun start-server (port)
  (let ((socket (socket-listen port)))
    (unwind-protect
         (loop
           (let ((client-socket (socket-accept socket)))
             (format t "Client connected~%")
             (let ((stream (socket-stream client-socket)))
               (write-line "Hello, client!" stream)
               (close client-socket))
             (format t "Client disconnected~%")))
      (socket-close socket))))
;; Note: Requires `sb-bsd-sockets` and careful error handling for real use.

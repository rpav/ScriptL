(in-package :scriptl)

(defclass packet-io-stream
    (stream trivial-gray-streams:trivial-gray-stream-mixin)
  ((stream :initarg :stream :initform nil)))

(defmethod trivial-gray-streams:stream-write-sequence
    ((pio-stream packet-io-stream) sequence start end &key &allow-other-keys)
  (with-slots (stream) pio-stream
    (send-packet stream ":print")
    (send-packet stream (subseq sequence start end))
    (finish-output stream))
  sequence)

(defmethod trivial-gray-streams:stream-write-string
    ((pio-stream packet-io-stream) string &optional start end)
  (let ((start (or start 0))
        (end (or end (length string))))
    (with-slots (stream) pio-stream
      (send-packet stream ":print")
      (send-packet stream (subseq string start end))
      (finish-output stream)))
  string)

(defmethod trivial-gray-streams:stream-write-char
    ((pio-stream packet-io-stream) character)
  (with-slots (stream) pio-stream
    (send-packet stream ":print")
    (send-packet stream character)
    (finish-output stream))
  character)

(defmethod trivial-gray-streams:stream-write-byte
    ((pio-stream packet-io-stream) byte)
  (with-slots (stream) pio-stream
    (send-packet stream ":print")
    (send-packet stream byte)
    (finish-output stream))
  byte)

(defmethod trivial-gray-streams:stream-read-line ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (send-packet stream ":read-line")
    (finish-output stream)
    (handler-case
        (read-packet stream)
      (end-of-file (c)
        (declare (ignore c))
        (values nil t)))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((pio-stream packet-io-stream) seq start end &key &allow-other-keys)
  (let ((start (or start 0))
        (end (or end (length seq))))
    (with-slots (stream) pio-stream
      (send-packet stream ":read-bytes")
      (send-packet stream (- end start))
      (finish-output stream)
      (let ((bytes (read-packet stream t)))
        (replace seq bytes :start1 start :end1 end)
        (length bytes)))))

(defmethod trivial-gray-streams:stream-read-byte
    ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (send-packet stream ":read-bytes")
    (send-packet stream "1")
    (finish-output stream)
    (elt (read-packet stream t) 0)))

(defmethod trivial-gray-streams:stream-read-char
    ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (send-packet stream ":read-bytes")
    (send-packet stream "1")
    (finish-output stream)
    (elt (read-packet stream) 0)))

(defmethod trivial-gray-streams:stream-finish-output
  ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (trivial-gray-streams:stream-finish-output stream)))

(defmethod trivial-gray-streams:stream-line-column
  ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (trivial-gray-streams:stream-line-column stream)))

(defmethod trivial-gray-streams:stream-fresh-line
    ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (send-packet stream ":print")
    (send-packet stream #\Newline)
    (finish-output stream))
  t)

(defmethod trivial-gray-streams:stream-terpri
    ((pio-stream packet-io-stream))
  (with-slots (stream) pio-stream
    (send-packet stream ":print")
    (send-packet stream #\Newline)
    (finish-output stream))
  nil)

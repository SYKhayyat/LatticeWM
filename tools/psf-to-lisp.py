#!/usr/bin/env python3
"""Convert a PSF console font into src/runtime/font.lisp.

Run once, by hand, when changing fonts.  The output is vendored so that the
ordinary build needs no font file and no font library:

    tools/psf-to-lisp.py $(nix-build '<nixpkgs>' -A terminus_font --no-out-link)\
        /share/consolefonts/ter-116n.psf.gz
"""
import gzip, struct, sys, os

def load(path):
    data = gzip.open(path, 'rb').read() if path.endswith('.gz') else open(path,'rb').read()
    if data[:2] == b'\x36\x04':
        charsize = data[3]
        return data[4:], 8, charsize
    if data[:4] == b'\x72\xb5\x4a\x86':
        _, hdr, _, _, charsize, h, w = struct.unpack('<IIIIIII', data[4:32])
        return data[hdr:], w, h
    raise SystemExit("not a PSF file: " + path)

def main():
    path = sys.argv[1] if len(sys.argv) > 1 else \
        "/nix/store/hqhz5vkxdj4h2ryvcib8l9gb5dp97hmf-terminus-font-4.49.1/share/consolefonts/ter-116n.psf.gz"
    glyphs, width, height = load(path)
    stride = (width + 7) // 8
    assert stride == 1, "only 8-pixel-wide fonts are supported"
    rows = []
    for code in range(32, 127):
        off = code * height * stride
        g = glyphs[off:off + height * stride]
        rows.append((code, list(g)))
    body = []
    for code, g in rows:
        ch = chr(code)
        label = {'"': 'quote', '\\': 'backslash', ';': 'semicolon'}.get(ch, ch)
        body.append("    " + " ".join("#x%02x" % b for b in g) + "   ; %s" % label)
    out = FONT_TEMPLATE.format(
        width=width, height=height, count=len(rows),
        source=os.path.basename(path), data="\n".join(body))
    with open("src/runtime/font.lisp", "w") as f:
        f.write(out)
    print("wrote src/runtime/font.lisp: %d glyphs, %dx%d" % (len(rows), width, height))

FONT_TEMPLATE = r''';;;; runtime/font.lisp --- The bitmap font.  GENERATED; see tools/psf-to-lisp.py.
;;;;
;;;; Terminus {width}x{height}, imported from {source}.
;;;;
;;;; Terminus Font is copyright (c) 2020 Dimitar Toshkov Zhekov and is licensed
;;;; under the SIL Open Font License, Version 1.1.  The full licence text is in
;;;; doc/OFL-TERMINUS.txt and must travel with any copy of this file.
;;;;
;;;; WHY A BITMAP FONT AT ALL.  The window manager puts text on screen for two
;;;; things: the coordinate a cell is at, and the echo area.  Both are short and
;;;; read at a glance.  Pulling in Pango and Cairo to draw "3,-2" would be a
;;;; dependency this project has to keep alive for as long as it lives, in
;;;; exchange for nothing anybody would notice — and it would put a C font stack
;;;; in the way of the one requirement that matters, which is that this survives
;;;; without a maintainer.
;;;;
;;;; WHY TERMINUS AND NOT A HAND-ROLLED 5x7.  There was a 5x7 here first, and it
;;;; looked exactly like a 5x7 scaled up: blocky, in a way that reads as unfinished
;;;; rather than as retro.  Terminus at {height} pixels is a real console font,
;;;; drawn by someone who knows how, and at scale 1 it is simply *text*.  The
;;;; whole cost is this generated file.
;;;;
;;;; Row-major: one byte per row, bit 7 leftmost.

(in-package #:latticewm/runtime)

(defparameter +font-width+ {width})
(defparameter +font-height+ {height})
(defparameter +font-first+ 32 "The first character the table covers.")

(defparameter +font+
  (make-array (* {count} {height}) :element-type (quote (unsigned-byte 8))
   :initial-contents
   (list
{data}
    ))
  "ASCII 32 to 126, {height} row bytes each.")

(declaim (inline glyph-row))
(defun glyph-row (character row)
  "Row ROW of CHARACTER, as a byte whose bit 7 is the leftmost pixel."
  (let ((code (char-code character)))
    (if (and (<= +font-first+ code 126) (< -1 row +font-height+))
        (aref +font+ (+ (* (- code +font-first+) +font-height+) row))
        0)))

(defun text-width (string &key (scale 1) (tracking 0))
  "How wide STRING will be drawn, in pixels."
  (let ((advance (* scale (+ +font-width+ tracking))))
    (max 0 (- (* advance (length string)) (* scale tracking)))))

(defun text-height (&key (scale 1))
  "How tall one line of text is, in pixels."
  (* scale +font-height+))
'''

main()

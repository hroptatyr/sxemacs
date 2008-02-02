;;; tty-init.el --- initialization code for tty's

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing <ben@xemacs.org>.

;; Maintainer: SXEmacs Development Team
;; Keywords: terminals, dumped

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when TTY support is compiled in).

;;; Code:

(defvar pre-tty-win-initted nil
  "t if the tty subsystem is initialized.")

(defconst tty-base-16-colors 
  '(("black"               0     0     0     0)
    ("red"                 1 65535     0     0)
    ("green"               2     0 65535     0)
    ("goldenrod"           3 56026 42405  8224)
    ("lightsteelblue"      4 45232 50372 57054)
    ("magenta"             5 65535     0 65535)
    ("cyan3"               6     0 52685 52685)
    ("gray90"              7 58853 58853 58853)
    ("gray30"              8 19789 19789 19789)
    ("lightsalmon"         9 65535 41120 31354)
    ("palegreen2"         10 37008 61166 37008)
    ("lightgoldenrod"     11 61166 56797 33410)
    ("lightskyblue"       12 34695 52942 64250)
    ("violet"             13 61166 33410 61166)
    ("cyan"               14     0 65535 65535)
    ("white"              15 65535 65535 65535))
    "RGB values for the base 16 terminal colors")

(defconst tty-xterm-256-colors
  (append tty-base-16-colors
	  '(("black"              16     0     0     0)
	    ("navy"               17     0     0 24415)
	    ("blue4"              18     0     0 34695)
	    ("mediumblue"         19     0     0 44975)
	    ("mediumblue"         20     0     0 55255)
	    ("blue"               21     0     0 65535)
	    ("darkgreen"          22     0 24415     0)
	    ("deepskyblue4"       23     0 24415 24415)
	    ("deepskyblue4"       24     0 24415 34695)
	    ("deepskyblue4"       25     0 24415 44975)
	    ("dodgerblue3"        26     0 24415 55255)
	    ("dodgerblue2"        27     0 24415 65535)
	    ("green4"             28     0 34695     0)
	    ("springgreen4"       29     0 34695 24415)
	    ("turquoise4"         30     0 34695 34695)
	    ("deepskyblue3"       31     0 34695 44975)
	    ("deepskyblue3"       32     0 34695 55255)
	    ("dodgerblue"         33     0 34695 65535)
	    ("green3"             34     0 44975     0)
	    ("springgreen3"       35     0 44975 24415)
	    ("cyan4"              36     0 44975 34695)
	    ("lightseagreen"      37     0 44975 44975)
	    ("deepskyblue2"       38     0 44975 55255)
	    ("deepskyblue"        39     0 44975 65535)
	    ("green3"             40     0 55255     0)
	    ("springgreen3"       41     0 55255 24415)
	    ("springgreen2"       42     0 55255 34695)
	    ("cyan3"              43     0 55255 44975)
	    ("darkturquoise"      44     0 55255 55255)
	    ("turquoise2"         45     0 55255 65535)
	    ("green"              46     0 65535     0)
	    ("springgreen2"       47     0 65535 24415)
	    ("springgreen"        48     0 65535 34695)
	    ("mediumspringgreen"  49     0 65535 44975)
	    ("cyan2"              50     0 65535 55255)
	    ("cyan"               51     0 65535 65535)
	    ("red4"               52 24415     0     0)
	    ("deeppink4"          53 24415     0 24415)
	    ("purple4"            54 24415     0 34695)
	    ("purple4"            55 24415     0 44975)
	    ("purple3"            56 24415     0 55255)
	    ("blueviolet"         57 24415     0 65535)
	    ("orange4"            58 24415 24415     0)
	    ("gray37"             59 24415 24415 24415)
	    ("mediumpurple4"      60 24415 24415 34695)
	    ("slateblue3"         61 24415 24415 44975)
	    ("slateblue3"         62 24415 24415 55255)
	    ("royalblue1"         63 24415 24415 65535)
	    ("chartreuse4"        64 24415 34695     0)
	    ("darkseagreen4"      65 24415 34695 24415)
	    ("paleturquoise4"     66 24415 34695 34695)
	    ("steelblue"          67 24415 34695 44975)
	    ("steelblue3"         68 24415 34695 55255)
	    ("cornflowerblue"     69 24415 34695 65535)
	    ("chartreuse3"        70 24415 44975     0)
	    ("darkseagreen4"      71 24415 44975 24415)
	    ("cadetblue"          72 24415 44975 34695)
	    ("cadetblue"          73 24415 44975 44975)
	    ("skyblue3"           74 24415 44975 55255)
	    ("steelblue1"         75 24415 44975 65535)
	    ("chartreuse3"        76 24415 55255     0)
	    ("palegreen3"         77 24415 55255 24415)
	    ("seagreen3"          78 24415 55255 34695)
	    ("mediumaquamarine"   79 24415 55255 44975)
	    ("mediumturquoise"    80 24415 55255 55255)
	    ("steelblue1"         81 24415 55255 65535)
	    ("chartreuse2"        82 24415 65535     0)
	    ("seagreen2"          83 24415 65535 24415)
	    ("seagreen1"          84 24415 65535 34695)
	    ("seagreen1"          85 24415 65535 44975)
	    ("aquamarine"         86 24415 65535 55255)
	    ("darkslategray2"     87 24415 65535 65535)
	    ("red4"               88 34695     0     0)
	    ("deeppink4"          89 34695     0 24415)
	    ("magenta4"           90 34695     0 34695)
	    ("magenta4"           91 34695     0 44975)
	    ("darkviolet"         92 34695     0 55255)
	    ("purple"             93 34695     0 65535)
	    ("orange4"            94 34695 24415     0)
	    ("lightpink4"         95 34695 24415 24415)
	    ("plum4"              96 34695 24415 34695)
	    ("mediumpurple3"      97 34695 24415 44975)
	    ("mediumpurple3"      98 34695 24415 55255)
	    ("slateblue1"         99 34695 24415 65535)
	    ("yellow4"           100 34695 34695     0)
	    ("wheat4"            101 34695 34695 24415)
	    ("gray53"            102 34695 34695 34695)
	    ("lightslategray"    103 34695 34695 44975)
	    ("mediumpurple"      104 34695 34695 55255)
	    ("lightslateblue"    105 34695 34695 65535)
	    ("yellow4"           106 34695 44975     0)
	    ("darkolivegreen3"   107 34695 44975 24415)
	    ("darkseagreen"      108 34695 44975 34695)
	    ("lightskyblue3"     109 34695 44975 44975)
	    ("lightskyblue3"     110 34695 44975 55255)
	    ("skyblue2"          111 34695 44975 65535)
	    ("chartreuse2"       112 34695 55255     0)
	    ("darkolivegreen3"   113 34695 55255 24415)
	    ("palegreen3"        114 34695 55255 34695)
	    ("darkseagreen3"     115 34695 55255 44975)
	    ("darkslategray3"    116 34695 55255 55255)
	    ("skyblue1"          117 34695 55255 65535)
	    ("chartreuse"        118 34695 65535     0)
	    ("palegreen2"        119 34695 65535 24415)
	    ("palegreen2"        120 34695 65535 34695)
	    ("palegreen1"        121 34695 65535 44975)
	    ("aquamarine"        122 34695 65535 55255)
	    ("darkslategray1"    123 34695 65535 65535)
	    ("red3"              124 44975     0     0)
	    ("deeppink4"         125 44975     0 24415)
	    ("mediumvioletred"   126 44975     0 34695)
	    ("magenta3"          127 44975     0 44975)
	    ("darkviolet"        128 44975     0 55255)
	    ("purple"            129 44975     0 65535)
	    ("darkorange3"       130 44975 24415     0)
	    ("indianred"         131 44975 24415 24415)
	    ("hotpink3"          132 44975 24415 34695)
	    ("mediumorchid3"     133 44975 24415 44975)
	    ("mediumorchid"      134 44975 24415 55255)
	    ("mediumpurple2"     135 44975 24415 65535)
	    ("darkgoldenrod"     136 44975 34695     0)
	    ("lightsalmon3"      137 44975 34695 24415)
	    ("rosybrown"         138 44975 34695 34695)
	    ("rosybrown"         139 44975 34695 44975)
	    ("mediumpurple2"     140 44975 34695 55255)
	    ("mediumpurple1"     141 44975 34695 65535)
	    ("gold3"             142 44975 44975     0)
	    ("darkkhaki"         143 44975 44975 24415)
	    ("navajowhite3"      144 44975 44975 34695)
	    ("gray69"            145 44975 44975 44975)
	    ("lightsteelblue3"   146 44975 44975 55255)
	    ("lightsteelblue"    147 44975 44975 65535)
	    ("yellow3"           148 44975 55255     0)
	    ("darkolivegreen3"   149 44975 55255 24415)
	    ("darkseagreen3"     150 44975 55255 34695)
	    ("darkseagreen2"     151 44975 55255 44975)
	    ("lightcyan3"        152 44975 55255 55255)
	    ("lightskyblue1"     153 44975 55255 65535)
	    ("greenyellow"       154 44975 65535     0)
	    ("darkolivegreen2"   155 44975 65535 24415)
	    ("palegreen1"        156 44975 65535 34695)
	    ("darkseagreen2"     157 44975 65535 44975)
	    ("darkseagreen1"     158 44975 65535 55255)
	    ("paleturquoise1"    159 44975 65535 65535)
	    ("red3"              160 55255     0     0)
	    ("deeppink3"         161 55255     0 24415)
	    ("deeppink3"         162 55255     0 34695)
	    ("magenta3"          163 55255     0 44975)
	    ("magenta3"          164 55255     0 55255)
	    ("magenta2"          165 55255     0 65535)
	    ("darkorange3"       166 55255 24415     0)
	    ("indianred"         167 55255 24415 24415)
	    ("hotpink3"          168 55255 24415 34695)
	    ("hotpink2"          169 55255 24415 44975)
	    ("orchid"            170 55255 24415 55255)
	    ("mediumorchid1"     171 55255 24415 65535)
	    ("orange3"           172 55255 34695     0)
	    ("lightsalmon3"      173 55255 34695 24415)
	    ("lightpink3"        174 55255 34695 34695)
	    ("pink3"             175 55255 34695 44975)
	    ("plum3"             176 55255 34695 55255)
	    ("violet"            177 55255 34695 65535)
	    ("gold3"             178 55255 44975     0)
	    ("lightgoldenrod3"   179 55255 44975 24415)
	    ("tan"               180 55255 44975 34695)
	    ("mistyrose3"        181 55255 44975 44975)
	    ("thistle3"          182 55255 44975 55255)
	    ("plum2"             183 55255 44975 65535)
	    ("yellow3"           184 55255 55255     0)
	    ("khaki3"            185 55255 55255 24415)
	    ("lightgoldenrod2"   186 55255 55255 34695)
	    ("lightyellow3"      187 55255 55255 44975)
	    ("gray84"            188 55255 55255 55255)
	    ("lightsteelblue1"   189 55255 55255 65535)
	    ("yellow2"           190 55255 65535     0)
	    ("darkolivegreen1"   191 55255 65535 24415)
	    ("darkolivegreen1"   192 55255 65535 34695)
	    ("darkseagreen1"     193 55255 65535 44975)
	    ("honeydew2"         194 55255 65535 55255)
	    ("lightcyan"         195 55255 65535 65535)
	    ("red"               196 65535     0     0)
	    ("deeppink2"         197 65535     0 24415)
	    ("deeppink"          198 65535     0 34695)
	    ("deeppink"          199 65535     0 44975)
	    ("magenta2"          200 65535     0 55255)
	    ("magenta"           201 65535     0 65535)
	    ("orangered"         202 65535 24415     0)
	    ("indianred1"        203 65535 24415 24415)
	    ("indianred1"        204 65535 24415 34695)
	    ("hotpink"           205 65535 24415 44975)
	    ("hotpink"           206 65535 24415 55255)
	    ("mediumorchid1"     207 65535 24415 65535)
	    ("darkorange"        208 65535 34695     0)
	    ("salmon1"           209 65535 34695 24415)
	    ("lightcoral"        210 65535 34695 34695)
	    ("palevioletred1"    211 65535 34695 44975)
	    ("orchid2"           212 65535 34695 55255)
	    ("orchid1"           213 65535 34695 65535)
	    ("orange"            214 65535 44975     0)
	    ("sandybrown"        215 65535 44975 24415)
	    ("lightsalmon"       216 65535 44975 34695)
	    ("lightpink1"        217 65535 44975 44975)
	    ("pink1"             218 65535 44975 55255)
	    ("plum1"             219 65535 44975 65535)
	    ("gold"              220 65535 55255     0)
	    ("lightgoldenrod2"   221 65535 55255 24415)
	    ("lightgoldenrod2"   222 65535 55255 34695)
	    ("navajowhite"       223 65535 55255 44975)
	    ("mistyrose"         224 65535 55255 55255)
	    ("thistle1"          225 65535 55255 65535)
	    ("yellow"            226 65535 65535     0)
	    ("lightgoldenrod1"   227 65535 65535 24415)
	    ("khaki1"            228 65535 65535 34695)
	    ("wheat1"            229 65535 65535 44975)
	    ("cornsilk"          230 65535 65535 55255)
	    ("white"             231 65535 65535 65535)
	    ("gray3"             232  2056  2056  2056)
	    ("gray7"             233  4626  4626  4626)
	    ("gray11"            234  7196  7196  7196)
	    ("gray15"            235  9766  9766  9766)
	    ("gray19"            236 12336 12336 12336)
	    ("gray23"            237 14906 14906 14906)
	    ("gray27"            238 17476 17476 17476)
	    ("gray30"            239 20046 20046 20046)
	    ("gray34"            240 22616 22616 22616)
	    ("gray38"            241 25186 25186 25186)
	    ("gray42"            242 27756 27756 27756)
	    ("gray46"            243 30326 30326 30326)
	    ("gray50"            244 32896 32896 32896)
	    ("gray54"            245 35466 35466 35466)
	    ("gray58"            246 38036 38036 38036)
	    ("gray62"            247 40606 40606 40606)
	    ("gray66"            248 43176 43176 43176)
	    ("gray70"            249 45746 45746 45746)
	    ("gray74"            250 48316 48316 48316)
	    ("gray78"            251 50886 50886 50886)
	    ("gray81"            252 53456 53456 53456)
	    ("gray85"            253 56026 56026 56026)
	    ("gray89"            254 58596 58596 58596)
	    ("gray93"            255 61166 61166 61166)))
    "RGB default colors of the 256 color terminals")

;; called both from init-tty-win and from the C code.
(defun init-pre-tty-win ()
  "Initialize TTY at startup (pre).  Don't call this."
  (unless pre-tty-win-initted
    (register-tty-color "black"   "\e[30m" "\e[40m")
    (register-tty-color "red"     "\e[31m" "\e[41m")
    (register-tty-color "green"   "\e[32m" "\e[42m")
    (register-tty-color "yellow"  "\e[33m" "\e[43m")
    (register-tty-color "blue"    "\e[34m" "\e[44m")
    (register-tty-color "magenta" "\e[35m" "\e[45m")
    (register-tty-color "cyan"    "\e[36m" "\e[46m")
    (register-tty-color "white"   "\e[37m" "\e[47m")

    ;; Define `highlighted' tty colors
    (register-tty-color "darkgrey"      "\e[1;30m" "\e[1;40m")
    (register-tty-color "brightred"     "\e[1;31m" "\e[1;41m")
    (register-tty-color "brightgreen"   "\e[1;32m" "\e[1;42m")
    (register-tty-color "brightyellow"  "\e[1;33m" "\e[1;43m")
    (register-tty-color "brightblue"    "\e[1;34m" "\e[1;44m")
    (register-tty-color "brightmagenta" "\e[1;35m" "\e[1;45m")
    (register-tty-color "brightcyan"    "\e[1;36m" "\e[1;46m")
    (register-tty-color "brightwhite"   "\e[1;37m" "\e[1;47m")

    (setq pre-tty-win-initted t)))

;; called both from init-tty-win and from the C code.
;; we have to do this for every created TTY console.
(defun init-post-tty-win (console)
  "Initialize TTY at console creation time (post).  Don't call this."
  ;; load the appropriate term-type-specific Lisp file.
  ;; we don't do this at startup here so that the user can
  ;; override term-file-prefix. (startup.el does it after
  ;; loading the init file.)
  (if (featurep 'mule)
      (init-mule-tty-win))
  (when init-file-loaded
    ;; temporarily select the console so that the changes
    ;; to function-key-map are made for the right console.
    (let ((foobar (selected-console)))
      (unwind-protect
	  (progn
	    (select-console console)
	    (load-terminal-library))
	(select-console foobar)))))

(defvar tty-win-initted nil)

(defun init-tty-win ()
  "Initialize TTY at startup.  Don't call this."
  (unless tty-win-initted
    (init-pre-tty-win)
    (make-tty-device nil nil)
    (init-post-tty-win (selected-console))
    (setq tty-win-initted t)))

(defun make-frame-on-tty (tty &optional props)
  "Create a frame on the TTY connection named TTY.
TTY should be a TTY device name such as \"/dev/ttyp3\" (as returned by
the `tty' command in that TTY), or nil for the standard input/output
of the running XEmacs process.

PROPS should be a plist of properties, as in the call to `make-frame'.

This function opens a connection to the TTY or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on TTY: ")
  (if (equal tty "") (setq tty nil))
  (make-frame-on-device 'tty tty props))

;;; tty-init.el ends here

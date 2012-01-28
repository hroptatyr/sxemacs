;; kinsoku.el -- Kinsoku (line wrap) processing for XEmacs/Mule -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of XEmacs).
;; This file contains Japanese and Chinese characters.

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

;; Written by Jareth Hein (jhod@po.iijnet.or.jp) based off of
;; code by S.Tomura, Electrotechnical Lab. (tomura@etl.go.jp) from
;; Mule-2.3

;;;    Special characters for JIS code
;;;     "$B!!!"!#!$!%!&!'!'!(!)!*!+!,!-!.!/(B"
;;;   "$B!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B"
;;;   "$B!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B"
;;;   "$B!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B"
;;;   "$B!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B"
;;;   "$B!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B"
;;;     "$B"!"""#"$"%"&"'"(")"*"+","-".(B "
;;;     "$B&!&"&#&$&%&&&'&(&)&*&+&,&-&.&/(B"
;;;   "$B&0&1&2&3&4&5&6&7&8(B"
;;;     "$B&A&B&C&D&E&F&G&H&I&J&K&L&M&N&O(B"
;;;   "$B&P&Q&R&S&T&U&V&W&X(B"
;;;     "$B'!'"'#'$'%'&'''(')'*'+','-'.'/(B"
;;;   "$B'0'1'2'3'4'5'6'7'8'9':';'<'='>'?(B"
;;;   "$B'@'A(B"
;;;     "$B'Q'R'S'T'U'V'W'X'Y'Z'['\']'^'_!I(B
;;;   "$B'`'a'b'c'd'e'f'g'h'i'j'k'l'm'n'o(B"
;;;   "$B'p'q(B"
;;;    $B#0#1#2#3#4#5#6#7#8#9#A#B#C#D#E#F(B
;;;   "$B$!$#$%$'$)$C$c$e$g$n(B"
;;;   "$B%!%#%%%'%)%C%c%e%g%n%u%v(B"

;;; Special characters for GB
;;;
;;;  $A!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
;;;$A!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
;;;$A!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
;;;$A!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
;;;$A!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
;;;$A!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
;;;  $A"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
;;;$A"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
;;;$A"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
;;;$A"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
;;;$A"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
;;;  $A#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
;;;$A#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
;;;$A#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
;;;$A#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
;;;$A#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
;;;$A#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
;;;  $A$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
;;;$A$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
;;;$A$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
;;;$A$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
;;;$A$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
;;;$A$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
;;;  $A%!%"%#%$%%%&%'%(%)%*%+%,%-%.%/(B
;;;$A%0%1%2%3%4%5%6%7%8%9%:%;%<%=%>%?(B
;;;$A%@%A%B%C%D%E%F%G%H%I%J%K%L%M%N%O(B
;;;$A%P%Q%R%S%T%U%V%W%X%Y%Z%[%\%]%^%_(B
;;;$A%`%a%b%c%d%e%f%g%h%i%j%k%l%m%n%o(B
;;;$A%p%q%r%s%t%u%v%w%x%y%z%{%|%}%~(B
;;;  $A&!&"&#&$&%&&&'&(&)&*&+&,&-&.&/(B
;;;$A&0&1&2&3&4&5&6&7&8&9&:&;&<&=&>&?(B
;;;$A&@&A&B&C&D&E&F&G&H&I&J&K&L&M&N&O(B
;;;$A&P&Q&R&S&T&U&V&W&X&Y&Z&[&\&]&^&_(B
;;;$A&`&a&b&c&d&e&f&g&h&i&j&k&l&m&n&o(B
;;;$A&p&q&r&s&t&u&v&w&x&y&z&{&|&}&~(B
;;;  $A'!'"'#'$'%'&'''(')'*'+','-'.'/(B
;;;$A'0'1'2'3'4'5'6'7'8'9':';'<'='>'?(B
;;;$A'@'A'B'C'D'E'F'G'H'I'J'K'L'M'N'O(B
;;;$A'P'Q'R'S'T'U'V'W'X'Y'Z'['\']'^'_(B
;;;$A'`'a'b'c'd'e'f'g'h'i'j'k'l'm'n'o(B
;;;$A'p'q'r's't'u'v'w'x'y'z'{'|'}'~(B
;;;  $A(!("(#($(%(&('((()(*(+(,(-(.(/(B
;;;$A(0(1(2(3(4(5(6(7(8(9(:(;(<(=(>(?(B
;;;$A(@(A(B(C(D(E(F(G(H(I(J(K(L(M(N(O(B
;;;$A(P(Q(R(S(T(U(V(W(X(Y(Z([(\(](^(_(B
;;;$A(`(a(b(c(d(e(f(g(h(i(j(k(l(m(n(o(B

;;; Special characters for BIG5
;;;
;;;  $(0!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
;;;$(0!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
;;;$(0!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
;;;$(0!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
;;;$(0!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
;;;$(0!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
;;;  $(0"!"""#"$"%"&"'"(")"*"+","-"."/(B
;;;$(0"0"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
;;;$(0"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
;;;$(0"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
;;;$(0"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
;;;$(0"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
;;;  $(0#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
;;;$(0#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
;;;$(0#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
;;;$(0#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
;;;$(0#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
;;;$(0#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
;;;  $(0$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
;;;$(0$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
;;;$(0$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
;;;$(0$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
;;;$(0$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
;;;$(0$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
;;;  $(0%!%"%#%$%%%&%'%(%)%*%+%,%-%.%/(B
;;;$(0%0%1%2%3%4%5%6%7%8%9%:%;%<%=%>%?(B

(defvar kinsoku-ascii nil "Do kinsoku-processing for ASCII.")
(make-variable-buffer-local 'kinsoku-ascii)
(set-default 'kinsoku-ascii nil)
(defvar kinsoku-jis t "Do kinsoku-processing for JISX0208.")
(make-variable-buffer-local 'kinsoku-jis)
(set-default 'kinsoku-jis t)
(defvar kinsoku-gb t "Do kinsoku-processing for GB2312.")
(make-variable-buffer-local 'kinsoku-gb)
(set-default 'kinsoku-gb t)
(defvar kinsoku-big5 t "Do kinsoku-processing for Big5..")
(make-variable-buffer-local 'kinsoku-big5)
(set-default 'kinsoku-big5 t)

(defvar kinsoku-ascii-bol "!)-_~}]:;',.?" "BOL kinsoku for ASCII.")
(defvar kinsoku-ascii-eol "({[" "EOL kinsoku for ASCII.")
(defvar kinsoku-jis-bol
  (concat  "$B!"!#!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>(B"
	   "$B!?!@!A!B!C!D!E!G!I!K!M!O!Q!S!U!W!Y![!k!l!m!n(B"
	   "$B$!$#$%$'$)$C$c$e$g$n%!%#%%%'%)%C%c%e%g%n%u%v(B")
  "BOL kinsoku for JISX0208.")
(defvar kinsoku-jis-eol
  "$B!F!H!J!L!N!P!R!T!V!X!Z!k!l!m!n!w!x(B"
  "EOL kinsoku for JISX0208.")
(defvar kinsoku-gb-bol
  (concat  "$A!"!##.#,!$!%!&!'!(!)!*!+!,!-!/!1#)!3!5!7!9!;!=(B"
	   "$A!?#;#:#?#!!@!A!B!C!c!d!e!f#/#\#"#_#~#|(e(B")
  "BOL kinsoku for GB2312.")
(defvar kinsoku-gb-eol
  (concat "$A!.!0#"#(!2!4!6!8!:!<!>!c!d!e#@!f!l(B"
	  "$A(E(F(G(H(I(J(K(L(M(N(O(P(Q(R(S(T(U(V(W(X(Y(h(B")
  "EOL kinsoku for GB2312.")
(defvar kinsoku-big5-bol
  (concat  "$(0!"!#!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2(B"
	   "$(0!3!4!5!6!7!8!9!:!;!<!=!?!A!C!E!G!I!K(B"
	   "$(0!M!O!Q(B	$(0!S!U!W!Y![!]!_!a!c!e!g!i!k!q(B"
	   "$(0"#"$"%"&"'"(")"*"+","2"3"4"j"k"l"x%7(B")
  "BOL kinsoku for BIG5.")
(defvar kinsoku-big5-eol
  (concat "$(0!>!@!B!D!F!H!J!L!N!P!R!T!V!X!Z!\!^!`!b(B"
	  "$(0!d!f!h!j!k!q!p"i"j"k"n"x$u$v$w$x$y$z${(B"
	  "$(0$|$}$~%!%"%#%$%%%&%'%(%)%*%+%:(B")
  "EOL kinsoku for BIG5.")

(define-category ?s "Kinsoku forbidden start of line characters")
(define-category ?e "Kinsoku forbidden end of line characters")

;; kinsoku ascii
(loop for char in (string-to-char-list kinsoku-ascii-bol)
      do (modify-category-entry char ?s))
(loop for char in (string-to-char-list kinsoku-ascii-eol)
      do (modify-category-entry char ?e))
;; kinsoku-jis
(loop for char in (string-to-char-list kinsoku-jis-bol)
      do (modify-category-entry char ?s))
(loop for char in (string-to-char-list kinsoku-jis-eol)
      do (modify-category-entry char ?e))
;; kinsoku-gb
(loop for char in (string-to-char-list kinsoku-gb-bol)
      do (modify-category-entry char ?s))
(loop for char in (string-to-char-list kinsoku-gb-eol)
      do (modify-category-entry char ?e))
;; kinsoku-big5
(loop for char in (string-to-char-list kinsoku-big5-bol)
      do (modify-category-entry char ?s))
(loop for char in (string-to-char-list kinsoku-big5-eol)
      do (modify-category-entry char ?e))

(defun kinsoku-bol-p ()
  "Check if point would break forbidden beginning-of-line rules
Uses category \'s\' to check.
point$B$G2~9T$9$k$H9TF,6XB'$K?($l$k$+$I$&$+$r$+$($9!#(B
$B9TF,6XB'J8;z$O(B\'s\'$B$N(Bcategory$B$G;XDj$9$k!#(B"
  (let ((before (char-before))
	(after (char-after)))
    (if (and after
	     (or
	      (and kinsoku-ascii (char-in-category-p after ?a))
	      (and kinsoku-jis (or (char-in-category-p after ?j)
				   (and before
					(char-in-category-p before ?j))))
	      (and kinsoku-gb (or (char-in-category-p after ?c)
				  (and before
				       (char-in-category-p before ?c))))
	      (and kinsoku-big5 (or (char-in-category-p after ?t)
				    (and before
					 (char-in-category-p before ?t))))))
	(char-in-category-p after ?s)
      nil)))

(defun kinsoku-eol-p ()
  "Check if point would break forbidden end-of-line rules
Uses category \'e\' to check.
point$B$G2~9T$9$k$H9TKv6XB'$K?($l$k$+$I$&$+$r$+$($9!#(B
$B9TKv6XB'J8;z$O(B\'s\'$B$N(Bcategory$B$G;XDj$9$k!#(B"
  (let ((before (char-before))
	(after (char-after)))
    (if (and before
	     (or
	      (and kinsoku-ascii (char-in-category-p before ?a))
	      (and kinsoku-jis (or (char-in-category-p before ?j)
				   (and after
					(char-in-category-p after ?j))))
	      (and kinsoku-gb (or (char-in-category-p before ?c)
				  (and after
				       (char-in-category-p after ?c))))
	      (and kinsoku-big5 (or (char-in-category-p before ?t)
				    (and after
					 (char-in-category-p after ?t))))))
	(char-in-category-p before ?e)
      nil)))

(defvar kinsoku-extend-limit nil
  "Defines how many characters kinsoku will search forward before giving up.
A value of nil equates to infinity.
$B6XB'=hM}$G9T$r?-$P$7$FNI$$H>3QJ8;z?t$r;XDj$9$k!#(B
$BHsIi@0?t0J30$N>l9g$OL58BBg$r0UL#$9$k!#(B")

(defun kinsoku-process ()
  "Move to a point that will not break forbidden line break rules.
$B6XB'$K?($l$J$$E@$X0\F0$9$k!#(B
point$B$,9TF,6XB'$K?($l$k>l9g$O9T$r?-$P$7$F!"6XB'$K?($l$J$$E@$rC5$9!#(B
point$B$,9TKv6XB'$K?($l$k>l9g$O9T$r=L$a$F!"6XB'$K?($l$J$$E@$rC5$9!#(B
$B$?$@$7!"9T?-$P$7H>3QJ8;z?t$,(Bkinsoku-extend-limit$B$r1[$($k$H!"(B
$B9T$r=L$a$F6XB'$K?($l$J$$E@$rC5$9!#(B"
  (let ((bol-kin nil) (eol-kin nil))
    (if (and (not (bolp))
	     (not (eolp))
	     (or (setq bol-kin (kinsoku-bol-p))
		 (setq eol-kin (kinsoku-eol-p))))
	(cond(bol-kin (kinsoku-process-extend))
	     (eol-kin (kinsoku-process-shrink))))))

(defun kinsoku-process-extend ()
  "Move point forward to a permissable for line-breaking.
$B9T$r?-$P$7$F6XB'$K?($l$J$$E@$X0\F0$9$k!#(B"
  (let ((max-column (+ fill-column
		       (if (and (numberp kinsoku-extend-limit)
				(>= kinsoku-extend-limit 0))
			   kinsoku-extend-limit
			 10000)))  ;;; 10000 is deliberatly unreasonably large
	ch1 ch2)
    (while (and (setq ch1 (char-after))
		(<= (+ (current-column)
		       (char-width ch1 ))
		    max-column)
		(not (bolp))
		(not (eolp))
		(or (kinsoku-eol-p)
		    (kinsoku-bol-p)
		    ;;; don't break in the middle of an English word
		    (and (char-in-category-p ch1 ?a)
			 (setq ch2 (char-before))
			 (char-in-category-p ch2 ?a)
			 (= ?w (char-syntax ch2))
			 (= ?w (char-syntax ch1)))))
      (forward-char))
    (if (or (kinsoku-eol-p) (kinsoku-bol-p))
	(kinsoku-process-shrink))))

(defun kinsoku-process-shrink ()
  "Move point backward to a point permissable for line-breaking.
$B9T$r=L$a$F6XB'$K?($l$J$$E@$X0\F0$9$k!#(B"
  (let (ch1 ch2)
    (while (and (not (bolp))
		(not (eolp))
		(or (kinsoku-bol-p)
		    (kinsoku-eol-p)
		;;; don't break in the middle of an English word
		    (and
		     (char-in-category-p (setq ch1 (following-char)) ?a)
		     (char-in-category-p (setq ch2 (preceding-char)) ?a)
		     (= ?w (char-syntax ch2))
		     (= ?w (char-syntax ch1)))))
      (backward-char))))

#!/bin/sh
if gnuclient -batch -eval t >/dev/null 2>&1; then
  exec gnuclient ${1+"$@"}
else
  xemacs -unmapped -f gnuserv-start &
  until gnuclient -batch -eval t >/dev/null 2>&1
  do
     sleep 1
  done
  exec gnuclient ${1+"$@"}
fi

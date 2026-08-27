# E18: an unrecognised option printed the usage text and returned 0, so
# `lispf4 -z basic.img` reported success to make or to a script; options
# after the image file name were dropped in silence; and -x together with an
# image file ignored the image without saying so.

"$LISPF4" -z "$LISPF4_IMG" < /dev/null > out.txt 2>&1
[ $? -ne 0 ] || { echo "an unknown option exited 0"; exit 1; }

"$LISPF4" -h > /dev/null 2>&1
[ $? -eq 0 ] || { echo "-h did not exit 0"; exit 1; }

"$LISPF4" --help > /dev/null 2>&1
[ $? -eq 0 ] || { echo "--help did not exit 0"; exit 1; }

"$LISPF4" "$LISPF4_IMG" -c200000 < /dev/null > out.txt 2>&1
[ $? -ne 0 ] || { echo "an option after the image name was ignored"; exit 1; }

"$LISPF4" -x "$LISPF4_IMG" < /dev/null > out.txt 2>&1
[ $? -ne 0 ] || { echo "-x with an image file was accepted"; exit 1; }

#  The forms that must keep working.
printf '(PLUS 2 3)\n(EXIT)\n' > in.lsp
"$LISPF4" -c200000 "$LISPF4_IMG" < in.lsp > out.txt 2>&1
[ $? -eq 0 ] || { echo "-c200000 IMG stopped working"; cat out.txt; exit 1; }
"$LISPF4" -c 200000 "$LISPF4_IMG" < in.lsp > out.txt 2>&1
[ $? -eq 0 ] || { echo "-c 200000 IMG stopped working"; cat out.txt; exit 1; }
exit 0

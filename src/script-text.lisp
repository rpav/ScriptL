(in-package :scriptl)

;;; Emacs gets really confused with syntax and thus this is separated
;;; into its own file.

(defparameter *scriptl-text-v2*
  "#!/bin/sh
SCRIPTLCOM=\"~A\"
FUNCTION=\"~A\"
ERRORS=\"~A\"

$SCRIPTLCOM --sl-version 2 -E \"$ERRORS\" -f \"$FUNCTION\" \"$@\"
")

(defparameter *script-text-v1*
  "#!/bin/sh
if [ -z \"$SCRIPTL_PORT\" ]; then SCRIPTL_PORT=4010; fi
encode_length () {
    printf %08X ${#1}
}
encode_packet () {
    encode_length \"$1\"; echo -n \"$1\"
}
hex_to_decimal () {
    printf %d \"0x$1\"
}
read_length () {
    hex_to_decimal `head -c 8`
}
read_packet () {
    head -c `read_length`
}
encode_args () {
    for i in \"$@\"; do encode_packet \"$i\"; done
}
parse () {
    status=`read_packet`
    if [ \"$status\" = \":ok\" ]; then
        ret=`read_packet`; out=`read_packet`
        if [ -z \"$out\" ]; then echo \"$ret\"; else echo \"$out\"; fi
    else
        err=`read_packet`; str=`read_packet`; echo \"Error: $err\"; echo; echo $str
    fi
}

{ encode_packet \"(:scriptl 1)\"
  encode_packet \"(:cwd #P\\\"`pwd`/\\\")\"
  encode_packet \"(:funcall $# #P\\\"$0\\\")\"
  encode_packet \"(:errors ~A)\"
  encode_packet \"~A\"
  encode_args \"$@\"
} | nc localhost $SCRIPTL_PORT | parse
")

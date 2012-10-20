(in-package :scriptl)

;;; Emacs gets really confused with syntax and thus this is separated
;;; into its own file.

(defparameter *script-text*
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
    read status
    if [ \"$status\" = \":ok\" ]; then
        ret=`read_packet`; out=`read_packet`
        if [ -z \"$out\" ]; then echo \"$ret\"; else echo \"$out\"; fi
    else
        read err; str=`read_packet`; echo \"Error: $err\"; echo; echo $str
    fi
}

{ echo \"(:scriptl 1)\"
  echo \"(:cwd #P\\\"`pwd`/\\\")\"
  echo \"(:funcall $# #P\\\"$0\\\")\"
  echo \"(:errors ~A)\"
  echo \"~A\"
  encode_args \"$@\"
} | nc localhost $SCRIPTL_PORT | parse
")

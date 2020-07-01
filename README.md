# ScriptL v2

There are various contortions for getting your Common Lisp started in
a shell script and running some of your code.  In the end, you're
still falling back on writing Lisp like you're writing anything else:
write, run, stop, repeat.

If only we could do this:

```lisp
(defun my-script (x)
  (format t "If this were a real script, we would use ~A responsibly~%" x))

;; Make a wrapper to call this in our running Lisp!
(make-script "my-script" 'my-script)
```

And then:

```console
$ my-script "foo"
If this were a real script, we would use foo responsibly.
$
```

Well, of course, that's exactly what ScriptL lets you do.  Since this
**calls your running lisp**, it means you're **live coding** your
scripts.

## Quick Guide

You can get ScriptL from Quicklisp via `(ql:quickload "scriptl")`.  This will load scriptl and create the `scriptl` command for managing scripts (see below).

First however, you must start the server.  It may be convenient to create a shell script that launches your favorite CL on login and does the following:

```lisp
(scriptl:start)
```

The scripts in `examples/` require `unix-options` for argument parsing
as well, so to run them load that system as well:

```lisp
(asdf:load-system :scriptl-examples)
```

Now you can call things via the generated shell scripts (you might have
to call them via `./[script]` from the local directory):

```console
$ funcall princ '"hello world"'
hello world
$ eval '(princ "hello world")'
hello world
$ eval '(+ 1 1)'
2
$
```

Alternatively, once you have something you want as a more permanent
script, you can use `SCRIPTL:MAKE-SCRIPT` as above:

```lisp
(make-script "SCRIPT-NAME" 'FUNCTION-NAME &optional ERROR-HANDLER)
```

This will make a shell script called `SCRIPT-NAME`, probably in your home
directory (or wherever you started lisp).  It will call
`FUNCTION-NAME`, with any parameters passed to the script given as
*strings*.

Alternatively, you can use the `make-script` script included in the examples, which
is a ScriptL wrapper for itself:

```console
$ make-script some-script some-function [error-function]
```

This will make the new script in the current working directory,
because wrappered calls rebind `*DEFAULT-PATHNAME-DEFAULTS*` for your
convenience.  Amusingly, you can use `make-script` to generate itself:

```console
$ make-script make-script scriptl:make-script scriptl:make-script-usage
```

This will overwrite itself with an updated copy, assuming you're using
an OS which isn't picky about such things.

## `scriptl` and registering scripts

After installing ScriptL, the `scriptl` command will be created in the `bin/` directory.  This is part of a small API for registering, listing, and creating scripts.  For instance:

```console
$ scriptl list
Script                           Description

SCRIPTL:SCRIPTL                - ScriptL management command: scriptl
$ scriptl make SCRIPTL:SCRIPTL
$ ls
scriptl
$
```

This is more useful with a lot of scripts loaded.  For example, after loading my personal scripts:

```console
$ scriptl list
Script                           Description

SCRIPTL.SCRIPT.JSON:JSON       - JSON manipulation commands: json, json2yaml,
                                 sexp2json, dir2json
SCRIPTL.SCRIPT.NEW:NEW         - Template system command: new
SCRIPTL.SCRIPT.YAML:YAML       - YAML manipulation commands: yaml2json
SCRIPTL:SCRIPTL                - ScriptL management command: scriptl
$
```

To register scripts for this list, you should make a function which calls `MAKE-SCRIPT` for each script you wish to create, and then call `SCRIPTL:REGISTER` to register this:

```lisp
(defun make-my-scripts ()
  (make-script ...)
  (make-script ...))

(scriptl:register 'my-scripts 'make-my-scripts "My script commands: foo, bar, baz")
(export 'my-scripts)
```

Now you would see the following:

```lisp
$ scriptl list
Script                           Description

MY-PACKAGE:MY-SCRIPTS          - My script commands: foo, bar, baz
SCRIPTL:SCRIPTL                - ScriptL management command: scriptl
$
```

## Defaults and the Header

A number of useful things are set up by default:

* The default package is `COMMON-LISP-USER`
* Wrappers set the current working directory
* The current script name is set in `SCRIPTL:*SCRIPT*`
* The parsed header is set in `SCRIPTL:*HEADER*`

As above, whenever a wrapper is run, ScriptL sets
`*DEFAULT-PATHNAME-DEFAULTS*` for you, which is enough for most Lisp
functions to find the right file.

However, for things like `osicat`, you may need to use
`(merge-pathnames FILENAME)` to get the appropriate thing.

You may find it interesting to dispatch on the script name, which is
set in `SCRIPTL:*SCRIPT*`.  It's a pathname, so you can extract the
various bits as appropriate.

Additionally, you can access the complete call information via
`SCRIPTL:*HEADER*`, e.g.:

```lisp
(defun show-header (&rest args) scriptl:*header*)
```

(You don't have to use `&rest args`; this is for demonstration.)

```console
$ funcall show-header 1 2 3
#S(SCRIPTL:HEADER
   :VERSION 1
   :CWD #P"/path/to/current/dir/"
   :COMMAND (:FUNCALL 3 #P"./funcall")
   :FUN SHOW-HEADER
   :ARGS ("1" "2" "3")
   :ERROR-FUN NIL)
$
```

Most of this isn't particularly interesting or beyond what you already
get, but it's there.

## Readline

The v2 client adds simple support for the GNU readline library.
There are two basic operations: `readline PROMPT` and `addhistory
LINE`:

```lisp
(defun test ()
  (format t "Type 'quit' to finish.~%")
  (loop as line = (readline "> ")
        until (string= line "quit") do
          (addhistory line)
          (format t "You typed: ~A~%" line)))
```

In the shell:

```console
Type 'quit' to finish.
> foo
You typed: foo
> quit
```

If GNU readline support is compiled into the client (autodetected),
this will have all the fancy editing features that provides, including
history.  If not, or if the function is not called from ScriptL, the
prompt will be shown, but only basic terminal input is provided, and
history is ignored.

## getenv

Also included in v2 is support for remote `getenv(3)`, allowing
client-side configuration via environment variables.  This is done via
the `scriptl:getenv` function.

This will return the value of a variable on the remote side, or for
functions not being called via a ScriptL command, will fall back to
`osicat-posix:getenv` and return the environment for the running lisp.

In both cases, this returns the value of the variable as a string if
set, or `NIL`.

## I/O

The v2 protocol adds a custom client which supports standard lisp I/O
operations on `*standard-input*` and `*standard-output*`.  For
example:

```lisp
(defun test-read-line ()
  (format t "Read line: ~A~%" (read-line)))
```

```console
$ echo foo | test-read-line
Read line: foo
$
```

Additionally, you can use `read-sequence`, `read-byte`, and
`read-char`.  The former two return octet vectors, which may be useful
for processing data byte-by-byte.

Reading and writing may be interleaved as desired.  Writing also
supports `write-sequence`, `write-byte`, and `write-char`.  In order
to write a raw byte sequence, however, it must be specified as an
array with `:element-type '(unsigned-byte 8)`.  Otherwise it will be
treated as a general object, and be pretty-printed.

## Results and Error Handling

When things run correctly, if there is output to `*standard-output*`,
this is shown to the user.  If there is no output, the return values
are shown instead:

```console
$ eval '(+ 2 2)'
4
$ eval '(values 1 2 3)'
(VALUES 1 2 3)
$
```

When things go wrong, the error condition is shown by default:

```console
$ eval 'foo'
Error: UNBOUND-VARIABLE

The variable FOO is unbound.
$
```

If you want slightly more sophisticated handling, you can define an
error handler when calling `MAKE-SCRIPT`, which will get passed the
condition via `HANDLER-BIND`.  If you handle it, return an non-`NIL`
value.  If you can't handle the condition, return `NIL`.

## Examples

The `examples/` directory has a number of examples; most of them are
simply talking to the ScriptL server.

The `example.lisp` and `test-cmd` example shows how to make a more
shell-scripty-feeling function with more conventional means.

Alternatively, if you look at `src/make-script.lisp`, you can see
a (rather naïve) error handler which provides usage.  `MAKE-SCRIPT`
also handles its parameters appropriately if they're strings.

## Details

This all works, of course, by making a server on a port in your Lisp
and listening there.  I *really* wanted to use Swank, but in the end
the problem went from simply talking to swank to implementing READ in
the shell, and that's a lot more work than just writing a new, more
targeted server.

For V1, this relied on netcat.  V2 presents a custom C client, which
builds as part of the ASDF load operation, and supports a number of
new features, such as I/O.

ScriptL initially used TCP port 4010.  This is bad for a number of
reasons .. each user can't have their own lisp, and anyone can access
it.  V2 uses Unix Domain Sockets by default, and only the owner can
access it.  This is restricted by file permissions.

You can still use the TCP server, by specifying `(scriptl:start
:internet)`.  You can adjust the port by binding
`scriptl:*scriptl-port*` to the desired port before doing this.
This talks *only* to localhost.  It's horribly insecure, though no
moreso than swank/slime, really.  If you're worried, or not the only
person with access to your host, *don't run it*.

In theory, making this talk over an ssh tunnel would be pretty easy,
but switching ports and doing the setup isn't at all nice.

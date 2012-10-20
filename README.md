# ScriptL

There are various contortions for getting your Common Lisp started in
a shell script and running some of your code.  In the end, you're
still falling back on writing Lisp like you're writing anything else:
write, run, stop, repeat.

If only we could do this:

```lisp
(defun my-command (x)
  (format t "If this were a real command, we would use ~A responsibly~%" x))

;; Make a wrapper to call this in our running Lisp!
(make-command "my-command" 'my-command)
```

And then:

```shell
$ my-command "foo"
If this were a real command, we would use foo responsibly.
$
```

Well, of course, that's exactly what ScriptL lets you do.  Since this
**calls your running lisp**, it means you're **live coding** your
commands.

## Quick Guide

ScriptL requires the following things:

* `usocket` (for listening)
* `osicat` (for chmod)
* netcat (for wrappers to call ScriptL)

Usage is simple:

```lisp
(asdf:load-system :scriptl)
(scriptl:start)
```

Now you can call things via the shell scripts included in `examples/`:

```shell
$ funcall princ "hello world"
hello world
$ eval '(princ "hello world")'
hello world
$ eval '(+ 1 1)'
2
$
```

Alternatively, once you have something you want as a more permanent
command, you can use `SCRIPTL:MAKE-COMMAND` as above:

```lisp
(make-command COMMAND-NAME FUNCTION-NAME &optional ERROR-HANDLER)
```

This will make a shell script called `COMMAND`, probably in your home
directory (or wherever you started lisp).  It will call
`FUNCTION-NAME`, with any parameters passed to the script given as
*strings*.

Alternatively, you can use the `make-command` script included, which
is a ScriptL wrapper for itself:

```shell
$ make-script some-command some-function [error-function]
```

This will make the new script in the current working directory,
because wrappered calls rebind `*DEFAULT-PATHNAME-DEFAULTS*` for your
convenience.  Amusingly, you can use `make-script` to generate itself:

```shell
$ make-script make-script scriptl:make-script scriptl:make-script-usage
```

This will overwrite itself with an updated copy, assuming you're using
an OS which isn't picky about such things.

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

```shell
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

## Results and Error Handling

When things run correctly, if there is output to `*standard-output*`,
this is shown to the user.  If there is no output, the return values
are shown instead:

```shell
$ eval '(+ 2 2)'
4
$ eval '(values 1 2 3)'
(VALUES 1 2 3)
$
```

When things go wrong, the error condition is shown by default:

```shell
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

The ScriptL server runs on port 4010 (swank by default runs on 4005).
You can change this by rebinding `scriptl:*scriptl-port*` before
running `(scriptl:start)`, and by setting `SCRIPTL_PORT` in the shell.

This talks *only* to localhost.  It's horribly insecure, though no
moreso than swank/slime, really.  If you're worried, or not the only
person with access to your host, *don't run it*.

In theory, making this talk over an ssh tunnel would be pretty easy,
but switching ports and doing the setup isn't at all nice.

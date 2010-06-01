#!/bin/sh

# $OSTYPE isn't exported by my Linux bash shell.
if [ "$OSTYPE" = "" ]; then
    OSTYPE=`uname`;
fi

if [ "$OSTYPE" = "Linux" -o "$OSTYPE" = "linux-gnu" ]; then
    if command -v sbcl 1>&2 > /dev/null; then  # Preferring SBCL on Linux.
        CL="sbcl";
    elif command -v lx86cl 1>&2 > /dev/null; then
        CL="lx86cl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
# elif OSX
#     ...
elif [ "$OSTYPE" = "Windows_NT" -o "$OSTYPE" = "msys" ]; then
    if command -v wx86cl 1>&2 > /dev/null; then  # Preferring CCL on Windows.
        CL="wx86cl";
    elif command -v sbcl 1>&2 > /dev/null; then
        CL="sbcl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
fi

if [ "$CL" = "sbcl" -o "$CL" = "lx86cl" -o "$CL" = "wx86cl" ]; then
    if [ -f cl-gtk2.core -a "$CL" = "sbcl" ]; then
        $CL --core cl-gtk2.core --no-userinit --load gtk-example.lisp \
            --eval "(run-app)";
    else
        $CL --load gtk-example.lisp --eval "(run-app)";
    fi
elif [ "$CL" = "clisp" ]; then
    $CL -ansi -repl -i gtk-example.lisp -x "(run-app)";
fi

exit 0;

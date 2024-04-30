# Install

- Requires SLIME.

Clone the repository to some ASDF-aware directory like `~/common-lisp`

After that, if you're already running lisp session
```
CL-USER> (asdf:clear-source-registry)
```

Then quickload, set the package name and run `(create-window)`
```
CL-USER> (ql:quickload :learngl)
CL-USER> (in-package :learngl)

LEARNGL> (create-window)
```

To kill the window, press the escape key with the window in focus or call `(kill-window)` from the REPL.

# Credit

https://gist.github.com/realark/d88866fa2fe2459f59e53620931ad155

This github gist by realark/Andrew Kent was immensely helpful in figuring out how to convert a Lisp array to a gl-compatible one, as well as learning how to specify CFFI types. The type-conversion loop and `gl:vertex-attrib-pointer` cffi lines were taken from it.

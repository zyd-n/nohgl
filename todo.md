## Adding Shaders still sucks

- A way of recompiling shaders as our main program is running
- A known directory (or many) to look for shader source location.

## Better error handling in the REPL

Currently we run the program in a single thread and the main thread used to run
CL code at the REPL. Any time there is an error from something we type at the
REPL, we effectively can't use the debugger as any option aside from "Continue"
will crash our program.

## Adding Shaders still sucks

Fine for now as a noob but inevitably we need
- A way of adding many shaders and their respective GL code
- A way of recompiling shaders as our main program is running
- More things that I'm probably not thinking of or aware of yet.

## Better error handling in the REPL

Currently we run the program in a single thread and the main thread used to run
CL code at the REPL. Any time there is an error from something we type at the
REPL, we effectively can't use the debugger as any option aside from "Continue"
will crash our program.

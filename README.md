# Anvil
A WIP compiler for the anvil language.

## Semantics of comptime

Any parameters to a function are considered run-time if not annotated with comptime.

Local parameters are by default "comptime", unless they at some are potentially assigned to a run-time variable.
Immutable variables are set to either comptime or runtime solely based on their initialization.
Mutable variables may initially be comptime, but later become run-time if assigned to another run-time variable/parameter.
To force comptime on a mutable variable, potentially allow comptime in front.



## Todo
### Standard
-while-loops
-for-loops

-dependencies

### Parser
-array indexing
-while loops
-ifexpr (different from ifstatement)

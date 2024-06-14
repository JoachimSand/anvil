```llvm
    ; Define a new struct with a list of fields
    <res> = struct def (<identifier : <opi>,)+;

    ; Specify a new block between instructions op1 and op2. 
    <res> = block(<op1>, <op2>);

    ; Declare a new function, it's return type and its entry basic block.
    <res> = fn <identifier>(...) -> <op1> at blk <op2>;
    
    ; Declare an argument to the current function.
    <res> = arg <identifier> of type <op>;

    ; Allocate enough space on the stack for type <op>.
    <res> = alloca <op>;
    ; From pointer <op1> load a value using the capability <op2>
    <res> = load <op1> with cap <op2>;
    <res> = store <op1> in <op2>;

    ; Get a field from the aggregrate pointed to by <op1>
    <res> = get_element_ptr <op1> (<fieldidentifier>,)+;
    <res> = type_of <op>;
    <res> = type_of_deref <op>;
    <res> = type <op1> as <op2>;
    <res> = int(N);
    br <op>;
    br_either <op1> then <op2> else <op3>; 
    <res> = lt <op1>, <op2>;   
    <res> = address of <op1> with cap <op2>;
    <res> = move <op>;

    ; Equivalent of @alloc and @free
    <res> = memalloc <op>;
    <res> = memfree <op>;
```

```llvm
<res> = constant_type(<type>);
<res> = constant_val(<type>, <val>);

<res> = blk <op1> to <op2>;
br <op>
br_either <op1> then <op2> else <op3>

<res> = fn def <identifier>(<opi>*) -> <type> at blk <op>;
<res> = arg(<type>, <identifier>);

<res> = alloca <type> 
<res> = load <type> from <op>
<res> = store <type> from <op1> to ptr at <op2>;
<res> = get_element_ptr <type>, from <op1> by (<fieldnum>,+);

; Enum related instructions
; Updates enum at pointer op1 to tag with given tagnum and value <op2>
<res> = update_enum_ptr_with_val <type> <op1>, tagnum <tagnum>, <op2>
<res> = update_enum_ptr_with_ptr <type> <op1>, tagnum <tagnum>, <op2>
; Match case, equivalent to switch
<res> = match on <op1> : (<tagnum> -> <opt>,)*;

;Takes an enum, returns pointer to the relevant data for the case with specified tagnum.
<res> = enum_project <type>, <op1>, tagnum <tagnum>; 

<res> = add <int_type> (<op1>, <op2>);
<res> = sub <int_type> (<op1>, <op2>);
<res> = move <op>;

<res> = memalloc <op>;
<res> = memfree <op>;
```



%0 = blk %1 to %19 {
    %1 = constant_type(struct{x : tir_u32, y : tir_u32, });
    %2 = fn def test (%4, %5, ) -> tir_void at blk %3;
    %3 = blk %4 to %17 {
        %4 = arg(%tir.Type.IndexRef.tir_u32, x);
        %5 = arg(%tir.Type.IndexRef.tir_u32, y);
        %6 = constant_val(unknown_int, 1);
        %7 = alloca struct{x : tir_u32, y : tir_u32, }, returns type &.tir_stackref struct{x : tir_u32, y : tir_u32, };
        %8 = get_element_ptr struct{x : tir_u32, y : tir_u32, }, from tir.TirInst.IndexRef(7) by 0, ;
        %9 = store tir_u32 from tir.TirInst.IndexRef(4), to ptr at tir.TirInst.IndexRef(8);
        %10 = get_element_ptr struct{x : tir_u32, y : tir_u32, }, from tir.TirInst.IndexRef(7) by 0, ;
        %11 = store tir_u32 from tir.TirInst.IndexRef(5), to ptr at tir.TirInst.IndexRef(10);
        %12 = constant_val(unknown_int, 5);
        %13 = constant_val(bool, false);
        %14 = get_element_ptr struct{x : tir_u32, y : tir_u32, }, from tir.TirInst.IndexRef(7) by 0, ;
        %15 = constant_val(u32, 1);
        %16 = store tir_u32 from tir.TirInst.IndexRef(15), to ptr at tir.TirInst.IndexRef(14);
        %17 = br %18;
    };
    %18 = blk %19 to %18 {
    };
};

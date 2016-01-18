Simple native code Forth compiler.

- Inlining
- Register allocation
- Constant folding

Sample output:

    Compile: t: rot   >r swap r> swap ;
    LD r0 0(SP)
    LD r1 4(SP)
    LD r2 8(SP)
    ST r2 0(SP)
    ST r0 4(SP)
    ST r1 8(SP)
    RET

    Compile: t: -rot   rot rot ;
    LD r0 0(SP)
    LD r1 4(SP)
    LD r2 8(SP)
    ST r1 0(SP)
    ST r2 4(SP)
    ST r0 8(SP)
    RET

    Compile: t: +!   tuck @ + swap ! ;
    LD r0 0(SP)
    LD r1 4(SP)
    LD r2 (r0)
    ADD r1 r2 
    ST r1 (r0)
    ADD SP #8 
    RET

    Compile: t: test4   2 + 3 ;
    LD r0 0(SP)
    ADD r0 #2 
    ADD SP #-4 
    ST #3 0(SP)
    ST r0 4(SP)
    RET

    Compile: t: test5   2 test4 * ;
    ADD SP #-4 
    ST #12 0(SP)
    RET

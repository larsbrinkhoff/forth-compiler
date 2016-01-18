\ Native code optimising compiler.  Copyright 2016 Lars Brinkhoff.

require search.fth
include lib/common.fth

\ Configuration

4 constant t-cell  \ Target cell size.
1 constant #tos    \ Number of TOS registers across calls.

: t-cells   t-cell * ;

\ Target vocabulary

vocabulary t-words
: target   only t-words definitions ;
: host   only forth definitions ;

\ Constants

hex 80000000 constant const# decimal
: const?   const# and ;
: >const   const# or ;
: >num   const# invert and ;

\ Stack model

10 constant #items

: shift-stack ( a -- ) dup 1 cells + #items 1- cells cmove> ;

variable #s
create data-stack  #items cells allot
variable #r
create return-stack  #items cells allot

: 0stacks   0 #s !  0 #r ! ;

: item   cells + ;
: s-push ( x -- ) data-stack #s @ item !  1 #s +! ;
: r-push ( x -- ) return-stack #r @ item !  1 #r +! ;
: s-pick ( u -- x ) data-stack #s @ rot - 1- item @ ;
: r-pick ( u -- x ) return-stack #r @ rot - 1- item @ ;
: s-drop   -1 #s +! ;
: r-drop   -1 #r +! ;
: s-pop   0 s-pick s-drop ;
: r-pop   0 r-pick r-drop ;
: s-bottom ( x -- ) data-stack shift-stack 1 #s +!  data-stack ! ;

: s-used? ( u -- f ) 0 #s @ 0 ?do over data-stack i item @ = or loop nip ;
: r-used? ( u -- f ) 0 #r @ 0 ?do over return-stack i item @ = or loop nip ;

: .op   dup const? if ." #" >num . else ." r" . then ;
: .item   item @ .op ;
: .ds   ." S: " #s @ 0 ?do data-stack i .item loop ;
: .rs   ." R: " #r @ 0 ?do return-stack i .item loop ;

\ Faux assembler instructions.

: ld,   ." LD " .op ." (r" (.) ." )" cr ;
: lds,  ." LD " .op (.) ." (SP)" cr ;
: st,   ." ST " .op ." (r" (.) ." )" cr ;
: sts,  ." ST " .op (.) ." (SP)" cr ;
: mov,  ." MOV " .op .op cr ;
: add,  ." ADD " .op .op cr ;
: add-sp, ." ADD SP #" . cr ;
: sub,  ." SUB " .op .op cr ;
: neg,  ." NEG " .op cr ;
: mul,  ." MUL " .op .op cr ;
: or,   ." OR " .op .op cr ;
: xor,  ." XOR " .op .op cr ;
: and,  ." AND " .op .op cr ;
: not,  ." NOT " .op cr ;
: call, ." CALL ..." cr ;
: ret,  ." RET" cr ;

\ Registers

variable #regs
: 0regs   0 #regs ! ;
: +reg ( -- u ) #regs @  1 #regs +! ;
: used? ( u -- f ) dup s-used? swap r-used? or ;
: ?+reg ( u1 -- u1|u2 ) dup used? if drop +reg then ;
: ?2+reg ( u1 u2 -- u1 u2 | u2 u1 | u1|u2 u3 )
   \ Reuse unused registers u1 or u2 for output,
   \ or allocate a new register u3.
   dup used? if swap
      dup used? if +reg dup >r mov, r> then
   then ;

\ Initial state at entry point.

: ret-push   3735928559 >const r-push ;
: enter   ret-push  #tos 0 ?do +reg s-bottom loop ;

\ Primitives

0 value #prims
: +prim   1 #prims +! ;

: ?move   2dup <> if 2dup mov, then nip ;
: src->dst   s-pop dup ?+reg dup s-push ?move ;
: src->src+dst   s-pop dup ?+reg dup s-push ;
: src+src->src+dst   s-pop s-pop ?2+reg dup s-push ;

: 'fold   @ ;
: #rs   cell+ @ ;
: #ds   2 cells + @ ;
: exe   3 cells + >r ;

: operands   dup >r #ds 0 ?do s-pop >num loop r> ;
: fold   operands 'fold execute >const s-push ;

: t-compile,   , +prim ;
: primitive:   create , , , ] !csp  does> t-compile, ;

also t-words definitions previous
   \ Define primitives: ( u1 u2 xt )
   \ u1 is the number of data stack inputs.
   \ u2 is the number of return stack inputs.
   \ xt is the constant folding function, or 0 if no folding is to be done.
   1 0 0 primitive: dup   0 s-pick s-push ;
   1 0 0 primitive: drop   s-drop ;
   2 0 0 primitive: swap   s-pop s-pop swap s-push s-push ;

   1 0 0 primitive: >r   s-pop r-push ;
   0 1 0 primitive: r>   r-pop s-push ;

   0 0 0 primitive: call   ret-push ;
   0 1 0 primitive: exit   r-drop ;

   \ jump, branch, 0branch

   1 0 0 primitive: @   src->src+dst ld, ;
   2 0 0 primitive: !   s-pop s-pop st, ;

   2 0 ' + primitive: +   src+src->src+dst add, ;
   2 0 ' - primitive: -   src+src->src+dst sub, ;
   1 0 ' negate primitive: negate   src->dst neg, ;
   2 0 ' * primitive: *   src+src->src+dst mul, ;
   2 0 ' or primitive: or   src+src->src+dst or, ;
   2 0 ' xor primitive: xor   src+src->src+dst xor, ;
   2 0 ' and primitive: and   src+src->src+dst and, ;
   1 0 ' invert primitive: invert   src->dst not, ;

   1 0 ' 0= primitive: 0=   ;
   1 0 ' 0< primitive: 0<   ;

   2 0 ' = primitive: =   s-drop ;
   2 0 ' < primitive: <   s-drop ;
   2 0 ' u< primitive: u<   s-drop ;

   0 0 0 primitive: 2   2 >const s-push ;
   0 0 0 primitive: 3   3 >const s-push ;
only forth definitions

also t-words
: call   call ;
: ret   exit ;
previous

\ Compile definitions to intermediate code

: inline   @+ 0 ?do @+ t-compile, loop drop ;
: 0prims,   here to #prims 0 , ;
: .source   ." Compile: " source type cr ;
: t:   .source target create 0prims,  does> call inline ret ;

\ Process intermediate code and generate output code

: constants?   #ds -1 swap 0 ?do i s-pick const? and loop ;
: ?const ( a -- ) dup constants? if fold else exe then ;
: ?fold   dup 'fold if ?const else exe then ;

variable load#
: 0load   0 load# ! ;
: +load   load# @  1 load# +! ;
: .load   +load t-cells swap lds, ;
: tos>   #tos 0 ?do #s @ if s-pop else +load t-cells 0 lds, 0 then loop ;

: >tos   #tos 0 ?do dup 0 <> if 0 mov, else drop then loop ;
: .store   t-cells s-pop sts, ;
: ?store   #s @ 0 ?do i .store loop ;

: reg ( -- u ) +reg dup .load ;
: regs   0 ?do reg s-bottom loop ;
: ?load   #ds #s @ - dup 0> if regs else drop then ;
: prim   dup ?load ?fold ;
: ?add-sp   load# @ #s @ - ?dup if t-cells add-sp, then ;
: return   tos> ?add-sp ?store >tos ret, ;
: 0compiler   0stacks 0regs 0load  enter ;
: generate ( a -- ) 0compiler  @+ 0 ?do @+ prim loop drop  return ;

also t-words definitions
: ;   latestxt >body generate host ;
only forth definitions

\ Tests

t: rot   >r swap r> swap ;
t: -rot   rot rot ;

t: over   >r dup r> swap ;
t: 2dup   over over ;
t: nip   swap drop ;
t: 2drop   drop drop ;

\ t: 2>r   r> swap rot >r >r >r ;
\ t: 2r>   r> r> r> rot >r swap ;

\ t: <>   = invert ;
\ t: >   swap < ;
\ t: u>   swap u< ;

t: tuck   swap over ;
t: +!   tuck @ + swap ! ;

\ Test that dead registes are reused, and live registers are not clobbered.
t: test1   >r dup r> + ;
t: test2   dup -rot + ;
t: test3   2dup + ;

\ Test constant folding.
t: test4   2 + 3 ;
t: test5   2 test4 * ;

\ In the future, it would be nice if this were compiled to "-".
\ t: test6   negate + ;

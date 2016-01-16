\ Native code optimising compiler.

include lib/common.fth

4 constant t-cell
: t-cells   t-cell * ;

\ Stack

: items   cells ;

variable #s
create data-stack  10 items allot
variable #r
create return-stack  10 items allot

: 0stacks   0 #s !  0 #r ! ;

: item   items + ;
: s-push ( x -- ) data-stack #s @ item !  1 #s +! ;
: r-push ( x -- ) return-stack #r @ item !  1 #r +! ;
: s-pick ( u -- x ) data-stack #s @ rot - 1- item @ ;
: r-pick ( u -- x ) return-stack #r @ rot - 1- item @ ;
: s-drop   -1 #s +! ;
: r-drop   -1 #r +! ;
: s-pop   0 s-pick s-drop ;
: r-pop   0 r-pick r-drop ;

: s-used? ( u -- f ) 0 #s @ 0 ?do over data-stack i item @ = or loop nip ;
: r-used? ( u -- f ) 0 #s @ 0 ?do over return-stack i item @ = or loop nip ;

: .ds   ." S: " #s @ 0 ?do data-stack i item @ . loop ;
: .rs   ." R: " #r @ 0 ?do return-stack i item @ . loop ;

\ Registers

variable #regs
: 0regs   0 #regs ! ;
: +reg ( -- u ) #regs @  1 #regs +! ;
: used? ( u -- f ) dup s-used? swap r-used? or ;
: ?+reg ( u1 -- u1|u2 ) dup used? if drop +reg then ;

\ Faux assembler instructions.

: ld,   ." LD r" . ." (r" (.) ." )" cr ;
: st,   ." ST r" . ." (r" (.) ." )" cr ;
: lds,  ." LD r" . (.) ." (SP)" cr ;
: sts,  ." ST r" . (.) ." (SP)" cr ;
: mov,  ." MOV r" . ." r" . cr ;
: add,  ." ADD r" . ." r" . cr ;
: add-sp, ." ADD SP #" . cr ;
: sub,  ." SUB r" . ." r" . cr ;
: neg,  ." NEG r" . cr ;
: or,   ." OR r" . ." r" . cr ;
: xor,  ." XOR r" . ." r" . cr ;
: and,  ." AND r" . ." r" . cr ;
: not,  ." NOT r" . cr ;
: call, ." CALL ..." cr ;
: ret,  ." RET" cr ;

\ Primitives

0 value #prims
: +prim   1 #prims +! ;

: t-compile,   , +prim ;
: primitive:   create , , dup , 1+ ] !csp  does> t-compile, ;

0
   1 0 primitive: %dup   0 s-pick s-push ;
   1 0 primitive: %drop   s-drop ;
   2 0 primitive: %swap   s-pop s-pop swap s-push s-push ;
   2 0 primitive: %over   1 s-pick s-push ;

   1 0 primitive: %>r   s-pop r-push ;
   0 1 primitive: %r>   r-pop s-push ;
   0 1 primitive: %r@   0 r-pick s-push ;

   \ call, return, jump ;
   \ branch, 0branch ;

   1 0 primitive: %@   s-pop dup ?+reg dup s-push ld, ;
   2 0 primitive: %!   s-pop s-pop st, ;

   2 0 primitive: %+   1 s-pick s-pop swap add, ;
   2 0 primitive: %-   1 s-pick s-pop swap sub, ;
   1 0 primitive: %negate   0 s-pick neg, ;
   2 0 primitive: %or   1 s-pick s-pop swap or, ;
   2 0 primitive: %xor   1 s-pick s-pop swap xor, ;
   2 0 primitive: %and   1 s-pick s-pop swap and, ;
   1 0 primitive: %invert   0 s-pick not, ;

   1 0 primitive: %0=   ;
   1 0 primitive: %0<   ;

   2 0 primitive: %=   s-drop ;
   2 0 primitive: %<   s-drop ;
   2 0 primitive: %u<   s-drop ;
drop

\ Compile definitions to intermediate code

: inline   @+ 0 ?do @+ t-compile, loop drop ;
: t:   ." Compile: " source type cr  create here to #prims 0 ,  does> inline ;
: t;   ;

\ Process intermediate code and generate output code

variable load#
: 0load   0 load# ! ;
: +load   load# @  1 load# +! ;
: .load   +load t-cells swap lds, ;

: .store   t-cells s-pop sts, ;
: ?store   #s @ 0 ?do i .store loop ;

: reg ( -- u ) +reg dup .load ;
: regs   ?dup if reg swap 1- recurse s-push then ;
: #ds   cell+ @ ;
: ?load   #ds #s @ - dup 0> if regs else drop then ;
: exe   3 cells + >r ;
: prim   dup ?load exe ;
: ?add-sp   ?dup if t-cells add-sp, then ;
: return   load# @ #s @ - ?add-sp  ?store  ret, ;
: generate ( xt -- ) 0stacks 0regs 0load  @+ 0 ?do @+ prim loop drop return ;

: t;   latestxt >body generate ;

\ Tests

t: %rot   %>r %swap %r> %swap t;
t: %-rot   %rot %rot t;

t: %2dup   %over %over t;
t: %nip   %swap %drop t;
t: %2drop   %drop %drop t;

\ t: %2>r   %r> %swap %rot %>r %>r %>r t;
\ t: %2r>   %r> %r> %r> %rot %>r %swap t;

\ t: %<>   %= %invert t;
\ t: %>   %swap %< t;
\ t: %u>   %swap %u< t;

t: %tuck   %swap %over t;
t: %+!   %tuck %@ %+ %swap %! t;

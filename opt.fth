\ Native code optimising compiler.

include lib/common.fth

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

: .ds   ." S: " #s @ 0 ?do data-stack i item @ . loop ;
: .rs   ." R: " #r @ 0 ?do return-stack i item @ . loop ;

\ Registers

variable #regs
: 0regs   0 #regs ! ;
: +reg ( -- u ) #regs @  1 #regs +! ;

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

   2 0 primitive: %+   ." ADD r" 1 s-pick . ." r" s-pop . cr ;
   2 0 primitive: %-   ." SUB r" 1 s-pick . ." r" s-pop . cr ;
   1 0 primitive: %negate   ." NEG r" 0 s-pick . cr ;
   2 0 primitive: %or   ." OR r" 1 s-pick . ." r" s-pop . cr ;
   2 0 primitive: %xor   ." XOR r" 1 s-pick . ." r" s-pop . cr ;
   2 0 primitive: %and   ." AND r" 1 s-pick . ." r" s-pop . cr ;
   1 0 primitive: %invert   ." NOT r" 0 s-pick . cr ;

   1 0 primitive: %0=   ;
   1 0 primitive: %0<   ;

   2 0 primitive: %=   s-drop ;
   2 0 primitive: %<   s-drop ;
   2 0 primitive: %u<   s-drop ;
drop

\ Compile definitions to intermediate code

: inline   @+ 0 ?do @+ t-compile, loop drop ;
: t:   create here to #prims 0 ,  does> inline ;
: t;   ;

t: %rot   %>r %swap %r> %swap t;
t: %-rot   %rot %rot t;
t: %2dup   %over %over t;
t: %nip   %swap %drop t;
t: %2drop   %drop %drop t;

t: %2>r   %r> %swap %rot %>r %>r %>r t;
t: %2r>   %r> %r> %r> %rot %>r %swap t;

t: %<>   %= %invert t;
t: %>   %swap %< t;
t: %u>   %swap %u< t;

t: %add   %+ t;

\ Process intermediate code and generate output code

variable load#
: 0load   0 load# ! ;
: +load   load# @  1 load# +! ;
: -load   -1 load# +!  load# @ ;
: .load   ." MOVE r" . ." stack[" +load (.) ." ]" cr ;

: .store   ." MOVE stack[" -load (.) ." ] r" . cr ;
: ?store   begin #s @ while s-pop .store repeat  ;

: reg ( -- u ) +reg dup .load ;
: regs   0 ?do reg s-push loop ;
: #ds   cell+ @ ;
: ?load   #ds #s @ - dup 0> if regs else drop then ;
: exe   3 cells + >r ;
: prim   dup ?load exe ;
: ?add-sp   ?dup if ." ADD SP #" . cr then ;
: return   load# @ #s @ -  ?store  ?add-sp  ." RETURN" cr ;
: process ( xt -- ) 0stacks 0regs 0load >body @+ 0 ?do @+ prim loop drop return ;

\ Tests

cr .( Compile ROT: ) cr
' %rot process

cr .( Compile -ROT: ) cr
' %-rot process

cr .( Compile ADD ) cr
' %add process

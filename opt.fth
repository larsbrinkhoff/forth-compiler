\ Native code optimising compiler.

require search.fth
include lib/common.fth

4 constant t-cell
: t-cells   t-cell * ;

vocabulary t-words
: target   only t-words definitions ;
: host   only forth definitions ;

\ Stack

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

: .ds   ." S: " #s @ 0 ?do data-stack i item @ . loop ;
: .rs   ." R: " #r @ 0 ?do return-stack i item @ . loop ;

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

\ Registers

variable #regs
: 0regs   0 #regs ! ;
: +reg ( -- u ) #regs @  1 #regs +! ;
: used? ( u -- f ) dup s-used? swap r-used? or ;
: ?+reg ( u1 -- u1|u2 ) dup used? if drop +reg then ;
: ?2+reg ( u1 u2 -- u1 u2 | u2 u1 | u1|u2 u3 )
   dup used? if swap
      dup used? if +reg dup >r mov, r> then
   then ;

\ Primitives

0 value #prims
: +prim   1 #prims +! ;

: ?move   2dup <> if 2dup mov, then nip ;
: src->dst   s-pop dup ?+reg dup s-push ?move ;
: src->src+dst   s-pop dup ?+reg dup s-push ;
: src+src->src+dst   s-pop s-pop ?2+reg dup s-push ;

: t-compile,   , +prim ;
: primitive:   create , , dup , 1+ ] !csp  does> t-compile, ;

0 also t-words definitions previous
   1 0 primitive: dup   0 s-pick s-push ;
   1 0 primitive: drop   s-drop ;
   2 0 primitive: swap   s-pop s-pop swap s-push s-push ;
   2 0 primitive: over   1 s-pick s-push ;

   1 0 primitive: >r   s-pop r-push ;
   0 1 primitive: r>   r-pop s-push ;
   0 1 primitive: r@   0 r-pick s-push ;

   \ call, return, jump ;
   \ branch, 0branch ;

   1 0 primitive: @   src->src+dst ld, ;
   2 0 primitive: !   s-pop s-pop st, ;

   2 0 primitive: +   src+src->src+dst add, ;
   2 0 primitive: -   src+src->src+dst sub, ;
   1 0 primitive: negate   src->dst neg, ;
   2 0 primitive: or   src+src->src+dst or, ;
   2 0 primitive: xor   src+src->src+dst xor, ;
   2 0 primitive: and   src+src->src+dst and, ;
   1 0 primitive: invert   src->dst not, ;

   1 0 primitive: 0=   ;
   1 0 primitive: 0<   ;

   2 0 primitive: =   s-drop ;
   2 0 primitive: <   s-drop ;
   2 0 primitive: u<   s-drop ;
drop only forth definitions

\ Compile definitions to intermediate code

: inline   @+ 0 ?do @+ t-compile, loop drop ;
: 0prims,   here to #prims 0 , ;
: .source   ." Compile: " source type cr ;
: t:   .source target create 0prims,  does> inline ;

\ Process intermediate code and generate output code

variable load#
: 0load   0 load# ! ;
: +load   load# @  1 load# +! ;
: .load   +load t-cells swap lds, ;

: .store   t-cells s-pop sts, ;
: ?store   #s @ 0 ?do i .store loop ;

: reg ( -- u ) +reg dup .load ;
: regs   0 ?do reg s-bottom loop ;
: #ds   cell+ @ ;
: ?load   #ds #s @ - dup 0> if regs else drop then ;
: exe   3 cells + >r ;
: prim   dup ?load exe ;
: ?add-sp   ?dup if t-cells add-sp, then ;
: return   load# @ #s @ - ?add-sp  ?store  ret, ;
: 0compiler   0stacks 0regs 0load ;
: generate ( a -- ) 0compiler  @+ 0 ?do @+ prim loop drop return ;

also t-words definitions
: ;   latestxt >body generate host ;
only forth definitions

\ Tests

t: rot   >r swap r> swap ;
t: -rot   rot rot ;

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

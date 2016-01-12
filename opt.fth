\ Native code optimising compiler.

: primitive:   create dup , >r , , , , r> ;

0
   1 2 0 0 primitive: dup
   1 0 0 0 primitive: drop		
   2 2 0 0 primitive: swap		
   2 3 0 0 primitive: over		

   1 0 0 1 primitive: >r
   0 1 1 0 primitive: r>
   0 1 1 1 primitive: r@

   \ call, return, jump
   \ branch, 0branch

   2 1 0 0 primitive: +
   2 1 0 0 primitive: -		
   1 1 0 0 primitive: negate	
   2 1 0 0 primitive: or		
   2 1 0 0 primitive: xor		
   2 1 0 0 primitive: and		
   1 1 0 0 primitive: invert	

   1 1 0 0 primitive: 0=		
   1 1 0 0 primitive: 0<		

   2 1 0 0 primitive: =		
   2 1 0 0 primitive: <		
   2 1 0 0 primitive: u<		
drop

: t:   postpone \ ;

t: rot    >r swap r> swap ;
t: 2dup   over over ;
t: nip    swap drop ;

t: 2>r   r> swap rot >r >r >r ;
t: 2r>   r> r> r> rot >r swap ;

t: <>   = invert ;
t: >   swap < ;
t: u>   swap u< ;

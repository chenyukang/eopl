/* Generated from 2.1.scm by the CHICKEN compiler
   http://www.call-cc.org
   2013-08-29 17:02
   Version 4.8.0.4 (stability/4.8.0) (rev 578619b)
   macosx-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2013-07-15 on aeryn.xorinia.dim (Darwin)
   command line: 2.1.scm -output-file 2.12.c -optimize-level 3
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[22];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,9),40,115,117,99,99,32,110,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,9),40,112,114,101,100,32,110,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,10),40,109,97,107,101,32,110,49,50,41,0,0,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,14),40,112,108,117,115,32,97,49,52,32,98,49,53,41,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,11),40,98,111,110,101,63,32,110,49,55,41,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,14),40,109,117,108,116,32,97,49,57,32,98,50,48,41,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,10),40,102,97,99,116,32,110,50,53,41,0,0,0,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,17),40,102,97,99,116,45,110,117,109,98,101,114,32,110,51,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_335)
static void C_ccall f_335(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_493)
static void C_ccall f_493(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_490)
static void C_ccall f_490(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_419)
static void C_ccall f_419(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_417)
static void C_ccall f_417(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_414)
static void C_ccall f_414(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_241)
static void C_ccall f_241(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_363)
static void C_ccall f_363(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_368)
static void C_ccall f_368(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_395)
static void C_ccall f_395(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_393)
static void C_ccall f_393(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_232)
static void C_ccall f_232(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_441)
static void C_ccall f_441(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_236)
static void C_ccall f_236(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_443)
static void C_ccall f_443(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_234)
static void C_ccall f_234(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_314)
static void C_ccall f_314(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f515)
static void C_ccall f515(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_262)
static void C_ccall f_262(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_438)
static void C_ccall f_438(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_462)
static void C_ccall f_462(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_467)
static void C_ccall f_467(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_469)
static void C_ccall f_469(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f520)
static void C_ccall f520(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_487)
static void C_ccall f_487(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_481)
static void C_ccall f_481(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_484)
static void C_ccall f_484(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_274)
static void C_ccall f_274(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_475)
static void C_ccall f_475(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_478)
static void C_ccall f_478(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_471)
static void C_ccall f_471(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_473)
static void C_ccall f_473(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_354)
static void C_ccall f_354(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_386)
static void C_ccall f_386(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_384)
static void C_ccall f_384(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_381)
static void C_ccall f_381(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

/* make in k235 in k233 in k231 */
static void C_ccall f_335(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
loop:
a=C_alloc(7);
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)tr3,(void*)f_335,3,t0,t1,t2);}
if(C_truep(C_i_zerop(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
if(C_truep(C_i_oddp(t2))){
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_354,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=C_a_i_minus(&a,2,t2,C_fix(1));
C_trace("2.1.scm:34: make");
t8=t3;
t9=t4;
t1=t8;
t2=t9;
c=3;
goto loop;}
else{
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_363,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=C_a_i_divide(&a,2,t2,C_fix(2));
C_trace("2.1.scm:35: make");
t8=t3;
t9=t4;
t1=t8;
t2=t9;
c=3;
goto loop;}}}

/* k492 in k235 in k233 in k231 */
static void C_ccall f_493(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:72: equal??");
((C_proc4)C_fast_retrieve_symbol_proc(lf[17]))(4,*((C_word*)lf[17]+1),((C_word*)t0)[2],t1,lf[19]);}

/* k489 in k466 in k235 in k233 in k231 */
static void C_ccall f_490(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:73: equal??");
((C_proc4)C_fast_retrieve_symbol_proc(lf[17]))(4,*((C_word*)lf[17]+1),((C_word*)t0)[2],t1,lf[18]);}

/* fact in k235 in k233 in k231 */
static void C_ccall f_419(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_419,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
t3=t2;
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f520,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("2.1.scm:46: pred");
t5=C_fast_retrieve(lf[6]);
f_274(3,t5,t4,t3);}}

/* k416 */
static void C_ccall f_417(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:54: mult");
t2=C_fast_retrieve(lf[9]);
f_395(4,t2,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k413 */
static void C_ccall f_414(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:54: plus");
t2=C_fast_retrieve(lf[11]);
f_368(4,t2,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* succ in k235 in k233 in k231 */
static void C_ccall f_241(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
loop:
a=C_alloc(7);
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)tr3,(void*)f_241,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,lf[5]);}
else{
t3=C_i_car(t2);
t4=C_a_i_plus(&a,2,t3,C_fix(1));
if(C_truep(C_i_nequalp(t4,C_fast_retrieve(lf[0])))){
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_262,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t6=t2;
t7=C_u_i_cdr(t6);
C_trace("2.1.scm:13: succ");
t11=t5;
t12=t7;
t1=t11;
t2=t12;
c=3;
goto loop;}
else{
t5=t2;
t6=C_u_i_cdr(t5);
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_a_i_cons(&a,2,t4,t6));}}}

/* k362 in make in k235 in k233 in k231 */
static void C_ccall f_363(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:35: mult");
t2=C_fast_retrieve(lf[9]);
f_395(4,t2,((C_word*)t0)[2],t1,lf[10]);}

/* plus in k235 in k233 in k231 */
static void C_ccall f_368(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_368,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t3))){
t4=t2;
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t4);}
else{
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_381,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("2.1.scm:41: succ");
t5=C_fast_retrieve(lf[4]);
f_241(3,t5,t4,t2);}}

/* mult in k235 in k233 in k231 */
static void C_ccall f_395(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_395,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_END_OF_LIST);}
else{
t4=t3;
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f515,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace("2.1.scm:46: pred");
t6=C_fast_retrieve(lf[6]);
f_274(3,t6,t5,t4);}}

/* k392 in bone? in k235 in k233 in k231 */
static void C_ccall f_393(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_i_nullp(t1));}

/* k231 */
static void C_ccall f_232(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_232,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_234,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k440 */
static void C_ccall f_441(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:62: fact");
t2=C_fast_retrieve(lf[13]);
f_419(3,t2,((C_word*)t0)[2],t1);}

/* k235 in k233 in k231 */
static void C_ccall f_236(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word ab[30],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_236,2,t0,t1);}
t2=C_set_block_item(lf[0] /* base */,0,C_fix(16));
t3=C_set_block_item(lf[1] /* bzero */,0,C_SCHEME_END_OF_LIST);
t4=C_mutate((C_word*)lf[2]+1 /* (set! bzero? ...) */,*((C_word*)lf[3]+1));
t5=C_mutate((C_word*)lf[4]+1 /* (set! succ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_241,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[6]+1 /* (set! pred ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_274,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[8]+1 /* (set! make ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_335,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[11]+1 /* (set! plus ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_368,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[12]+1 /* (set! bone? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_386,a[2]=((C_word)li4),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[9]+1 /* (set! mult ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_395,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[13]+1 /* (set! fact ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_419,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[15]+1 /* (set! fact-number ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_443,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t13=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_467,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t14=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_493,a[2]=t13,tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:72: make");
t15=C_fast_retrieve(lf[8]);
f_335(3,t15,t14,C_fix(10));}

/* fact-number in k235 in k233 in k231 */
static void C_ccall f_443(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word *a;
loop:
a=C_alloc(8);
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)tr3,(void*)f_443,3,t0,t1,t2);}
if(C_truep(C_i_zerop(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}
else{
if(C_truep(C_i_nequalp(t2,C_fix(1)))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(1));}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_462,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_a_i_minus(&a,2,t2,C_fix(1));
C_trace("2.1.scm:70: fact-number");
t6=t3;
t7=t4;
t1=t6;
t2=t7;
c=3;
goto loop;}}}

/* k233 in k231 */
static void C_ccall f_234(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_234,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_236,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:1: load");
t3=C_fast_retrieve(lf[20]);
((C_proc3)(void*)(*((C_word*)t3+1)))(3,t3,t2,lf[21]);}

/* k313 in pred in k235 in k233 in k231 */
static void C_ccall f_314(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_314,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* f515 in mult in k235 in k233 in k231 */
static void C_ccall f515(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f515,2,t0,t1);}
if(C_truep(C_i_nullp(t1))){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[3]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_414,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_417,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("2.1.scm:54: pred");
t4=C_fast_retrieve(lf[6]);
f_274(3,t4,t3,((C_word*)t0)[4]);}}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(171)){
C_save(t1);
C_rereclaim2(171*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,22);
lf[0]=C_h_intern(&lf[0],4,"base");
lf[1]=C_h_intern(&lf[1],5,"bzero");
lf[2]=C_h_intern(&lf[2],6,"bzero\077");
lf[3]=C_h_intern(&lf[3],5,"null\077");
lf[4]=C_h_intern(&lf[4],4,"succ");
lf[5]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\001\376\377\016");
lf[6]=C_h_intern(&lf[6],4,"pred");
lf[7]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\001\376\377\016");
lf[8]=C_h_intern(&lf[8],4,"make");
lf[9]=C_h_intern(&lf[9],4,"mult");
lf[10]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\002\376\377\016");
lf[11]=C_h_intern(&lf[11],4,"plus");
lf[12]=C_h_intern(&lf[12],5,"bone\077");
lf[13]=C_h_intern(&lf[13],4,"fact");
lf[14]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\001\376\377\016");
lf[15]=C_h_intern(&lf[15],11,"fact-number");
lf[16]=C_h_intern(&lf[16],25,"\003sysimplicit-exit-handler");
lf[17]=C_h_intern(&lf[17],7,"equal\077\077");
lf[18]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\000\376\003\000\000\002\376\377\001\000\000\000\001\376\377\016");
lf[19]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\012\376\377\016");
lf[20]=C_h_intern(&lf[20],4,"load");
lf[21]=C_decode_literal(C_heaptop,"\376B\000\000,/Users/kang/code/eopl/libs/scheme48-init.scm");
C_register_lf2(lf,22,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_232,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k261 in succ in k235 in k233 in k231 */
static void C_ccall f_262(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_262,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,C_fix(0),t1));}

/* k437 */
static void C_ccall f_438(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:62: mult");
t2=C_fast_retrieve(lf[9]);
f_395(4,t2,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k461 in fact-number in k235 in k233 in k231 */
static void C_ccall f_462(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_462,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_times(&a,2,((C_word*)t0)[3],t1));}

/* k466 in k235 in k233 in k231 */
static void C_ccall f_467(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_467,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_469,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_490,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:73: make");
t4=C_fast_retrieve(lf[8]);
f_335(3,t4,t3,C_fix(16));}

/* k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_469(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_469,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_471,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:74: make");
t3=C_fast_retrieve(lf[8]);
f_335(3,t3,t2,C_fix(5040));}

/* f520 in fact in k235 in k233 in k231 */
static void C_ccall f520(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f520,2,t0,t1);}
if(C_truep(C_i_nullp(t1))){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[14]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_438,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_441,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:62: pred");
t4=C_fast_retrieve(lf[6]);
f_274(3,t4,t3,((C_word*)t0)[3]);}}

/* k486 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_487(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:75: fact");
t2=C_fast_retrieve(lf[13]);
f_419(3,t2,((C_word*)t0)[2],t1);}

/* k480 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_481(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_481,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_484,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("2.1.scm:75: make");
t3=C_fast_retrieve(lf[8]);
f_335(3,t3,t2,C_fix(5040));}

/* k483 in k480 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_484(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:75: equal??");
((C_proc4)C_fast_retrieve_symbol_proc(lf[17]))(4,*((C_word*)lf[17]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* pred in k235 in k233 in k231 */
static void C_ccall f_274(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word *a;
loop:
a=C_alloc(8);
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)tr3,(void*)f_274,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}
else{
t3=C_i_car(t2);
if(C_truep(C_i_greater_or_equalp(t3,C_fast_retrieve(lf[0])))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_FALSE);}
else{
if(C_truep(C_i_equalp(t2,lf[7]))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_END_OF_LIST);}
else{
t4=t2;
t5=C_u_i_car(t4);
if(C_truep(C_i_zerop(t5))){
t6=t2;
t7=C_u_i_cdr(t6);
if(C_truep(C_i_nullp(t7))){
t8=t1;
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,C_SCHEME_FALSE);}
else{
t8=C_a_i_minus(&a,2,C_fast_retrieve(lf[0]),C_fix(1));
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_314,a[2]=t1,a[3]=t8,tmp=(C_word)a,a+=4,tmp);
t10=t2;
t11=C_u_i_cdr(t10);
C_trace("2.1.scm:25: pred");
t18=t9;
t19=t11;
t1=t18;
t2=t19;
c=3;
goto loop;}}
else{
t6=t2;
t7=C_u_i_car(t6);
t8=C_a_i_minus(&a,2,t7,C_fix(1));
t9=t2;
t10=C_u_i_cdr(t9);
t11=t1;
((C_proc2)(void*)(*((C_word*)t11+1)))(2,t11,C_a_i_cons(&a,2,t8,t10));}}}}}

/* k474 in k472 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_475(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* k476 in k472 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_478(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_471(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_471,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_473,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_481,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_487,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:75: make");
t5=C_fast_retrieve(lf[8]);
f_335(3,t5,t4,C_fix(7));}

/* k472 in k470 in k468 in k466 in k235 in k233 in k231 */
static void C_ccall f_473(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_473,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_475,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_478,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[16]))(2,*((C_word*)lf[16]+1),t3);}

/* k353 in make in k235 in k233 in k231 */
static void C_ccall f_354(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:34: succ");
t2=C_fast_retrieve(lf[4]);
f_241(3,t2,((C_word*)t0)[2],t1);}

/* bone? in k235 in k233 in k231 */
static void C_ccall f_386(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_386,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_393,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("2.1.scm:46: pred");
t4=C_fast_retrieve(lf[6]);
f_274(3,t4,t3,t2);}

/* k383 in k380 in plus in k235 in k233 in k231 */
static void C_ccall f_384(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.1.scm:41: plus");
t2=C_fast_retrieve(lf[11]);
f_368(4,t2,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k380 in plus in k235 in k233 in k231 */
static void C_ccall f_381(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_381,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_384,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("2.1.scm:41: pred");
t3=C_fast_retrieve(lf[6]);
f_274(3,t3,t2,((C_word*)t0)[3]);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[38] = {
{"f_335:_32_2e1_2escm",(void*)f_335},
{"f_493:_32_2e1_2escm",(void*)f_493},
{"f_490:_32_2e1_2escm",(void*)f_490},
{"f_419:_32_2e1_2escm",(void*)f_419},
{"f_417:_32_2e1_2escm",(void*)f_417},
{"f_414:_32_2e1_2escm",(void*)f_414},
{"f_241:_32_2e1_2escm",(void*)f_241},
{"f_363:_32_2e1_2escm",(void*)f_363},
{"f_368:_32_2e1_2escm",(void*)f_368},
{"f_395:_32_2e1_2escm",(void*)f_395},
{"f_393:_32_2e1_2escm",(void*)f_393},
{"f_232:_32_2e1_2escm",(void*)f_232},
{"f_441:_32_2e1_2escm",(void*)f_441},
{"f_236:_32_2e1_2escm",(void*)f_236},
{"f_443:_32_2e1_2escm",(void*)f_443},
{"f_234:_32_2e1_2escm",(void*)f_234},
{"f_314:_32_2e1_2escm",(void*)f_314},
{"f515:_32_2e1_2escm",(void*)f515},
{"toplevel:_32_2e1_2escm",(void*)C_toplevel},
{"f_262:_32_2e1_2escm",(void*)f_262},
{"f_438:_32_2e1_2escm",(void*)f_438},
{"f_462:_32_2e1_2escm",(void*)f_462},
{"f_467:_32_2e1_2escm",(void*)f_467},
{"f_469:_32_2e1_2escm",(void*)f_469},
{"f520:_32_2e1_2escm",(void*)f520},
{"f_487:_32_2e1_2escm",(void*)f_487},
{"f_481:_32_2e1_2escm",(void*)f_481},
{"f_484:_32_2e1_2escm",(void*)f_484},
{"f_274:_32_2e1_2escm",(void*)f_274},
{"f_475:_32_2e1_2escm",(void*)f_475},
{"f_478:_32_2e1_2escm",(void*)f_478},
{"f_471:_32_2e1_2escm",(void*)f_471},
{"f_473:_32_2e1_2escm",(void*)f_473},
{"f_354:_32_2e1_2escm",(void*)f_354},
{"f_386:_32_2e1_2escm",(void*)f_386},
{"f_384:_32_2e1_2escm",(void*)f_384},
{"f_381:_32_2e1_2escm",(void*)f_381},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|specializations:
o|  2 (car pair)
o|  5 (cdr pair)
o|inlining procedure: k244 
o|inlining procedure: k244 
o|inlining procedure: k277 
o|inlining procedure: k277 
o|inlining procedure: k288 
o|inlining procedure: k288 
o|inlining procedure: k300 
o|inlining procedure: k300 
o|inlining procedure: k338 
o|inlining procedure: k338 
o|inlining procedure: k371 
o|inlining procedure: k371 
o|inlining procedure: k398 
o|inlining procedure: k398 
o|inlining procedure: k422 
o|inlining procedure: k422 
o|inlining procedure: k446 
o|inlining procedure: k446 
o|replaced variables: 29 
o|removed binding forms: 11 
o|substituted constant variable: f_243494 
o|substituted constant variable: f_276496 
o|substituted constant variable: r289498 
o|substituted constant variable: r301500 
o|substituted constant variable: f_337502 
o|substituted constant variable: f_397506 
o|substituted constant variable: f_421508 
o|substituted constant variable: f_445510 
o|removed binding forms: 27 
o|inlining procedure: "(2.1.scm:52) bone?" 
o|inlining procedure: "(2.1.scm:60) bone?" 
o|removed binding forms: 8 
o|replaced variables: 2 
o|removed binding forms: 2 
o|simplifications: ((##core#call . 30)) 
o|  call simplifications:
o|    *
o|    odd?
o|    /
o|    >=
o|    equal?
o|    zero?	3
o|    -	4
o|    null?	9
o|    car	2
o|    +
o|    =	2
o|    cons	4
o|contracted procedure: k246 
o|contracted procedure: k271 
o|contracted procedure: k249 
o|contracted procedure: k254 
o|contracted procedure: k279 
o|contracted procedure: k332 
o|contracted procedure: k285 
o|contracted procedure: k291 
o|contracted procedure: k297 
o|contracted procedure: k303 
o|contracted procedure: k310 
o|contracted procedure: k323 
o|contracted procedure: k340 
o|contracted procedure: k346 
o|contracted procedure: k356 
o|contracted procedure: k365 
o|contracted procedure: k373 
o|contracted procedure: k400 
o|contracted procedure: k406 
o|contracted procedure: k424 
o|contracted procedure: k430 
o|contracted procedure: k448 
o|contracted procedure: k454 
o|contracted procedure: k464 
o|simplifications: ((let . 16)) 
o|removed binding forms: 24 
o|calls to known targets: 25 
o|identified direct recursive calls: f_241 1 
o|identified direct recursive calls: f_274 1 
o|identified direct recursive calls: f_335 2 
o|identified direct recursive calls: f_443 1 
*/
/* end of file */

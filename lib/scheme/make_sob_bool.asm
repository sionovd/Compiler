/* scheme/make_sob_bool.asm
 * Takes 0 or 1 as an argument, and places in R0 either #f or #t
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_BOOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_EQ(MAKE_FALSE_SOB_BOOL);
  MOV(R0, IMM(SOB_TRUE));
  JUMP(LEAVE_MAKE_SOB_BOOL);
  MAKE_FALSE_SOB_BOOL:
  MOV(R0, IMM(SOB_FALSE));
  LEAVE_MAKE_SOB_BOOL:
  POP(FP);
  RETURN;

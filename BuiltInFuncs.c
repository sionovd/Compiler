LcarBody:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R0,INDD(R1, 1));
  POP(FP);
  RETURN;
LcdrBody:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R0,INDD(R1, 2));
  POP(FP);
  RETURN;
LconsBody:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(FPARG(3));
  PUSH(FPARG(2));
  CALL(MAKE_SOB_PAIR);
  DROP(2);
  POP(FP);
  RETURN;
GCD:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  PUSH(R5);
  PUSH(R6);
  PUSH(R7);
  PUSH(R8);
  PUSH(R9);
  PUSH(R10);
  MOV(R1, FPARG(0));
  MOV(R2, FPARG(1));
  GCD_LOOP:
  MOV(R3, R1);
  REM(R3, R2);
  CMP(R3, IMM(0));
  JUMP_EQ(GCD_EXIT);
  MOV(R1, R2);
  MOV(R2, R3);
  JUMP(GCD_LOOP);
  GCD_EXIT:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(R10);
  POP(R9);
  POP(R8);
  POP(R7);
  POP(R6);
  POP(R5);
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
LplusBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  PUSH(R5);
  PUSH(R6);
  PUSH(R7);
  PUSH(R8);
  PUSH(R9);
  PUSH(R10);
  MOV(R1,FPARG(1));
  MOV(R2,IMM(0));                   
  MOV(R3,IMM(1));
  INCR(R1);
  PlusLoop:
  CMP(IMM(1),R1);
  JUMP_EQ(PlusEnd);
  MOV(R4,FPARG(R1));                 
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFraction);
  MOV(R5, INDD(R4, 1));
  MOV(R6, INDD(R4, 2));
  JUMP(AddNums);
  ConvertToFraction:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  AddNums:
  MUL(R2, R6);
  MUL(R5, R3);
  MUL(R3, R6);
  ADD(R2,R5);
  DECR(R1);
  JUMP(PlusLoop);
  PlusEnd:
  PUSH(R3);
  PUSH(R2);
  CALL(GCD);
  POP(R2);
  POP(R3);
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(ABS);
  DROP(1);
  DIV(R2, R0);
  DIV(R3, R0);
  CMP(R3, IMM(1));
  JUMP_NE(MakeFrac);
  MakeInt:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LeavePlus);
  MakeFrac:
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  LeavePlus:
  POP(R10);
  POP(R9);
  POP(R8);
  POP(R7);
  POP(R6);
  POP(R5);
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
LmulBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(1));
  MOV(R2,IMM(1));
  MOV(R3,IMM(1));
  INCR(R1);
  MulLoop:
  CMP(IMM(1),R1);
  JUMP_EQ(MulEnd);
  MOV(R4,FPARG(R1));                 
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionMul);
  MOV(R5, INDD(R4, 1));
  MOV(R6, INDD(R4, 2));
  JUMP(MulNums);
  ConvertToFractionMul:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  MulNums:
  MUL(R2,R5);
  MUL(R3, R6);
  DECR(R1);
  JUMP(MulLoop);
  MulEnd:
  PUSH(R3);
  PUSH(R2);
  CALL(GCD);
  POP(R2);
  POP(R3);
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(ABS);
  DROP(1);
  DIV(R2, R0);
  DIV(R3, R0);
  CMP(R3, IMM(1));
  JUMP_NE(MakeFracMul);
  MakeIntMul:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LeaveMul);
  MakeFracMul:
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  LeaveMul:
  POP(FP);
  RETURN;
LdivBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R7,FPARG(1));
  DECR(R7);
  CMP(R7, IMM(0));
  JUMP_NE(Div_cont);
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionDiv1);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2)); 
  MOV(R8, R3);
  MOV(R3, R2);
  MOV(R2, R8);
  JUMP(DivEnd);
  ConvertToFractionDiv1:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  MOV(R8, R3);
  MOV(R3, R2);
  MOV(R2, R8);
  JUMP(DivEnd);
  Div_cont:
  MOV(R1, IMM(0));
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionDiv2);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2));             
  JUMP(DivLoop);
  ConvertToFractionDiv2:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  DivLoop:
  CMP(R1,R7);
  JUMP_EQ(DivEnd);
  MOV(R4,FPARG(R1 + 3));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionDiv3);
  MOV(R5, INDD(R4, 1));
  MOV(R6, INDD(R4, 2));
  JUMP(DivNums);
  ConvertToFractionDiv3:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  DivNums:
  MUL(R2, R6);
  MUL(R3, R5);
  INCR(R1);
  JUMP(DivLoop);
  DivEnd:
  CMP(R3, IMM(0));
  JUMP_LT(NegDenom);
  JUMP(DivEndCont);
  NegDenom:
  NEG(R3);
  INCR(R3);
  NEG(R2);
  INCR(R2);
  DivEndCont:
  PUSH(R3);
  PUSH(R2);
  CALL(GCD);
  POP(R2);
  POP(R3);
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(ABS);
  DROP(1);
  DIV(R2, R0);
  DIV(R3, R0);
  CMP(R3, IMM(1));
  JUMP_NE(MakeFracDiv);
  MakeIntDiv:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LeaveDiv);
  MakeFracDiv:
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  LeaveDiv:
  POP(FP);
  RETURN;
LminusBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  PUSH(R5);
  PUSH(R6);
  PUSH(R7);
  PUSH(R8);
  PUSH(R9);
  PUSH(R10);
  MOV(R7,FPARG(1));
  DECR(R7);
  CMP(R7, IMM(0));
  JUMP_NE(Minus_cont);
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFraction4);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2));
  NEG(R2);
  INCR(R2);
  JUMP(MinusEnd);
  ConvertToFraction4:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  NEG(R2);
  INCR(R2);
  JUMP(MinusEnd);
  Minus_cont:
  MOV(R1, IMM(0));
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFraction3);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2));             
  JUMP(MinusLoop);
  ConvertToFraction3:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  MinusLoop:
  CMP(R1,R7);
  JUMP_EQ(MinusEnd);
  MOV(R4,FPARG(R1 + 3));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFraction2);
  MOV(R5, INDD(R4, 1));
  MOV(R6, INDD(R4, 2));
  JUMP(SubNums);
  ConvertToFraction2:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  SubNums:
  MUL(R2, R6);
  MUL(R5, R3);
  MUL(R3, R6);
  SUB(R2,R5);
  INCR(R1);
  JUMP(MinusLoop);
  MinusEnd:
  PUSH(R3);
  PUSH(R2);
  CALL(GCD);
  POP(R2);
  POP(R3);
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(ABS);
  DROP(1);
  DIV(R2, R0);
  DIV(R3, R0);
  CMP(R3, IMM(1));
  JUMP_NE(MakeFrac2);
  MakeInt2:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LeaveMinus);
  MakeFrac2:
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  LeaveMinus:
  POP(R10);
  POP(R9);
  POP(R8);
  POP(R7);
  POP(R6);
  POP(R5);
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
LbiggerBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  PUSH(R5);
  PUSH(R6);
  PUSH(R7);
  PUSH(R8);
  PUSH(R9);
  PUSH(R10);
  MOV(R10,FPARG(1));
  DECR(R10);
  CMP(R10, IMM(0));
  JUMP_NE(Bigger_cont);
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  JUMP(LeaveBigger);
  Bigger_cont:
  MOV(R1, IMM(0));
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionBigger1);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2));             
  JUMP(BiggerLoop);
  ConvertToFractionBigger1:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  BiggerLoop:
  CMP(R1,R10);
  JUMP_EQ(BiggerEnd);
  MOV(R4,FPARG(R1 + 3));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionBigger2);
  MOV(R5, INDD(R4, 1));             /* R5 = numerator of arg */
  MOV(R6, INDD(R4, 2));
  JUMP(BiggerNums);
  ConvertToFractionBigger2:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  BiggerNums:
  PUSH(R3);
  PUSH(R2);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  MOV(R8, R0);      /* R8 = T_FRACTION, 2, 1 */
  PUSH(R6);
  PUSH(R5);
  CALL(MAKE_SOB_FRACTION);
  POP(R5);
  POP(R6);
  MOV(R9, R0);      /* R9 = T_FRACTION, 4, 5 */
  PUSH(R9);
  PUSH(R8);
  PUSH(IMM(2));
  PUSH(R1);
  CALL(LminusBody);
  POP(R1);
  DROP(3);
  MOV(R2, INDD(R0, 1));
  MOV(R3, INDD(R0, 2));
  CMP(R2, 0);
  JUMP_LE(NotBigger);
  MOV(R2, R5);
  MOV(R3, R6);
  INCR(R1);
  JUMP(BiggerLoop);
  BiggerEnd:
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  JUMP(LeaveBigger);
  NotBigger:
  PUSH(IMM(0));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  LeaveBigger:
  POP(R10);
  POP(R9);
  POP(R8);
  POP(R7);
  POP(R6);
  POP(R5);
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
LequalBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  PUSH(R5);
  PUSH(R6);
  PUSH(R7);
  PUSH(R8);
  PUSH(R9);
  PUSH(R10);
  MOV(R10,FPARG(1));
  DECR(R10);
  CMP(R10, IMM(0));
  JUMP_NE(Equal_cont);
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  JUMP(LeaveEqual);
  Equal_cont:
  MOV(R1, IMM(0));
  MOV(R4, FPARG(2));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionEqual1);
  MOV(R2, INDD(R4, 1));            
  MOV(R3, INDD(R4, 2));             
  JUMP(EqualLoop);
  ConvertToFractionEqual1:
  MOV(R2, INDD(R4, 1));
  MOV(R3, IMM(1));
  EqualLoop:
  CMP(R1,R10);
  JUMP_EQ(EqualEnd);
  MOV(R4,FPARG(R1 + 3));
  CMP(IND(R4), IMM(T_FRACTION));
  JUMP_NE(ConvertToFractionEqual2);
  MOV(R5, INDD(R4, 1));
  MOV(R6, INDD(R4, 2));
  JUMP(EqualNums);
  ConvertToFractionEqual2:
  MOV(R5, INDD(R4, 1));
  MOV(R6, IMM(1));
  EqualNums:
  MUL(R2, R6);
  MUL(R3, R5);
  CMP(R2, R3);
  JUMP_NE(NotEqual);
  MOV(R2, R5);
  MOV(R3, R6);
  INCR(R1);
  JUMP(EqualLoop);
  EqualEnd:
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  JUMP(LeaveEqual);
  NotEqual:
  PUSH(IMM(0));
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  LeaveEqual:
  POP(R10);
  POP(R9);
  POP(R8);
  POP(R7);
  POP(R6);
  POP(R5);
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
LcharBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_CHAR);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LbooleanBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_BOOL);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LintegerBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_INTEGER);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LnullBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_NIL);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LpairBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_PAIR);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LprocedureBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_CLOSURE);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;  
LstringBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_STRING);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LsymbolBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_SYMBOL);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LisvectorBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_VECTOR);
  DROP(1);
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LnumberBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_INTEGER);
  DROP(1);
  CMP(R0,IMM(1));
  JUMP_EQ(number_exit);
  PUSH(FPARG(2));
  CALL(IS_SOB_FRACTION);
  DROP(1);
  number_exit:
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LrationalBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_INTEGER);
  DROP(1);
  CMP(R0,IMM(1));
  JUMP_EQ(rational_exit);
  PUSH(FPARG(2));
  CALL(IS_SOB_FRACTION);
  DROP(1);
  rational_exit:
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
LzeroBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_INTEGER);
  DROP(1);
  CMP(R0,IMM(1));
  JUMP_NE(is_not_zero);
  MOV(R1,FPARG(2));
  MOV(R1,INDD(R1,1));
  CMP(R1,IMM(0));
  JUMP_EQ(is_zero);
  MOV(R0,0);
  is_not_zero:
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
  is_zero:
  MOV(R0,1);
  JUMP(is_not_zero);
LdenominatorBody:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(FPARG(2));
  CALL(IS_SOB_INTEGER);
  DROP(1);
  CMP(R0,IMM(1));
  JUMP_EQ(is_integer);
  MOV(R1,FPARG(2));
  MOV(R0,INDD(R1,2));
  is_integer:
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LnumeratorBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(2));
  MOV(R0,INDD(R1,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LstringlengthBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0,FPARG(2));
  MOV(R0,INDD(R0,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LvectorlengthBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0,FPARG(2));
  MOV(R0,INDD(R0,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LvectorBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,IMM(0));
  push_vector_elems:
  CMP(R1,FPARG(1));
  JUMP_EQ(vector_exit);
  PUSH(FPARG(R1+2));
  INCR(R1);
  JUMP(push_vector_elems);
  vector_exit:
  PUSH(FPARG(1));
  CALL(MAKE_SOB_VECTOR);
  POP(R2);
  DROP(R2);
  POP(FP);
  RETURN;
LstringrefBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));
  MOV(R2,INDD(R2,1));
  MOV(R1,INDD(R1,2+R2));
  PUSH(R1);
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN;  
LvectorrefBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));
  MOV(R2,INDD(R2,1));
  MOV(R1,INDD(R1,2+R2));
  MOV(R0,R1);
  POP(FP);
  RETURN;  
LstringsetBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(4));
  MOV(R2,FPARG(3));
  MOV(R2,INDD(R2,1));
  MOV(R1,INDD(R1,1));
  MOV(R3,FPARG(2));
  MOV(INDD(R3,2+R2),R1);
  MOV(FPARG(2),R3);
  MOV(R0,IMM(SOB_VOID));
  POP(FP);
  RETURN;  
LvectorsetBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(4));
  MOV(R2,FPARG(3));
  MOV(R2,INDD(R2,1));
  MOV(R3,FPARG(2));
  MOV(INDD(R3,2+R2),R1);
  MOV(FPARG(2),R3);
  MOV(R0,IMM(SOB_VOID));
  POP(FP);
  RETURN;  
LsetcarBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(3));
  MOV(R2,FPARG(2));
  MOV(INDD(R2,1),R1);
  MOV(FPARG(2),R2);
  MOV(R0,IMM(SOB_VOID));
  POP(FP);
  RETURN;
LsetcdrBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(3));
  MOV(R2,FPARG(2));
  MOV(INDD(R2,2),R1);
  MOV(FPARG(2),R2);
  MOV(R0,IMM(SOB_VOID));
  POP(FP);
  RETURN;
LintegertocharBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(2));
  MOV(R1,INDD(R1,1));
  PUSH(R1);
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN;
LchartointegerBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R1,FPARG(2));
  MOV(R1,INDD(R1,1));
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LremainderBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0,FPARG(2));
  MOV(R0,INDD(R0,1));
  MOV(R1,FPARG(3));
  MOV(R1,INDD(R1,1));
  REM(R0,R1);
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
LmakestringBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R2,IMM(0));
  MOV(R3,FPARG(2));
  MOV(R3,INDD(R3,1));
  MOV(R1,FPARG(1));
  CMP(R1,IMM(1));
  JUMP_EQ(makestringwithnull);
  MOV(R4,FPARG(3));
  MOV(R4,INDD(R4,1));
  makestringwithelement:
  CMP(R2,R3);
  JUMP_EQ(makeStringExit);
  PUSH(R4);
  INCR(R2);
  JUMP(makestringwithelement);
  makestringwithnull:
  CMP(R2,R3);
  JUMP_EQ(makeStringExit);
  PUSH(IMM(0));
  INCR(R2);
  JUMP(makestringwithnull);
  makeStringExit:
  PUSH(R3);
  CALL(MAKE_SOB_STRING);
  DROP(1);
  DROP(R3);
  POP(FP);
  RETURN;
LmakevectorBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R2,IMM(0));
  MOV(R3,FPARG(2));
  MOV(R3,INDD(R3,1));
  MOV(R1,FPARG(1));
  MOV(R5,IMM(0));
  PUSH(R5);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R5,R0);
  CMP(R1,IMM(1));
  JUMP_EQ(makevectorwithnull);
  MOV(R4,FPARG(3));
  makevectorwithelement:
  CMP(R2,R3);
  JUMP_EQ(makeVectorExit);
  PUSH(R4);
  INCR(R2);
  JUMP(makevectorwithelement);
  makevectorwithnull:
  CMP(R2,R3);
  JUMP_EQ(makeVectorExit);
  PUSH(R5);
  INCR(R2);
  JUMP(makevectorwithnull);
  makeVectorExit:
  PUSH(R3);
  CALL(MAKE_SOB_VECTOR);
  DROP(1);
  DROP(R3);
  POP(FP);
  RETURN;
LapplyBody:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R11,FPARG(2));
  MOV(R12,FPARG(3));
  MOV(R15,IMM(0));
  applypushargs:
  MOV(R13,INDD(R12,0));
  CMP(R13,IMM(T_PAIR));
  JUMP_NE(applystackfix);
  MOV(R13,INDD(R12,1));
  PUSH(R13);
  INCR(R15);
  MOV(R12,INDD(R12,2));
  JUMP(applypushargs);
  applystackfix:
  MOV(R11,FPARG(2));
  MOV(R12,FPARG(3));
  MOV(R15,IMM(0));
  applyfixloop:
  MOV(R13,INDD(R12,0));
  CMP(R13,IMM(T_PAIR));
  JUMP_NE(applyexit);
  MOV(R13,INDD(R12,1));
  MOV(STARG(R15-1),R13);
  INCR(R15);
  MOV(R12,INDD(R12,2));
  JUMP(applyfixloop);
  applyexit:
  MOV(R12,INDD(R11,1));
  MOV(R11,INDD(R11,2));
  PUSH(R15);
  PUSH(R12);
  CALLA(R11);
  DROP(1);
  POP(R15);
  DROP(R15);
  POP(FP);
  RETURN;
LsymbolTostringBody:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  MOV(R0, INDD(R0, 1));
  POP(FP);
  RETURN;
LstringTosymbolBody:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, IND(999));
  CMP(R1, 0);
  JUMP_NE(SearchLoop);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), FPARG(2));
  MOV(INDD(R0, 1), IMM(0));
  MOV(IND(999), R0);
  JUMP(ReturnSymbol);
  SearchLoop:
  CMP(INDD(R1, 0), FPARG(2));
  JUMP_EQ(ReturnSymbol);
  CMP(INDD(R1, 1), 0);
  JUMP_EQ(AddStringToList);
  MOV(R1, INDD(R1, 1));
  JUMP(SearchLoop);
  AddStringToList:
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), FPARG(2));
  MOV(INDD(R0, 1), IMM(0));
  MOV(INDD(R1, 1), R0);
  ReturnSymbol:
  MOV(R0, FPARG(2));
  PUSH(R0);
  CALL(MAKE_SOB_SYMBOL);
  DROP(1);
  POP(FP);
  RETURN;
LeqBody:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0,FPARG(2));
  MOV(R1,FPARG(3));
  CMP(IND(R0),IND(R1));
  JUMP_NE(eq_not_equal);
  CMP(IND(R0),IMM(T_INTEGER));
  JUMP_EQ(eq_int);
  CMP(IND(R0),IMM(T_FRACTION));
  JUMP_EQ(eq_fraction);
  CMP(IND(R0),IMM(T_CHAR));
  JUMP_EQ(eq_char); 
  CMP(IND(R0),IMM(T_SYMBOL));
  JUMP_EQ(eq_symbol); 
  CMP(IND(R0),IMM(T_VOID));
  JUMP_EQ(eq_address); 
  CMP(IND(R0),IMM(T_NIL));
  JUMP_EQ(eq_address); 
  CMP(IND(R0),IMM(T_BOOL));
  JUMP_EQ(eq_address);
  CMP(IND(R0),IMM(T_STRING));
  JUMP_EQ(eq_address); 
  CMP(IND(R0),IMM(T_VECTOR));
  JUMP_EQ(eq_address); 
  CMP(IND(R0),IMM(T_PAIR));
  JUMP_EQ(eq_address);
  CMP(IND(R0),IMM(T_CLOSURE));
  JUMP_EQ(eq_address);
  
  eq_int:
  CMP(INDD(R0,1),INDD(R1,1));
  JUMP_EQ(eq_equal);
  JUMP(eq_not_equal);

 eq_fraction:
  CMP(INDD(R0,1),INDD(R1,1));
  JUMP_NE(eq_not_equal);
  CMP(INDD(R0,2),INDD(R1,2));
  JUMP_EQ(eq_equal);
  JUMP(eq_not_equal);

  eq_char:
  CMP(INDD(R0,1),INDD(R1,1));
  JUMP_EQ(eq_equal);
  JUMP(eq_not_equal);
  
  eq_symbol:
  MOV(R4, 2);
  CMP(INDD(R0,1),INDD(R1,1));
  JUMP_EQ(eq_equal);
  MOV(R0, INDD(R0, 1));
  MOV(R1, INDD(R1, 1));
  MOV(R2, INDD(R0, 1));
  MOV(R3, INDD(R1, 1));
  CMP(R2, R3);
  JUMP_NE(eq_not_equal);
  DECR(R2);
  
  CompareStringsLoop:
  SUB(R4, IMM(2));
  CMP(R4, R2);
  JUMP_GT(eq_equal);
  ADD(R4, IMM(2));
  CMP(INDD(R0, R4), INDD(R1, R4));
  JUMP_NE(eq_not_equal);
  INCR(R4);
  JUMP(CompareStringsLoop);
  
  eq_address:
  CMP(FPARG(2),FPARG(3));
  JUMP_EQ(eq_equal);
  eq_not_equal:
  MOV(R0,IMM(0));
  JUMP(eq_exit);
  eq_equal:
  MOV(R0,IMM(1));
  eq_exit:
  PUSH(R0);
  CALL(MAKE_SOB_BOOL);
  DROP(1);
  POP(FP);
  RETURN;
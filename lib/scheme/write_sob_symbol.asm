
WRITE_SOB_SYMBOL:
    PUSH(FP);
    MOV(FP, SP);
    MOV(R0, FPARG(0));
    MOV(R0, INDD(R0, 1));
    PUSH(R0);
    CALL(WRITE_SOB_SYMBOL_STRING);
    DROP(1);
    POP(FP);
    RETURN;
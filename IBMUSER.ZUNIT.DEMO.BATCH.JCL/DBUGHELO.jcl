//IBMTEST2 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//GO    EXEC   PROC=ELAXFGO,GO=HELLOGB,
//*        LOADDSN=IBMTEST.LOAD
//        LOADDSN=IBMUSER.ZUNIT.DEMO.BATCH.LOAD
//CEEOPTS DD *
TEST(,,,TCPIP&10.10.5.6%8001:*)
/*

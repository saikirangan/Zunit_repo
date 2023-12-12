//RUNX1  JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//GO    EXEC   PROC=ELAXFGO,GO=PGM0X1GB,
//*        LOADDSN=IBMUSER.ZUNIT.DEMO.BATCH.LOAD
//        LOADDSN=IBMUSER.ZUNIT.DEMO.BATCH.LOAD
//CEEOPTS DD *
TEST(,,,TCPIP&10.10.5.6%8001:*)
ENVAR("EQA_STARTUP_KEY=CC")
/*
//SYSPRINT DD SYSOUT=*
00004
00005
//*





































































































//RUN05 JOB (123),'SAI',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//STEP1 EXEC PGM=PGM05
//SYSPRINT DD SYSOUT=*
//STEPLIB DD DISP=SHR,DSN=IBMUSER.ZUNIT.DEMO.BATCH.LOAD(PGM05)
/*
//DDINPUT  DD DISP=SHR,DSN=ADCDS.INPUT.PGM05
//DDOUTPUT  DD DSN=ADCDS.OUTPUT.PGM05,DISP=OLD
//*
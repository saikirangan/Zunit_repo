//IBMUSER1 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(1440),REGION=144M,COND=(16,LT)
//PROCS JCLLIB ORDER=(ADCD.Z24C.PROCLIB)
//*
//        SET FELJOB=CMPLNK
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=,
// PARM.COBOL=('SQL',
// 'LIB,SOURCE',
// 'TEST')
//COBOL.SYSPRINT DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.LISTING(COBDB2)
//COBOL.SYSDEBUG DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.SYSDEBUG(COBDB2)
//COBOL.SYSLIN DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.OBJECT(COBDB2)
//COBOL.DBRMLIB  DD  DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.DBRMLIB(COBDB2)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.COPY
//        DD DISP=SHR,
//        DSN=SYS1.IBM.HAL6100.BZU.SBZUSAMP
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.COBOL(COBDB2)
//*
//******* ADDITIONAL JCL FOR COMPILE HERE ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.OBJECT(COBDB2)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=IBMUSER.ZUNIT.DEMO.BATCH.LOAD(COBDB2)
//*
//******* ADDITIONAL JCL FOR LINK HERE ******
//BIND EXEC PGM=IKJEFT01
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//DBRMLIB  DD  DSN=IBMUSER.ZUNIT.DEMO.BATCH.DBRMLIB,DISP=SHR
//*UNCOMMENT AND TAILOR THE FOLLOWING CODE IF YOUR SYSTSIN STATEMENT**
//*CONTAINS BIND INSTRUCTIONS:
//*//SYSTSIN   DD  *
//* DSN SYSTEM(YOURSUBSYSTEM)
//*    BIND PACKAGE(YOURLOCATION.YOURPACKAGE)-
//*       OWNER(YOURUSERID) -
//*       MEMBER(YOURMEMBER) -
//*       LIBRARY('YOUR.DBRM.LIBRARY') -
//*       ACTION(REP) -
//*       VALIDATE(BIND)
//*     BIND PLAN(YOURPLAN) -
//*       PKLIST(YOURLOCATION.YOURPACKAGENAME.*)
//* END
//*    OR
//*UNCOMMENT AND TAILOR THE FOLLOWING CODE IF YOUR SYSTSIN STATEMENT**
//*POINTS TO A DATA SET CONTAINING BIND INSTRUCTIONS
//*//SYSTSIN DD DSN=USERID.BIND(MEMBER),DISP=SHR
//*
/*
//

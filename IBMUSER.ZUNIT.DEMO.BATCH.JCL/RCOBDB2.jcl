//ADCDST   JOB 0,'AAA',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//RUN      EXEC PGM=IKJEFT01,
//             COND=(4,LT),
//             REGION=4096K
//STEPLIB  DD  DISP=SHR,
//             DSN=DSNB10.SDSNLOAD
//DBRMLIB  DD  DISP=SHR,
//             DSN=ADCDS.SOURCE.DBRM(COBDB2)
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSTSIN DD *
TSOLIB ACT DSN('SYS1.IBM.HAL6100.BZU.SBZULOAD',-
 'SYS1.IBM.HAL6100.BZU.SBZURESL', 'SYS1.IBM.HAL6100.BZU.SBZULLEP',-
 'SYS1.IBM.HAL6100.BZU.SBZULMOD', 'ADCDS.LOAD.LIB')
DSN SYSTEM(DBCG)
RUN PROGRAM(BZUBCP) PLAN(COBDB2) PA('COBDB2')
//DDOUTPUT DD DSN=ADCDS.DB2.INPUT,DISP=SHR

/*
//SYSTSIN  DD  *
    DSN SYSTEM (DBCG)
    RUN PROGRAM(COBDB2)PLAN(COBDB2) -
    LIB('ADCDS.LOAD.LIB')
//DDOUTPUT DD   DSN=ADCDS.DB2.INPUT,DISP=SHR
//*
//BZUMSG DD SYSOUT=*
//BZUPLAY DD DSN=IBMUSER.ZUNIT.PB.AZUNEXT.TCOBDB2,DISP=SHR
//BZUCFG DD *
<?xml version="1.0" encoding="UTF-8"?>
<runner:RunnerConfiguration
 xmlns:runner="http://www.ibm.com/zUnit/3.0.0.0/TestRunner"
 id="2aef16ef-05c8-491a-9f79-dfbe007bfac7">
  <runner:options contOnTestCaseError="false" contOnTestCaseFail="true"
 contOnTestError="false" contOnTestFail="true" fileIOCapture="enable"/>
  <runner:intercept module="COBDB2" stub="false" lengths=""
 retcode="true" exist="false"/>
</runner:RunnerConfiguration>

/*
//RUNX1   JOB ('POPUP'),'POPUP',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//RUN EXEC PGM=BZUBCP,PARM='PGM0X1GB='
//*//RUN   EXEC PGM=PGM0X1GB
//STEPLIB  DD DSN=SYS1.IBM.BZU.SBZULLEP,DISP=SHR
//         DD DSN=SYS1.IBM.BZU.SBZURESL,DISP=SHR
//         DD DSN=SYS1.IBM.BZU.SBZULOAD,DISP=SHR
//         DD DISP=SHR,DSN=ADCDMST.ZUNIT.DEMO.BATCH.LOAD
//SYSPRINT DD SYSOUT=*
//BZUMSG DD SYSOUT=*
//BZUPLAY DD DSN=ADCDMST.ZUNIT.PB.AZUPLAY.TPGM0X1G,DISP=SHR
//BZUCFG DD *
<?xml version="1.0" encoding="UTF-8"?>
<runner:RunnerConfiguration
 xmlns:runner="http://www.ibm.com/zUnit/3.0.0.0/TestRunner"
 id="ee716739-ee04-4115-bb27-53e282aab178">
  <runner:options contOnTestCaseError="false" contOnTestCaseFail="true"
 contOnTestError="false" contOnTestFail="true" fileIOCapture="enable"/>
  <runner:intercept module="PGM0X1GB" stub="false" lengths=""
 retcode="true" exist="false"/>
  <runner:intercept module="PGM0X1GB" stub="false" csect="PGM0X2GB"
 lengths="5,5,6" retcode="false" exist="false"/>
  <runner:intercept module="PGM0X2GB" stub="false" lengths="5,5,6"
 retcode="false" exist="false"/>
</runner:RunnerConfiguration>

/*
00004
00005
//*
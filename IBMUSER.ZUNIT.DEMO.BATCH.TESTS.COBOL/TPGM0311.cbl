       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| TPGM0311                                                      |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| TEST CASE VERSION: 103                                        |
      *| DATE GENERATED: 11/28/2023 15:55                              |
      *| ID: 578277d4-2197-4d96-9389-0a43120f9359                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST1                                              |
      *|     THIS PROGRAM IS FOR TEST TEST1                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'PGM03'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY BZUITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01 AZ-LINKPARM0              PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL PGM03'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       END PROGRAM TEST_TEST1.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST2                                              |
      *|     THIS PROGRAM IS FOR TEST TEST2                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'PGM03'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY BZUITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01 AZ-LINKPARM0              PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST2 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL PGM03'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST2 END.'
           GOBACK.
       END PROGRAM TEST_TEST2.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST5                                              |
      *|     THIS PROGRAM IS FOR TEST TEST5                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST5'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'PGM03'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY BZUITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01 AZ-LINKPARM0              PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST5 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL PGM03'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST5 END.'
           GOBACK.
       END PROGRAM TEST_TEST5.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TEST                                                |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'PGM03'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_PGM03" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_INPT_PGM03 INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_PGM03" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_OUTP_PGM03 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST2'
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST5'
             MOVE 0 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           GOBACK.
       END PROGRAM BZU_TEST.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_INIT                                                |
      *|     INITIAL PROCEDURE                                         |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE '578277d4-2197-4d96-9389-0a43120f9359'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-TEST-ID
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_INIT: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           DISPLAY 'AZU0000I TEST CASE VERSION: 103'
           DISPLAY 'AZU0001I FOR TEST RUNNER: <no_value> (NOT FOR:CICSCO
      -    'UNT,DLL,CSECT)'
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TERM                                                |
      *|     TERMINATION PROCEDURE                                     |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.

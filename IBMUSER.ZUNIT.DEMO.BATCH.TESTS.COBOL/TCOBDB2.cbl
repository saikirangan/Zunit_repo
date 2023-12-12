       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| TCOBDB2                                                       |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| TEST CASE VERSION: 103                                        |
      *| DATE GENERATED: 12/05/2023 12:56                              |
      *| ID: f4ee0210-b035-4a42-af74-8d42a4ff4e9c                      |
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
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
           DISPLAY 'AZU0000I CALL COBDB2'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       END PROGRAM TEST_TEST1.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST3                                              |
      *|     THIS PROGRAM IS FOR TEST TEST3                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST3'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST3 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL COBDB2'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST3 END.'
           GOBACK.
       END PROGRAM TEST_TEST3.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST4                                              |
      *|     THIS PROGRAM IS FOR TEST TEST4                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST4'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST4 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL COBDB2'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST4 END.'
           GOBACK.
       END PROGRAM TEST_TEST4.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST5                                              |
      *|     THIS PROGRAM IS FOR TEST TEST5                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST5'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
           DISPLAY 'AZU0000I CALL COBDB2'
           CALL PROGRAM-NAME
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST5 END.'
           GOBACK.
       END PROGRAM TEST_TEST5.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST6                                              |
      *|     THIS PROGRAM IS FOR TEST TEST6                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST6'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE
           ASSIGN TO DDOUTPUT
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE
           RECORDING MODE IS F.
      *  *** FL-OUTREC : ZUT00000000
       1 ZUT00000000.
      *    *** ENAME : ZUT00000001
         5 ZUT00000001 PIC X(10).
      *    *** FILLER : ZUT00000002
         5 ZUT00000002 PIC X(70).
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
       01 AZ-FS-CODE        PIC X(2).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY BZUITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST6 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * OPEN OUTFILE FOR OUTPUT
           OPEN OUTPUT OUTFILE
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL COBDB2'
           CALL PROGRAM-NAME
           .
      * CLOSE OUTFILE
           CLOSE OUTFILE
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST6 END.'
           GOBACK.
       END PROGRAM TEST_TEST6.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TEST                                                |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'COBDB2'.
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
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RC-WORK          PIC S9(4) USAGE BINARY.
       01 AZ-OUTPUT-COUNT-STR PIC X(5).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_COBDB2" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_INPT_COBDB2 INPUT VALUES...'.
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
           ENTRY "PGM_OUTP_COBDB2" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_OUTP_COBDB2 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM CHECK-REC-TEST1
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST3'
             PERFORM CHECK-REC-TEST3
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST4'
             PERFORM CHECK-REC-TEST4
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST5'
             PERFORM CHECK-REC-TEST5
             MOVE 0 TO RETURN-CODE
           WHEN 'TEST6'
             MOVE 0 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           GOBACK.
       CHECK-REC-TEST1.
      * CHECK RECORD COUNT FOR TEST1
      * FOR OUTFILE
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 3 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''3''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN OUTFILE.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       CHECK-REC-TEST3.
      * CHECK RECORD COUNT FOR TEST3
      * FOR OUTFILE
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN OUTFILE.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       CHECK-REC-TEST4.
      * CHECK RECORD COUNT FOR TEST4
      * FOR OUTFILE
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN OUTFILE.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       CHECK-REC-TEST5.
      * CHECK RECORD COUNT FOR TEST5
      * FOR OUTFILE
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN OUTFILE.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
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
           VALUE 'f4ee0210-b035-4a42-af74-8d42a4ff4e9c'.
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
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR FILE                                        |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'FILE_COBDB2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST              PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-INFO-DDNM         PIC X(8).
       01 AZ-INFO-STAT         PIC X(1).
       01 AZ-INFO-COND         PIC X(1).
       01 AZ-ACMDVA            PIC X(4).
       01 AZ-PARM              PIC X(80).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * QSAM_INPT_WRIT_COBDB2.
           ENTRY 'QSAM_INPT_WRIT_COBDB2' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM.
           DISPLAY 'AZU0000I QSAM_INPT_WRIT_COBDB2 CHECK VALUES...'
           IF AZ-INFO-STAT = X'00' THEN
             PERFORM WRITE-OUTPUT
           END-IF.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
      * END
       READ-OUTPUT.
           EXIT.
       READ-INPUT.
           EXIT.
       WRITE-OUTPUT.
      *    FILE "OUTFILE"" (DD:DDOUTPUT)"
           IF AZ-INFO-DDNM = "DDOUTPUT" THEN
             CALL 'OUTFILE_OUTPUT_WRITE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           END-IF.
           EXIT.
       WRITE-INPUT.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I FILE_COBDB2 END.'
           GOBACK.
       END PROGRAM 'FILE_COBDB2'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR FILE QSAM                                   |
      *|   FILE NAME: OUTFILE                                          |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'QSAM_OUTFILE_COBDB2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-INFO-DDNM        PIC X(8).
       01 AZ-INFO-STAT        PIC X(1).
       01 AZ-INFO-COND        PIC X(1).
       01 AZ-ACMDVA           PIC X(4).
       01 AZ-PARM             PIC X(80).
       01 AZ-WK-RECORD-COUNT  PIC 9(5) COMP-5.
      *  *** FL-OUTREC : ZUT00000000
       1 ZUT00000000.
      *    *** ENAME : ZUT00000001
         5 ZUT00000001 PIC X(10).
      *    *** FILLER : ZUT00000002
         5 ZUT00000002 PIC X(70).
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY 'OUTFILE_OUTPUT_WRITE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE OUTFILE CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN 'TEST3'
             PERFORM P-OUTPUT-TEST3
           WHEN 'TEST4'
             PERFORM P-OUTPUT-TEST4
           WHEN 'TEST5'
             PERFORM P-OUTPUT-TEST5
           WHEN 'TEST6'
             PERFORM P-OUTPUT-TEST6
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY 'OUTFILE_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE OUTFILE INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN 'TEST3'
             PERFORM P-INPUT-TEST3
           WHEN 'TEST4'
             PERFORM P-INPUT-TEST4
           WHEN 'TEST5'
             PERFORM P-INPUT-TEST5
           WHEN 'TEST6'
             PERFORM P-INPUT-TEST6
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
      *     DISPLAY 'AZU0000I QSAM_OUTFILE_COBDB2 END.'
           GOBACK.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST3.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST4.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST5.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST6.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST3.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST4.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST5.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST6.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'QSAM_OUTFILE_COBDB2'.
      *+---------------------------------------------------------------+
      *| ZUNIT GTMEMRC                                                 |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'GTMEMRC'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZUGTMEM            PIC X(8) VALUE 'BZUGTMEM'.
       01 DATA-SIZE           PIC 9(8) COMP-4.
       LINKAGE SECTION.
       01 AZ-TC-WORK-AREA        PIC X(256).
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RECORD-PTR-VALUE
            REDEFINES AZ-RECORD-PTR  PIC S9(9) COMP-5.
       01 DATA-PTR            POINTER.
       01 DATA-PTR-VALUE
            REDEFINES DATA-PTR  PIC S9(9) COMP-5.
       01 DATA-AREA.
         03 RECORD-COUNT-IO OCCURS 4.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
       MAINPROC SECTION.
      * GET DATA AREA
           SET ADDRESS OF DATA-PTR TO ADDRESS OF AZ-TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 4
             CALL BZUGTMEM USING DATA-SIZE RETURNING DATA-PTR
             SET ADDRESS OF DATA-AREA TO DATA-PTR
             DISPLAY 'AZU0000I AREA ALLOCATED FOR RECORD COUNT:'
           DATA-SIZE
           END-IF
           SET AZ-RECORD-PTR TO DATA-PTR
           COMPUTE AZ-RECORD-PTR-VALUE = AZ-RECORD-PTR-VALUE +
                 LENGTH OF WK-RECORD-COUNT * 2 * (AZ-GRP-INDEX - 1)
           IF AZ-FLAG-IN = 1 THEN
             ADD LENGTH OF WK-RECORD-COUNT TO AZ-RECORD-PTR-VALUE
           END-IF
           SET ADDRESS OF WK-RECORD-COUNT TO AZ-RECORD-PTR
           GOBACK.
       CB-ENTRY.
      * ENTRY FOR CALLBACK
           ENTRY "PGM_INPT_GTMEMRC" USING AZ-TEST AZ-INFO-BLOCK
             AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR.
           PERFORM MAINPROC.
           EXIT.
       END PROGRAM 'GTMEMRC'.
      *+---------------------------------------------------------------+
      *| ZUNIT AZU_GENERIC_DB2                                         |
      *|   GENERIC DB2 CALLBACK EXIT POINT                             |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_DB2'.
       DATA DIVISION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT.
           ENTRY 'DB2_INPT' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I DB2_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * DB2_OUTP.
           ENTRY 'DB2_OUTP' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I DB2_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_DB2'.
      *+---------------------------------------------------------------+
      *| ZUNIT AZU_GENERIC_FILE                                        |
      *|   GENERIC FILE CALLBACK EXIT POINT                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_FILE'.
       PROCEDURE DIVISION.
      * QSAM_INPT.
           ENTRY 'QSAM_INPT'.
           DISPLAY 'AZU0000I QSAM_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * QSAM_OUTP.
           ENTRY 'QSAM_OUTP'.
           DISPLAY 'AZU0000I QSAM_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_FILE'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR EXEC SQL OPEN                               |
      *|    FUNCTION CODE: 0003                                        |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0003_COBDB2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-APLIST.
          COPY BZUDB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-SQLDA.
          COPY BZUDB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0003_COBDB2.
           ENTRY 'DB2_INPT_0003_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0003_COBDB2 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL OPEN : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL OPEN'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
             MOVE 2 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
      * DB2_OUTP_0003_COBDB2.
           ENTRY 'DB2_OUTP_0003_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0003_COBDB2 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL OPEN : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL OPEN'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
             MOVE 2 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I DB2_0003_COBDB2 END.'
           GOBACK.
       END PROGRAM 'DB2_0003_COBDB2'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR EXEC SQL CLOSE                              |
      *|    FUNCTION CODE: 0005                                        |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0005_COBDB2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-APLIST.
          COPY BZUDB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-SQLDA.
          COPY BZUDB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0005_COBDB2.
           ENTRY 'DB2_INPT_0005_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0005_COBDB2 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL CLOSE : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL CLOSE'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
             MOVE 3 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
      * DB2_OUTP_0005_COBDB2.
           ENTRY 'DB2_OUTP_0005_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0005_COBDB2 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL CLOSE : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL CLOSE'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
             MOVE 3 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I DB2_0005_COBDB2 END.'
           GOBACK.
       END PROGRAM 'DB2_0005_COBDB2'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR EXEC SQL FETCH                              |
      *|    FUNCTION CODE: 0004                                        |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0004_COBDB2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-APLIST.
          COPY BZUDB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 ARGI1          .
          COPY BZUDB2CV.
       01 AZ-SQLDA.
          COPY BZUDB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0004_COBDB2.
           ENTRY 'DB2_INPT_0004_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0004_COBDB2 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL FETCH : OUT=0 IN=1
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 1 THEN
             DISPLAY 'AZU0000I EXEC SQL FETCH'
              ' : OUT=' 0 ' IN=' 1
              ' L=' AZ-STMT-NUM
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
      * DB2_OUTP_0004_COBDB2.
           ENTRY 'DB2_OUTP_0004_COBDB2' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST ARGI1.
           DISPLAY 'AZU0000I DB2_0004_COBDB2 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL FETCH : OUT=0 IN=1
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 1 THEN
             DISPLAY 'AZU0000I EXEC SQL FETCH'
              ' : OUT=' 0 ' IN=' 1
              ' L=' AZ-STMT-NUM
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I DB2_0004_COBDB2 END.'
           GOBACK.
       END PROGRAM 'DB2_0004_COBDB2'.

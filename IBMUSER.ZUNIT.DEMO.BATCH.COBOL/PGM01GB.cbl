       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM01GB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER-A PIC 9(5).
       01 WS-NUMBER-B PIC 9(5).
       01 WS-RESULT-B PIC 9(6).
       01 WS-CALLED-PGM PIC X(8) VALUE 'PGM02GB'.
       PROCEDURE DIVISION.
       MAIN-ROUTINE.
            ACCEPT WS-NUMBER-A.
            ACCEPT WS-NUMBER-B.
            DISPLAY 'Program A is calling Program B'.

            CALL WS-CALLED-PGM USING WS-NUMBER-A, WS-NUMBER-B,
                                          WS-RESULT-B.
            DISPLAY 'Result from Program B: ' WS-RESULT-B.
            IF WS-RESULT-B > 100 THEN
               DISPLAY 'GREATER THAN 100'
            ELSE
               DISPLAY 'NOT GREATER THAN 100'
            END-IF
            GOBACK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM02GB.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NUMBER-A PIC 9(5).
       01 LS-NUMBER-B PIC 9(5).
       01 LS-RESULT-B PIC 9(6).
       PROCEDURE DIVISION USING LS-NUMBER-A, LS-NUMBER-B, LS-RESULT-B.

       MAIN-ROUTINE.
            DISPLAY 'Program B is performing arithmetic operations'.
            COMPUTE LS-RESULT-B = LS-NUMBER-A + LS-NUMBER-B.
            GOBACK.
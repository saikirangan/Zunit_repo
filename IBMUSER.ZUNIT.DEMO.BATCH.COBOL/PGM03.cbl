       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM03.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X    PIC 9(02).
       01 WS-Y    PIC 9(02).
       01 WS-Z    PIC 9(03).
       01 WS-TOT  PIC 9(03).
       PROCEDURE DIVISION.
           ACCEPT WS-X.
           ACCEPT WS-Y.
           ACCEPT WS-Z.
           COMPUTE WS-TOT = WS-X + WS-Y.
           DISPLAY 'SUM OF ' WS-X ' AND 'WS-Y ' IS:' WS-TOT.
           IF WS-TOT NOT = WS-Z THEN
              MOVE +12  TO RETURN-CODE
           ELSE
              MOVE ZERO TO RETURN-CODE.
           GOBACK.
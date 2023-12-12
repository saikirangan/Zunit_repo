       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDB2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                SELECT OUTFILE ASSIGN TO DDOUTPUT
                ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE
            RECORDING MODE IS F.
        01 FL-OUTREC.
           05 ENAME    PIC X(10).
           05 FILLER   PIC X(70).
       WORKING-STORAGE SECTION.
            EXEC SQL
                 INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
                 INCLUDE DCLGEN
            END-EXEC.

       01  DCLEMPOLYEE.
           10 WS-EMPID     PIC S9(9) USAGE COMP.
           10 WS-EMPNAME.
                49 WS-EMPNAME-LEN  PIC S9(4) USAGE COMP.
                 49 WS-EMPNAME-TEXT   PIC X(20).
           10 WS-SALARY       PIC S9(8)V9(2) USAGE COMP-3.

            EXEC SQL
                 DECLARE CSR1 CURSOR FOR
                 SELECT EMPNAME
                 FROM   EMP_DB
                 WHERE  EMPNAME LIKE 'S%'
                 FOR FETCH ONLY
            END-EXEC.
       PROCEDURE DIVISION.
            OPEN OUTPUT OUTFILE.
            EXEC SQL
                 OPEN CSR1
            END-EXEC.

            IF SQLCODE EQUAL TO ZERO
                 PERFORM FETCH-EMPLOYE
                 THRU FETCH-EMPLOYE-EXIT
                 UNTIL SQLCODE NOT EQUAL 0
            END-IF.
            EXEC SQL
                CLOSE CSR1
            END-EXEC.
            GOBACK.
       FETCH-EMPLOYE.
            EXEC SQL
                 FETCH CSR1
                 INTO : EMPNAME
            END-EXEC.
            EVALUATE SQLCODE
                WHEN ZERO
                    DISPLAY EMPNAME
                    MOVE EMPNAME TO FL-OUTREC
                    WRITE FL-OUTREC
                    INITIALIZE EMPNAME
                WHEN 100
                     CONTINUE
                WHEN OTHER
                     DISPLAY "DB2 ERROR: "  SQLCODE
            END-EVALUATE.
       FETCH-EMPLOYE-EXIT.
             EXIT.
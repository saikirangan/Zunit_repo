       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.   PGM04.                                             00020002
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00040000
       FILE-CONTROL.                                                    00050000
                SELECT INFILE ASSIGN TO DDINPUT.                        00060001
                SELECT OUTFILE ASSIGN TO DDOUTPUT.                      00070001
       DATA DIVISION.                                                   00080000
       FILE SECTION.                                                    00090000
       FD INFILE                                                        00100001
            RECORDING MODE IS F.                                        00101001
       01 INREC PIC X(80).                                              00110000
       FD OUTFILE                                                       00120001
            RECORDING MODE IS F.                                        00121001
       01 OUTREC PIC X(80).                                             00130000
       WORKING-STORAGE SECTION.                                         00140000
       01 WS-EOF PIC X(1) VALUE 'N'.                                    00150001
       PROCEDURE DIVISION.                                              00160000
            OPEN INPUT INFILE.                                          00161000
            OPEN OUTPUT OUTFILE.                                        00162000
            PERFORM UNTIL WS-EOF = 'Y'                                  00163000
             READ INFILE                                                00164000
              AT END MOVE 'Y' TO WS-EOF                                 00170000
              NOT AT END PERFORM A000-WRITE-PARA                        00180000
             END-READ                                                   00190000
             END-PERFORM.                                               00200000
             CLOSE INFILE.                                              00210000
             CLOSE OUTFILE.                                             00211000
             GOBACK.                                                    00220004
       A000-WRITE-PARA.                                                 00230000
           MOVE INREC TO OUTREC.                                        00240000
           WRITE OUTREC                                                 00250000
           END-WRITE.                                                   00260000

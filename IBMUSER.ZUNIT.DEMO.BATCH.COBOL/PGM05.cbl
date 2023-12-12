       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.   PGM05.                                             00020003
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00031000
       FILE-CONTROL.                                                    00032000
                SELECT INFILE ASSIGN TO DDINPUT                         00033000
                ORGANIZATION IS SEQUENTIAL.                             00034000
                SELECT OUTFILE ASSIGN TO DDOUTPUT                       00035000
                ORGANIZATION IS SEQUENTIAL.                             00036000
       DATA DIVISION.                                                   00037000
       FILE SECTION.                                                    00038000
       FD INFILE                                                        00039000
            RECORDING MODE IS F.                                        00040000
       01 FL-REC.                                                       00041000
          05 FL-NAME  PIC X(20).                                        00042002
          05 FILLER   PIC X(60).                                        00043002
       FD OUTFILE                                                       00044000
            RECORDING MODE IS F.                                        00045000
       01 FL-OUTREC.                                                    00046000
          05 FL-NAME  PIC X(20).                                        00047002
          05 FILLER   PIC X(60).                                        00048002
       WORKING-STORAGE SECTION.                                         00049000
       01 WS-NAME     PIC  X(20).                                       00050004
       01 WS-EOF      PIC  A VALUE SPACE.                               00060000
       PROCEDURE DIVISION.                                              00070000
            OPEN INPUT INFILE.                                          00080000
            OPEN OUTPUT OUTFILE.                                        00081000
            PERFORM UNTIL WS-EOF = 'Y'                                  00082000
               READ INFILE INTO WS-NAME                                 00083000
                 AT END MOVE 'Y' TO WS-EOF                              00084000
                 NOT AT END PERFORM A000-WRITE-PARA                     00085000
               END-READ                                                 00086000
            END-PERFORM.                                                00087000
            CLOSE INFILE.                                               00088000
            GOBACK.                                                     00089004
       A000-WRITE-PARA.                                                 00090000
           MOVE WS-NAME TO FL-OUTREC.                                   00100000
           WRITE FL-OUTREC                                              00110000
           END-WRITE.                                                   00120000
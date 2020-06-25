        01  SC05.
            02  SC05-PRTYM  PIC X(16).
            02  SC05-HOSPCDN    PIC X(20).
            02  SC05-SRYKA  PIC X(2).
            02  SC05-SEIYMD PIC X(22).
            02  SC05-ADRS   PIC X(100).
            02  SC05-HOSPNAME   PIC X(100).
            02  SC05-KAISETUNAME    PIC X(60).
            02  SC05-TEL    PIC X(26).
            02  SC05-SRYTBL     OCCURS  10 TIMES.
              03  SC05-JKYSNUM1   PIC X(2).
              03  SC05-JKYSNUM4   PIC X(8).
              03  SC05-JKYSNUM7   PIC X(14).
              03  SC05-NAME   PIC X(20).
              03  SC05-NYUGAIKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  SC05-HKNKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  SC05-TYOKI  PIC X(1).
              03  SC05-TENSU1 PIC X(7).
              03  SC05-TENSU2 PIC X(7).
              03  SC05-TENSU3 PIC X(7).
              03  SC05-KOHTENSU   PIC X(7).
              03  SC05-ITBFTN PIC X(6).
              03  SC05-SKINGK PIC X(6).
              03  SC05-SFTNGK PIC X(6).
              03  SC05-BIKO   PIC X(16).
            02  SC05-SUM-TBL.
              03  SC05-GKENSU
                      PIC X(2)    OCCURS  3 TIMES.
              03  SC05-GTENSU
                      PIC X(9)    OCCURS  3 TIMES.
              03  SC05-GKOHTENSU  PIC X(9).
              03  SC05-GITBFTN    PIC X(7).
              03  SC05-GSKENSU    PIC X(2).
              03  SC05-GSKINGK    PIC X(7).
              03  SC05-GSFTNGK    PIC X(7).

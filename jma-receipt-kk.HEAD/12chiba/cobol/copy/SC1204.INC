        01  SC04.
            02  SC04-PRTYM  PIC X(16).
            02  SC04-ADRS   PIC X(100).
            02  SC04-TEL    PIC X(12).
            02  SC04-HOSPNAME   PIC X(100).
            02  SC04-KAISETUNAME    PIC X(40).
            02  SC04-SRYTBL     OCCURS  10 TIMES.
              03  SC04-NAME   PIC X(20).
              03  SC04-JKYSNUM    PIC X(9).
              03  SC04-SYAHO  PIC X(2).
              03  SC04-KOKUHO PIC X(2).
              03  SC04-TENSU  PIC X(7).
              03  SC04-FTN1   PIC X(2).
              03  SC04-FTN2   PIC X(2).
              03  SC04-FTN3   PIC X(2).
              03  SC04-NISSU  PIC X(3).
              03  SC04-ITBFTN PIC X(6).
              03  SC04-SKJFTN PIC X(7).
              03  SC04-SKJFTNGK   PIC X(3).
              03  SC04-SKJKAISU   PIC X(2).
            02  SC04-BIKO-TBL           OCCURS  10 TIMES.
              03  SC04-BIKO   PIC X(7).
            02  SC04-GOKEI-TBL.
              03  SC04-GTENSU PIC X(9).
              03  SC04-GKAISU PIC X(3).
              03  SC04-GITBFTN    PIC X(7).
              03  SC04-GSKJFTN    PIC X(9).
              03  SC04-GBIKO  PIC X(9).
            02  SC04-KOUZA.
              03  SC04-BANKNAME   PIC X(20).
              03  SC04-SITEN  PIC X(20).
              03  SC04-SITENCD    PIC X(3).
              03  SC04-KOUZANUM   PIC X(16).
              03  SC04-MEIGI  PIC X(20).
            02  SC04-KANRINUM   PIC X(4).
            02  SC04-BIKO2-TBL  OCCURS  10 TIMES.
              03  SC04-BIKO2      PIC X(30)  OCCURS  2 TIMES.

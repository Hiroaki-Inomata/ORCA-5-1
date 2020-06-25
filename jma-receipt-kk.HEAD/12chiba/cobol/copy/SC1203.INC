        01  SC03.
            02  SC03-PRTYM  PIC X(16).
            02  SC03-ADRS   PIC X(100).
            02  SC03-TEL    PIC X(12).
            02  SC03-HOSPNAME   PIC X(100).
            02  SC03-KAISETUNAME    PIC X(40).
            02  SC03-SRYTBL     OCCURS  10 TIMES.
              03  SC03-NAME   PIC X(20).
              03  SC03-JKYSNUM    PIC X(9).
              03  SC03-SYAHO  PIC X(2).
              03  SC03-KOKUHO PIC X(2).
              03  SC03-TENSU  PIC X(7).
              03  SC03-FTN1   PIC X(2).
              03  SC03-FTN2   PIC X(2).
              03  SC03-FTN3   PIC X(2).
              03  SC03-KAISU  PIC X(3).
              03  SC03-ITBFTN PIC X(6).
            02  SC03-BIKO-TBL           OCCURS  10 TIMES.
              03  SC03-BIKO   PIC X(7).
            02  SC03-GOKEI-TBL.
              03  SC03-GTENSU PIC X(9).
              03  SC03-GKAISU PIC X(3).
              03  SC03-GITBFTN    PIC X(7).
              03  SC03-GBIKO  PIC X(9).
            02  SC03-KOUZA.
              03  SC03-BANKNAME   PIC X(20).
              03  SC03-SITEN  PIC X(20).
              03  SC03-SITENCD    PIC X(3).
              03  SC03-KOUZANUM   PIC X(16).
              03  SC03-MEIGI  PIC X(20).
            02  SC03-KANRINUM   PIC X(4).
            02  SC03-BIKO2-TBL  OCCURS  10 TIMES.
              03  SC03-BIKO2      PIC X(30)  OCCURS  2 TIMES.

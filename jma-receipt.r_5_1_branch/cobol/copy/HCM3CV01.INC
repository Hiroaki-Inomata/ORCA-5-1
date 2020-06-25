        01  HCM3CV01.
            02  HCM3CV01-PRTYM  PIC X(16).
            02  HCM3CV01-SEIYMD PIC X(22).
            02  HCM3CV01-HOSPCD PIC X(20).
            02  HCM3CV01-ADRS   PIC X(100).
            02  HCM3CV01-HOSPNAME   PIC X(100).
            02  HCM3CV01-KAISETUNAME    PIC X(60).
            02  HCM3CV01-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM3CV01-KENSU1 PIC X(6).
              03  HCM3CV01-NISSU1 PIC X(6).
              03  HCM3CV01-TENSU1 PIC X(10).
              03  HCM3CV01-ITBFTN1    PIC X(9).
            02  HCM3CV01-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM3CV01-KENSU2 PIC X(6).
              03  HCM3CV01-NISSU2 PIC X(6).
              03  HCM3CV01-TENSU2 PIC X(10).
              03  HCM3CV01-ITBFTN2    PIC X(9).
            02  HCM3CV01-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM3CV01-KENSU3 PIC X(6).
              03  HCM3CV01-NISSU3 PIC X(6).
              03  HCM3CV01-TENSU3 PIC X(10).
            02  HCM3CV01-ITBFTN3    PIC X(9).
            02  HCM3CV01-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM3CV01-KENSU4 PIC X(6).
              03  HCM3CV01-NISSU4 PIC X(6).
              03  HCM3CV01-TENSU4 PIC X(10).
            02  HCM3CV01-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM3CV01-KENSU5 PIC X(6).
              03  HCM3CV01-NISSU5 PIC X(6).
              03  HCM3CV01-TENSU5 PIC X(10).
            02  HCM3CV01-KENSUG PIC X(6).
            02  HCM3CV01-TITLE  PIC X(50).
            02  HCM3CV01-TITLE1 PIC X(10).
            02  HCM3CV01-SRYKA  PIC X(40).
            02  HCM3CV01-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3CV01-SEIDONAME
                    PIC X(8)    OCCURS  5 TIMES.

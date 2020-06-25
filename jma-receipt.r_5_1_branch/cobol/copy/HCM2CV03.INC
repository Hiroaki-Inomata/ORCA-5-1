        01  HCM2CV03.
            02  HCM2CV03-PRTYM  PIC X(16).
            02  HCM2CV03-SEIYMD PIC X(22).
            02  HCM2CV03-HOSPCD PIC X(20).
            02  HCM2CV03-ADRS   PIC X(100).
            02  HCM2CV03-HOSPNAME   PIC X(100).
            02  HCM2CV03-KAISETUNAME    PIC X(60).
            02  HCM2CV03-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2CV03-KENSU1 PIC X(6).
              03  HCM2CV03-NISSU1 PIC X(6).
              03  HCM2CV03-TENSU1 PIC X(10).
              03  HCM2CV03-ITBFTN1    PIC X(9).
            02  HCM2CV03-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2CV03-KENSU2 PIC X(6).
              03  HCM2CV03-NISSU2 PIC X(6).
              03  HCM2CV03-TENSU2 PIC X(10).
              03  HCM2CV03-ITBFTN2    PIC X(9).
            02  HCM2CV03-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2CV03-KENSU3 PIC X(6).
              03  HCM2CV03-YKZFTN3    PIC X(8).
              03  HCM2CV03-NISSU3 PIC X(6).
              03  HCM2CV03-TENSU3 PIC X(10).
            02  HCM2CV03-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2CV03-KENSU5 PIC X(6).
              03  HCM2CV03-NISSU5 PIC X(6).
              03  HCM2CV03-TENSU5 PIC X(10).
            02  HCM2CV03-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2CV03-KENSU4 PIC X(6).
              03  HCM2CV03-NISSU4 PIC X(6).
              03  HCM2CV03-TENSU4 PIC X(10).
              03  HCM2CV03-YKZFTN4    PIC X(8).
            02  HCM2CV03-KENSUG PIC X(6).
            02  HCM2CV03-TITLE  PIC X(50).

        01  HCM2CV02.
            02  HCM2CV02-PRTYM  PIC X(16).
            02  HCM2CV02-SEIYMD PIC X(22).
            02  HCM2CV02-HOSPCD PIC X(20).
            02  HCM2CV02-ADRS   PIC X(100).
            02  HCM2CV02-HOSPNAME   PIC X(100).
            02  HCM2CV02-KAISETUNAME    PIC X(30).
            02  HCM2CV02-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2CV02-KENSU1 PIC X(6).
              03  HCM2CV02-NISSU1 PIC X(6).
              03  HCM2CV02-TENSU1 PIC X(10).
              03  HCM2CV02-ITBFTN1    PIC X(9).
            02  HCM2CV02-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2CV02-KENSU2 PIC X(6).
              03  HCM2CV02-NISSU2 PIC X(6).
              03  HCM2CV02-TENSU2 PIC X(10).
              03  HCM2CV02-ITBFTN2    PIC X(9).
            02  HCM2CV02-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2CV02-KENSU3 PIC X(6).
              03  HCM2CV02-YKZFTN3    PIC X(8).
              03  HCM2CV02-NISSU3 PIC X(6).
              03  HCM2CV02-TENSU3 PIC X(10).
            02  HCM2CV02-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2CV02-KENSU5 PIC X(6).
              03  HCM2CV02-NISSU5 PIC X(6).
              03  HCM2CV02-TENSU5 PIC X(10).
            02  HCM2CV02-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2CV02-KENSU4 PIC X(6).
              03  HCM2CV02-NISSU4 PIC X(6).
              03  HCM2CV02-TENSU4 PIC X(10).
              03  HCM2CV02-YKZFTN4    PIC X(8).
            02  HCM2CV02-KENSUG PIC X(6).
            02  HCM2CV02-TITLE  PIC X(50).

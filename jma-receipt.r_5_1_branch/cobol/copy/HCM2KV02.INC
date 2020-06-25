        01  HCM2KV02.
            02  HCM2KV02-PRTYM  PIC X(16).
            02  HCM2KV02-SEIYMD PIC X(22).
            02  HCM2KV02-HOSPCD PIC X(20).
            02  HCM2KV02-ADRS   PIC X(100).
            02  HCM2KV02-HOSPNAME   PIC X(100).
            02  HCM2KV02-KAISETUNAME    PIC X(60).
            02  HCM2KV02-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2KV02-KENSU1 PIC X(6).
              03  HCM2KV02-NISSU1 PIC X(6).
              03  HCM2KV02-TENSU1 PIC X(10).
              03  HCM2KV02-ITBFTN1    PIC X(9).
            02  HCM2KV02-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2KV02-KENSU2 PIC X(6).
              03  HCM2KV02-NISSU2 PIC X(6).
              03  HCM2KV02-TENSU2 PIC X(10).
              03  HCM2KV02-ITBFTN2    PIC X(9).
            02  HCM2KV02-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2KV02-KENSU3 PIC X(6).
              03  HCM2KV02-NISSU3 PIC X(6).
              03  HCM2KV02-TENSU3 PIC X(10).
            02  HCM2KV02-ITBFTN3    PIC X(9).
            02  HCM2KV02-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2KV02-KENSU4 PIC X(6).
              03  HCM2KV02-NISSU4 PIC X(6).
              03  HCM2KV02-TENSU4 PIC X(10).
            02  HCM2KV02-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2KV02-KENSU5 PIC X(6).
              03  HCM2KV02-NISSU5 PIC X(6).
              03  HCM2KV02-TENSU5 PIC X(10).
            02  HCM2KV02-KENSUG PIC X(6).
            02  HCM2KV02-TITLE  PIC X(50).
            02  HCM2KV02-TITLE1 PIC X(10).

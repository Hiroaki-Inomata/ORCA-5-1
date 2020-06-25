        01  HCM2KV06.
            02  HCM2KV06-PRTYM  PIC X(16).
            02  HCM2KV06-SEIYMD PIC X(22).
            02  HCM2KV06-HOSPCD PIC X(20).
            02  HCM2KV06-ADRS   PIC X(100).
            02  HCM2KV06-HOSPNAME   PIC X(100).
            02  HCM2KV06-KAISETUNAME    PIC X(60).
            02  HCM2KV06-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2KV06-KENSU1 PIC X(6).
              03  HCM2KV06-NISSU1 PIC X(6).
              03  HCM2KV06-TENSU1 PIC X(10).
              03  HCM2KV06-ITBFTN1    PIC X(9).
            02  HCM2KV06-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2KV06-KENSU2 PIC X(6).
              03  HCM2KV06-NISSU2 PIC X(6).
              03  HCM2KV06-TENSU2 PIC X(10).
              03  HCM2KV06-ITBFTN2    PIC X(9).
            02  HCM2KV06-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2KV06-KENSU3 PIC X(6).
              03  HCM2KV06-NISSU3 PIC X(6).
              03  HCM2KV06-TENSU3 PIC X(10).
            02  HCM2KV06-ITBFTN3    PIC X(9).
            02  HCM2KV06-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2KV06-KENSU4 PIC X(6).
              03  HCM2KV06-NISSU4 PIC X(6).
              03  HCM2KV06-TENSU4 PIC X(10).
            02  HCM2KV06-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2KV06-KENSU5 PIC X(6).
              03  HCM2KV06-NISSU5 PIC X(6).
              03  HCM2KV06-TENSU5 PIC X(10).
            02  HCM2KV06-KENSUG PIC X(6).
            02  HCM2KV06-TITLE  PIC X(50).
            02  HCM2KV06-TITLE1 PIC X(10).
            02  HCM2KV06-SRYKA  PIC X(40).
            02  HCM2KV06-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2KV06-TITLE2 PIC X(2).
            02  HCM2KV06-TITLE3 PIC X(2).

        01  HCM2KV05.
            02  HCM2KV05-PRTYM  PIC X(16).
            02  HCM2KV05-SEIYMD PIC X(22).
            02  HCM2KV05-HOSPCD PIC X(20).
            02  HCM2KV05-ADRS   PIC X(100).
            02  HCM2KV05-HOSPNAME   PIC X(100).
            02  HCM2KV05-KAISETUNAME    PIC X(60).
            02  HCM2KV05-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2KV05-KENSU1 PIC X(6).
              03  HCM2KV05-NISSU1 PIC X(6).
              03  HCM2KV05-TENSU1 PIC X(10).
              03  HCM2KV05-ITBFTN1    PIC X(9).
            02  HCM2KV05-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2KV05-KENSU2 PIC X(6).
              03  HCM2KV05-NISSU2 PIC X(6).
              03  HCM2KV05-TENSU2 PIC X(10).
              03  HCM2KV05-ITBFTN2    PIC X(9).
            02  HCM2KV05-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2KV05-KENSU3 PIC X(6).
              03  HCM2KV05-NISSU3 PIC X(6).
              03  HCM2KV05-TENSU3 PIC X(10).
            02  HCM2KV05-ITBFTN3    PIC X(9).
            02  HCM2KV05-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2KV05-KENSU4 PIC X(6).
              03  HCM2KV05-NISSU4 PIC X(6).
              03  HCM2KV05-TENSU4 PIC X(10).
            02  HCM2KV05-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2KV05-KENSU5 PIC X(6).
              03  HCM2KV05-NISSU5 PIC X(6).
              03  HCM2KV05-TENSU5 PIC X(10).
            02  HCM2KV05-KENSUG PIC X(6).
            02  HCM2KV05-TITLE  PIC X(50).
            02  HCM2KV05-TITLE1 PIC X(10).
            02  HCM2KV05-SRYKA  PIC X(40).
            02  HCM2KV05-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2KV05-TITLE2 PIC X(2).
            02  HCM2KV05-TITLE3 PIC X(2).

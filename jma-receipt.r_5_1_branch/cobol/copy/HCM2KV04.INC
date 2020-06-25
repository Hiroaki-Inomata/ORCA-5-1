        01  HCM2KV04.
            02  HCM2KV04-PRTYM  PIC X(16).
            02  HCM2KV04-SEIYMD PIC X(22).
            02  HCM2KV04-HOSPCD PIC X(20).
            02  HCM2KV04-ADRS   PIC X(100).
            02  HCM2KV04-HOSPNAME   PIC X(100).
            02  HCM2KV04-KAISETUNAME    PIC X(60).
            02  HCM2KV04-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2KV04-KENSU1 PIC X(6).
              03  HCM2KV04-NISSU1 PIC X(6).
              03  HCM2KV04-TENSU1 PIC X(10).
              03  HCM2KV04-ITBFTN1    PIC X(9).
            02  HCM2KV04-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2KV04-KENSU2 PIC X(6).
              03  HCM2KV04-NISSU2 PIC X(6).
              03  HCM2KV04-TENSU2 PIC X(10).
              03  HCM2KV04-ITBFTN2    PIC X(9).
            02  HCM2KV04-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2KV04-KENSU3 PIC X(6).
              03  HCM2KV04-NISSU3 PIC X(6).
              03  HCM2KV04-TENSU3 PIC X(10).
            02  HCM2KV04-ITBFTN3    PIC X(9).
            02  HCM2KV04-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2KV04-KENSU4 PIC X(6).
              03  HCM2KV04-NISSU4 PIC X(6).
              03  HCM2KV04-TENSU4 PIC X(10).
            02  HCM2KV04-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2KV04-KENSU5 PIC X(6).
              03  HCM2KV04-NISSU5 PIC X(6).
              03  HCM2KV04-TENSU5 PIC X(10).
            02  HCM2KV04-KENSUG PIC X(6).
            02  HCM2KV04-TITLE  PIC X(50).
            02  HCM2KV04-TITLE1 PIC X(10).
            02  HCM2KV04-SRYKA  PIC X(40).
            02  HCM2KV04-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

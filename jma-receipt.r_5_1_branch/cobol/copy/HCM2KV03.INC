        01  HCM2KV03.
            02  HCM2KV03-PRTYM  PIC X(16).
            02  HCM2KV03-SEIYMD PIC X(22).
            02  HCM2KV03-HOSPCD PIC X(20).
            02  HCM2KV03-ADRS   PIC X(100).
            02  HCM2KV03-HOSPNAME   PIC X(100).
            02  HCM2KV03-KAISETUNAME    PIC X(60).
            02  HCM2KV03-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2KV03-KENSU1 PIC X(6).
              03  HCM2KV03-NISSU1 PIC X(6).
              03  HCM2KV03-TENSU1 PIC X(10).
              03  HCM2KV03-ITBFTN1    PIC X(9).
            02  HCM2KV03-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2KV03-KENSU2 PIC X(6).
              03  HCM2KV03-NISSU2 PIC X(6).
              03  HCM2KV03-TENSU2 PIC X(10).
              03  HCM2KV03-ITBFTN2    PIC X(9).
            02  HCM2KV03-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2KV03-KENSU3 PIC X(6).
              03  HCM2KV03-NISSU3 PIC X(6).
              03  HCM2KV03-TENSU3 PIC X(10).
            02  HCM2KV03-ITBFTN3    PIC X(9).
            02  HCM2KV03-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2KV03-KENSU4 PIC X(6).
              03  HCM2KV03-NISSU4 PIC X(6).
              03  HCM2KV03-TENSU4 PIC X(10).
            02  HCM2KV03-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2KV03-KENSU5 PIC X(6).
              03  HCM2KV03-NISSU5 PIC X(6).
              03  HCM2KV03-TENSU5 PIC X(10).
            02  HCM2KV03-KENSUG PIC X(6).
            02  HCM2KV03-TITLE  PIC X(50).
            02  HCM2KV03-TITLE1 PIC X(10).
            02  HCM2KV03-SRYKA  PIC X(40).

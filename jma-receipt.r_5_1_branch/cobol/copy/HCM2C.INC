        01  HCM2C.
            02  HCM2C-PRTYM PIC X(16).
            02  HCM2C-SEIYMD    PIC X(22).
            02  HCM2C-HOSPCD    PIC X(20).
            02  HCM2C-ADRS  PIC X(50).
            02  HCM2C-HOSPNAME  PIC X(100).
            02  HCM2C-KAISETUNAME   PIC X(30).
            02  HCM2C-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM2C-KENSU1    PIC X(6).
              03  HCM2C-NISSU1    PIC X(6).
              03  HCM2C-TENSU1    PIC X(10).
              03  HCM2C-ITBFTN1   PIC X(9).
            02  HCM2C-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM2C-KENSU2    PIC X(6).
              03  HCM2C-NISSU2    PIC X(6).
              03  HCM2C-TENSU2    PIC X(10).
              03  HCM2C-ITBFTN2   PIC X(9).
            02  HCM2C-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM2C-KENSU3    PIC X(6).
              03  HCM2C-YKZFTN3   PIC X(8).
              03  HCM2C-NISSU3    PIC X(6).
              03  HCM2C-TENSU3    PIC X(10).
            02  HCM2C-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM2C-KENSU5    PIC X(6).
              03  HCM2C-NISSU5    PIC X(6).
              03  HCM2C-TENSU5    PIC X(10).
            02  HCM2C-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM2C-KENSU4    PIC X(6).
              03  HCM2C-NISSU4    PIC X(6).
              03  HCM2C-TENSU4    PIC X(10).
              03  HCM2C-YKZFTN4   PIC X(8).
            02  HCM2C-KENSUG    PIC X(6).
            02  HCM2C-TITLE PIC X(36).

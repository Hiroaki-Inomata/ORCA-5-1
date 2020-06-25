        01  HCM2K.
            02  HCM2K-PRTYM PIC X(16).
            02  HCM2K-SEIYMD    PIC X(22).
            02  HCM2K-HOSPCD    PIC X(20).
            02  HCM2K-ADRS  PIC X(100).
            02  HCM2K-HOSPNAME  PIC X(100).
            02  HCM2K-KAISETUNAME   PIC X(30).
            02  HCM2K-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM2K-KENSU1    PIC X(6).
              03  HCM2K-NISSU1    PIC X(6).
              03  HCM2K-TENSU1    PIC X(10).
              03  HCM2K-ITBFTN1   PIC X(9).
            02  HCM2K-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM2K-KENSU2    PIC X(6).
              03  HCM2K-NISSU2    PIC X(6).
              03  HCM2K-TENSU2    PIC X(10).
              03  HCM2K-ITBFTN2   PIC X(9).
            02  HCM2K-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM2K-KENSU3    PIC X(6).
              03  HCM2K-NISSU3    PIC X(6).
              03  HCM2K-TENSU3    PIC X(10).
            02  HCM2K-ITBFTN3   PIC X(9).
            02  HCM2K-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM2K-KENSU4    PIC X(6).
              03  HCM2K-NISSU4    PIC X(6).
              03  HCM2K-TENSU4    PIC X(10).
            02  HCM2K-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM2K-KENSU5    PIC X(6).
              03  HCM2K-NISSU5    PIC X(6).
              03  HCM2K-TENSU5    PIC X(10).
            02  HCM2K-KENSUG    PIC X(6).
            02  HCM2K-TITLE PIC X(50).

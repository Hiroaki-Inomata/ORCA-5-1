        01  HCM3C.
            02  HCM3C-PRTYM PIC X(16).
            02  HCM3C-SEIYMD    PIC X(22).
            02  HCM3C-HOSPCD    PIC X(20).
            02  HCM3C-ADRS  PIC X(100).
            02  HCM3C-HOSPNAME  PIC X(100).
            02  HCM3C-KAISETUNAME   PIC X(60).
            02  HCM3C-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM3C-KENSU1    PIC X(6).
              03  HCM3C-NISSU1    PIC X(6).
              03  HCM3C-TENSU1    PIC X(10).
              03  HCM3C-ITBFTN1   PIC X(9).
            02  HCM3C-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM3C-KENSU2    PIC X(6).
              03  HCM3C-NISSU2    PIC X(6).
              03  HCM3C-TENSU2    PIC X(10).
              03  HCM3C-ITBFTN2   PIC X(9).
            02  HCM3C-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM3C-KENSU3    PIC X(6).
              03  HCM3C-NISSU3    PIC X(6).
              03  HCM3C-TENSU3    PIC X(10).
            02  HCM3C-ITBFTN3   PIC X(9).
            02  HCM3C-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM3C-KENSU4    PIC X(6).
              03  HCM3C-NISSU4    PIC X(6).
              03  HCM3C-TENSU4    PIC X(10).
            02  HCM3C-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM3C-KENSU5    PIC X(6).
              03  HCM3C-NISSU5    PIC X(6).
              03  HCM3C-TENSU5    PIC X(10).
            02  HCM3C-KENSUG    PIC X(6).
            02  HCM3C-TITLE PIC X(50).
            02  HCM3C-TITLE1    PIC X(10).
            02  HCM3C-SRYKA PIC X(40).
            02  HCM3C-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

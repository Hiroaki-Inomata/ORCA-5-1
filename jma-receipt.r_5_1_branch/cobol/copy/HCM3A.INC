        01  HCM3A.
            02  HCM3A-PRTYM PIC X(16).
            02  HCM3A-SEIYMD    PIC X(22).
            02  HCM3A-HOSPCD    PIC X(20).
            02  HCM3A-ADRS  PIC X(100).
            02  HCM3A-HOSPNAME  PIC X(100).
            02  HCM3A-KAISETUNAME   PIC X(60).
            02  HCM3A-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM3A-KENSU1    PIC X(6).
              03  HCM3A-NISSU1    PIC X(6).
              03  HCM3A-TENSU1    PIC X(13).
              03  HCM3A-ITBFTN1   PIC X(13).
              03  HCM3A-SYOKENSU1 PIC X(6).
              03  HCM3A-SYONISSU1 PIC X(6).
              03  HCM3A-SYOKINGAK1    PIC X(13).
              03  HCM3A-SYOFTNGAK1    PIC X(13).
            02  HCM3A-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM3A-KENSU2    PIC X(6).
              03  HCM3A-NISSU2    PIC X(6).
              03  HCM3A-TENSU2    PIC X(13).
              03  HCM3A-ITBFTN2   PIC X(13).
              03  HCM3A-SYOKENSU2 PIC X(6).
              03  HCM3A-SYONISSU2 PIC X(6).
              03  HCM3A-SYOKINGAK2    PIC X(13).
              03  HCM3A-SYOFTNGAK2    PIC X(13).
            02  HCM3A-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM3A-KENSU3    PIC X(6).
              03  HCM3A-NISSU3    PIC X(6).
              03  HCM3A-TENSU3    PIC X(13).
              03  HCM3A-SYOKENSU3 PIC X(6).
              03  HCM3A-SYONISSU3 PIC X(6).
              03  HCM3A-SYOKINGAK3    PIC X(13).
              03  HCM3A-SYOFTNGAK3    PIC X(13).
            02  HCM3A-ITBFTN3   PIC X(13).
            02  HCM3A-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM3A-KENSU4    PIC X(6).
              03  HCM3A-NISSU4    PIC X(6).
              03  HCM3A-TENSU4    PIC X(13).
              03  HCM3A-SYOKENSU4 PIC X(6).
              03  HCM3A-SYONISSU4 PIC X(6).
              03  HCM3A-SYOKINGAK4    PIC X(13).
              03  HCM3A-SYOFTNGAK4    PIC X(13).
            02  HCM3A-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM3A-KENSU5    PIC X(6).
              03  HCM3A-NISSU5    PIC X(6).
              03  HCM3A-TENSU5    PIC X(13).
              03  HCM3A-SYOKENSU5 PIC X(6).
              03  HCM3A-SYONISSU5 PIC X(6).
              03  HCM3A-SYOKINGAK5    PIC X(13).
              03  HCM3A-SYOFTNGAK5    PIC X(13).
            02  HCM3A-KENSUG    PIC X(6).
            02  HCM3A-SYOKENSUG PIC X(6).
            02  HCM3A-TITLE PIC X(50).
            02  HCM3A-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3A-TITLE1    PIC X(10).
            02  HCM3A-SRYKA PIC X(40).
            02  HCM3A-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

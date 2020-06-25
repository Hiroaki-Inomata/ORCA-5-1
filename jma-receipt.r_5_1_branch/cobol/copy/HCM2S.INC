        01  HCM2S.
            02  HCM2S-PRTYM PIC X(16).
            02  HCM2S-SEIYMD    PIC X(22).
            02  HCM2S-HOSPCD    PIC X(20).
            02  HCM2S-ADRS  PIC X(100).
            02  HCM2S-HOSPNAME  PIC X(100).
            02  HCM2S-KAISETUNAME   PIC X(60).
            02  HCM2S-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM2S-KENSU1    PIC X(6).
              03  HCM2S-NISSU1    PIC X(6).
              03  HCM2S-TENSU1    PIC X(13).
              03  HCM2S-ITBFTN1   PIC X(13).
              03  HCM2S-SYOKENSU1 PIC X(6).
              03  HCM2S-SYONISSU1 PIC X(6).
              03  HCM2S-SYOKINGAK1    PIC X(13).
              03  HCM2S-SYOFTNGAK1    PIC X(13).
            02  HCM2S-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM2S-KENSU2    PIC X(6).
              03  HCM2S-NISSU2    PIC X(6).
              03  HCM2S-TENSU2    PIC X(13).
              03  HCM2S-ITBFTN2   PIC X(13).
              03  HCM2S-SYOKENSU2 PIC X(6).
              03  HCM2S-SYONISSU2 PIC X(6).
              03  HCM2S-SYOKINGAK2    PIC X(13).
              03  HCM2S-SYOFTNGAK2    PIC X(13).
            02  HCM2S-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM2S-KENSU3    PIC X(6).
              03  HCM2S-NISSU3    PIC X(6).
              03  HCM2S-TENSU3    PIC X(13).
              03  HCM2S-SYOKENSU3 PIC X(6).
              03  HCM2S-SYONISSU3 PIC X(6).
              03  HCM2S-SYOKINGAK3    PIC X(13).
              03  HCM2S-SYOFTNGAK3    PIC X(13).
            02  HCM2S-ITBFTN3   PIC X(13).
            02  HCM2S-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM2S-KENSU4    PIC X(6).
              03  HCM2S-NISSU4    PIC X(6).
              03  HCM2S-TENSU4    PIC X(13).
              03  HCM2S-SYOKENSU4 PIC X(6).
              03  HCM2S-SYONISSU4 PIC X(6).
              03  HCM2S-SYOKINGAK4    PIC X(13).
              03  HCM2S-SYOFTNGAK4    PIC X(13).
            02  HCM2S-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM2S-KENSU5    PIC X(6).
              03  HCM2S-NISSU5    PIC X(6).
              03  HCM2S-TENSU5    PIC X(13).
              03  HCM2S-SYOKENSU5 PIC X(6).
              03  HCM2S-SYONISSU5 PIC X(6).
              03  HCM2S-SYOKINGAK5    PIC X(13).
              03  HCM2S-SYOFTNGAK5    PIC X(13).
            02  HCM2S-KENSUG    PIC X(6).
            02  HCM2S-SYOKENSUG PIC X(6).
            02  HCM2S-TITLE PIC X(50).
            02  HCM2S-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2S-TITLE1    PIC X(10).
            02  HCM2S-SRYKA PIC X(40).
            02  HCM2S-TITLE2    PIC X(4).

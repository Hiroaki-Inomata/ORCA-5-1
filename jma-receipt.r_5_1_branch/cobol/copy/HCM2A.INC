        01  HCM2A.
            02  HCM2A-PRTYM PIC X(16).
            02  HCM2A-SEIYMD    PIC X(22).
            02  HCM2A-HOSPCD    PIC X(20).
            02  HCM2A-ADRS  PIC X(100).
            02  HCM2A-HOSPNAME  PIC X(100).
            02  HCM2A-KAISETUNAME   PIC X(30).
            02  HCM2A-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM2A-KENSU1    PIC X(6).
              03  HCM2A-NISSU1    PIC X(6).
              03  HCM2A-TENSU1    PIC X(13).
              03  HCM2A-ITBFTN1   PIC X(13).
              03  HCM2A-SYOKENSU1 PIC X(6).
              03  HCM2A-SYONISSU1 PIC X(6).
              03  HCM2A-SYOKINGAK1    PIC X(13).
              03  HCM2A-SYOFTNGAK1    PIC X(13).
            02  HCM2A-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM2A-KENSU2    PIC X(6).
              03  HCM2A-NISSU2    PIC X(6).
              03  HCM2A-TENSU2    PIC X(13).
              03  HCM2A-ITBFTN2   PIC X(13).
              03  HCM2A-SYOKENSU2 PIC X(6).
              03  HCM2A-SYONISSU2 PIC X(6).
              03  HCM2A-SYOKINGAK2    PIC X(13).
              03  HCM2A-SYOFTNGAK2    PIC X(13).
            02  HCM2A-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM2A-KENSU3    PIC X(6).
              03  HCM2A-NISSU3    PIC X(6).
              03  HCM2A-TENSU3    PIC X(13).
              03  HCM2A-YKZFTN3   PIC X(8).
              03  HCM2A-SYOKENSU3 PIC X(6).
              03  HCM2A-SYONISSU3 PIC X(6).
              03  HCM2A-SYOKINGAK3    PIC X(13).
              03  HCM2A-SYOFTNGAK3    PIC X(13).
            02  HCM2A-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM2A-KENSU4    PIC X(6).
              03  HCM2A-NISSU4    PIC X(6).
              03  HCM2A-TENSU4    PIC X(13).
              03  HCM2A-YKZFTN4   PIC X(8).
              03  HCM2A-SYOKENSU4 PIC X(6).
              03  HCM2A-SYONISSU4 PIC X(6).
              03  HCM2A-SYOKINGAK4    PIC X(13).
              03  HCM2A-SYOFTNGAK4    PIC X(13).
            02  HCM2A-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM2A-KENSU5    PIC X(6).
              03  HCM2A-NISSU5    PIC X(6).
              03  HCM2A-TENSU5    PIC X(13).
              03  HCM2A-YKZFTN5   PIC X(8).
              03  HCM2A-SYOKENSU5 PIC X(6).
              03  HCM2A-SYONISSU5 PIC X(6).
              03  HCM2A-SYOKINGAK5    PIC X(13).
              03  HCM2A-SYOFTNGAK5    PIC X(13).
            02  HCM2A-KENSUG    PIC X(6).
            02  HCM2A-SYOKENSUG PIC X(6).
            02  HCM2A-TITLE PIC X(36).
            02  HCM2A-NYUGAIKBN PIC X(2)        OCCURS  2 TIMES.

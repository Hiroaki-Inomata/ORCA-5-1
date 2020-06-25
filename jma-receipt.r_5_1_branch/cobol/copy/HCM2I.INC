        01  HCM2I.
            02  HCM2I-PRTYM PIC X(16).
            02  HCM2I-SEIYMD    PIC X(22).
            02  HCM2I-HOSPCD    PIC X(20).
            02  HCM2I-ADRS  PIC X(100).
            02  HCM2I-HOSPNAME  PIC X(100).
            02  HCM2I-KAISETUNAME   PIC X(30).
            02  HCM2I-IHO-TBL1          OCCURS  11 TIMES.
              03  HCM2I-KENSU1    PIC X(6).
              03  HCM2I-NISSU1    PIC X(6).
              03  HCM2I-TENSU1    PIC X(13).
              03  HCM2I-ITBFTN1   PIC X(13).
              03  HCM2I-SYOKENSU1 PIC X(6).
              03  HCM2I-SYONISSU1 PIC X(6).
              03  HCM2I-SYOKINGAK1    PIC X(13).
              03  HCM2I-SYOFTNGAK1    PIC X(13).
            02  HCM2I-IHO-TBL2          OCCURS  9 TIMES.
              03  HCM2I-KENSU2    PIC X(6).
              03  HCM2I-NISSU2    PIC X(6).
              03  HCM2I-TENSU2    PIC X(13).
              03  HCM2I-ITBFTN2   PIC X(13).
              03  HCM2I-SYOKENSU2 PIC X(6).
              03  HCM2I-SYONISSU2 PIC X(6).
              03  HCM2I-SYOKINGAK2    PIC X(13).
              03  HCM2I-SYOFTNGAK2    PIC X(13).
            02  HCM2I-IHO-TBL3          OCCURS  12 TIMES.
              03  HCM2I-KENSU3    PIC X(6).
              03  HCM2I-NISSU3    PIC X(6).
              03  HCM2I-TENSU3    PIC X(13).
              03  HCM2I-SYOKENSU3 PIC X(6).
              03  HCM2I-SYONISSU3 PIC X(6).
              03  HCM2I-SYOKINGAK3    PIC X(13).
              03  HCM2I-SYOFTNGAK3    PIC X(13).
            02  HCM2I-ITBFTN3   PIC X(13).
            02  HCM2I-IHO-TBL4          OCCURS  9 TIMES.
              03  HCM2I-KENSU4    PIC X(6).
              03  HCM2I-NISSU4    PIC X(6).
              03  HCM2I-TENSU4    PIC X(13).
              03  HCM2I-SYOKENSU4 PIC X(6).
              03  HCM2I-SYONISSU4 PIC X(6).
              03  HCM2I-SYOKINGAK4    PIC X(13).
              03  HCM2I-SYOFTNGAK4    PIC X(13).
            02  HCM2I-IHO-TBL5          OCCURS  9 TIMES.
              03  HCM2I-KENSU5    PIC X(6).
              03  HCM2I-NISSU5    PIC X(6).
              03  HCM2I-TENSU5    PIC X(13).
              03  HCM2I-SYOKENSU5 PIC X(6).
              03  HCM2I-SYONISSU5 PIC X(6).
              03  HCM2I-SYOKINGAK5    PIC X(13).
              03  HCM2I-SYOFTNGAK5    PIC X(13).
            02  HCM2I-KENSUG    PIC X(6).
            02  HCM2I-SYOKENSUG PIC X(6).
            02  HCM2I-TITLE PIC X(50).
            02  HCM2I-NYUGAIKBN PIC X(2)        OCCURS  2 TIMES.

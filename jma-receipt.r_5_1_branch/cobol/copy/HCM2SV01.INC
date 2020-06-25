        01  HCM2SV01.
            02  HCM2SV01-PRTYM  PIC X(16).
            02  HCM2SV01-SEIYMD PIC X(22).
            02  HCM2SV01-HOSPCD PIC X(20).
            02  HCM2SV01-ADRS   PIC X(100).
            02  HCM2SV01-HOSPNAME   PIC X(100).
            02  HCM2SV01-KAISETUNAME    PIC X(60).
            02  HCM2SV01-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2SV01-KENSU1 PIC X(6).
              03  HCM2SV01-NISSU1 PIC X(6).
              03  HCM2SV01-TENSU1 PIC X(13).
              03  HCM2SV01-ITBFTN1    PIC X(13).
              03  HCM2SV01-SYOKENSU1  PIC X(6).
              03  HCM2SV01-SYONISSU1  PIC X(6).
              03  HCM2SV01-SYOKINGAK1 PIC X(13).
              03  HCM2SV01-SYOFTNGAK1 PIC X(13).
            02  HCM2SV01-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2SV01-KENSU2 PIC X(6).
              03  HCM2SV01-NISSU2 PIC X(6).
              03  HCM2SV01-TENSU2 PIC X(13).
              03  HCM2SV01-ITBFTN2    PIC X(13).
              03  HCM2SV01-SYOKENSU2  PIC X(6).
              03  HCM2SV01-SYONISSU2  PIC X(6).
              03  HCM2SV01-SYOKINGAK2 PIC X(13).
              03  HCM2SV01-SYOFTNGAK2 PIC X(13).
            02  HCM2SV01-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2SV01-KENSU3 PIC X(6).
              03  HCM2SV01-NISSU3 PIC X(6).
              03  HCM2SV01-TENSU3 PIC X(13).
              03  HCM2SV01-SYOKENSU3  PIC X(6).
              03  HCM2SV01-SYONISSU3  PIC X(6).
              03  HCM2SV01-SYOKINGAK3 PIC X(13).
              03  HCM2SV01-SYOFTNGAK3 PIC X(13).
            02  HCM2SV01-ITBFTN3    PIC X(13).
            02  HCM2SV01-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2SV01-KENSU4 PIC X(6).
              03  HCM2SV01-NISSU4 PIC X(6).
              03  HCM2SV01-TENSU4 PIC X(13).
              03  HCM2SV01-SYOKENSU4  PIC X(6).
              03  HCM2SV01-SYONISSU4  PIC X(6).
              03  HCM2SV01-SYOKINGAK4 PIC X(13).
              03  HCM2SV01-SYOFTNGAK4 PIC X(13).
            02  HCM2SV01-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2SV01-KENSU5 PIC X(6).
              03  HCM2SV01-NISSU5 PIC X(6).
              03  HCM2SV01-TENSU5 PIC X(13).
              03  HCM2SV01-SYOKENSU5  PIC X(6).
              03  HCM2SV01-SYONISSU5  PIC X(6).
              03  HCM2SV01-SYOKINGAK5 PIC X(13).
              03  HCM2SV01-SYOFTNGAK5 PIC X(13).
            02  HCM2SV01-KENSUG PIC X(6).
            02  HCM2SV01-SYOKENSUG  PIC X(6).
            02  HCM2SV01-TITLE  PIC X(50).
            02  HCM2SV01-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2SV01-TITLE1 PIC X(10).
            02  HCM2SV01-SRYKA  PIC X(40).
            02  HCM2SV01-TITLE2 PIC X(4).
            02  HCM2SV01-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

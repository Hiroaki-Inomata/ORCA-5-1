        01  HCM2SV03.
            02  HCM2SV03-PRTYM  PIC X(16).
            02  HCM2SV03-SEIYMD PIC X(22).
            02  HCM2SV03-HOSPCD PIC X(20).
            02  HCM2SV03-ADRS   PIC X(100).
            02  HCM2SV03-HOSPNAME   PIC X(100).
            02  HCM2SV03-KAISETUNAME    PIC X(60).
            02  HCM2SV03-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2SV03-KENSU1 PIC X(6).
              03  HCM2SV03-NISSU1 PIC X(6).
              03  HCM2SV03-TENSU1 PIC X(13).
              03  HCM2SV03-ITBFTN1    PIC X(13).
              03  HCM2SV03-SYOKENSU1  PIC X(6).
              03  HCM2SV03-SYONISSU1  PIC X(6).
              03  HCM2SV03-SYOKINGAK1 PIC X(13).
              03  HCM2SV03-SYOFTNGAK1 PIC X(13).
            02  HCM2SV03-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2SV03-KENSU2 PIC X(6).
              03  HCM2SV03-NISSU2 PIC X(6).
              03  HCM2SV03-TENSU2 PIC X(13).
              03  HCM2SV03-ITBFTN2    PIC X(13).
              03  HCM2SV03-SYOKENSU2  PIC X(6).
              03  HCM2SV03-SYONISSU2  PIC X(6).
              03  HCM2SV03-SYOKINGAK2 PIC X(13).
              03  HCM2SV03-SYOFTNGAK2 PIC X(13).
            02  HCM2SV03-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2SV03-KENSU3 PIC X(6).
              03  HCM2SV03-NISSU3 PIC X(6).
              03  HCM2SV03-TENSU3 PIC X(13).
              03  HCM2SV03-SYOKENSU3  PIC X(6).
              03  HCM2SV03-SYONISSU3  PIC X(6).
              03  HCM2SV03-SYOKINGAK3 PIC X(13).
              03  HCM2SV03-SYOFTNGAK3 PIC X(13).
            02  HCM2SV03-ITBFTN3    PIC X(13).
            02  HCM2SV03-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2SV03-KENSU4 PIC X(6).
              03  HCM2SV03-NISSU4 PIC X(6).
              03  HCM2SV03-TENSU4 PIC X(13).
              03  HCM2SV03-SYOKENSU4  PIC X(6).
              03  HCM2SV03-SYONISSU4  PIC X(6).
              03  HCM2SV03-SYOKINGAK4 PIC X(13).
              03  HCM2SV03-SYOFTNGAK4 PIC X(13).
            02  HCM2SV03-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2SV03-KENSU5 PIC X(6).
              03  HCM2SV03-NISSU5 PIC X(6).
              03  HCM2SV03-TENSU5 PIC X(13).
              03  HCM2SV03-SYOKENSU5  PIC X(6).
              03  HCM2SV03-SYONISSU5  PIC X(6).
              03  HCM2SV03-SYOKINGAK5 PIC X(13).
              03  HCM2SV03-SYOFTNGAK5 PIC X(13).
            02  HCM2SV03-KENSUG PIC X(6).
            02  HCM2SV03-SYOKENSUG  PIC X(6).
            02  HCM2SV03-TITLE  PIC X(50).
            02  HCM2SV03-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2SV03-TITLE1 PIC X(10).
            02  HCM2SV03-SRYKA  PIC X(40).
            02  HCM2SV03-TITLE2 PIC X(4).
            02  HCM2SV03-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2SV03-TITLE3 PIC X(18).
            02  HCM2SV03-TITLE4 PIC X(2).
            02  HCM2SV03-TITLE5 PIC X(2).

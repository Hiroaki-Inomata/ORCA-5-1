        01  HCM2SV02.
            02  HCM2SV02-PRTYM  PIC X(16).
            02  HCM2SV02-SEIYMD PIC X(22).
            02  HCM2SV02-HOSPCD PIC X(20).
            02  HCM2SV02-ADRS   PIC X(100).
            02  HCM2SV02-HOSPNAME   PIC X(100).
            02  HCM2SV02-KAISETUNAME    PIC X(60).
            02  HCM2SV02-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2SV02-KENSU1 PIC X(6).
              03  HCM2SV02-NISSU1 PIC X(6).
              03  HCM2SV02-TENSU1 PIC X(13).
              03  HCM2SV02-ITBFTN1    PIC X(13).
              03  HCM2SV02-SYOKENSU1  PIC X(6).
              03  HCM2SV02-SYONISSU1  PIC X(6).
              03  HCM2SV02-SYOKINGAK1 PIC X(13).
              03  HCM2SV02-SYOFTNGAK1 PIC X(13).
            02  HCM2SV02-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2SV02-KENSU2 PIC X(6).
              03  HCM2SV02-NISSU2 PIC X(6).
              03  HCM2SV02-TENSU2 PIC X(13).
              03  HCM2SV02-ITBFTN2    PIC X(13).
              03  HCM2SV02-SYOKENSU2  PIC X(6).
              03  HCM2SV02-SYONISSU2  PIC X(6).
              03  HCM2SV02-SYOKINGAK2 PIC X(13).
              03  HCM2SV02-SYOFTNGAK2 PIC X(13).
            02  HCM2SV02-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2SV02-KENSU3 PIC X(6).
              03  HCM2SV02-NISSU3 PIC X(6).
              03  HCM2SV02-TENSU3 PIC X(13).
              03  HCM2SV02-SYOKENSU3  PIC X(6).
              03  HCM2SV02-SYONISSU3  PIC X(6).
              03  HCM2SV02-SYOKINGAK3 PIC X(13).
              03  HCM2SV02-SYOFTNGAK3 PIC X(13).
            02  HCM2SV02-ITBFTN3    PIC X(13).
            02  HCM2SV02-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2SV02-KENSU4 PIC X(6).
              03  HCM2SV02-NISSU4 PIC X(6).
              03  HCM2SV02-TENSU4 PIC X(13).
              03  HCM2SV02-SYOKENSU4  PIC X(6).
              03  HCM2SV02-SYONISSU4  PIC X(6).
              03  HCM2SV02-SYOKINGAK4 PIC X(13).
              03  HCM2SV02-SYOFTNGAK4 PIC X(13).
            02  HCM2SV02-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2SV02-KENSU5 PIC X(6).
              03  HCM2SV02-NISSU5 PIC X(6).
              03  HCM2SV02-TENSU5 PIC X(13).
              03  HCM2SV02-SYOKENSU5  PIC X(6).
              03  HCM2SV02-SYONISSU5  PIC X(6).
              03  HCM2SV02-SYOKINGAK5 PIC X(13).
              03  HCM2SV02-SYOFTNGAK5 PIC X(13).
            02  HCM2SV02-KENSUG PIC X(6).
            02  HCM2SV02-SYOKENSUG  PIC X(6).
            02  HCM2SV02-TITLE  PIC X(50).
            02  HCM2SV02-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2SV02-TITLE1 PIC X(10).
            02  HCM2SV02-SRYKA  PIC X(40).
            02  HCM2SV02-TITLE2 PIC X(4).
            02  HCM2SV02-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2SV02-TITLE3 PIC X(18).
            02  HCM2SV02-TITLE4 PIC X(2).
            02  HCM2SV02-TITLE5 PIC X(2).

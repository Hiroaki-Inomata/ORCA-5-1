        01  HCM2AV02.
            02  HCM2AV02-PRTYM  PIC X(16).
            02  HCM2AV02-SEIYMD PIC X(22).
            02  HCM2AV02-HOSPCD PIC X(20).
            02  HCM2AV02-ADRS   PIC X(100).
            02  HCM2AV02-HOSPNAME   PIC X(100).
            02  HCM2AV02-KAISETUNAME    PIC X(60).
            02  HCM2AV02-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2AV02-KENSU1 PIC X(6).
              03  HCM2AV02-NISSU1 PIC X(6).
              03  HCM2AV02-TENSU1 PIC X(13).
              03  HCM2AV02-ITBFTN1    PIC X(13).
              03  HCM2AV02-SYOKENSU1  PIC X(6).
              03  HCM2AV02-SYONISSU1  PIC X(6).
              03  HCM2AV02-SYOKINGAK1 PIC X(13).
              03  HCM2AV02-SYOFTNGAK1 PIC X(13).
            02  HCM2AV02-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2AV02-KENSU2 PIC X(6).
              03  HCM2AV02-NISSU2 PIC X(6).
              03  HCM2AV02-TENSU2 PIC X(13).
              03  HCM2AV02-ITBFTN2    PIC X(13).
              03  HCM2AV02-SYOKENSU2  PIC X(6).
              03  HCM2AV02-SYONISSU2  PIC X(6).
              03  HCM2AV02-SYOKINGAK2 PIC X(13).
              03  HCM2AV02-SYOFTNGAK2 PIC X(13).
            02  HCM2AV02-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2AV02-KENSU3 PIC X(6).
              03  HCM2AV02-NISSU3 PIC X(6).
              03  HCM2AV02-TENSU3 PIC X(13).
              03  HCM2AV02-YKZFTN3    PIC X(8).
              03  HCM2AV02-SYOKENSU3  PIC X(6).
              03  HCM2AV02-SYONISSU3  PIC X(6).
              03  HCM2AV02-SYOKINGAK3 PIC X(13).
              03  HCM2AV02-SYOFTNGAK3 PIC X(13).
            02  HCM2AV02-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2AV02-KENSU4 PIC X(6).
              03  HCM2AV02-NISSU4 PIC X(6).
              03  HCM2AV02-TENSU4 PIC X(13).
              03  HCM2AV02-YKZFTN4    PIC X(8).
              03  HCM2AV02-SYOKENSU4  PIC X(6).
              03  HCM2AV02-SYONISSU4  PIC X(6).
              03  HCM2AV02-SYOKINGAK4 PIC X(13).
              03  HCM2AV02-SYOFTNGAK4 PIC X(13).
            02  HCM2AV02-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2AV02-KENSU5 PIC X(6).
              03  HCM2AV02-NISSU5 PIC X(6).
              03  HCM2AV02-TENSU5 PIC X(13).
              03  HCM2AV02-YKZFTN5    PIC X(8).
              03  HCM2AV02-SYOKENSU5  PIC X(6).
              03  HCM2AV02-SYONISSU5  PIC X(6).
              03  HCM2AV02-SYOKINGAK5 PIC X(13).
              03  HCM2AV02-SYOFTNGAK5 PIC X(13).
            02  HCM2AV02-KENSUG PIC X(6).
            02  HCM2AV02-SYOKENSUG  PIC X(6).
            02  HCM2AV02-TITLE  PIC X(50).
            02  HCM2AV02-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.

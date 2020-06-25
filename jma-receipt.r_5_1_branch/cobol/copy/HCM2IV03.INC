        01  HCM2IV03.
            02  HCM2IV03-PRTYM  PIC X(16).
            02  HCM2IV03-SEIYMD PIC X(22).
            02  HCM2IV03-HOSPCD PIC X(20).
            02  HCM2IV03-ADRS   PIC X(100).
            02  HCM2IV03-HOSPNAME   PIC X(100).
            02  HCM2IV03-KAISETUNAME    PIC X(60).
            02  HCM2IV03-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2IV03-KENSU1 PIC X(6).
              03  HCM2IV03-NISSU1 PIC X(6).
              03  HCM2IV03-TENSU1 PIC X(13).
              03  HCM2IV03-ITBFTN1    PIC X(13).
              03  HCM2IV03-SYOKENSU1  PIC X(6).
              03  HCM2IV03-SYONISSU1  PIC X(6).
              03  HCM2IV03-SYOKINGAK1 PIC X(13).
              03  HCM2IV03-SYOFTNGAK1 PIC X(13).
            02  HCM2IV03-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2IV03-KENSU2 PIC X(6).
              03  HCM2IV03-NISSU2 PIC X(6).
              03  HCM2IV03-TENSU2 PIC X(13).
              03  HCM2IV03-ITBFTN2    PIC X(13).
              03  HCM2IV03-SYOKENSU2  PIC X(6).
              03  HCM2IV03-SYONISSU2  PIC X(6).
              03  HCM2IV03-SYOKINGAK2 PIC X(13).
              03  HCM2IV03-SYOFTNGAK2 PIC X(13).
            02  HCM2IV03-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2IV03-KENSU3 PIC X(6).
              03  HCM2IV03-NISSU3 PIC X(6).
              03  HCM2IV03-TENSU3 PIC X(13).
              03  HCM2IV03-SYOKENSU3  PIC X(6).
              03  HCM2IV03-SYONISSU3  PIC X(6).
              03  HCM2IV03-SYOKINGAK3 PIC X(13).
              03  HCM2IV03-SYOFTNGAK3 PIC X(13).
            02  HCM2IV03-ITBFTN3    PIC X(13).
            02  HCM2IV03-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2IV03-KENSU4 PIC X(6).
              03  HCM2IV03-NISSU4 PIC X(6).
              03  HCM2IV03-TENSU4 PIC X(13).
              03  HCM2IV03-SYOKENSU4  PIC X(6).
              03  HCM2IV03-SYONISSU4  PIC X(6).
              03  HCM2IV03-SYOKINGAK4 PIC X(13).
              03  HCM2IV03-SYOFTNGAK4 PIC X(13).
            02  HCM2IV03-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2IV03-KENSU5 PIC X(6).
              03  HCM2IV03-NISSU5 PIC X(6).
              03  HCM2IV03-TENSU5 PIC X(13).
              03  HCM2IV03-SYOKENSU5  PIC X(6).
              03  HCM2IV03-SYONISSU5  PIC X(6).
              03  HCM2IV03-SYOKINGAK5 PIC X(13).
              03  HCM2IV03-SYOFTNGAK5 PIC X(13).
            02  HCM2IV03-KENSUG PIC X(6).
            02  HCM2IV03-SYOKENSUG  PIC X(6).
            02  HCM2IV03-TITLE  PIC X(50).
            02  HCM2IV03-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2IV03-TITLE1 PIC X(10).
            02  HCM2IV03-SRYKA  PIC X(40).

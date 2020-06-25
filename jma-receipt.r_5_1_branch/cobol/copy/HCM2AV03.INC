        01  HCM2AV03.
            02  HCM2AV03-PRTYM  PIC X(16).
            02  HCM2AV03-SEIYMD PIC X(22).
            02  HCM2AV03-HOSPCD PIC X(20).
            02  HCM2AV03-ADRS   PIC X(100).
            02  HCM2AV03-HOSPNAME   PIC X(100).
            02  HCM2AV03-KAISETUNAME    PIC X(60).
            02  HCM2AV03-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2AV03-KENSU1 PIC X(6).
              03  HCM2AV03-NISSU1 PIC X(6).
              03  HCM2AV03-TENSU1 PIC X(13).
              03  HCM2AV03-ITBFTN1    PIC X(13).
              03  HCM2AV03-SYOKENSU1  PIC X(6).
              03  HCM2AV03-SYONISSU1  PIC X(6).
              03  HCM2AV03-SYOKINGAK1 PIC X(13).
              03  HCM2AV03-SYOFTNGAK1 PIC X(13).
            02  HCM2AV03-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2AV03-KENSU2 PIC X(6).
              03  HCM2AV03-NISSU2 PIC X(6).
              03  HCM2AV03-TENSU2 PIC X(13).
              03  HCM2AV03-ITBFTN2    PIC X(13).
              03  HCM2AV03-SYOKENSU2  PIC X(6).
              03  HCM2AV03-SYONISSU2  PIC X(6).
              03  HCM2AV03-SYOKINGAK2 PIC X(13).
              03  HCM2AV03-SYOFTNGAK2 PIC X(13).
            02  HCM2AV03-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2AV03-KENSU3 PIC X(6).
              03  HCM2AV03-NISSU3 PIC X(6).
              03  HCM2AV03-TENSU3 PIC X(13).
              03  HCM2AV03-YKZFTN3    PIC X(8).
              03  HCM2AV03-SYOKENSU3  PIC X(6).
              03  HCM2AV03-SYONISSU3  PIC X(6).
              03  HCM2AV03-SYOKINGAK3 PIC X(13).
              03  HCM2AV03-SYOFTNGAK3 PIC X(13).
            02  HCM2AV03-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2AV03-KENSU4 PIC X(6).
              03  HCM2AV03-NISSU4 PIC X(6).
              03  HCM2AV03-TENSU4 PIC X(13).
              03  HCM2AV03-YKZFTN4    PIC X(8).
              03  HCM2AV03-SYOKENSU4  PIC X(6).
              03  HCM2AV03-SYONISSU4  PIC X(6).
              03  HCM2AV03-SYOKINGAK4 PIC X(13).
              03  HCM2AV03-SYOFTNGAK4 PIC X(13).
            02  HCM2AV03-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2AV03-KENSU5 PIC X(6).
              03  HCM2AV03-NISSU5 PIC X(6).
              03  HCM2AV03-TENSU5 PIC X(13).
              03  HCM2AV03-YKZFTN5    PIC X(8).
              03  HCM2AV03-SYOKENSU5  PIC X(6).
              03  HCM2AV03-SYONISSU5  PIC X(6).
              03  HCM2AV03-SYOKINGAK5 PIC X(13).
              03  HCM2AV03-SYOFTNGAK5 PIC X(13).
            02  HCM2AV03-KENSUG PIC X(6).
            02  HCM2AV03-SYOKENSUG  PIC X(6).
            02  HCM2AV03-TITLE  PIC X(50).
            02  HCM2AV03-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2AV03-TITLE1 PIC X(10).

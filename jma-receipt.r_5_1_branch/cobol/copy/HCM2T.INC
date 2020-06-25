        01  HCM2T.
            02  HCM2T-HOSPCD    PIC X(20).
            02  HCM2T-ROU-TBL.
              03  HCM2T-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2T-ROU-G     OCCURS  3 TIMES.
                  05  HCM2T-ROU-KENSU PIC X(6).
                  05  HCM2T-ROU-NISSU PIC X(6).
                  05  HCM2T-ROU-TENSU PIC X(13).
                  05  HCM2T-ROU-ITBFTN    PIC X(13).
                  05  HCM2T-ROU-SYOKENSU  PIC X(6).
                  05  HCM2T-ROU-SYONISSU  PIC X(6).
                  05  HCM2T-ROU-SYOKINGAK PIC X(13).
                  05  HCM2T-ROU-SYOFTNGAK PIC X(13).
            02  HCM2T-KOH-TBL.
              03  HCM2T-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2T-KOH-G     OCCURS  6 TIMES.
                  05  HCM2T-KOHKBN    PIC X(44).
                  05  HCM2T-KOH-KENSU PIC X(6).
                  05  HCM2T-KOH-TENSU PIC X(13).
                  05  HCM2T-KOH-ITBFTN    PIC X(13).
                  05  HCM2T-KOH-SYOKENSU  PIC X(6).
                  05  HCM2T-KOH-SYOKINGAK PIC X(13).
                  05  HCM2T-KOH-SYOFTNGAK PIC X(13).
            02  HCM2T-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2T-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2T-GKENSU    PIC X(7).
            02  HCM2T-TITLE PIC X(50).
            02  HCM2T-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2T-BKO-TBL           OCCURS  10 TIMES.
              03  HCM2T-BKO-KENSU PIC X(6).
              03  HCM2T-BKO-NISSU PIC X(6).
              03  HCM2T-BKO-ITBFTN    PIC X(9).
              03  HCM2T-BKO-KOJYO PIC X(9).
              03  HCM2T-BKO-SYOKENSU  PIC X(6).
              03  HCM2T-BKO-SYONISSU  PIC X(6).
              03  HCM2T-BKO-SYOKINGAK PIC X(13).
              03  HCM2T-BKO-SYOFTNGAK PIC X(13).
            02  HCM2T-TITLE1    PIC X(10).
            02  HCM2T-SRYKA PIC X(40).
            02  HCM2T-TITLE2    PIC X(4).
            02  HCM2T-TITLE3    PIC X(8).

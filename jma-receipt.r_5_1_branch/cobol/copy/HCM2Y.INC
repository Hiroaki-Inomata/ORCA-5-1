        01  HCM2Y.
            02  HCM2Y-HOSPCD    PIC X(20).
            02  HCM2Y-ROU-TBL.
              03  HCM2Y-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2Y-ROU-G     OCCURS  3 TIMES.
                  05  HCM2Y-ROU-KENSU PIC X(6).
                  05  HCM2Y-ROU-NISSU PIC X(6).
                  05  HCM2Y-ROU-TENSU PIC X(10).
                  05  HCM2Y-ROU-ITBFTN    PIC X(9).
            02  HCM2Y-KOH-TBL.
              03  HCM2Y-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2Y-KOH-G     OCCURS  6 TIMES.
                  05  HCM2Y-KOHKBN    PIC X(44).
                  05  HCM2Y-KOH-KENSU PIC X(6).
                  05  HCM2Y-KOH-TENSU PIC X(10).
                  05  HCM2Y-KOH-ITBFTN    PIC X(9).
            02  HCM2Y-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2Y-GKENSU    PIC X(7).
            02  HCM2Y-TITLE PIC X(50).
            02  HCM2Y-BKO-TBL           OCCURS  10 TIMES.
              03  HCM2Y-BKO-KENSU PIC X(6).
              03  HCM2Y-BKO-NISSU PIC X(6).
              03  HCM2Y-BKO-ITBFTN    PIC X(9).
              03  HCM2Y-BKO-KOJYO PIC X(9).
            02  HCM2Y-TITLE1    PIC X(10).
            02  HCM2Y-SRYKA PIC X(40).
            02  HCM2Y-TITLE2    PIC X(8).

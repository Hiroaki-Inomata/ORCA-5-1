        01  HCM2D.
            02  HCM2D-HOSPCD    PIC X(20).
            02  HCM2D-ROU-TBL.
              03  HCM2D-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2D-ROU-G     OCCURS  3 TIMES.
                  05  HCM2D-ROU-KENSU PIC X(6).
                  05  HCM2D-ROU-NISSU PIC X(6).
                  05  HCM2D-ROU-TENSU PIC X(10).
                  05  HCM2D-ROU-ITBFTN    PIC X(9).
            02  HCM2D-KOH-TBL.
              03  HCM2D-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2D-KOH-G     OCCURS  6 TIMES.
                  05  HCM2D-KOHKBN    PIC X(44).
                  05  HCM2D-KOH-KENSU PIC X(6).
                  05  HCM2D-KOH-TENSU PIC X(10).
                  05  HCM2D-KOH-YKZFTN    PIC X(8).
                  05  HCM2D-KOH-ITBFTN    PIC X(9).
            02  HCM2D-KENSUG    PIC X(6)        OCCURS  2 TIMES.
            02  HCM2D-GKENSU    PIC X(7).
            02  HCM2D-TITLE PIC X(36).

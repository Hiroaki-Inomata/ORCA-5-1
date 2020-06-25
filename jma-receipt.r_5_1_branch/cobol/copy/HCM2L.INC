        01  HCM2L.
            02  HCM2L-HOSPCD    PIC X(20).
            02  HCM2L-ROU-TBL.
              03  HCM2L-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2L-ROU-G     OCCURS  3 TIMES.
                  05  HCM2L-ROU-KENSU PIC X(6).
                  05  HCM2L-ROU-NISSU PIC X(6).
                  05  HCM2L-ROU-TENSU PIC X(10).
                  05  HCM2L-ROU-ITBFTN    PIC X(9).
            02  HCM2L-KOH-TBL.
              03  HCM2L-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2L-KOH-G     OCCURS  6 TIMES.
                  05  HCM2L-KOHKBN    PIC X(44).
                  05  HCM2L-KOH-KENSU PIC X(6).
                  05  HCM2L-KOH-TENSU PIC X(10).
                  05  HCM2L-KOH-ITBFTN    PIC X(9).
            02  HCM2L-KENSUG    PIC X(6)        OCCURS  2 TIMES.
            02  HCM2L-GKENSU    PIC X(7).
            02  HCM2L-TITLE PIC X(50).

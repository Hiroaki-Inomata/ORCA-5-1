        01  HCM2DV01.
            02  HCM2DV01-HOSPCD PIC X(20).
            02  HCM2DV01-ROU-TBL.
              03  HCM2DV01-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2DV01-ROU-G          OCCURS  3 TIMES.
                  05  HCM2DV01-ROU-KENSU  PIC X(6).
                  05  HCM2DV01-ROU-NISSU  PIC X(6).
                  05  HCM2DV01-ROU-TENSU  PIC X(10).
                  05  HCM2DV01-ROU-ITBFTN PIC X(9).
            02  HCM2DV01-KOH-TBL.
              03  HCM2DV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2DV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM2DV01-KOHKBN PIC X(44).
                  05  HCM2DV01-KOH-KENSU  PIC X(6).
                  05  HCM2DV01-KOH-TENSU  PIC X(10).
                  05  HCM2DV01-KOH-YKZFTN PIC X(8).
                  05  HCM2DV01-KOH-ITBFTN PIC X(9).
            02  HCM2DV01-KENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2DV01-GKENSU PIC X(7).
            02  HCM2DV01-TITLE  PIC X(50).

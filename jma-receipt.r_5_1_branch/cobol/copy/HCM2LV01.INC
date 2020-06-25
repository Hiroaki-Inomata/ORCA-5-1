        01  HCM2LV01.
            02  HCM2LV01-HOSPCD PIC X(20).
            02  HCM2LV01-ROU-TBL.
              03  HCM2LV01-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2LV01-ROU-G          OCCURS  3 TIMES.
                  05  HCM2LV01-ROU-KENSU  PIC X(6).
                  05  HCM2LV01-ROU-NISSU  PIC X(6).
                  05  HCM2LV01-ROU-TENSU  PIC X(10).
                  05  HCM2LV01-ROU-ITBFTN PIC X(9).
            02  HCM2LV01-KOH-TBL.
              03  HCM2LV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2LV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM2LV01-KOHKBN PIC X(44).
                  05  HCM2LV01-KOH-KENSU  PIC X(6).
                  05  HCM2LV01-KOH-TENSU  PIC X(10).
                  05  HCM2LV01-KOH-ITBFTN PIC X(9).
            02  HCM2LV01-KENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2LV01-GKENSU PIC X(7).
            02  HCM2LV01-TITLE  PIC X(50).
            02  HCM2LV01-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2LV01-BKO-KENSU  PIC X(6).
              03  HCM2LV01-BKO-NISSU  PIC X(6).
              03  HCM2LV01-BKO-ITBFTN PIC X(9).
              03  HCM2LV01-BKO-KOJYO  PIC X(9).

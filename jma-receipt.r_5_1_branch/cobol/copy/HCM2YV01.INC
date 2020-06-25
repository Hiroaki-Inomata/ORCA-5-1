        01  HCM2YV01.
            02  HCM2YV01-HOSPCD PIC X(20).
            02  HCM2YV01-ROU-TBL.
              03  HCM2YV01-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2YV01-ROU-G          OCCURS  3 TIMES.
                  05  HCM2YV01-ROU-KENSU  PIC X(6).
                  05  HCM2YV01-ROU-NISSU  PIC X(6).
                  05  HCM2YV01-ROU-TENSU  PIC X(10).
                  05  HCM2YV01-ROU-ITBFTN PIC X(9).
            02  HCM2YV01-KOH-TBL.
              03  HCM2YV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2YV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM2YV01-KOHKBN PIC X(44).
                  05  HCM2YV01-KOH-KENSU  PIC X(6).
                  05  HCM2YV01-KOH-TENSU  PIC X(10).
                  05  HCM2YV01-KOH-ITBFTN PIC X(9).
            02  HCM2YV01-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2YV01-GKENSU PIC X(7).
            02  HCM2YV01-TITLE  PIC X(50).
            02  HCM2YV01-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2YV01-BKO-KENSU  PIC X(6).
              03  HCM2YV01-BKO-NISSU  PIC X(6).
              03  HCM2YV01-BKO-ITBFTN PIC X(9).
              03  HCM2YV01-BKO-KOJYO  PIC X(9).
            02  HCM2YV01-TITLE1 PIC X(10).
            02  HCM2YV01-SRYKA  PIC X(40).
            02  HCM2YV01-TITLE2 PIC X(8).
            02  HCM2YV01-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

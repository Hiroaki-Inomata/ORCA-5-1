        01  HCM2YV03.
            02  HCM2YV03-HOSPCD PIC X(20).
            02  HCM2YV03-ROU-TBL.
              03  HCM2YV03-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2YV03-ROU-G          OCCURS  3 TIMES.
                  05  HCM2YV03-ROU-KENSU  PIC X(6).
                  05  HCM2YV03-ROU-NISSU  PIC X(6).
                  05  HCM2YV03-ROU-TENSU  PIC X(10).
                  05  HCM2YV03-ROU-ITBFTN PIC X(9).
            02  HCM2YV03-KOH-TBL.
              03  HCM2YV03-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2YV03-KOH-G          OCCURS  6 TIMES.
                  05  HCM2YV03-KOHKBN PIC X(44).
                  05  HCM2YV03-KOH-KENSU  PIC X(6).
                  05  HCM2YV03-KOH-TENSU  PIC X(10).
                  05  HCM2YV03-KOH-ITBFTN PIC X(9).
            02  HCM2YV03-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2YV03-GKENSU PIC X(7).
            02  HCM2YV03-TITLE  PIC X(50).
            02  HCM2YV03-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2YV03-BKO-KOHKBN PIC X(16).
              03  HCM2YV03-BKO-KENSU  PIC X(6).
              03  HCM2YV03-BKO-NISSU  PIC X(6).
              03  HCM2YV03-BKO-ITBFTN PIC X(9).
              03  HCM2YV03-BKO-KOJYO  PIC X(9).
            02  HCM2YV03-TITLE1 PIC X(10).
            02  HCM2YV03-SRYKA  PIC X(40).
            02  HCM2YV03-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

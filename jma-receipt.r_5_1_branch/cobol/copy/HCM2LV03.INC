        01  HCM2LV03.
            02  HCM2LV03-HOSPCD PIC X(20).
            02  HCM2LV03-ROU-TBL.
              03  HCM2LV03-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2LV03-ROU-G          OCCURS  3 TIMES.
                  05  HCM2LV03-ROU-KENSU  PIC X(6).
                  05  HCM2LV03-ROU-NISSU  PIC X(6).
                  05  HCM2LV03-ROU-TENSU  PIC X(10).
                  05  HCM2LV03-ROU-ITBFTN PIC X(9).
            02  HCM2LV03-KOH-TBL.
              03  HCM2LV03-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2LV03-KOH-G          OCCURS  6 TIMES.
                  05  HCM2LV03-KOHKBN PIC X(44).
                  05  HCM2LV03-KOH-KENSU  PIC X(6).
                  05  HCM2LV03-KOH-TENSU  PIC X(10).
                  05  HCM2LV03-KOH-ITBFTN PIC X(9).
            02  HCM2LV03-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2LV03-GKENSU PIC X(7).
            02  HCM2LV03-TITLE  PIC X(50).
            02  HCM2LV03-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2LV03-BKO-KENSU  PIC X(6).
              03  HCM2LV03-BKO-NISSU  PIC X(6).
              03  HCM2LV03-BKO-ITBFTN PIC X(9).
              03  HCM2LV03-BKO-KOJYO  PIC X(9).
            02  HCM2LV03-TITLE1 PIC X(10).
            02  HCM2LV03-SRYKA  PIC X(40).

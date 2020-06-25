        01  HCM2JV01.
            02  HCM2JV01-HOSPCD PIC X(20).
            02  HCM2JV01-ROU-TBL.
              03  HCM2JV01-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2JV01-ROU-G          OCCURS  3 TIMES.
                  05  HCM2JV01-ROU-KENSU  PIC X(6).
                  05  HCM2JV01-ROU-NISSU  PIC X(6).
                  05  HCM2JV01-ROU-TENSU  PIC X(13).
                  05  HCM2JV01-ROU-ITBFTN PIC X(13).
                  05  HCM2JV01-ROU-SYOKENSU   PIC X(6).
                  05  HCM2JV01-ROU-SYONISSU   PIC X(6).
                  05  HCM2JV01-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2JV01-ROU-SYOFTNGAK  PIC X(13).
            02  HCM2JV01-KOH-TBL.
              03  HCM2JV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2JV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM2JV01-KOHKBN PIC X(44).
                  05  HCM2JV01-KOH-KENSU  PIC X(6).
                  05  HCM2JV01-KOH-TENSU  PIC X(13).
                  05  HCM2JV01-KOH-ITBFTN PIC X(13).
                  05  HCM2JV01-KOH-SYOKENSU   PIC X(6).
                  05  HCM2JV01-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2JV01-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2JV01-KENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2JV01-SYOKENSUG  PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV01-GKENSU PIC X(7).
            02  HCM2JV01-TITLE  PIC X(50).
            02  HCM2JV01-NYUGAIKBN  PIC X(2)    OCCURS  2 TIMES.
            02  HCM2JV01-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2JV01-BKO-KENSU  PIC X(6).
              03  HCM2JV01-BKO-NISSU  PIC X(6).
              03  HCM2JV01-BKO-ITBFTN PIC X(9).
              03  HCM2JV01-BKO-KOJYO  PIC X(9).

        01  HCM2JV04.
            02  HCM2JV04-HOSPCD PIC X(20).
            02  HCM2JV04-ROU-TBL.
              03  HCM2JV04-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2JV04-ROU-G          OCCURS  3 TIMES.
                  05  HCM2JV04-ROU-KENSU  PIC X(6).
                  05  HCM2JV04-ROU-NISSU  PIC X(6).
                  05  HCM2JV04-ROU-TENSU  PIC X(13).
                  05  HCM2JV04-ROU-ITBFTN PIC X(13).
                  05  HCM2JV04-ROU-SYOKENSU   PIC X(6).
                  05  HCM2JV04-ROU-SYONISSU   PIC X(6).
                  05  HCM2JV04-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2JV04-ROU-SYOFTNGAK  PIC X(13).
            02  HCM2JV04-KOH-TBL.
              03  HCM2JV04-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2JV04-KOH-G          OCCURS  6 TIMES.
                  05  HCM2JV04-KOHKBN PIC X(44).
                  05  HCM2JV04-KOH-KENSU  PIC X(6).
                  05  HCM2JV04-KOH-TENSU  PIC X(13).
                  05  HCM2JV04-KOH-ITBFTN PIC X(13).
                  05  HCM2JV04-KOH-SYOKENSU   PIC X(6).
                  05  HCM2JV04-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2JV04-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2JV04-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV04-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV04-GKENSU PIC X(7).
            02  HCM2JV04-TITLE  PIC X(50).
            02  HCM2JV04-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2JV04-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2JV04-BKO-KENSU  PIC X(6).
              03  HCM2JV04-BKO-NISSU  PIC X(6).
              03  HCM2JV04-BKO-ITBFTN PIC X(9).
              03  HCM2JV04-BKO-KOJYO  PIC X(9).
              03  HCM2JV04-BKO-SYOKENSU   PIC X(6).
              03  HCM2JV04-BKO-SYONISSU   PIC X(6).
              03  HCM2JV04-BKO-SYOKINGAK  PIC X(13).
              03  HCM2JV04-BKO-SYOFTNGAK  PIC X(13).
            02  HCM2JV04-TITLE1 PIC X(10).
            02  HCM2JV04-SRYKA  PIC X(40).

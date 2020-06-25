        01  HCM2JV03.
            02  HCM2JV03-HOSPCD PIC X(20).
            02  HCM2JV03-ROU-TBL.
              03  HCM2JV03-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2JV03-ROU-G          OCCURS  3 TIMES.
                  05  HCM2JV03-ROU-KENSU  PIC X(6).
                  05  HCM2JV03-ROU-NISSU  PIC X(6).
                  05  HCM2JV03-ROU-TENSU  PIC X(13).
                  05  HCM2JV03-ROU-ITBFTN PIC X(13).
                  05  HCM2JV03-ROU-SYOKENSU   PIC X(6).
                  05  HCM2JV03-ROU-SYONISSU   PIC X(6).
                  05  HCM2JV03-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2JV03-ROU-SYOFTNGAK  PIC X(13).
            02  HCM2JV03-KOH-TBL.
              03  HCM2JV03-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2JV03-KOH-G          OCCURS  6 TIMES.
                  05  HCM2JV03-KOHKBN PIC X(44).
                  05  HCM2JV03-KOH-KENSU  PIC X(6).
                  05  HCM2JV03-KOH-TENSU  PIC X(13).
                  05  HCM2JV03-KOH-ITBFTN PIC X(13).
                  05  HCM2JV03-KOH-SYOKENSU   PIC X(6).
                  05  HCM2JV03-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2JV03-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2JV03-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV03-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV03-GKENSU PIC X(7).
            02  HCM2JV03-TITLE  PIC X(50).
            02  HCM2JV03-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2JV03-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2JV03-BKO-KENSU  PIC X(6).
              03  HCM2JV03-BKO-NISSU  PIC X(6).
              03  HCM2JV03-BKO-ITBFTN PIC X(9).
              03  HCM2JV03-BKO-KOJYO  PIC X(9).
              03  HCM2JV03-BKO-SYOKENSU   PIC X(6).
              03  HCM2JV03-BKO-SYONISSU   PIC X(6).
              03  HCM2JV03-BKO-SYOKINGAK  PIC X(13).
              03  HCM2JV03-BKO-SYOFTNGAK  PIC X(13).
            02  HCM2JV03-TITLE1 PIC X(10).

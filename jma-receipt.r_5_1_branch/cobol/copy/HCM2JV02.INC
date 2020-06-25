        01  HCM2JV02.
            02  HCM2JV02-HOSPCD PIC X(20).
            02  HCM2JV02-ROU-TBL.
              03  HCM2JV02-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2JV02-ROU-G          OCCURS  3 TIMES.
                  05  HCM2JV02-ROU-KENSU  PIC X(6).
                  05  HCM2JV02-ROU-NISSU  PIC X(6).
                  05  HCM2JV02-ROU-TENSU  PIC X(13).
                  05  HCM2JV02-ROU-ITBFTN PIC X(13).
                  05  HCM2JV02-ROU-SYOKENSU   PIC X(6).
                  05  HCM2JV02-ROU-SYONISSU   PIC X(6).
                  05  HCM2JV02-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2JV02-ROU-SYOFTNGAK  PIC X(13).
            02  HCM2JV02-KOH-TBL.
              03  HCM2JV02-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2JV02-KOH-G          OCCURS  6 TIMES.
                  05  HCM2JV02-KOHKBN PIC X(44).
                  05  HCM2JV02-KOH-KENSU  PIC X(6).
                  05  HCM2JV02-KOH-TENSU  PIC X(13).
                  05  HCM2JV02-KOH-ITBFTN PIC X(13).
                  05  HCM2JV02-KOH-SYOKENSU   PIC X(6).
                  05  HCM2JV02-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2JV02-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2JV02-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV02-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2JV02-GKENSU PIC X(7).
            02  HCM2JV02-TITLE  PIC X(50).
            02  HCM2JV02-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2JV02-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2JV02-BKO-KENSU  PIC X(6).
              03  HCM2JV02-BKO-NISSU  PIC X(6).
              03  HCM2JV02-BKO-ITBFTN PIC X(9).
              03  HCM2JV02-BKO-KOJYO  PIC X(9).
              03  HCM2JV02-BKO-SYOKENSU   PIC X(6).
              03  HCM2JV02-BKO-SYONISSU   PIC X(6).
              03  HCM2JV02-BKO-SYOKINGAK  PIC X(13).
              03  HCM2JV02-BKO-SYOFTNGAK  PIC X(13).

        01  HCM2TV03.
            02  HCM2TV03-HOSPCD PIC X(20).
            02  HCM2TV03-ROU-TBL.
              03  HCM2TV03-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2TV03-ROU-G          OCCURS  3 TIMES.
                  05  HCM2TV03-ROU-KENSU  PIC X(6).
                  05  HCM2TV03-ROU-NISSU  PIC X(6).
                  05  HCM2TV03-ROU-TENSU  PIC X(13).
                  05  HCM2TV03-ROU-ITBFTN PIC X(13).
                  05  HCM2TV03-ROU-SYOKENSU   PIC X(6).
                  05  HCM2TV03-ROU-SYONISSU   PIC X(6).
                  05  HCM2TV03-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2TV03-ROU-SYOFTNGAK  PIC X(13).
            02  HCM2TV03-KOH-TBL.
              03  HCM2TV03-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2TV03-KOH-G          OCCURS  6 TIMES.
                  05  HCM2TV03-KOHKBN PIC X(44).
                  05  HCM2TV03-KOH-KENSU  PIC X(6).
                  05  HCM2TV03-KOH-TENSU  PIC X(13).
                  05  HCM2TV03-KOH-ITBFTN PIC X(13).
                  05  HCM2TV03-KOH-SYOKENSU   PIC X(6).
                  05  HCM2TV03-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2TV03-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2TV03-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2TV03-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2TV03-GKENSU PIC X(7).
            02  HCM2TV03-TITLE  PIC X(50).
            02  HCM2TV03-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2TV03-BKO-TBL        OCCURS  10 TIMES.
              03  HCM2TV03-BKO-KOHKBN PIC X(16).
              03  HCM2TV03-BKO-KENSU  PIC X(6).
              03  HCM2TV03-BKO-NISSU  PIC X(6).
              03  HCM2TV03-BKO-ITBFTN PIC X(9).
              03  HCM2TV03-BKO-KOJYO  PIC X(9).
              03  HCM2TV03-BKO-SYOKENSU   PIC X(6).
              03  HCM2TV03-BKO-SYONISSU   PIC X(6).
              03  HCM2TV03-BKO-SYOKINGAK  PIC X(13).
              03  HCM2TV03-BKO-SYOFTNGAK  PIC X(13).
            02  HCM2TV03-TITLE1 PIC X(10).
            02  HCM2TV03-SRYKA  PIC X(40).
            02  HCM2TV03-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

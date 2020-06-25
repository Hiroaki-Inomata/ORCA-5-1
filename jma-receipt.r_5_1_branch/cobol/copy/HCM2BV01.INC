        01  HCM2BV01.
            02  HCM2BV01-HOSPCD PIC X(20).
            02  HCM2BV01-ROU-TBL.
              03  HCM2BV01-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2BV01-ROU-G          OCCURS  3 TIMES.
                  05  HCM2BV01-ROU-KENSU  PIC X(6).
                  05  HCM2BV01-ROU-NISSU  PIC X(6).
                  05  HCM2BV01-ROU-TENSU  PIC X(13).
                  05  HCM2BV01-ROU-ITBFTN PIC X(13).
                  05  HCM2BV01-ROU-SYOKENSU   PIC X(6).
                  05  HCM2BV01-ROU-SYONISSU   PIC X(6).
                  05  HCM2BV01-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2BV01-ROU-SYOFTNGAK  PIC X(13).
              03  HCM2BV01-ROU-GOK-NISSU  PIC X(6).
              03  HCM2BV01-ROU-GOK-TENSU  PIC X(13).
              03  HCM2BV01-ROU-GOK-ITBFTN PIC X(13).
              03  HCM2BV01-ROU-GOK-SYONISSU   PIC X(6).
              03  HCM2BV01-ROU-GOK-SYOKINGAK  PIC X(13).
              03  HCM2BV01-ROU-GOK-SYOFTNGAK  PIC X(13).
            02  HCM2BV01-KOH-TBL.
              03  HCM2BV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2BV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM2BV01-KOHKBN PIC X(44).
                  05  HCM2BV01-KOH-KENSU  PIC X(6).
                  05  HCM2BV01-KOH-YKZFTN PIC X(8).
                  05  HCM2BV01-KOH-TENSU  PIC X(13).
                  05  HCM2BV01-KOH-ITBFTN PIC X(13).
                  05  HCM2BV01-KOH-SYOKENSU   PIC X(6).
                  05  HCM2BV01-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2BV01-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2BV01-KENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2BV01-SYOKENSUG  PIC X(6)    OCCURS  2 TIMES.
            02  HCM2BV01-GKENSU PIC X(7).
            02  HCM2BV01-TITLE  PIC X(50).
            02  HCM2BV01-NYUGAIKBN  PIC X(2)    OCCURS  2 TIMES.

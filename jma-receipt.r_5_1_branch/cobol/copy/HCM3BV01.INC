        01  HCM3BV01.
            02  HCM3BV01-HOSPCD PIC X(20).
            02  HCM3BV01-KOH-TBL.
              03  HCM3BV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM3BV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM3BV01-KOHKBN PIC X(44).
                  05  HCM3BV01-KOH-KENSU  PIC X(6).
                  05  HCM3BV01-KOH-TENSU  PIC X(13).
                  05  HCM3BV01-KOH-ITBFTN PIC X(13).
                  05  HCM3BV01-KOH-SYOKENSU   PIC X(6).
                  05  HCM3BV01-KOH-SYOKINGAK  PIC X(13).
                  05  HCM3BV01-KOH-SYOFTNGAK  PIC X(13).
            02  HCM3BV01-KENSUG PIC X(6).
            02  HCM3BV01-SYOKENSUG  PIC X(6).
            02  HCM3BV01-GKENSU PIC X(7).
            02  HCM3BV01-TITLE  PIC X(50).
            02  HCM3BV01-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3BV01-TITLE1 PIC X(10).
            02  HCM3BV01-SRYKA  PIC X(40).
            02  HCM3BV01-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3BV01-BIKOH.
              03  HCM3BV01-BIKOH-TITLE    PIC X(10).
              03  HCM3BV01-BIKOH-TITLE-KENSU  PIC X(4).
              03  HCM3BV01-BIKOH-KENSU    PIC X(6).
              03  HCM3BV01-BIKOH-TITLE-NISSU  PIC X(6).
              03  HCM3BV01-BIKOH-NISSU    PIC X(6).
              03  HCM3BV01-BIKOH-TITLE-TENSU  PIC X(4).
              03  HCM3BV01-BIKOH-TENSU    PIC X(13).
              03  HCM3BV01-BIKOH-TITLE-ITBFTN PIC X(10).
              03  HCM3BV01-BIKOH-ITBFTN   PIC X(13).
              03  HCM3BV01-BIKOH-TITLE-SYOKENSU   PIC X(8).
              03  HCM3BV01-BIKOH-SYOKENSU PIC X(6).
              03  HCM3BV01-BIKOH-TITLE-SYONISSU   PIC X(8).
              03  HCM3BV01-BIKOH-SYONISSU PIC X(6).
              03  HCM3BV01-BIKOH-TITLE-SYOKINGAK  PIC X(8).
              03  HCM3BV01-BIKOH-SYOKINGAK    PIC X(13).
              03  HCM3BV01-BIKOH-TITLE-SYOFTNGAK  PIC X(14).
              03  HCM3BV01-BIKOH-SYOFTNGAK    PIC X(13).

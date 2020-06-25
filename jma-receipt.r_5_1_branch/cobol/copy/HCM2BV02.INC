        01  HCM2BV02.
            02  HCM2BV02-HOSPCD PIC X(20).
            02  HCM2BV02-ROU-TBL.
              03  HCM2BV02-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2BV02-ROU-G          OCCURS  3 TIMES.
                  05  HCM2BV02-ROU-KENSU  PIC X(6).
                  05  HCM2BV02-ROU-NISSU  PIC X(6).
                  05  HCM2BV02-ROU-TENSU  PIC X(13).
                  05  HCM2BV02-ROU-ITBFTN PIC X(13).
                  05  HCM2BV02-ROU-SYOKENSU   PIC X(6).
                  05  HCM2BV02-ROU-SYONISSU   PIC X(6).
                  05  HCM2BV02-ROU-SYOKINGAK  PIC X(13).
                  05  HCM2BV02-ROU-SYOFTNGAK  PIC X(13).
              03  HCM2BV02-ROU-GOK-NISSU  PIC X(6).
              03  HCM2BV02-ROU-GOK-TENSU  PIC X(13).
              03  HCM2BV02-ROU-GOK-ITBFTN PIC X(13).
              03  HCM2BV02-ROU-GOK-SYONISSU   PIC X(6).
              03  HCM2BV02-ROU-GOK-SYOKINGAK  PIC X(13).
              03  HCM2BV02-ROU-GOK-SYOFTNGAK  PIC X(13).
            02  HCM2BV02-KOH-TBL.
              03  HCM2BV02-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2BV02-KOH-G          OCCURS  6 TIMES.
                  05  HCM2BV02-KOHKBN PIC X(44).
                  05  HCM2BV02-KOH-KENSU  PIC X(6).
                  05  HCM2BV02-KOH-YKZFTN PIC X(8).
                  05  HCM2BV02-KOH-TENSU  PIC X(13).
                  05  HCM2BV02-KOH-ITBFTN PIC X(13).
                  05  HCM2BV02-KOH-SYOKENSU   PIC X(6).
                  05  HCM2BV02-KOH-SYOKINGAK  PIC X(13).
                  05  HCM2BV02-KOH-SYOFTNGAK  PIC X(13).
            02  HCM2BV02-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2BV02-SYOKENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2BV02-GKENSU PIC X(7).
            02  HCM2BV02-TITLE  PIC X(50).
            02  HCM2BV02-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2BV02-TITLE1 PIC X(10).

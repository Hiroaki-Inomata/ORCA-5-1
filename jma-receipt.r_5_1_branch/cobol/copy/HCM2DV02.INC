        01  HCM2DV02.
            02  HCM2DV02-HOSPCD PIC X(20).
            02  HCM2DV02-ROU-TBL.
              03  HCM2DV02-ROU-TBL1       OCCURS  2 TIMES.
                04  HCM2DV02-ROU-G          OCCURS  3 TIMES.
                  05  HCM2DV02-ROU-KENSU  PIC X(6).
                  05  HCM2DV02-ROU-NISSU  PIC X(6).
                  05  HCM2DV02-ROU-TENSU  PIC X(10).
                  05  HCM2DV02-ROU-ITBFTN PIC X(9).
            02  HCM2DV02-KOH-TBL.
              03  HCM2DV02-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM2DV02-KOH-G          OCCURS  6 TIMES.
                  05  HCM2DV02-KOHKBN PIC X(44).
                  05  HCM2DV02-KOH-KENSU  PIC X(6).
                  05  HCM2DV02-KOH-TENSU  PIC X(10).
                  05  HCM2DV02-KOH-YKZFTN PIC X(8).
                  05  HCM2DV02-KOH-ITBFTN PIC X(9).
            02  HCM2DV02-KENSUG
                    PIC X(6)    OCCURS  2 TIMES.
            02  HCM2DV02-GKENSU PIC X(7).
            02  HCM2DV02-TITLE  PIC X(50).
            02  HCM2DV02-TITLE1 PIC X(10).

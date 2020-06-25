        01  HCM2J.
            02  HCM2J-HOSPCD    PIC X(20).
            02  HCM2J-ROU-TBL.
              03  HCM2J-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2J-ROU-G     OCCURS  3 TIMES.
                  05  HCM2J-ROU-KENSU PIC X(6).
                  05  HCM2J-ROU-NISSU PIC X(6).
                  05  HCM2J-ROU-TENSU PIC X(13).
                  05  HCM2J-ROU-ITBFTN    PIC X(13).
                  05  HCM2J-ROU-SYOKENSU  PIC X(6).
                  05  HCM2J-ROU-SYONISSU  PIC X(6).
                  05  HCM2J-ROU-SYOKINGAK PIC X(13).
                  05  HCM2J-ROU-SYOFTNGAK PIC X(13).
            02  HCM2J-KOH-TBL.
              03  HCM2J-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2J-KOH-G     OCCURS  6 TIMES.
                  05  HCM2J-KOHKBN    PIC X(44).
                  05  HCM2J-KOH-KENSU PIC X(6).
                  05  HCM2J-KOH-TENSU PIC X(13).
                  05  HCM2J-KOH-ITBFTN    PIC X(13).
                  05  HCM2J-KOH-SYOKENSU  PIC X(6).
                  05  HCM2J-KOH-SYOKINGAK PIC X(13).
                  05  HCM2J-KOH-SYOFTNGAK PIC X(13).
            02  HCM2J-KENSUG    PIC X(6)        OCCURS  2 TIMES.
            02  HCM2J-SYOKENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2J-GKENSU    PIC X(7).
            02  HCM2J-TITLE PIC X(50).
            02  HCM2J-NYUGAIKBN PIC X(2)        OCCURS  2 TIMES.

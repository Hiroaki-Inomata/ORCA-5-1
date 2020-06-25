        01  HCM2B.
            02  HCM2B-HOSPCD    PIC X(20).
            02  HCM2B-ROU-TBL.
              03  HCM2B-ROU-TBL1          OCCURS  2 TIMES.
                04  HCM2B-ROU-G     OCCURS  3 TIMES.
                  05  HCM2B-ROU-KENSU PIC X(6).
                  05  HCM2B-ROU-NISSU PIC X(6).
                  05  HCM2B-ROU-TENSU PIC X(13).
                  05  HCM2B-ROU-ITBFTN    PIC X(13).
                  05  HCM2B-ROU-SYOKENSU  PIC X(6).
                  05  HCM2B-ROU-SYONISSU  PIC X(6).
                  05  HCM2B-ROU-SYOKINGAK PIC X(13).
                  05  HCM2B-ROU-SYOFTNGAK PIC X(13).
              03  HCM2B-ROU-GOK-NISSU PIC X(6).
              03  HCM2B-ROU-GOK-TENSU PIC X(13).
              03  HCM2B-ROU-GOK-ITBFTN    PIC X(13).
              03  HCM2B-ROU-GOK-SYONISSU  PIC X(6).
              03  HCM2B-ROU-GOK-SYOKINGAK PIC X(13).
              03  HCM2B-ROU-GOK-SYOFTNGAK PIC X(13).
            02  HCM2B-KOH-TBL.
              03  HCM2B-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM2B-KOH-G     OCCURS  6 TIMES.
                  05  HCM2B-KOHKBN    PIC X(44).
                  05  HCM2B-KOH-KENSU PIC X(6).
                  05  HCM2B-KOH-YKZFTN    PIC X(8).
                  05  HCM2B-KOH-TENSU PIC X(13).
                  05  HCM2B-KOH-ITBFTN    PIC X(13).
                  05  HCM2B-KOH-SYOKENSU  PIC X(6).
                  05  HCM2B-KOH-SYOKINGAK PIC X(13).
                  05  HCM2B-KOH-SYOFTNGAK PIC X(13).
            02  HCM2B-KENSUG    PIC X(6)        OCCURS  2 TIMES.
            02  HCM2B-SYOKENSUG PIC X(6)        OCCURS  2 TIMES.
            02  HCM2B-GKENSU    PIC X(7).
            02  HCM2B-TITLE PIC X(36).
            02  HCM2B-NYUGAIKBN PIC X(2)        OCCURS  2 TIMES.

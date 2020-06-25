        01  HCM3B.
            02  HCM3B-HOSPCD    PIC X(20).
            02  HCM3B-KOH-TBL.
              03  HCM3B-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM3B-KOH-G     OCCURS  6 TIMES.
                  05  HCM3B-KOHKBN    PIC X(44).
                  05  HCM3B-KOH-KENSU PIC X(6).
                  05  HCM3B-KOH-TENSU PIC X(13).
                  05  HCM3B-KOH-ITBFTN    PIC X(13).
                  05  HCM3B-KOH-SYOKENSU  PIC X(6).
                  05  HCM3B-KOH-SYOKINGAK PIC X(13).
                  05  HCM3B-KOH-SYOFTNGAK PIC X(13).
            02  HCM3B-KENSUG    PIC X(6).
            02  HCM3B-SYOKENSUG PIC X(6).
            02  HCM3B-GKENSU    PIC X(7).
            02  HCM3B-TITLE PIC X(50).
            02  HCM3B-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3B-TITLE1    PIC X(10).
            02  HCM3B-SRYKA PIC X(40).
            02  HCM3B-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

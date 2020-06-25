        01  HCM3DV01.
            02  HCM3DV01-HOSPCD PIC X(20).
            02  HCM3DV01-KOH-TBL.
              03  HCM3DV01-KOH-TBL1       OCCURS  3 TIMES.
                04  HCM3DV01-KOH-G          OCCURS  6 TIMES.
                  05  HCM3DV01-KOHKBN PIC X(44).
                  05  HCM3DV01-KOH-KENSU  PIC X(6).
                  05  HCM3DV01-KOH-TENSU  PIC X(10).
                  05  HCM3DV01-KOH-ITBFTN PIC X(9).
            02  HCM3DV01-KENSUG PIC X(6).
            02  HCM3DV01-GKENSU PIC X(7).
            02  HCM3DV01-TITLE  PIC X(50).
            02  HCM3DV01-TITLE1 PIC X(10).
            02  HCM3DV01-SRYKA  PIC X(40).
            02  HCM3DV01-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM3DV01-BIKOH.
              03  HCM3DV01-BIKOH-TITLE    PIC X(10).
              03  HCM3DV01-BIKOH-TITLE-KENSU  PIC X(4).
              03  HCM3DV01-BIKOH-KENSU    PIC X(6).
              03  HCM3DV01-BIKOH-TITLE-NISSU  PIC X(6).
              03  HCM3DV01-BIKOH-NISSU    PIC X(6).
              03  HCM3DV01-BIKOH-TITLE-TENSU  PIC X(4).
              03  HCM3DV01-BIKOH-TENSU    PIC X(10).
              03  HCM3DV01-BIKOH-TITLE-ITBFTN PIC X(10).
              03  HCM3DV01-BIKOH-ITBFTN   PIC X(9).

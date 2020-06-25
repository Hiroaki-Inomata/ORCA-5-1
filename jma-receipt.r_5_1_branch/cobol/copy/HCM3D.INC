        01  HCM3D.
            02  HCM3D-HOSPCD    PIC X(20).
            02  HCM3D-KOH-TBL.
              03  HCM3D-KOH-TBL1          OCCURS  3 TIMES.
                04  HCM3D-KOH-G     OCCURS  6 TIMES.
                  05  HCM3D-KOHKBN    PIC X(44).
                  05  HCM3D-KOH-KENSU PIC X(6).
                  05  HCM3D-KOH-TENSU PIC X(10).
                  05  HCM3D-KOH-ITBFTN    PIC X(9).
            02  HCM3D-KENSUG    PIC X(6).
            02  HCM3D-GKENSU    PIC X(7).
            02  HCM3D-TITLE PIC X(50).
            02  HCM3D-TITLE1    PIC X(10).
            02  HCM3D-SRYKA PIC X(40).
            02  HCM3D-MARUCHO
                    PIC X(2)    OCCURS  2 TIMES.

        01  HCM15.
            02  HCM15-DRCD  PIC X(20).
            02  HCM15-HAKYMD    PIC X(22).
            02  HCM15-TITLE PIC X(14).
            02  HCM15-PAGE  PIC X(3).
            02  HCM15-TITLE2    PIC X(20).
            02  HCM15-YYKYMD    PIC X(28)       OCCURS  6 TIMES.
            02  HCM15-TBL       OCCURS  6 TIMES.
              03  HCM15-SRYNAIYO  PIC X(10)       OCCURS  33 TIMES.
              03  HCM15-YYKTIME   PIC X(5)        OCCURS  33 TIMES.
              03  HCM15-NAME  PIC X(14)   OCCURS  33 TIMES.

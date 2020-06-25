        01  HCM45.
            02  HCM45-UKEYMD    PIC X(9).
            02  HCM45-SYORIKBN  PIC X(8).
            02  HCM45-SAKYMD    PIC X(9).
            02  HCM45-PAGE  PIC X(5).
            02  HCM45-TBL       OCCURS  40 TIMES.
              03  HCM45-NUM   PIC X(5).
              03  HCM45-PTNUM PIC X(10).
              03  HCM45-NAME  PIC X(20).
              03  HCM45-IDOYMD    PIC X(9).
              03  HCM45-IDONAIYO  PIC X(20).
              03  HCM45-JYOTAI    PIC X(6).
              03  HCM45-ITEM  PIC X(100).

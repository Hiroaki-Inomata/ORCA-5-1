        01  HCM47.
            02  HCM47-TITLE PIC X(28).
            02  HCM47-KIKAN PIC X(20).
            02  HCM47-SYORIBI   PIC X(22).
            02  HCM47-PAGE  PIC X(4).
            02  HCM47-TBL       OCCURS  5 TIMES.
              03  HCM47-NUM   PIC X(4).
              03  HCM47-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM47-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM47-GAZO  PIC X(1024).
              03  HCM47-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM47-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.
              03  HCM47-SRYCD PIC X(11).

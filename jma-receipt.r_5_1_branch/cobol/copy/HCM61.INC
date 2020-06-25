        01  HCM61.
            02  HCM61-TITLE PIC X(28).
            02  HCM61-KIKAN PIC X(20).
            02  HCM61-SYORIBI   PIC X(22).
            02  HCM61-PAGE  PIC X(4).
            02  HCM61-TBL       OCCURS  9 TIMES.
              03  HCM61-NUM   PIC X(4).
              03  HCM61-GAZO  PIC X(1024).
              03  HCM61-SRYCD PIC X(11).
              03  HCM61-YKZNAME-G.
                04  HCM61-YKZNAME
                        PIC X(32)   OCCURS  3 TIMES.
              03  HCM61-KONOKOKA-G.
                04  HCM61-KONOKOKA
                        PIC X(34)   OCCURS  8 TIMES.
              03  HCM61-KEIJYO-G.
                04  HCM61-KEIJYO
                        PIC X(20)   OCCURS  8 TIMES.
              03  HCM61-CYUJKO-G.
                04  HCM61-CYUJKO
                        PIC X(44)   OCCURS  8 TIMES.

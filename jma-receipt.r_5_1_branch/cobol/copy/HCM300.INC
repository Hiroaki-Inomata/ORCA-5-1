        01  HCM300.
            02  HCM300-SYORIBI  PIC X(22).
            02  HCM300-PAGE PIC X(3).
            02  HCM300-SYURUI   PIC X(12).
            02  HCM300-ID   PIC X(20).
            02  HCM300-KANANAME PIC X(44).
            02  HCM300-NAME PIC X(34).
            02  HCM300-NAME2    PIC X(88).
            02  HCM300-JYUSYO   PIC X(100).
            02  HCM300-HSPNAME  PIC X(100).
            02  HCM300-TEL  PIC X(15).
            02  HCM300-DRNAME   PIC X(26).
            02  HCM300-COMMENT
                    PIC X(108)  OCCURS  3 TIMES.
            02  HCM300-TBL      OCCURS  8 TIMES.
              03  HCM300-NUM  PIC X(2).
              03  HCM300-GAZO PIC X(1024).
              03  HCM300-YKZNAME-G.
                04  HCM300-YKZNAME
                        PIC X(32)   OCCURS  3 TIMES.
              03  HCM300-KONOKOKA-G.
                04  HCM300-KONOKOKA
                        PIC X(34)   OCCURS  8 TIMES.
              03  HCM300-KEIJYO-G.
                04  HCM300-KEIJYO
                        PIC X(20)   OCCURS  8 TIMES.
              03  HCM300-KBN-G.
                04  HCM300-KBN
                        PIC X(3)    OCCURS  5 TIMES.
              03  HCM300-NAIYO-G.
                04  HCM300-NAIYO
                        PIC X(20)   OCCURS  8 TIMES.
              03  HCM300-CYUJKO-G.
                04  HCM300-CYUJKO
                        PIC X(44)   OCCURS  8 TIMES.
              03  HCM300-TANI-G.
                04  HCM300-TANI
                        PIC X(2)    OCCURS  5 TIMES.

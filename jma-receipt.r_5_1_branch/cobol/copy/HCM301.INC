        01  HCM301.
            02  HCM301-SYORIBI  PIC X(22).
            02  HCM301-PAGE PIC X(3).
            02  HCM301-SYURUI   PIC X(12).
            02  HCM301-ID   PIC X(20).
            02  HCM301-KANANAME PIC X(44).
            02  HCM301-NAME PIC X(34).
            02  HCM301-NAME2    PIC X(88).
            02  HCM301-JYUSYO   PIC X(100).
            02  HCM301-HSPNAME  PIC X(100).
            02  HCM301-TEL  PIC X(15).
            02  HCM301-DRNAME   PIC X(26).
            02  HCM301-COMMENT
                    PIC X(108)  OCCURS  3 TIMES.
            02  HCM301-TBL      OCCURS  6 TIMES.
              03  HCM301-NUM  PIC X(2).
              03  HCM301-GAZO PIC X(1024).
              03  HCM301-YKZNAME-G.
                04  HCM301-YKZNAME
                        PIC X(26)   OCCURS  3 TIMES.
              03  HCM301-KONOKOKA-G.
                04  HCM301-KONOKOKA
                        PIC X(30)   OCCURS  11 TIMES.
              03  HCM301-KEIJYO-G.
                04  HCM301-KEIJYO
                        PIC X(16)   OCCURS  11 TIMES.
              03  HCM301-KBN-G.
                04  HCM301-KBN
                        PIC X(3)    OCCURS  5 TIMES.
              03  HCM301-NAIYO-G.
                04  HCM301-NAIYO
                        PIC X(18)   OCCURS  11 TIMES.
              03  HCM301-CYUJKO-G.
                04  HCM301-CYUJKO
                        PIC X(40)   OCCURS  11 TIMES.
              03  HCM301-TANI-G.
                04  HCM301-TANI
                        PIC X(2)    OCCURS  5 TIMES.

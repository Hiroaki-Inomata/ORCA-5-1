        01  HCN300.
            02  HCN300-SYORIBI  PIC X(22).
            02  HCN300-PAGE PIC X(3).
            02  HCN300-SYURUI   PIC X(12).
            02  HCN300-ID   PIC X(20).
            02  HCN300-KANANAME PIC X(44).
            02  HCN300-NAME PIC X(34).
            02  HCN300-NAME2    PIC X(88).
            02  HCN300-BTUNAME  PIC X(30).
            02  HCN300-JYUSYO   PIC X(100).
            02  HCN300-HSPNAME  PIC X(100).
            02  HCN300-TEL  PIC X(15).
            02  HCN300-DRNAME   PIC X(26).
            02  HCN300-COMMENT
                    PIC X(108)  OCCURS  3 TIMES.
            02  HCN300-TBL      OCCURS  8 TIMES.
              03  HCN300-NUM  PIC X(2).
              03  HCN300-GAZO PIC X(1024).
              03  HCN300-YKZNAME-G.
                04  HCN300-YKZNAME
                        PIC X(32)   OCCURS  3 TIMES.
              03  HCN300-KONOKOKA-G.
                04  HCN300-KONOKOKA
                        PIC X(34)   OCCURS  8 TIMES.
              03  HCN300-KEIJYO-G.
                04  HCN300-KEIJYO
                        PIC X(20)   OCCURS  8 TIMES.
              03  HCN300-KBN-G.
                04  HCN300-KBN
                        PIC X(3)    OCCURS  5 TIMES.
              03  HCN300-NAIYO-G.
                04  HCN300-NAIYO
                        PIC X(20)   OCCURS  8 TIMES.
              03  HCN300-CYUJKO-G.
                04  HCN300-CYUJKO
                        PIC X(44)   OCCURS  8 TIMES.
              03  HCN300-TANI-G.
                04  HCN300-TANI
                        PIC X(2)    OCCURS  5 TIMES.

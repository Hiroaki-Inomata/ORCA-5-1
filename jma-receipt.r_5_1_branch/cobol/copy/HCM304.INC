        01  HCM304.
            02  HCM304-SYORIBI  PIC X(22).
            02  HCM304-PAGE PIC X(3).
            02  HCM304-SYURUI   PIC X(12).
            02  HCM304-ID   PIC X(20).
            02  HCM304-KANANAME PIC X(44).
            02  HCM304-NAME PIC X(30).
            02  HCM304-NAME2    PIC X(88).
            02  HCM304-JYUSYO   PIC X(100).
            02  HCM304-HSPNAME  PIC X(100).
            02  HCM304-TEL  PIC X(15).
            02  HCM304-DRNAME   PIC X(26).
            02  HCM304-COMMENT
                    PIC X(108)  OCCURS  7 TIMES.
            02  HCM304-TBL      OCCURS  4 TIMES.
              03  HCM304-NUM  PIC X(2).
              03  HCM304-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM304-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM304-GAZO PIC X(1024).
              03  HCM304-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM304-KBN
                      PIC X(3)    OCCURS  4 TIMES.
              03  HCM304-NAIYO
                      PIC X(16)   OCCURS  8 TIMES.
              03  HCM304-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.

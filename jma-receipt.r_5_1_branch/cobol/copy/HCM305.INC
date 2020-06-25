        01  HCM305.
            02  HCM305-SYORIBI  PIC X(22).
            02  HCM305-PAGE PIC X(3).
            02  HCM305-SYURUI   PIC X(12).
            02  HCM305-ID   PIC X(20).
            02  HCM305-KANANAME PIC X(44).
            02  HCM305-NAME PIC X(34).
            02  HCM305-NAME2    PIC X(88).
            02  HCM305-JYUSYO   PIC X(100).
            02  HCM305-HSPNAME  PIC X(100).
            02  HCM305-TEL  PIC X(15).
            02  HCM305-DRNAME   PIC X(26).
            02  HCM305-COMMENT
                    PIC X(108)  OCCURS  7 TIMES.
            02  HCM305-TBL      OCCURS  5 TIMES.
              03  HCM305-NUM  PIC X(2).
              03  HCM305-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM305-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM305-GAZO PIC X(1024).
              03  HCM305-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM305-KBN
                      PIC X(3)    OCCURS  4 TIMES.
              03  HCM305-NAIYO
                      PIC X(16)   OCCURS  8 TIMES.
              03  HCM305-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.

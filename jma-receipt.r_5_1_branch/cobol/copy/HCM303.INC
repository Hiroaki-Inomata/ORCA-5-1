        01  HCM303.
            02  HCM303-SYORIBI  PIC X(22).
            02  HCM303-PAGE PIC X(3).
            02  HCM303-SYURUI   PIC X(12).
            02  HCM303-ID   PIC X(20).
            02  HCM303-KANANAME PIC X(44).
            02  HCM303-NAME PIC X(34).
            02  HCM303-NAME2    PIC X(88).
            02  HCM303-JYUSYO   PIC X(100).
            02  HCM303-HSPNAME  PIC X(100).
            02  HCM303-TEL  PIC X(15).
            02  HCM303-DRNAME   PIC X(26).
            02  HCM303-COMMENT
                    PIC X(108)  OCCURS  7 TIMES.
            02  HCM303-TBL      OCCURS  3 TIMES.
              03  HCM303-NUM  PIC X(2).
              03  HCM303-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM303-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM303-GAZO PIC X(1024).
              03  HCM303-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM303-KBN
                      PIC X(3)    OCCURS  4 TIMES.
              03  HCM303-NAIYO
                      PIC X(16)   OCCURS  8 TIMES.
              03  HCM303-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.

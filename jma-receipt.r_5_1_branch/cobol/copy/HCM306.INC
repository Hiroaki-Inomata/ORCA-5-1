        01  HCM306.
            02  HCM306-SYORIBI  PIC X(22).
            02  HCM306-PAGE PIC X(3).
            02  HCM306-SYURUI   PIC X(12).
            02  HCM306-ID   PIC X(20).
            02  HCM306-KANANAME PIC X(44).
            02  HCM306-NAME PIC X(34).
            02  HCM306-NAME2    PIC X(88).
            02  HCM306-JYUSYO   PIC X(100).
            02  HCM306-HSPNAME  PIC X(100).
            02  HCM306-TEL  PIC X(15).
            02  HCM306-DRNAME   PIC X(26).
            02  HCM306-COMMENT
                    PIC X(108)  OCCURS  7 TIMES.
            02  HCM306-TBL      OCCURS  6 TIMES.
              03  HCM306-NUM  PIC X(2).
              03  HCM306-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM306-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM306-GAZO PIC X(1024).
              03  HCM306-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM306-KBN
                      PIC X(3)    OCCURS  4 TIMES.
              03  HCM306-NAIYO
                      PIC X(16)   OCCURS  8 TIMES.
              03  HCM306-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.

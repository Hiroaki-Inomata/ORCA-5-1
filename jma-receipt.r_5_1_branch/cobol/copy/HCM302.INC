        01  HCM302.
            02  HCM302-SYORIBI  PIC X(22).
            02  HCM302-PAGE PIC X(3).
            02  HCM302-SYURUI   PIC X(12).
            02  HCM302-ID   PIC X(20).
            02  HCM302-KANANAME PIC X(44).
            02  HCM302-NAME PIC X(34).
            02  HCM302-NAME2    PIC X(88).
            02  HCM302-JYUSYO   PIC X(100).
            02  HCM302-HSPNAME  PIC X(100).
            02  HCM302-TEL  PIC X(15).
            02  HCM302-DRNAME   PIC X(26).
            02  HCM302-COMMENT
                    PIC X(108)  OCCURS  7 TIMES.
            02  HCM302-TBL      OCCURS  2 TIMES.
              03  HCM302-NUM  PIC X(2).
              03  HCM302-YKZNAME
                      PIC X(26)   OCCURS  3 TIMES.
              03  HCM302-KONOKOKA
                      PIC X(30)   OCCURS  6 TIMES.
              03  HCM302-GAZO PIC X(1024).
              03  HCM302-KEIJYO
                      PIC X(16)   OCCURS  10 TIMES.
              03  HCM302-KBN
                      PIC X(3)    OCCURS  4 TIMES.
              03  HCM302-NAIYO
                      PIC X(16)   OCCURS  8 TIMES.
              03  HCM302-CYUJKO
                      PIC X(40)   OCCURS  10 TIMES.

        01  HCM33N.
            02  HCM33N-PTNUM    PIC X(20).
            02  HCM33N-TANJYOBI PIC X(24).
            02  HCM33N-SEX  PIC X(2).
            02  HCM33N-NAME PIC X(30).
            02  HCM33N-HKNSYUBETU   PIC X(124).
            02  HCM33N-PAGE PIC X(3).
            02  HCM33N-SRYKA    PIC X(10).
            02  HCM33N-SRYYM    PIC X(34).
            02  HCM33N-A-TBL            OCCURS  40 TIMES.
              03  HCM33N-NUM  PIC X(2).
              03  HCM33N-SRYMD    PIC X(5).
              03  HCM33N-NAIYO
                      PIC X(8)    OCCURS  11 TIMES.

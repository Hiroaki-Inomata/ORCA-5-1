        01  HCM33.
            02  HCM33-PTNUM PIC X(20).
            02  HCM33-TANJYOBI  PIC X(24).
            02  HCM33-SEX   PIC X(2).
            02  HCM33-NAME  PIC X(30).
            02  HCM33-HKNSYUBETU    PIC X(124).
            02  HCM33-PAGE  PIC X(3).
            02  HCM33-SRYKA PIC X(10).
            02  HCM33-SRYYM PIC X(34).
            02  HCM33-A-TBL     OCCURS  40 TIMES.
              03  HCM33-NUM   PIC X(2).
              03  HCM33-SRYMD PIC X(5).
              03  HCM33-NAIYO
                      PIC X(8)    OCCURS  10 TIMES.
              03  HCM33-BIKOU PIC X(8).

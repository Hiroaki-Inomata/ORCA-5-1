        01  HCM12.
            02  HCM12-TEISYUTUSAKI  PIC X(6).
            02  HCM12-PTNUM PIC X(20).
            02  HCM12-SRYY  PIC X(2).
            02  HCM12-SRYM  PIC X(2).
            02  HCM12-PREFNUM   PIC X(2).
            02  HCM12-HOSPCDN   PIC X(20).
            02  HCM12-NAIYO-TBL         OCCURS  140 TIMES.
              03  HCM12-SRYKBN    PIC X(2).
              03  HCM12-TEKIYO    PIC X(52).
            02  HCM12-RJNFTNJANUM.
              03  HCM12-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM12-KOHFTNJANUM1.
              03  HCM12-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM12-KOHFTNJANUM2.
              03  HCM12-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM12-KOHFTNJANUM1N PIC X(20).
            02  HCM12-KOHFTNJANUM2N PIC X(20).
            02  HCM12-HKNJANUM.
              03  HCM12-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM12-KYURATE   PIC X(8).
            02  HCM12-RJNJKYSNUM.
              03  HCM12-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM12-KOHJKYSNUM1.
              03  HCM12-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM12-KOHJKYSNUM2.
              03  HCM12-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM12-KOHJKYSNUM1N  PIC X(20).
            02  HCM12-KOHJKYSNUM2N  PIC X(20).
            02  HCM12-HKNSBT1   PIC X(6).
            02  HCM12-HKNSBT2   PIC X(6).
            02  HCM12-HONKZK    PIC X(10).
            02  HCM12-KIGO  PIC X(34).
            02  HCM12-NUM   PIC X(34).
            02  HCM12-RENNUM    PIC X(7).
            02  HCM12-NAME  PIC X(44).
            02  HCM12-TYTLE PIC X(14).
            02  HCM12-PAGE  PIC X(5).
            02  HCM12-MOJI1 PIC X(10).
            02  HCM12-MOJI2 PIC X(10).
            02  HCM12-BTUNUM    PIC X(9).

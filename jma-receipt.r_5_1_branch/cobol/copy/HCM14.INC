        01  HCM14.
            02  HCM14-TEISYUTUSAKI  PIC X(6).
            02  HCM14-PTNUM PIC X(20).
            02  HCM14-SRYY  PIC X(2).
            02  HCM14-SRYM  PIC X(2).
            02  HCM14-PREFNUM   PIC X(2).
            02  HCM14-HOSPCDN   PIC X(20).
            02  HCM14-NAIYO-TBL         OCCURS  138 TIMES.
              03  HCM14-SRYKBN    PIC X(2).
              03  HCM14-TEKIYO    PIC X(52).
            02  HCM14-RJNFTNJANUM.
              03  HCM14-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14-KOHFTNJANUM1.
              03  HCM14-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14-KOHFTNJANUM2.
              03  HCM14-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14-KOHFTNJANUM1N PIC X(20).
            02  HCM14-KOHFTNJANUM2N PIC X(20).
            02  HCM14-HKNJANUM.
              03  HCM14-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14-KYURATE   PIC X(8).
            02  HCM14-RJNJKYSNUM.
              03  HCM14-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14-KOHJKYSNUM1.
              03  HCM14-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14-KOHJKYSNUM2.
              03  HCM14-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14-KOHJKYSNUM1N  PIC X(20).
            02  HCM14-KOHJKYSNUM2N  PIC X(20).
            02  HCM14-HKNSBT1   PIC X(6).
            02  HCM14-HKNSBT2   PIC X(6).
            02  HCM14-HONKZK    PIC X(10).
            02  HCM14-KIGO  PIC X(34).
            02  HCM14-NUM   PIC X(34).
            02  HCM14-RENNUM    PIC X(7).
            02  HCM14-KANANAME  PIC X(30).
            02  HCM14-NAME  PIC X(44).
            02  HCM14-TYTLE PIC X(14).
            02  HCM14-PAGE  PIC X(5).
            02  HCM14-MOJI1 PIC X(10).
            02  HCM14-MOJI2 PIC X(10).
            02  HCM14-BTUNUM    PIC X(9).

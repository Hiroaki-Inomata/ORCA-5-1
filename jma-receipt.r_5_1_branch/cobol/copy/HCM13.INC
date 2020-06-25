        01  HCM13.
            02  HCM13-TEISYUTUSAKI  PIC X(6).
            02  HCM13-PTNUM PIC X(20).
            02  HCM13-SRYY  PIC X(2).
            02  HCM13-SRYM  PIC X(2).
            02  HCM13-PREFNUM   PIC X(2).
            02  HCM13-HOSPCDN   PIC X(20).
            02  HCM13-NAIYO-TBL         OCCURS  110 TIMES.
              03  HCM13-SRYKBN    PIC X(2).
              03  HCM13-TEKIYO    PIC X(52).
            02  HCM13-RJNFTNJANUM.
              03  HCM13-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM13-KOHFTNJANUM1.
              03  HCM13-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM13-KOHFTNJANUM2.
              03  HCM13-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM13-KOHFTNJANUM1N PIC X(20).
            02  HCM13-KOHFTNJANUM2N PIC X(20).
            02  HCM13-HKNJANUM.
              03  HCM13-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM13-KYURATE   PIC X(8).
            02  HCM13-RJNJKYSNUM.
              03  HCM13-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM13-KOHJKYSNUM1.
              03  HCM13-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM13-KOHJKYSNUM2.
              03  HCM13-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM13-KOHJKYSNUM1N  PIC X(20).
            02  HCM13-KOHJKYSNUM2N  PIC X(20).
            02  HCM13-HKNSBT1   PIC X(6).
            02  HCM13-HKNSBT2   PIC X(6).
            02  HCM13-HONKZK    PIC X(10).
            02  HCM13-KIGO  PIC X(34).
            02  HCM13-NUM   PIC X(34).
            02  HCM13-RENNUM    PIC X(7).
            02  HCM13-KANANAME  PIC X(30).
            02  HCM13-NAME  PIC X(44).
            02  HCM13-TYTLE PIC X(14).
            02  HCM13-PAGE  PIC X(5).
            02  HCM13-MOJI1 PIC X(10).
            02  HCM13-MOJI2 PIC X(10).
            02  HCM13-BTUNUM    PIC X(9).

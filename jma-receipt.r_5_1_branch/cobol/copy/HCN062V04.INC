        01  HCN062V04.
            02  HCN062V04-TEISYUTUSAKI  PIC X(6).
            02  HCN062V04-PTNUM PIC X(20).
            02  HCN062V04-SRYY  PIC X(2).
            02  HCN062V04-SRYM  PIC X(2).
            02  HCN062V04-PREFNUM   PIC X(2).
            02  HCN062V04-HOSPCDN   PIC X(20).
            02  HCN062V04-NAIYO-TBL     OCCURS  110 TIMES.
              03  HCN062V04-SRYKBN    PIC X(2).
              03  HCN062V04-TEKIYO    PIC X(52).
            02  HCN062V04-RJNFTNJANUM.
              03  HCN062V04-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN062V04-KOHFTNJANUM1.
              03  HCN062V04-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN062V04-KOHFTNJANUM2.
              03  HCN062V04-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN062V04-KOHFTNJANUM1N PIC X(20).
            02  HCN062V04-KOHFTNJANUM2N PIC X(20).
            02  HCN062V04-HKNJANUM.
              03  HCN062V04-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN062V04-KYURATE   PIC X(8).
            02  HCN062V04-RJNJKYSNUM.
              03  HCN062V04-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN062V04-KOHJKYSNUM1.
              03  HCN062V04-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN062V04-KOHJKYSNUM2.
              03  HCN062V04-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN062V04-KOHJKYSNUM1N  PIC X(20).
            02  HCN062V04-KOHJKYSNUM2N  PIC X(20).
            02  HCN062V04-HKNSBT1   PIC X(6).
            02  HCN062V04-HKNSBT2   PIC X(6).
            02  HCN062V04-HONKZK    PIC X(10).
            02  HCN062V04-KIGO  PIC X(34).
            02  HCN062V04-NUM   PIC X(34).
            02  HCN062V04-RENNUM    PIC X(7).
            02  HCN062V04-KANANAME  PIC X(30).
            02  HCN062V04-NAME  PIC X(44).
            02  HCN062V04-TYTLE PIC X(14).
            02  HCN062V04-PAGE  PIC X(5).
            02  HCN062V04-MOJI1 PIC X(10).
            02  HCN062V04-MOJI2 PIC X(10).
            02  HCN062V04-BTUNUM    PIC X(9).

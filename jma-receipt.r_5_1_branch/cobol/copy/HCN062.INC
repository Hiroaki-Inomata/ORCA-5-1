        01  HCN062.
            02  HCN062-TEISYUTUSAKI PIC X(6).
            02  HCN062-PTNUM    PIC X(20).
            02  HCN062-SRYY PIC X(2).
            02  HCN062-SRYM PIC X(2).
            02  HCN062-PREFNUM  PIC X(2).
            02  HCN062-HOSPCDN  PIC X(20).
            02  HCN062-NAIYO-TBL        OCCURS  110 TIMES.
              03  HCN062-SRYKBN   PIC X(2).
              03  HCN062-TEKIYO   PIC X(52).
            02  HCN062-RJNFTNJANUM.
              03  HCN062-RJNFTNJANUMS PIC X(1)    OCCURS  8 TIMES.
            02  HCN062-KOHFTNJANUM1.
              03  HCN062-KOHFTNJANUM1S    PIC X(1)
                                                  OCCURS  8 TIMES.
            02  HCN062-KOHFTNJANUM2.
              03  HCN062-KOHFTNJANUM2S    PIC X(1)
                                                  OCCURS  8 TIMES.
            02  HCN062-KOHFTNJANUM1N    PIC X(20).
            02  HCN062-KOHFTNJANUM2N    PIC X(20).
            02  HCN062-HKNJANUM.
              03  HCN062-HKNJANUMS    PIC X(1)    OCCURS  8 TIMES.
            02  HCN062-KYURATE  PIC X(8).
            02  HCN062-RJNJKYSNUM.
              03  HCN062-RJNJKYSNUMS  PIC X(1)    OCCURS  7 TIMES.
            02  HCN062-KOHJKYSNUM1.
              03  HCN062-KOHJKYSNUM1S PIC X(1)    OCCURS  7 TIMES.
            02  HCN062-KOHJKYSNUM2.
              03  HCN062-KOHJKYSNUM2S PIC X(1)    OCCURS  7 TIMES.
            02  HCN062-KOHJKYSNUM1N PIC X(20).
            02  HCN062-KOHJKYSNUM2N PIC X(20).
            02  HCN062-HKNSBT1  PIC X(6).
            02  HCN062-HKNSBT2  PIC X(6).
            02  HCN062-HONKZK   PIC X(10).
            02  HCN062-KIGO PIC X(34).
            02  HCN062-NUM  PIC X(34).
            02  HCN062-RENNUM   PIC X(7).
            02  HCN062-KANANAME PIC X(30).
            02  HCN062-NAME PIC X(44).

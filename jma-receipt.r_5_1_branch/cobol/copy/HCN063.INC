        01  HCN063.
            02  HCN063-TEISYUTUSAKI PIC X(6).
            02  HCN063-PTNUM    PIC X(20).
            02  HCN063-SRYY PIC X(2).
            02  HCN063-SRYM PIC X(2).
            02  HCN063-PREFNUM  PIC X(2).
            02  HCN063-HOSPCDN  PIC X(20).
            02  HCN063-NAIYO-TBL        OCCURS  138 TIMES.
              03  HCN063-SRYKBN   PIC X(2).
              03  HCN063-TEKIYO   PIC X(52).
            02  HCN063-RJNFTNJANUM.
              03  HCN063-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN063-KOHFTNJANUM1.
              03  HCN063-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN063-KOHFTNJANUM2.
              03  HCN063-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN063-KOHFTNJANUM1N    PIC X(20).
            02  HCN063-KOHFTNJANUM2N    PIC X(20).
            02  HCN063-HKNJANUM.
              03  HCN063-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN063-KYURATE  PIC X(8).
            02  HCN063-RJNJKYSNUM.
              03  HCN063-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN063-KOHJKYSNUM1.
              03  HCN063-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN063-KOHJKYSNUM2.
              03  HCN063-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN063-KOHJKYSNUM1N PIC X(20).
            02  HCN063-KOHJKYSNUM2N PIC X(20).
            02  HCN063-HKNSBT1  PIC X(6).
            02  HCN063-HKNSBT2  PIC X(6).
            02  HCN063-HONKZK   PIC X(10).
            02  HCN063-KIGO PIC X(34).
            02  HCN063-NUM  PIC X(34).
            02  HCN063-RENNUM   PIC X(7).
            02  HCN063-KANANAME PIC X(30).
            02  HCN063-NAME PIC X(44).
            02  HCN063-TYTLE    PIC X(14).

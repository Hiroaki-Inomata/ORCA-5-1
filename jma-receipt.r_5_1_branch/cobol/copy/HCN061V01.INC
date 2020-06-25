        01  HCN061V01.
            02  HCN061V01-TEISYUTUSAKI  PIC X(6).
            02  HCN061V01-PTNUM PIC X(20).
            02  HCN061V01-SRYY  PIC X(2).
            02  HCN061V01-SRYM  PIC X(2).
            02  HCN061V01-PREFNUM   PIC X(2).
            02  HCN061V01-HOSPCDN   PIC X(20).
            02  HCN061V01-NAIYO-TBL     OCCURS  140 TIMES.
              03  HCN061V01-SRYKBN    PIC X(2).
              03  HCN061V01-TEKIYO    PIC X(52).
            02  HCN061V01-RJNFTNJANUM.
              03  HCN061V01-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V01-KOHFTNJANUM1.
              03  HCN061V01-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V01-KOHFTNJANUM2.
              03  HCN061V01-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V01-KOHFTNJANUM1N PIC X(20).
            02  HCN061V01-KOHFTNJANUM2N PIC X(20).
            02  HCN061V01-HKNJANUM.
              03  HCN061V01-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V01-KYURATE   PIC X(8).
            02  HCN061V01-RJNJKYSNUM.
              03  HCN061V01-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V01-KOHJKYSNUM1.
              03  HCN061V01-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V01-KOHJKYSNUM2.
              03  HCN061V01-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V01-KOHJKYSNUM1N  PIC X(20).
            02  HCN061V01-KOHJKYSNUM2N  PIC X(20).
            02  HCN061V01-HKNSBT1   PIC X(6).
            02  HCN061V01-HKNSBT2   PIC X(6).
            02  HCN061V01-HONKZK    PIC X(10).
            02  HCN061V01-KIGO  PIC X(34).
            02  HCN061V01-NUM   PIC X(34).
            02  HCN061V01-RENNUM    PIC X(7).
            02  HCN061V01-NAME  PIC X(44).
            02  HCN061V01-TYTLE PIC X(14).

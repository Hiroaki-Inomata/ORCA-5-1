        01  HCN061V02.
            02  HCN061V02-TEISYUTUSAKI  PIC X(6).
            02  HCN061V02-PTNUM PIC X(20).
            02  HCN061V02-SRYY  PIC X(2).
            02  HCN061V02-SRYM  PIC X(2).
            02  HCN061V02-PREFNUM   PIC X(2).
            02  HCN061V02-HOSPCDN   PIC X(20).
            02  HCN061V02-NAIYO-TBL     OCCURS  140 TIMES.
              03  HCN061V02-SRYKBN    PIC X(2).
              03  HCN061V02-TEKIYO    PIC X(52).
            02  HCN061V02-RJNFTNJANUM.
              03  HCN061V02-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V02-KOHFTNJANUM1.
              03  HCN061V02-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V02-KOHFTNJANUM2.
              03  HCN061V02-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V02-KOHFTNJANUM1N PIC X(20).
            02  HCN061V02-KOHFTNJANUM2N PIC X(20).
            02  HCN061V02-HKNJANUM.
              03  HCN061V02-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN061V02-KYURATE   PIC X(8).
            02  HCN061V02-RJNJKYSNUM.
              03  HCN061V02-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V02-KOHJKYSNUM1.
              03  HCN061V02-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V02-KOHJKYSNUM2.
              03  HCN061V02-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN061V02-KOHJKYSNUM1N  PIC X(20).
            02  HCN061V02-KOHJKYSNUM2N  PIC X(20).
            02  HCN061V02-HKNSBT1   PIC X(6).
            02  HCN061V02-HKNSBT2   PIC X(6).
            02  HCN061V02-HONKZK    PIC X(10).
            02  HCN061V02-KIGO  PIC X(34).
            02  HCN061V02-NUM   PIC X(34).
            02  HCN061V02-RENNUM    PIC X(7).
            02  HCN061V02-NAME  PIC X(44).
            02  HCN061V02-TYTLE PIC X(14).
            02  HCN061V02-PAGE  PIC X(5).

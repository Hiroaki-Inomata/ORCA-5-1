        01  HCM14V01.
            02  HCM14V01-TEISYUTUSAKI   PIC X(6).
            02  HCM14V01-PTNUM  PIC X(20).
            02  HCM14V01-SRYY   PIC X(2).
            02  HCM14V01-SRYM   PIC X(2).
            02  HCM14V01-PREFNUM    PIC X(2).
            02  HCM14V01-HOSPCDN    PIC X(20).
            02  HCM14V01-NAIYO-TBL      OCCURS  138 TIMES.
              03  HCM14V01-SRYKBN PIC X(2).
              03  HCM14V01-TEKIYO PIC X(52).
            02  HCM14V01-RJNFTNJANUM.
              03  HCM14V01-RJNFTNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14V01-KOHFTNJANUM1.
              03  HCM14V01-KOHFTNJANUM1S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14V01-KOHFTNJANUM2.
              03  HCM14V01-KOHFTNJANUM2S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14V01-KOHFTNJANUM1N  PIC X(20).
            02  HCM14V01-KOHFTNJANUM2N  PIC X(20).
            02  HCM14V01-HKNJANUM.
              03  HCM14V01-HKNJANUMS
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM14V01-KYURATE    PIC X(8).
            02  HCM14V01-RJNJKYSNUM.
              03  HCM14V01-RJNJKYSNUMS
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14V01-KOHJKYSNUM1.
              03  HCM14V01-KOHJKYSNUM1S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14V01-KOHJKYSNUM2.
              03  HCM14V01-KOHJKYSNUM2S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM14V01-KOHJKYSNUM1N   PIC X(20).
            02  HCM14V01-KOHJKYSNUM2N   PIC X(20).
            02  HCM14V01-HKNSBT1    PIC X(6).
            02  HCM14V01-HKNSBT2    PIC X(6).
            02  HCM14V01-HONKZK PIC X(10).
            02  HCM14V01-KIGO   PIC X(34).
            02  HCM14V01-NUM    PIC X(34).
            02  HCM14V01-RENNUM PIC X(7).
            02  HCM14V01-KANANAME   PIC X(30).
            02  HCM14V01-NAME   PIC X(44).
            02  HCM14V01-TYTLE  PIC X(14).
            02  HCM14V01-PAGE   PIC X(5).
            02  HCM14V01-MOJI1  PIC X(10).
            02  HCM14V01-MOJI2  PIC X(10).
            02  HCM14V01-BTUNUM PIC X(9).
            02  HCM14V01-LABEL  PIC X(14).

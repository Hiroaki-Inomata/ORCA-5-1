        01  HCRCP060V03.
            02  HCRCP060V03-TEISYUTUSAKI    PIC X(6).
            02  HCRCP060V03-PTNUM   PIC X(20).
            02  HCRCP060V03-SRYY    PIC X(2).
            02  HCRCP060V03-SRYM    PIC X(2).
            02  HCRCP060V03-PREFNUM PIC X(2).
            02  HCRCP060V03-HOSPCDN PIC X(20).
            02  HCRCP060V03-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCRCP060V03-KHNTEN1 PIC X(7).
            02  HCRCP060V03-SRYKBN
                    PIC X(2)    OCCURS  33 TIMES.
            02  HCRCP060V03-TEKIYO
                    PIC X(52)   OCCURS  33 TIMES.
            02  HCRCP060V03-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP060V03-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP060V03-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP060V03-OCR1    PIC X(67).
            02  HCRCP060V03-OCR2    PIC X(67).
            02  HCRCP060V03-OCR3    PIC X(67).
            02  HCRCP060V03-RJNFTNJANUM.
              03  HCRCP060V03-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP060V03-KOH-TBL1.
              03  HCRCP060V03-KOHFTNJANUM1.
                04  HCRCP060V03-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP060V03-KOHFTNJANUM1N   PIC X(20).
              03  HCRCP060V03-KOHJKYSNUM1.
                04  HCRCP060V03-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP060V03-KOHJKYSNUM1N    PIC X(20).
            02  HCRCP060V03-KOH-TBL2.
              03  HCRCP060V03-KOHFTNJANUM2.
                04  HCRCP060V03-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP060V03-KOHFTNJANUM2N   PIC X(20).
              03  HCRCP060V03-KOHJKYSNUM2.
                04  HCRCP060V03-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP060V03-KOHJKYSNUM2N    PIC X(20).
            02  HCRCP060V03-HKNJANUM.
              03  HCRCP060V03-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP060V03-KYURATE PIC X(8).
            02  HCRCP060V03-RJNJKYSNUM.
              03  HCRCP060V03-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP060V03-HKNSBT1 PIC X(6).
            02  HCRCP060V03-HKNSBT2 PIC X(6).
            02  HCRCP060V03-HONKZK  PIC X(10).
            02  HCRCP060V03-KIGO    PIC X(34).
            02  HCRCP060V03-NUM PIC X(34).
            02  HCRCP060V03-KANANAME    PIC X(40).
            02  HCRCP060V03-KANANAME2   PIC X(50).
            02  HCRCP060V03-SEXNUM  PIC X(2).
            02  HCRCP060V03-BIRTHDAY    PIC X(22).
            02  HCRCP060V03-JIYU    PIC X(20).
            02  HCRCP060V03-SEXN    PIC X(2).
            02  HCRCP060V03-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP060V03-HOSPTEL PIC X(15).
            02  HCRCP060V03-SRYKA   PIC X(30).
            02  HCRCP060V03-BEDSU   PIC X(4).
            02  HCRCP060V03-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP060V03-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP060V03-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP060V03-JINISSU PIC X(2).
            02  HCRCP060V03-JINISSU1    PIC X(2).
            02  HCRCP060V03-JINISSU2    PIC X(2).
            02  HCRCP060V03-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP060V03-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCRCP060V03-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCRCP060V03-HOSPNAME    PIC X(80).
            02  HCRCP060V03-HOSPADRS    PIC X(100).
            02  HCRCP060V03-NAME    PIC X(50).
            02  HCRCP060V03-HEAD1-G.
              03  HCRCP060V03-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP060V03-HEAD2-G.
              03  HCRCP060V03-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP060V03-HEAD3-G.
              03  HCRCP060V03-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP060V03-HEAD4-G.
              03  HCRCP060V03-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCRCP060V03-BODY    PIC X(52).
            02  HCRCP060V03-FOOT-G.
              03  HCRCP060V03-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCRCP060V03-SHOSHINTIME PIC X(10).
            02  HCRCP060V03-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP060V03-FTNMONEYSAI PIC X(11).
            02  HCRCP060V03-RENNUM  PIC X(7).
            02  HCRCP060V03-SRYKA2  PIC X(60).
            02  HCRCP060V03-KHNTEN3 PIC X(7).
            02  HCRCP060V03-SONOTA  PIC X(10).
            02  HCRCP060V03-KBN.
              03  HCRCP060V03-KBN-S
                      PIC X(1)    OCCURS  2 TIMES.
            02  HCRCP060V03-KBNN.
              03  HCRCP060V03-KBNN-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCRCP060V03-SICYOSON.
              03  HCRCP060V03-SICYOSON-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCRCP060V03-SICYOSONN.
              03  HCRCP060V03-SICYOSONN-S
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCRCP060V03-SYONUM  PIC X(9).
            02  HCRCP060V03-SYONUMN PIC X(20).
            02  HCRCP060V03-ITBKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCRCP060V03-ITBKINGAK   PIC X(8).
            02  HCRCP060V03-SAKUJYOSEN  PIC X(52).
            02  HCRCP060V03-PAGE    PIC X(5).
            02  HCRCP060V03-TOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCRCP060V03-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.

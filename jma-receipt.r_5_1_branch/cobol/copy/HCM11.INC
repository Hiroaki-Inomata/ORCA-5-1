        01  HCM11.
            02  HCM11-TEISYUTUSAKI  PIC X(6).
            02  HCM11-PTNUM PIC X(20).
            02  HCM11-SRYY  PIC X(2).
            02  HCM11-SRYM  PIC X(2).
            02  HCM11-PREFNUM   PIC X(2).
            02  HCM11-HOSPCDN   PIC X(20).
            02  HCM11-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM11-KHNTEN1   PIC X(7).
            02  HCM11-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM11-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM11-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM11-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCM11-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCM11-OCR1  PIC X(67).
            02  HCM11-OCR2  PIC X(67).
            02  HCM11-OCR3  PIC X(67).
            02  HCM11-RJNFTNJANUM.
              03  HCM11-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM11-KOH-TBL1.
              03  HCM11-KOHFTNJANUM1.
                04  HCM11-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM11-KOHFTNJANUM1N PIC X(20).
              03  HCM11-KOHJKYSNUM1.
                04  HCM11-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM11-KOHJKYSNUM1N  PIC X(20).
            02  HCM11-KOH-TBL2.
              03  HCM11-KOHFTNJANUM2.
                04  HCM11-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM11-KOHFTNJANUM2N PIC X(20).
              03  HCM11-KOHJKYSNUM2.
                04  HCM11-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM11-KOHJKYSNUM2N  PIC X(20).
            02  HCM11-HKNJANUM.
              03  HCM11-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM11-KYURATE   PIC X(8).
            02  HCM11-RJNJKYSNUM.
              03  HCM11-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM11-HKNSBT1   PIC X(6).
            02  HCM11-HKNSBT2   PIC X(6).
            02  HCM11-HONKZK    PIC X(10).
            02  HCM11-KIGO  PIC X(34).
            02  HCM11-NUM   PIC X(34).
            02  HCM11-KANANAME  PIC X(40).
            02  HCM11-KANANAME2 PIC X(50).
            02  HCM11-SEXNUM    PIC X(2).
            02  HCM11-BIRTHDAY  PIC X(22).
            02  HCM11-JIYU  PIC X(20).
            02  HCM11-SEXN  PIC X(2).
            02  HCM11-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCM11-HOSPTEL   PIC X(15).
            02  HCM11-SRYKA PIC X(30).
            02  HCM11-BEDSU PIC X(4).
            02  HCM11-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM11-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM11-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCM11-JINISSU   PIC X(2).
            02  HCM11-JINISSU1  PIC X(2).
            02  HCM11-JINISSU2  PIC X(2).
            02  HCM11-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM11-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM11-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCM11-HOSPNAME  PIC X(80).
            02  HCM11-HOSPADRS  PIC X(100).
            02  HCM11-NAME  PIC X(50).
            02  HCM11-HEAD1-G.
              03  HCM11-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCM11-HEAD2-G.
              03  HCM11-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCM11-HEAD3-G.
              03  HCM11-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCM11-HEAD4-G.
              03  HCM11-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM11-BODY  PIC X(52).
            02  HCM11-FOOT-G.
              03  HCM11-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCM11-SHOSHINTIME   PIC X(10).
            02  HCM11-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCM11-FTNMONEYSAI   PIC X(11).
            02  HCM11-FTNMONEYSAI2  PIC X(11).
            02  HCM11-RENNUM    PIC X(7).
            02  HCM11-SRYKA2    PIC X(60).
            02  HCM11-KHNTEN3   PIC X(7).
            02  HCM11-SONOTA    PIC X(10).
            02  HCM11-PAGE  PIC X(5).
            02  HCM11-MOJI1 PIC X(10).
            02  HCM11-MOJI2 PIC X(10).
            02  HCM11-TOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM11-FTNMONEY1
                    PIC X(9)    OCCURS  4 TIMES.
            02  HCM11-FTNMONEY-GEN  PIC X(8).
            02  HCM11-GENMEN    PIC X(10).
            02  HCM11-GENMENKIN PIC X(7).
            02  HCM11-GENMEN2   PIC X(2).

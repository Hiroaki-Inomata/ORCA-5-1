        01  HCM10.
            02  HCM10-TEISYUTUSAKI  PIC X(6).
            02  HCM10-PTNUM PIC X(20).
            02  HCM10-SRYY  PIC X(2).
            02  HCM10-SRYM  PIC X(2).
            02  HCM10-PREFNUM   PIC X(2).
            02  HCM10-HOSPCDN   PIC X(20).
            02  HCM10-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM10-KHNTEN1   PIC X(7).
            02  HCM10-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM10-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM10-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM10-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCM10-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCM10-OCR1  PIC X(67).
            02  HCM10-OCR2  PIC X(67).
            02  HCM10-OCR3  PIC X(67).
            02  HCM10-RJNFTNJANUM.
              03  HCM10-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM10-KOH-TBL1.
              03  HCM10-KOHFTNJANUM1.
                04  HCM10-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM10-KOHFTNJANUM1N PIC X(20).
              03  HCM10-KOHJKYSNUM1.
                04  HCM10-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM10-KOHJKYSNUM1N  PIC X(20).
            02  HCM10-KOH-TBL2.
              03  HCM10-KOHFTNJANUM2.
                04  HCM10-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM10-KOHFTNJANUM2N PIC X(20).
              03  HCM10-KOHJKYSNUM2.
                04  HCM10-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM10-KOHJKYSNUM2N  PIC X(20).
            02  HCM10-HKNJANUM.
              03  HCM10-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM10-KYURATE   PIC X(8).
            02  HCM10-RJNJKYSNUM.
              03  HCM10-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM10-HKNSBT1   PIC X(6).
            02  HCM10-HKNSBT2   PIC X(6).
            02  HCM10-HONKZK    PIC X(10).
            02  HCM10-KIGO  PIC X(34).
            02  HCM10-NUM   PIC X(34).
            02  HCM10-KANANAME  PIC X(40).
            02  HCM10-KANANAME2 PIC X(50).
            02  HCM10-SEXNUM    PIC X(2).
            02  HCM10-BIRTHDAY  PIC X(22).
            02  HCM10-JIYU  PIC X(20).
            02  HCM10-SEXN  PIC X(2).
            02  HCM10-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCM10-HOSPTEL   PIC X(15).
            02  HCM10-SRYKA PIC X(30).
            02  HCM10-BEDSU PIC X(4).
            02  HCM10-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM10-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM10-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCM10-JINISSU   PIC X(2).
            02  HCM10-JINISSU1  PIC X(2).
            02  HCM10-JINISSU2  PIC X(2).
            02  HCM10-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM10-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM10-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCM10-HOSPNAME  PIC X(80).
            02  HCM10-HOSPADRS  PIC X(100).
            02  HCM10-NAME  PIC X(50).
            02  HCM10-HEAD1-G.
              03  HCM10-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCM10-HEAD2-G.
              03  HCM10-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCM10-HEAD3-G.
              03  HCM10-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCM10-HEAD4-G.
              03  HCM10-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM10-BODY  PIC X(52).
            02  HCM10-FOOT-G.
              03  HCM10-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCM10-SHOSHINTIME   PIC X(10).
            02  HCM10-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCM10-FTNMONEYSAI   PIC X(11).
            02  HCM10-RENNUM    PIC X(7).
            02  HCM10-SRYKA2    PIC X(60).
            02  HCM10-KHNTEN3   PIC X(7).
            02  HCM10-SONOTA    PIC X(10).
            02  HCM10-PAGE  PIC X(5).
            02  HCM10-MOJI1 PIC X(10).
            02  HCM10-MOJI2 PIC X(10).
            02  HCM10-TOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM10-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCM10-TITLE PIC X(8).

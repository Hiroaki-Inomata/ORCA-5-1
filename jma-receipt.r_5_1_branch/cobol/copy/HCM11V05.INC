        01  HCM11V05.
            02  HCM11V05-TEISYUTUSAKI
                        PIC X(6).
            02  HCM11V05-PTNUM
                        PIC X(20).
            02  HCM11V05-SRYY
                        PIC X(2).
            02  HCM11V05-SRYM
                        PIC X(2).
            02  HCM11V05-PREFNUM
                        PIC X(2).
            02  HCM11V05-HOSPCDN
                        PIC X(20).
            02  HCM11V05-KHNTEN2
                        PIC X(7)        OCCURS 2 TIMES.
            02  HCM11V05-KHNTEN1
                        PIC X(7).
            02  HCM11V05-SRYKBN
                        PIC X(2)        OCCURS 35 TIMES.
            02  HCM11V05-TEKIYO
                        PIC X(52)       OCCURS 35 TIMES.
            02  HCM11V05-KOUTEN
                        PIC X(7)        OCCURS 35 TIMES.
            02  HCM11V05-TOTALTEN
                        PIC X(7)        OCCURS 3 TIMES.
            02  HCM11V05-FTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCM11V05-OCR1
                        PIC X(67).
            02  HCM11V05-OCR2
                        PIC X(67).
            02  HCM11V05-OCR3
                        PIC X(67).
            02  HCM11V05-RJNFTNJANUM.
              03  HCM11V05-RJNFTNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM11V05-KOH-TBL1.
              03  HCM11V05-KOHFTNJANUM1.
                04  HCM11V05-KOHFTNJANUM1-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCM11V05-KOHFTNJANUM1N
                        PIC X(20).
              03  HCM11V05-KOHJKYSNUM1.
                04  HCM11V05-KOHJKYSNUM1-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCM11V05-KOHJKYSNUM1N
                        PIC X(20).
            02  HCM11V05-KOH-TBL2.
              03  HCM11V05-KOHFTNJANUM2.
                04  HCM11V05-KOHFTNJANUM2-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCM11V05-KOHFTNJANUM2N
                        PIC X(20).
              03  HCM11V05-KOHJKYSNUM2.
                04  HCM11V05-KOHJKYSNUM2-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCM11V05-KOHJKYSNUM2N
                        PIC X(20).
            02  HCM11V05-HKNJANUM.
              03  HCM11V05-HKNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM11V05-KYURATE
                        PIC X(8).
            02  HCM11V05-RJNJKYSNUM.
              03  HCM11V05-RJNJKYSNUM-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM11V05-HKNSBT1
                        PIC X(6).
            02  HCM11V05-HKNSBT2
                        PIC X(6).
            02  HCM11V05-HONKZK
                        PIC X(10).
            02  HCM11V05-KIGO
                        PIC X(34).
            02  HCM11V05-NUM
                        PIC X(34).
            02  HCM11V05-KANANAME
                        PIC X(40).
            02  HCM11V05-KANANAME2
                        PIC X(50).
            02  HCM11V05-SEXNUM
                        PIC X(2).
            02  HCM11V05-BIRTHDAY
                        PIC X(22).
            02  HCM11V05-JIYU
                        PIC X(20).
            02  HCM11V05-SEXN
                        PIC X(2).
            02  HCM11V05-TOKKI
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCM11V05-HOSPTEL
                        PIC X(15).
            02  HCM11V05-SRYKA
                        PIC X(30).
            02  HCM11V05-BEDSU
                        PIC X(4).
            02  HCM11V05-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCM11V05-SRYYMD
                        PIC X(24)       OCCURS 5 TIMES.
            02  HCM11V05-TENKI
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCM11V05-JINISSU
                        PIC X(2).
            02  HCM11V05-JINISSU1
                        PIC X(2).
            02  HCM11V05-JINISSU2
                        PIC X(2).
            02  HCM11V05-TENSU
                        PIC X(7)        OCCURS 35 TIMES.
            02  HCM11V05-KAISU
                        PIC X(3)        OCCURS 35 TIMES.
            02  HCM11V05-KHNTEN
                        PIC X(7)        OCCURS 5 TIMES.
            02  HCM11V05-HOSPNAME
                        PIC X(80).
            02  HCM11V05-HOSPADRS
                        PIC X(100).
            02  HCM11V05-NAME
                        PIC X(50).
            02  HCM11V05-HEAD1-G.
              03  HCM11V05-HEAD1
                        PIC X(46)       OCCURS 2 TIMES.
            02  HCM11V05-HEAD2-G.
              03  HCM11V05-HEAD2
                        PIC X(2)        OCCURS 7 TIMES.
            02  HCM11V05-HEAD3-G.
              03  HCM11V05-HEAD3
                        PIC X(50)       OCCURS 2 TIMES.
            02  HCM11V05-HEAD4-G.
              03  HCM11V05-HEAD4
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCM11V05-BODY
                        PIC X(52).
            02  HCM11V05-FOOT-G.
              03  HCM11V05-FOOT
                        PIC X(46)       OCCURS 4 TIMES.
            02  HCM11V05-SHOSHINTIME
                        PIC X(10).
            02  HCM11V05-TENKIYMD
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCM11V05-FTNMONEYSAI
                        PIC X(11).
            02  HCM11V05-FTNMONEYSAI2
                        PIC X(11).
            02  HCM11V05-RENNUM
                        PIC X(7).
            02  HCM11V05-SRYKA2
                        PIC X(60).
            02  HCM11V05-KHNTEN3
                        PIC X(7).
            02  HCM11V05-SONOTA
                        PIC X(10).
            02  HCM11V05-PAGE
                        PIC X(5).
            02  HCM11V05-MOJI1
                        PIC X(10).
            02  HCM11V05-MOJI2
                        PIC X(10).
            02  HCM11V05-TOTALTEN1
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCM11V05-FTNMONEY1
                        PIC X(9)        OCCURS 4 TIMES.
            02  HCM11V05-FTNMONEY-GEN
                        PIC X(8).
            02  HCM11V05-GENMEN
                        PIC X(10).
            02  HCM11V05-GENMENKIN
                        PIC X(7).
            02  HCM11V05-GENMEN2
                        PIC X(2).
            02  HCM11V05-LABEL
                        PIC X(14).
            02  HCM11V05-HEAD5
                        PIC X(20).
            02  HCM11V05-LOPTION
                        PIC X(100).

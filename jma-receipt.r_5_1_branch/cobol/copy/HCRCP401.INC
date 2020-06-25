        01  HCRCP401.
            02  HCRCP401-TEISYUTUSAKI   PIC X(6).
            02  HCRCP401-PTNUM  PIC X(20).
            02  HCRCP401-SRYY   PIC X(2).
            02  HCRCP401-SRYM   PIC X(2).
            02  HCRCP401-PREFNUM    PIC X(2).
            02  HCRCP401-HOSPCDN    PIC X(20).
            02  HCRCP401-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCRCP401-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCRCP401-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCRCP401-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCRCP401-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP401-OCR1   PIC X(67).
            02  HCRCP401-OCR2   PIC X(67).
            02  HCRCP401-OCR3   PIC X(67).
            02  HCRCP401-RJNFTNJANUM.
              03  HCRCP401-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP401-KOH-TBL1.
              03  HCRCP401-KOHFTNJANUM1.
                04  HCRCP401-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP401-KOHFTNJANUM1N  PIC X(20).
              03  HCRCP401-KOHJKYSNUM1.
                04  HCRCP401-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP401-KOHJKYSNUM1N   PIC X(20).
            02  HCRCP401-KOH-TBL2.
              03  HCRCP401-KOHFTNJANUM2.
                04  HCRCP401-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP401-KOHFTNJANUM2N  PIC X(20).
              03  HCRCP401-KOHJKYSNUM2.
                04  HCRCP401-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP401-KOHJKYSNUM2N   PIC X(20).
            02  HCRCP401-HKNJANUM.
              03  HCRCP401-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP401-KYURATE    PIC X(8).
            02  HCRCP401-RJNJKYSNUM.
              03  HCRCP401-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP401-HKNSBT1    PIC X(6).
            02  HCRCP401-HKNSBT2    PIC X(6).
            02  HCRCP401-HONKZK PIC X(10).
            02  HCRCP401-KIGO   PIC X(34).
            02  HCRCP401-NUM    PIC X(34).
            02  HCRCP401-KANANAME   PIC X(40).
            02  HCRCP401-KANANAME2  PIC X(50).
            02  HCRCP401-SEXNUM PIC X(2).
            02  HCRCP401-BIRTHDAY   PIC X(22).
            02  HCRCP401-JIYU   PIC X(20).
            02  HCRCP401-SEXN   PIC X(2).
            02  HCRCP401-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP401-HOSPTEL    PIC X(15).
            02  HCRCP401-SRYKA  PIC X(30).
            02  HCRCP401-BEDSU  PIC X(4).
            02  HCRCP401-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP401-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP401-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP401-JINISSU    PIC X(2).
            02  HCRCP401-JINISSU1   PIC X(2).
            02  HCRCP401-JINISSU2   PIC X(2).
            02  HCRCP401-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCRCP401-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCRCP401-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP401-HOSPNAME   PIC X(80).
            02  HCRCP401-HOSPADRS   PIC X(100).
            02  HCRCP401-NAME   PIC X(50).
            02  HCRCP401-HEAD1-G.
              03  HCRCP401-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP401-HEAD2-G.
              03  HCRCP401-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP401-HEAD3-G.
              03  HCRCP401-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP401-HEAD4-G.
              03  HCRCP401-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCRCP401-BODY   PIC X(52).
            02  HCRCP401-SHOSHINTIME    PIC X(10).
            02  HCRCP401-SIDOTIME   PIC X(10).
            02  HCRCP401-ZAITAKUTIME    PIC X(10).
            02  HCRCP401-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP401-FTNMONEYSAI    PIC X(11).
            02  HCRCP401-RENNUM PIC X(7).
            02  HCRCP401-SRYKA2 PIC X(60).
            02  HCRCP401-NYUINKBN   PIC X(42).
            02  HCRCP401-NYUINYMD.
              03  HCRCP401-NYUINYY    PIC X(2).
              03  HCRCP401-NYUINMM    PIC X(2).
              03  HCRCP401-NYUINDD    PIC X(2).
            02  HCRCP401-HOSPSBT    PIC X(2).
            02  HCRCP401-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCRCP401-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCRCP401-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCRCP401-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCRCP401-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCRCP401-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCRCP401-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCRCP401-FOOT   PIC X(28).
            02  HCRCP401-GAIITBFTN  PIC X(7).
            02  HCRCP401-GENGAKUMEI PIC X(30).
            02  HCRCP401-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCRCP401-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP401-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP401-SYOFTNMONEYSAI PIC X(11).
            02  HCRCP401-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCRCP401-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCRCP401-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCRCP401-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCRCP401-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCRCP401-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCRCP401-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP401-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP401-KYUFUGAI1  PIC X(10).
            02  HCRCP401-KYUFUGAI2  PIC X(10).
            02  HCRCP401-KYUFUGAI3  PIC X(12).
            02  HCRCP401-KYUFUGAI-TEN   PIC X(5).
            02  HCRCP401-KYUFUGAI4  PIC X(2).

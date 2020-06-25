        01  HCRCP403.
            02  HCRCP403-TEISYUTUSAKI   PIC X(6).
            02  HCRCP403-PTNUM  PIC X(20).
            02  HCRCP403-SRYY   PIC X(2).
            02  HCRCP403-SRYM   PIC X(2).
            02  HCRCP403-PREFNUM    PIC X(2).
            02  HCRCP403-HOSPCDN    PIC X(20).
            02  HCRCP403-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCRCP403-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCRCP403-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCRCP403-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCRCP403-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP403-OCR1   PIC X(67).
            02  HCRCP403-OCR2   PIC X(67).
            02  HCRCP403-OCR3   PIC X(67).
            02  HCRCP403-RJNFTNJANUM.
              03  HCRCP403-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP403-KOH-TBL1.
              03  HCRCP403-KOHFTNJANUM1.
                04  HCRCP403-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP403-KOHFTNJANUM1N  PIC X(20).
              03  HCRCP403-KOHJKYSNUM1.
                04  HCRCP403-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP403-KOHJKYSNUM1N   PIC X(20).
            02  HCRCP403-KOH-TBL2.
              03  HCRCP403-KOHFTNJANUM2.
                04  HCRCP403-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP403-KOHFTNJANUM2N  PIC X(20).
              03  HCRCP403-KOHJKYSNUM2.
                04  HCRCP403-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP403-KOHJKYSNUM2N   PIC X(20).
            02  HCRCP403-HKNJANUM.
              03  HCRCP403-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP403-KYURATE    PIC X(8).
            02  HCRCP403-RJNJKYSNUM.
              03  HCRCP403-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP403-HKNSBT1    PIC X(6).
            02  HCRCP403-HKNSBT2    PIC X(6).
            02  HCRCP403-HONKZK PIC X(10).
            02  HCRCP403-KIGO   PIC X(34).
            02  HCRCP403-NUM    PIC X(34).
            02  HCRCP403-KANANAME   PIC X(40).
            02  HCRCP403-KANANAME2  PIC X(50).
            02  HCRCP403-SEXNUM PIC X(2).
            02  HCRCP403-BIRTHDAY   PIC X(22).
            02  HCRCP403-JIYU   PIC X(20).
            02  HCRCP403-SEXN   PIC X(2).
            02  HCRCP403-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP403-HOSPTEL    PIC X(15).
            02  HCRCP403-SRYKA  PIC X(30).
            02  HCRCP403-BEDSU  PIC X(4).
            02  HCRCP403-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP403-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP403-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP403-JINISSU    PIC X(2).
            02  HCRCP403-JINISSU1   PIC X(2).
            02  HCRCP403-JINISSU2   PIC X(2).
            02  HCRCP403-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCRCP403-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCRCP403-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP403-HOSPNAME   PIC X(80).
            02  HCRCP403-HOSPADRS   PIC X(100).
            02  HCRCP403-NAME   PIC X(50).
            02  HCRCP403-HEAD1-G.
              03  HCRCP403-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP403-HEAD2-G.
              03  HCRCP403-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP403-HEAD3-G.
              03  HCRCP403-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP403-HEAD4-G.
              03  HCRCP403-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCRCP403-BODY   PIC X(52).
            02  HCRCP403-SHOSHINTIME    PIC X(10).
            02  HCRCP403-SIDOTIME   PIC X(10).
            02  HCRCP403-ZAITAKUTIME    PIC X(10).
            02  HCRCP403-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP403-FTNMONEYSAI    PIC X(11).
            02  HCRCP403-RENNUM PIC X(7).
            02  HCRCP403-SRYKA2 PIC X(60).
            02  HCRCP403-NYUINKBN   PIC X(42).
            02  HCRCP403-NYUINYMD.
              03  HCRCP403-NYUINYY    PIC X(2).
              03  HCRCP403-NYUINMM    PIC X(2).
              03  HCRCP403-NYUINDD    PIC X(2).
            02  HCRCP403-HOSPSBT    PIC X(2).
            02  HCRCP403-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCRCP403-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCRCP403-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCRCP403-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCRCP403-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCRCP403-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCRCP403-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCRCP403-FOOT   PIC X(28).
            02  HCRCP403-GAIITBFTN  PIC X(7).
            02  HCRCP403-GENGAKUMEI PIC X(30).
            02  HCRCP403-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCRCP403-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP403-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP403-SYOFTNMONEYSAI PIC X(11).
            02  HCRCP403-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCRCP403-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCRCP403-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCRCP403-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCRCP403-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCRCP403-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCRCP403-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP403-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCRCP403-KYUFUGAI1  PIC X(10).
            02  HCRCP403-KYUFUGAI2  PIC X(10).
            02  HCRCP403-KYUFUGAI3  PIC X(12).
            02  HCRCP403-KYUFUGAI-TEN   PIC X(5).
            02  HCRCP403-KYUFUGAI4  PIC X(2).
            02  HCRCP403-PAGE   PIC X(5).
            02  HCRCP403-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCRCP403-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCRCP403-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCRCP403-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCRCP403-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCRCP403-TITLE  PIC X(8).
            02  HCRCP403-TITLE1 PIC X(2).
            02  HCRCP403-TITLE2
                    PIC X(2)    OCCURS  4 TIMES.

        01  HCN07.
            02  HCN07-TEISYUTUSAKI  PIC X(6).
            02  HCN07-PTNUM PIC X(20).
            02  HCN07-SRYY  PIC X(2).
            02  HCN07-SRYM  PIC X(2).
            02  HCN07-PREFNUM   PIC X(2).
            02  HCN07-HOSPCDN   PIC X(20).
            02  HCN07-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN07-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN07-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN07-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN07-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN07-OCR1  PIC X(67).
            02  HCN07-OCR2  PIC X(67).
            02  HCN07-OCR3  PIC X(67).
            02  HCN07-RJNFTNJANUM.
              03  HCN07-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN07-KOH-TBL1.
              03  HCN07-KOHFTNJANUM1.
                04  HCN07-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN07-KOHFTNJANUM1N PIC X(20).
              03  HCN07-KOHJKYSNUM1.
                04  HCN07-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN07-KOHJKYSNUM1N  PIC X(20).
            02  HCN07-KOH-TBL2.
              03  HCN07-KOHFTNJANUM2.
                04  HCN07-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN07-KOHFTNJANUM2N PIC X(20).
              03  HCN07-KOHJKYSNUM2.
                04  HCN07-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN07-KOHJKYSNUM2N  PIC X(20).
            02  HCN07-HKNJANUM.
              03  HCN07-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN07-KYURATE   PIC X(8).
            02  HCN07-RJNJKYSNUM.
              03  HCN07-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN07-HKNSBT1   PIC X(6).
            02  HCN07-HKNSBT2   PIC X(6).
            02  HCN07-HONKZK    PIC X(10).
            02  HCN07-KIGO  PIC X(34).
            02  HCN07-NUM   PIC X(34).
            02  HCN07-KANANAME  PIC X(40).
            02  HCN07-KANANAME2 PIC X(50).
            02  HCN07-SEXNUM    PIC X(2).
            02  HCN07-BIRTHDAY  PIC X(22).
            02  HCN07-JIYU  PIC X(20).
            02  HCN07-SEXN  PIC X(2).
            02  HCN07-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN07-HOSPTEL   PIC X(15).
            02  HCN07-SRYKA PIC X(30).
            02  HCN07-BEDSU PIC X(4).
            02  HCN07-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN07-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN07-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN07-JINISSU   PIC X(2).
            02  HCN07-JINISSU1  PIC X(2).
            02  HCN07-JINISSU2  PIC X(2).
            02  HCN07-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN07-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN07-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN07-HOSPNAME  PIC X(80).
            02  HCN07-HOSPADRS  PIC X(100).
            02  HCN07-NAME  PIC X(50).
            02  HCN07-HEAD1-G.
              03  HCN07-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN07-HEAD2-G.
              03  HCN07-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN07-HEAD3-G.
              03  HCN07-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN07-HEAD4-G.
              03  HCN07-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN07-BODY  PIC X(52).
            02  HCN07-SHOSHINTIME   PIC X(10).
            02  HCN07-SIDOTIME  PIC X(10).
            02  HCN07-ZAITAKUTIME   PIC X(10).
            02  HCN07-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN07-FTNMONEYSAI   PIC X(11).
            02  HCN07-RENNUM    PIC X(7).
            02  HCN07-SRYKA2    PIC X(60).
            02  HCN07-NYUINKBN  PIC X(42).
            02  HCN07-NYUINYMD.
              03  HCN07-NYUINYY   PIC X(2).
              03  HCN07-NYUINMM   PIC X(2).
              03  HCN07-NYUINDD   PIC X(2).
            02  HCN07-HOSPSBT   PIC X(2).
            02  HCN07-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN07-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN07-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN07-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN07-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN07-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN07-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN07-FOOT  PIC X(28).
            02  HCN07-GAIITBFTN PIC X(7).
            02  HCN07-GENGAKUMEI    PIC X(30).
            02  HCN07-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN07-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN07-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN07-SYOFTNMONEYSAI    PIC X(11).
            02  HCN07-NYUTENSUN-G       OCCURS  10 TIMES.
              03  HCN07-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN07-NYUKOHTENN-G      OCCURS  10 TIMES.
              03  HCN07-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN07-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN07-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN07-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN07-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN07-PAGE  PIC X(5).
            02  HCN07-MOJI1 PIC X(10).
            02  HCN07-MOJI2 PIC X(10).
            02  HCN07-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN07-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN07-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN07-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN07-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN07-TITLE PIC X(8).
            02  HCN07-TITLE1    PIC X(2).
            02  HCN07-TITLE2
                    PIC X(2)    OCCURS  4 TIMES.

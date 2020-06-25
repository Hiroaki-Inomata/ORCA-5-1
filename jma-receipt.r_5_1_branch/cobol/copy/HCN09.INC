        01  HCN09.
            02  HCN09-TEISYUTUSAKI  PIC X(6).
            02  HCN09-PTNUM PIC X(20).
            02  HCN09-SRYY  PIC X(2).
            02  HCN09-SRYM  PIC X(2).
            02  HCN09-PREFNUM   PIC X(2).
            02  HCN09-HOSPCDN   PIC X(20).
            02  HCN09-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN09-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN09-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN09-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN09-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN09-OCR1  PIC X(67).
            02  HCN09-OCR2  PIC X(67).
            02  HCN09-OCR3  PIC X(67).
            02  HCN09-RJNFTNJANUM.
              03  HCN09-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN09-KOH-TBL1.
              03  HCN09-KOHFTNJANUM1.
                04  HCN09-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN09-KOHFTNJANUM1N PIC X(20).
              03  HCN09-KOHJKYSNUM1.
                04  HCN09-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN09-KOHJKYSNUM1N  PIC X(20).
            02  HCN09-KOH-TBL2.
              03  HCN09-KOHFTNJANUM2.
                04  HCN09-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN09-KOHFTNJANUM2N PIC X(20).
              03  HCN09-KOHJKYSNUM2.
                04  HCN09-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN09-KOHJKYSNUM2N  PIC X(20).
            02  HCN09-HKNJANUM.
              03  HCN09-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN09-KYURATE   PIC X(8).
            02  HCN09-RJNJKYSNUM.
              03  HCN09-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN09-HKNSBT1   PIC X(6).
            02  HCN09-HKNSBT2   PIC X(6).
            02  HCN09-HONKZK    PIC X(10).
            02  HCN09-KIGO  PIC X(34).
            02  HCN09-NUM   PIC X(34).
            02  HCN09-KANANAME  PIC X(40).
            02  HCN09-KANANAME2 PIC X(50).
            02  HCN09-SEXNUM    PIC X(2).
            02  HCN09-BIRTHDAY  PIC X(22).
            02  HCN09-JIYU  PIC X(20).
            02  HCN09-SEXN  PIC X(2).
            02  HCN09-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN09-HOSPTEL   PIC X(15).
            02  HCN09-SRYKA PIC X(30).
            02  HCN09-BEDSU PIC X(4).
            02  HCN09-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN09-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN09-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN09-JINISSU   PIC X(2).
            02  HCN09-JINISSU1  PIC X(2).
            02  HCN09-JINISSU2  PIC X(2).
            02  HCN09-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN09-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN09-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09-HOSPNAME  PIC X(80).
            02  HCN09-HOSPADRS  PIC X(100).
            02  HCN09-NAME  PIC X(50).
            02  HCN09-HEAD1-G.
              03  HCN09-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN09-HEAD2-G.
              03  HCN09-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN09-HEAD3-G.
              03  HCN09-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN09-HEAD4-G.
              03  HCN09-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN09-BODY  PIC X(52).
            02  HCN09-SHOSHINTIME   PIC X(10).
            02  HCN09-SIDOTIME  PIC X(10).
            02  HCN09-ZAITAKUTIME   PIC X(10).
            02  HCN09-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN09-FTNMONEYSAI   PIC X(11).
            02  HCN09-FTNMONEYSAI2  PIC X(11).
            02  HCN09-RENNUM    PIC X(7).
            02  HCN09-SRYKA2    PIC X(60).
            02  HCN09-NYUINKBN  PIC X(42).
            02  HCN09-NYUINYMD.
              03  HCN09-NYUINYY   PIC X(2).
              03  HCN09-NYUINMM   PIC X(2).
              03  HCN09-NYUINDD   PIC X(2).
            02  HCN09-HOSPSBT   PIC X(2).
            02  HCN09-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN09-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN09-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN09-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN09-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN09-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN09-SYOKJN-KHN    PIC X(2).
            02  HCN09-SYOKJN-KHNSEI PIC X(2).
            02  HCN09-SYOGAK-SEI
                    PIC X(4)    OCCURS  2 TIMES.
            02  HCN09-SYOKAISU-SEI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN09-FOOT  PIC X(28).
            02  HCN09-GENGAKUMEI    PIC X(30).
            02  HCN09-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN09-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN09-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN09-SYOFTNMONEYSAI    PIC X(11).
            02  HCN09-NYUTENSUN-G       OCCURS  10 TIMES.
              03  HCN09-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN09-NYUKOHTENN-G      OCCURS  10 TIMES.
              03  HCN09-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN09-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN09-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN09-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09-PAGE  PIC X(5).
            02  HCN09-MOJI1 PIC X(10).
            02  HCN09-MOJI2 PIC X(10).
            02  HCN09-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN09-FTNMONEY1
                    PIC X(9)    OCCURS  4 TIMES.
            02  HCN09-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN09-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN09-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN09-FTNMONEY-GEN  PIC X(8).
            02  HCN09-GENMEN    PIC X(10).
            02  HCN09-GENMENKIN PIC X(7).
            02  HCN09-GENMEN2   PIC X(2).
            02  HCN09-BTUNUM    PIC X(9).

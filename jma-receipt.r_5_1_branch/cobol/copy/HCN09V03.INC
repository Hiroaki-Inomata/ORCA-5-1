        01  HCN09V03.
            02  HCN09V03-TEISYUTUSAKI   PIC X(6).
            02  HCN09V03-PTNUM  PIC X(20).
            02  HCN09V03-SRYY   PIC X(2).
            02  HCN09V03-SRYM   PIC X(2).
            02  HCN09V03-PREFNUM    PIC X(2).
            02  HCN09V03-HOSPCDN    PIC X(20).
            02  HCN09V03-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN09V03-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN09V03-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN09V03-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN09V03-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN09V03-OCR1   PIC X(67).
            02  HCN09V03-OCR2   PIC X(67).
            02  HCN09V03-OCR3   PIC X(67).
            02  HCN09V03-RJNFTNJANUM.
              03  HCN09V03-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN09V03-KOH-TBL1.
              03  HCN09V03-KOHFTNJANUM1.
                04  HCN09V03-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN09V03-KOHFTNJANUM1N  PIC X(20).
              03  HCN09V03-KOHJKYSNUM1.
                04  HCN09V03-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN09V03-KOHJKYSNUM1N   PIC X(20).
            02  HCN09V03-KOH-TBL2.
              03  HCN09V03-KOHFTNJANUM2.
                04  HCN09V03-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN09V03-KOHFTNJANUM2N  PIC X(20).
              03  HCN09V03-KOHJKYSNUM2.
                04  HCN09V03-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN09V03-KOHJKYSNUM2N   PIC X(20).
            02  HCN09V03-HKNJANUM.
              03  HCN09V03-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN09V03-KYURATE    PIC X(8).
            02  HCN09V03-RJNJKYSNUM.
              03  HCN09V03-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN09V03-HKNSBT1    PIC X(6).
            02  HCN09V03-HKNSBT2    PIC X(6).
            02  HCN09V03-HONKZK PIC X(10).
            02  HCN09V03-KIGO   PIC X(34).
            02  HCN09V03-NUM    PIC X(34).
            02  HCN09V03-KANANAME   PIC X(40).
            02  HCN09V03-KANANAME2  PIC X(50).
            02  HCN09V03-SEXNUM PIC X(2).
            02  HCN09V03-BIRTHDAY   PIC X(22).
            02  HCN09V03-JIYU   PIC X(20).
            02  HCN09V03-SEXN   PIC X(2).
            02  HCN09V03-TOKKI
                    PIC X(12)   OCCURS  5 TIMES.
            02  HCN09V03-HOSPTEL    PIC X(15).
            02  HCN09V03-SRYKA  PIC X(30).
            02  HCN09V03-BEDSU  PIC X(4).
            02  HCN09V03-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN09V03-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN09V03-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN09V03-JINISSU    PIC X(2).
            02  HCN09V03-JINISSU1   PIC X(2).
            02  HCN09V03-JINISSU2   PIC X(2).
            02  HCN09V03-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN09V03-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN09V03-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09V03-HOSPNAME   PIC X(80).
            02  HCN09V03-HOSPADRS   PIC X(100).
            02  HCN09V03-NAME   PIC X(50).
            02  HCN09V03-HEAD1-G.
              03  HCN09V03-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN09V03-HEAD2-G.
              03  HCN09V03-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN09V03-HEAD3-G.
              03  HCN09V03-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN09V03-HEAD4-G.
              03  HCN09V03-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN09V03-BODY   PIC X(52).
            02  HCN09V03-SHOSHINTIME    PIC X(10).
            02  HCN09V03-SIDOTIME   PIC X(10).
            02  HCN09V03-ZAITAKUTIME    PIC X(10).
            02  HCN09V03-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN09V03-FTNMONEYSAI    PIC X(11).
            02  HCN09V03-FTNMONEYSAI2   PIC X(11).
            02  HCN09V03-RENNUM PIC X(7).
            02  HCN09V03-SRYKA2 PIC X(60).
            02  HCN09V03-NYUINKBN   PIC X(42).
            02  HCN09V03-NYUINYMD.
              03  HCN09V03-NYUINYY    PIC X(2).
              03  HCN09V03-NYUINMM    PIC X(2).
              03  HCN09V03-NYUINDD    PIC X(2).
            02  HCN09V03-HOSPSBT    PIC X(2).
            02  HCN09V03-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN09V03-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN09V03-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN09V03-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN09V03-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN09V03-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN09V03-SYOKJN-KHN PIC X(2).
            02  HCN09V03-SYOKJN-KHNSEI  PIC X(2).
            02  HCN09V03-SYOGAK-SEI
                    PIC X(4)    OCCURS  2 TIMES.
            02  HCN09V03-SYOKAISU-SEI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN09V03-FOOT   PIC X(28).
            02  HCN09V03-GENGAKUMEI PIC X(30).
            02  HCN09V03-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN09V03-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN09V03-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN09V03-SYOFTNMONEYSAI PIC X(11).
            02  HCN09V03-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCN09V03-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN09V03-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCN09V03-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN09V03-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN09V03-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN09V03-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09V03-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN09V03-PAGE   PIC X(5).
            02  HCN09V03-MOJI1  PIC X(10).
            02  HCN09V03-MOJI2  PIC X(10).
            02  HCN09V03-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN09V03-FTNMONEY1
                    PIC X(9)    OCCURS  4 TIMES.
            02  HCN09V03-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN09V03-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN09V03-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN09V03-FTNMONEY-GEN   PIC X(8).
            02  HCN09V03-GENMEN PIC X(10).
            02  HCN09V03-GENMENKIN  PIC X(7).
            02  HCN09V03-GENMEN2    PIC X(2).
            02  HCN09V03-BTUNUM PIC X(9).
            02  HCN09V03-LABEL  PIC X(14).
            02  HCN09V03-HEAD5  PIC X(20).

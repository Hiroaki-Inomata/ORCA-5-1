        01  HCN08V01.
            02  HCN08V01-TEISYUTUSAKI   PIC X(6).
            02  HCN08V01-PTNUM  PIC X(20).
            02  HCN08V01-SRYY   PIC X(2).
            02  HCN08V01-SRYM   PIC X(2).
            02  HCN08V01-PREFNUM    PIC X(2).
            02  HCN08V01-HOSPCDN    PIC X(20).
            02  HCN08V01-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN08V01-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN08V01-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN08V01-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN08V01-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN08V01-OCR1   PIC X(67).
            02  HCN08V01-OCR2   PIC X(67).
            02  HCN08V01-OCR3   PIC X(67).
            02  HCN08V01-RJNFTNJANUM.
              03  HCN08V01-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN08V01-KOH-TBL1.
              03  HCN08V01-KOHFTNJANUM1.
                04  HCN08V01-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN08V01-KOHFTNJANUM1N  PIC X(20).
              03  HCN08V01-KOHJKYSNUM1.
                04  HCN08V01-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN08V01-KOHJKYSNUM1N   PIC X(20).
            02  HCN08V01-KOH-TBL2.
              03  HCN08V01-KOHFTNJANUM2.
                04  HCN08V01-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN08V01-KOHFTNJANUM2N  PIC X(20).
              03  HCN08V01-KOHJKYSNUM2.
                04  HCN08V01-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN08V01-KOHJKYSNUM2N   PIC X(20).
            02  HCN08V01-HKNJANUM.
              03  HCN08V01-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN08V01-KYURATE    PIC X(8).
            02  HCN08V01-RJNJKYSNUM.
              03  HCN08V01-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN08V01-HKNSBT1    PIC X(6).
            02  HCN08V01-HKNSBT2    PIC X(6).
            02  HCN08V01-HONKZK PIC X(10).
            02  HCN08V01-KIGO   PIC X(34).
            02  HCN08V01-NUM    PIC X(34).
            02  HCN08V01-KANANAME   PIC X(40).
            02  HCN08V01-KANANAME2  PIC X(50).
            02  HCN08V01-SEXNUM PIC X(2).
            02  HCN08V01-BIRTHDAY   PIC X(22).
            02  HCN08V01-JIYU   PIC X(20).
            02  HCN08V01-SEXN   PIC X(2).
            02  HCN08V01-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN08V01-HOSPTEL    PIC X(15).
            02  HCN08V01-SRYKA  PIC X(30).
            02  HCN08V01-BEDSU  PIC X(4).
            02  HCN08V01-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN08V01-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN08V01-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN08V01-JINISSU    PIC X(2).
            02  HCN08V01-JINISSU1   PIC X(2).
            02  HCN08V01-JINISSU2   PIC X(2).
            02  HCN08V01-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN08V01-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN08V01-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN08V01-HOSPNAME   PIC X(80).
            02  HCN08V01-HOSPADRS   PIC X(100).
            02  HCN08V01-NAME   PIC X(50).
            02  HCN08V01-HEAD1-G.
              03  HCN08V01-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN08V01-HEAD2-G.
              03  HCN08V01-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN08V01-HEAD3-G.
              03  HCN08V01-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN08V01-HEAD4-G.
              03  HCN08V01-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN08V01-BODY   PIC X(52).
            02  HCN08V01-SHOSHINTIME    PIC X(10).
            02  HCN08V01-SIDOTIME   PIC X(10).
            02  HCN08V01-ZAITAKUTIME    PIC X(10).
            02  HCN08V01-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN08V01-FTNMONEYSAI    PIC X(11).
            02  HCN08V01-FTNMONEYSAI2   PIC X(11).
            02  HCN08V01-RENNUM PIC X(7).
            02  HCN08V01-SRYKA2 PIC X(60).
            02  HCN08V01-NYUINKBN   PIC X(42).
            02  HCN08V01-NYUINYMD.
              03  HCN08V01-NYUINYY    PIC X(2).
              03  HCN08V01-NYUINMM    PIC X(2).
              03  HCN08V01-NYUINDD    PIC X(2).
            02  HCN08V01-HOSPSBT    PIC X(2).
            02  HCN08V01-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN08V01-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN08V01-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN08V01-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN08V01-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN08V01-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN08V01-SYOKJN-KHN PIC X(2).
            02  HCN08V01-SYOKJN-KHNSEI  PIC X(2).
            02  HCN08V01-SYOGAK-SEI
                    PIC X(4)    OCCURS  2 TIMES.
            02  HCN08V01-SYOKAISU-SEI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN08V01-FOOT   PIC X(28).
            02  HCN08V01-GENGAKUMEI PIC X(30).
            02  HCN08V01-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN08V01-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN08V01-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN08V01-SYOFTNMONEYSAI PIC X(11).
            02  HCN08V01-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCN08V01-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN08V01-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCN08V01-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN08V01-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN08V01-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN08V01-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN08V01-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN08V01-PAGE   PIC X(5).
            02  HCN08V01-MOJI1  PIC X(10).
            02  HCN08V01-MOJI2  PIC X(10).
            02  HCN08V01-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN08V01-FTNMONEY1
                    PIC X(9)    OCCURS  4 TIMES.
            02  HCN08V01-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN08V01-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN08V01-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN08V01-FTNMONEY-GEN   PIC X(8).
            02  HCN08V01-GENMEN PIC X(10).
            02  HCN08V01-GENMENKIN  PIC X(7).
            02  HCN08V01-GENMEN2    PIC X(2).

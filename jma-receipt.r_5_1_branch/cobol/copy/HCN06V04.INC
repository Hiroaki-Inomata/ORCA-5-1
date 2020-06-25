        01  HCN06V04.
            02  HCN06V04-TEISYUTUSAKI   PIC X(6).
            02  HCN06V04-PTNUM  PIC X(20).
            02  HCN06V04-SRYY   PIC X(2).
            02  HCN06V04-SRYM   PIC X(2).
            02  HCN06V04-PREFNUM    PIC X(2).
            02  HCN06V04-HOSPCDN    PIC X(20).
            02  HCN06V04-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN06V04-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN06V04-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN06V04-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN06V04-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN06V04-OCR1   PIC X(67).
            02  HCN06V04-OCR2   PIC X(67).
            02  HCN06V04-OCR3   PIC X(67).
            02  HCN06V04-RJNFTNJANUM.
              03  HCN06V04-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN06V04-KOH-TBL1.
              03  HCN06V04-KOHFTNJANUM1.
                04  HCN06V04-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN06V04-KOHFTNJANUM1N  PIC X(20).
              03  HCN06V04-KOHJKYSNUM1.
                04  HCN06V04-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN06V04-KOHJKYSNUM1N   PIC X(20).
            02  HCN06V04-KOH-TBL2.
              03  HCN06V04-KOHFTNJANUM2.
                04  HCN06V04-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN06V04-KOHFTNJANUM2N  PIC X(20).
              03  HCN06V04-KOHJKYSNUM2.
                04  HCN06V04-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN06V04-KOHJKYSNUM2N   PIC X(20).
            02  HCN06V04-HKNJANUM.
              03  HCN06V04-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN06V04-KYURATE    PIC X(8).
            02  HCN06V04-RJNJKYSNUM.
              03  HCN06V04-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN06V04-HKNSBT1    PIC X(6).
            02  HCN06V04-HKNSBT2    PIC X(6).
            02  HCN06V04-HONKZK PIC X(10).
            02  HCN06V04-KIGO   PIC X(34).
            02  HCN06V04-NUM    PIC X(34).
            02  HCN06V04-KANANAME   PIC X(40).
            02  HCN06V04-KANANAME2  PIC X(50).
            02  HCN06V04-SEXNUM PIC X(2).
            02  HCN06V04-BIRTHDAY   PIC X(22).
            02  HCN06V04-JIYU   PIC X(20).
            02  HCN06V04-SEXN   PIC X(2).
            02  HCN06V04-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN06V04-HOSPTEL    PIC X(15).
            02  HCN06V04-SRYKA  PIC X(30).
            02  HCN06V04-BEDSU  PIC X(4).
            02  HCN06V04-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN06V04-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN06V04-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN06V04-JINISSU    PIC X(2).
            02  HCN06V04-JINISSU1   PIC X(2).
            02  HCN06V04-JINISSU2   PIC X(2).
            02  HCN06V04-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN06V04-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN06V04-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN06V04-HOSPNAME   PIC X(80).
            02  HCN06V04-HOSPADRS   PIC X(100).
            02  HCN06V04-NAME   PIC X(50).
            02  HCN06V04-HEAD1-G.
              03  HCN06V04-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN06V04-HEAD2-G.
              03  HCN06V04-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN06V04-HEAD3-G.
              03  HCN06V04-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN06V04-HEAD4-G.
              03  HCN06V04-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN06V04-BODY   PIC X(52).
            02  HCN06V04-SHOSHINTIME    PIC X(10).
            02  HCN06V04-SIDOTIME   PIC X(10).
            02  HCN06V04-ZAITAKUTIME    PIC X(10).
            02  HCN06V04-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN06V04-FTNMONEYSAI    PIC X(11).
            02  HCN06V04-RENNUM PIC X(7).
            02  HCN06V04-SRYKA2 PIC X(60).
            02  HCN06V04-NYUINKBN   PIC X(42).
            02  HCN06V04-NYUINYMD.
              03  HCN06V04-NYUINYY    PIC X(2).
              03  HCN06V04-NYUINMM    PIC X(2).
              03  HCN06V04-NYUINDD    PIC X(2).
            02  HCN06V04-HOSPSBT    PIC X(2).
            02  HCN06V04-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN06V04-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN06V04-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN06V04-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN06V04-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN06V04-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN06V04-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN06V04-FOOT   PIC X(28).
            02  HCN06V04-GAIITBFTN  PIC X(7).
            02  HCN06V04-GENGAKUMEI PIC X(30).
            02  HCN06V04-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN06V04-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN06V04-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN06V04-SYOFTNMONEYSAI PIC X(11).
            02  HCN06V04-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCN06V04-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN06V04-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCN06V04-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN06V04-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN06V04-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN06V04-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN06V04-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN06V04-PAGE   PIC X(5).
            02  HCN06V04-TOTALTEN1
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN06V04-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN06V04-SYOTOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN06V04-SYOFTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCN06V04-SYONISSU1
                    PIC X(2)    OCCURS  4 TIMES.

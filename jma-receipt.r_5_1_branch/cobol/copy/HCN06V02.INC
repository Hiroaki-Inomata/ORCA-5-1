        01  HCN06V02.
            02  HCN06V02-TEISYUTUSAKI   PIC X(6).
            02  HCN06V02-PTNUM  PIC X(20).
            02  HCN06V02-SRYY   PIC X(2).
            02  HCN06V02-SRYM   PIC X(2).
            02  HCN06V02-PREFNUM    PIC X(2).
            02  HCN06V02-HOSPCDN    PIC X(20).
            02  HCN06V02-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCN06V02-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCN06V02-KOHTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN06V02-TOTALTEN
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCN06V02-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN06V02-RJNFTNJANUM.
              03  HCN06V02-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN06V02-KOH-TBL1.
              03  HCN06V02-KOHFTNJANUM1.
                04  HCN06V02-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN06V02-KOHFTNJANUM1N  PIC X(20).
              03  HCN06V02-KOHJKYSNUM1.
                04  HCN06V02-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN06V02-KOHJKYSNUM1N   PIC X(20).
            02  HCN06V02-KOH-TBL2.
              03  HCN06V02-KOHFTNJANUM2.
                04  HCN06V02-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCN06V02-KOHFTNJANUM2N  PIC X(20).
              03  HCN06V02-KOHJKYSNUM2.
                04  HCN06V02-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCN06V02-KOHJKYSNUM2N   PIC X(20).
            02  HCN06V02-HKNJANUM.
              03  HCN06V02-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN06V02-KYURATE    PIC X(8).
            02  HCN06V02-RJNJKYSNUM.
              03  HCN06V02-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN06V02-HKNSBT1    PIC X(6).
            02  HCN06V02-HKNSBT2    PIC X(6).
            02  HCN06V02-HONKZK PIC X(10).
            02  HCN06V02-KIGO   PIC X(34).
            02  HCN06V02-NUM    PIC X(34).
            02  HCN06V02-KANANAME   PIC X(40).
            02  HCN06V02-KANANAME2  PIC X(50).
            02  HCN06V02-SEXNUM PIC X(2).
            02  HCN06V02-BIRTHDAY   PIC X(22).
            02  HCN06V02-JIYU   PIC X(20).
            02  HCN06V02-SEXN   PIC X(2).
            02  HCN06V02-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN06V02-HOSPTEL    PIC X(15).
            02  HCN06V02-SRYKA  PIC X(30).
            02  HCN06V02-BEDSU  PIC X(4).
            02  HCN06V02-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN06V02-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN06V02-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN06V02-JINISSU    PIC X(2).
            02  HCN06V02-JINISSU1   PIC X(2).
            02  HCN06V02-JINISSU2   PIC X(2).
            02  HCN06V02-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN06V02-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN06V02-NYUKHNTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN06V02-HOSPNAME   PIC X(80).
            02  HCN06V02-HOSPADRS   PIC X(100).
            02  HCN06V02-NAME   PIC X(50).
            02  HCN06V02-HEAD1-G.
              03  HCN06V02-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCN06V02-HEAD2-G.
              03  HCN06V02-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCN06V02-HEAD3-G.
              03  HCN06V02-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCN06V02-HEAD4-G.
              03  HCN06V02-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN06V02-BODY   PIC X(52).
            02  HCN06V02-SHOSHINTIME    PIC X(10).
            02  HCN06V02-SIDOTIME   PIC X(10).
            02  HCN06V02-ZAITAKUTIME    PIC X(10).
            02  HCN06V02-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCN06V02-FTNMONEYSAI    PIC X(11).
            02  HCN06V02-RENNUM PIC X(7).
            02  HCN06V02-SRYKA2 PIC X(60).
            02  HCN06V02-NYUINKBN   PIC X(42).
            02  HCN06V02-NYUINYMD.
              03  HCN06V02-NYUINYY    PIC X(2).
              03  HCN06V02-NYUINMM    PIC X(2).
              03  HCN06V02-NYUINDD    PIC X(2).
            02  HCN06V02-HOSPSBT    PIC X(2).
            02  HCN06V02-NYUSYUBETU
                    PIC X(12)   OCCURS  11 TIMES.
            02  HCN06V02-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN06V02-NICHI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN06V02-SYOKJN
                    PIC X(4)    OCCURS  3 TIMES.
            02  HCN06V02-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN06V02-SYOGAK
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN06V02-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN06V02-FOOT   PIC X(28).
            02  HCN06V02-GAIITBFTN  PIC X(7).
            02  HCN06V02-GENGAKUMEI PIC X(30).
            02  HCN06V02-SYONISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN06V02-SYOTOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN06V02-SYOFTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCN06V02-SYOFTNMONEYSAI PIC X(11).
            02  HCN06V02-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCN06V02-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN06V02-NYUKOHTENN-G           OCCURS  10 TIMES.
              03  HCN06V02-NYUKOHTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN06V02-NYUKHNTENN
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCN06V02-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN06V02-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN06V02-NYUKOHTEN
                    PIC X(7)    OCCURS  10 TIMES.

        01  HCN09V08.
            02  HCN09V08-TEISYUTUSAKI
                        PIC X(6).
            02  HCN09V08-PTNUM
                        PIC X(20).
            02  HCN09V08-SRYGG
                        PIC X(4).
            02  HCN09V08-SRYY
                        PIC X(2).
            02  HCN09V08-SRYM
                        PIC X(2).
            02  HCN09V08-PREFNUM
                        PIC X(2).
            02  HCN09V08-HOSPCDN
                        PIC X(20).
            02  HCN09V08-SRYKBN
                        PIC X(2)        OCCURS 35 TIMES.
            02  HCN09V08-TEKIYO
                        PIC X(52)       OCCURS 35 TIMES.
            02  HCN09V08-KOHTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V08-TOTALTEN
                        PIC X(10)       OCCURS 3 TIMES.
            02  HCN09V08-FTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V08-OCR1
                        PIC X(67).
            02  HCN09V08-OCR2
                        PIC X(67).
            02  HCN09V08-OCR3
                        PIC X(67).
            02  HCN09V08-RJNFTNJANUM.
              03  HCN09V08-RJNFTNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V08-KOH-TBL1.
              03  HCN09V08-KOHFTNJANUM1.
                04  HCN09V08-KOHFTNJANUM1-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V08-KOHFTNJANUM1N
                        PIC X(20).
              03  HCN09V08-KOHJKYSNUM1.
                04  HCN09V08-KOHJKYSNUM1-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V08-KOHJKYSNUM1N
                        PIC X(20).
            02  HCN09V08-KOH-TBL2.
              03  HCN09V08-KOHFTNJANUM2.
                04  HCN09V08-KOHFTNJANUM2-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V08-KOHFTNJANUM2N
                        PIC X(20).
              03  HCN09V08-KOHJKYSNUM2.
                04  HCN09V08-KOHJKYSNUM2-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V08-KOHJKYSNUM2N
                        PIC X(20).
            02  HCN09V08-HKNJANUM.
              03  HCN09V08-HKNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V08-KYURATE
                        PIC X(8).
            02  HCN09V08-RJNJKYSNUM.
              03  HCN09V08-RJNJKYSNUM-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN09V08-HKNSBT1
                        PIC X(6).
            02  HCN09V08-HKNSBT2
                        PIC X(6).
            02  HCN09V08-HONKZK
                        PIC X(10).
            02  HCN09V08-KIGO
                        PIC X(34).
            02  HCN09V08-NUM
                        PIC X(34).
            02  HCN09V08-KANANAME
                        PIC X(40).
            02  HCN09V08-KANANAME2
                        PIC X(50).
            02  HCN09V08-SEXNUM
                        PIC X(2).
            02  HCN09V08-BIRTHDAY
                        PIC X(22).
            02  HCN09V08-JIYU
                        PIC X(20).
            02  HCN09V08-SEXN
                        PIC X(2).
            02  HCN09V08-TOKKI
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCN09V08-HOSPTEL
                        PIC X(15).
            02  HCN09V08-SRYKA
                        PIC X(30).
            02  HCN09V08-BEDSU
                        PIC X(4).
            02  HCN09V08-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCN09V08-SRYYMD
                        PIC X(26)       OCCURS 5 TIMES.
            02  HCN09V08-TENKI
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN09V08-JINISSU
                        PIC X(2).
            02  HCN09V08-JINISSU1
                        PIC X(2).
            02  HCN09V08-JINISSU2
                        PIC X(2).
            02  HCN09V08-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V08-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN09V08-NYUKHNTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V08-HOSPNAME
                        PIC X(80).
            02  HCN09V08-HOSPADRS
                        PIC X(100).
            02  HCN09V08-NAME
                        PIC X(50).
            02  HCN09V08-HEAD1-G.
              03  HCN09V08-HEAD1
                        PIC X(46)       OCCURS 2 TIMES.
            02  HCN09V08-HEAD2-G.
              03  HCN09V08-HEAD2
                        PIC X(2)        OCCURS 7 TIMES.
            02  HCN09V08-HEAD3-G.
              03  HCN09V08-HEAD3
                        PIC X(50)       OCCURS 2 TIMES.
            02  HCN09V08-HEAD4-G.
              03  HCN09V08-HEAD4
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCN09V08-BODY
                        PIC X(52).
            02  HCN09V08-SHOSHINTIME
                        PIC X(10).
            02  HCN09V08-SIDOTIME
                        PIC X(10).
            02  HCN09V08-ZAITAKUTIME
                        PIC X(10).
            02  HCN09V08-TENKIYMD
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCN09V08-FTNMONEYSAI
                        PIC X(11).
            02  HCN09V08-FTNMONEYSAI2
                        PIC X(11).
            02  HCN09V08-RENNUM
                        PIC X(7).
            02  HCN09V08-SRYKA2
                        PIC X(60).
            02  HCN09V08-NYUINKBN
                        PIC X(42).
            02  HCN09V08-NYUINGG
                        PIC X(4).
            02  HCN09V08-NYUINYMD.
              03  HCN09V08-NYUINYY
                        PIC X(2).
              03  HCN09V08-NYUINMM
                        PIC X(2).
              03  HCN09V08-NYUINDD
                        PIC X(2).
            02  HCN09V08-HOSPSBT
                        PIC X(2).
            02  HCN09V08-NYUSYUBETU
                        PIC X(12)       OCCURS 11 TIMES.
            02  HCN09V08-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V08-NICHI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V08-SYOKJN
                        PIC X(4)        OCCURS 3 TIMES.
            02  HCN09V08-SYOGAK
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN09V08-SYOKAISU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V08-SYOKJN-KHN
                        PIC X(2).
            02  HCN09V08-SYOKJN-KHNSEI
                        PIC X(2).
            02  HCN09V08-SYOGAK-SEI
                        PIC X(4)        OCCURS 2 TIMES.
            02  HCN09V08-SYOKAISU-SEI
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN09V08-FOOT
                        PIC X(28).
            02  HCN09V08-GENGAKUMEI
                        PIC X(30).
            02  HCN09V08-SYONISSU
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN09V08-SYOTOTALTEN
                        PIC X(7)        OCCURS 3 TIMES.
            02  HCN09V08-SYOFTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V08-SYOFTNMONEYSAI
                        PIC X(11).
            02  HCN09V08-NYUTENSUN-G
                                        OCCURS 10 TIMES.
              03  HCN09V08-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V08-NYUKOHTENN-G
                                        OCCURS 10 TIMES.
              03  HCN09V08-NYUKOHTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V08-NYUKHNTENN
                        PIC X(10)       OCCURS 10 TIMES.
            02  HCN09V08-NYUKAISU
                        PIC X(3)        OCCURS 10 TIMES.
            02  HCN09V08-NYUTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V08-NYUKOHTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V08-PAGE
                        PIC X(5).
            02  HCN09V08-MOJI1
                        PIC X(10).
            02  HCN09V08-MOJI2
                        PIC X(10).
            02  HCN09V08-TOTALTEN1
                        PIC X(10)       OCCURS 4 TIMES.
            02  HCN09V08-FTNMONEY1
                        PIC X(9)        OCCURS 4 TIMES.
            02  HCN09V08-SYOTOTALTEN1
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN09V08-SYOFTNMONEY1
                        PIC X(8)        OCCURS 4 TIMES.
            02  HCN09V08-SYONISSU1
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V08-FTNMONEY-GEN
                        PIC X(8).
            02  HCN09V08-GENMEN
                        PIC X(10).
            02  HCN09V08-GENMENKIN
                        PIC X(7).
            02  HCN09V08-GENMEN2
                        PIC X(2).
            02  HCN09V08-BTUNUM
                        PIC X(9).
            02  HCN09V08-LABEL
                        PIC X(14).
            02  HCN09V08-HEAD5
                        PIC X(20).
            02  HCN09V08-NYUSYUBETU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN09V08-DRNAME
                        PIC X(20).
            02  HCN09V08-NYUKHNTENN2
                        PIC X(14)       OCCURS 10 TIMES.
            02  HCN09V08-LOPTION
                        PIC X(100).
            02  HCN09V08-GENMEN3
                        PIC X(20).

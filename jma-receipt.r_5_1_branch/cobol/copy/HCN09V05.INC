        01  HCN09V05.
            02  HCN09V05-TEISYUTUSAKI
                        PIC X(6).
            02  HCN09V05-PTNUM
                        PIC X(20).
            02  HCN09V05-SRYY
                        PIC X(2).
            02  HCN09V05-SRYM
                        PIC X(2).
            02  HCN09V05-PREFNUM
                        PIC X(2).
            02  HCN09V05-HOSPCDN
                        PIC X(20).
            02  HCN09V05-SRYKBN
                        PIC X(2)        OCCURS 35 TIMES.
            02  HCN09V05-TEKIYO
                        PIC X(52)       OCCURS 35 TIMES.
            02  HCN09V05-KOHTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V05-TOTALTEN
                        PIC X(10)       OCCURS 3 TIMES.
            02  HCN09V05-FTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V05-OCR1
                        PIC X(67).
            02  HCN09V05-OCR2
                        PIC X(67).
            02  HCN09V05-OCR3
                        PIC X(67).
            02  HCN09V05-RJNFTNJANUM.
              03  HCN09V05-RJNFTNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V05-KOH-TBL1.
              03  HCN09V05-KOHFTNJANUM1.
                04  HCN09V05-KOHFTNJANUM1-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V05-KOHFTNJANUM1N
                        PIC X(20).
              03  HCN09V05-KOHJKYSNUM1.
                04  HCN09V05-KOHJKYSNUM1-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V05-KOHJKYSNUM1N
                        PIC X(20).
            02  HCN09V05-KOH-TBL2.
              03  HCN09V05-KOHFTNJANUM2.
                04  HCN09V05-KOHFTNJANUM2-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V05-KOHFTNJANUM2N
                        PIC X(20).
              03  HCN09V05-KOHJKYSNUM2.
                04  HCN09V05-KOHJKYSNUM2-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V05-KOHJKYSNUM2N
                        PIC X(20).
            02  HCN09V05-HKNJANUM.
              03  HCN09V05-HKNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V05-KYURATE
                        PIC X(8).
            02  HCN09V05-RJNJKYSNUM.
              03  HCN09V05-RJNJKYSNUM-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN09V05-HKNSBT1
                        PIC X(6).
            02  HCN09V05-HKNSBT2
                        PIC X(6).
            02  HCN09V05-HONKZK
                        PIC X(10).
            02  HCN09V05-KIGO
                        PIC X(34).
            02  HCN09V05-NUM
                        PIC X(34).
            02  HCN09V05-KANANAME
                        PIC X(40).
            02  HCN09V05-KANANAME2
                        PIC X(50).
            02  HCN09V05-SEXNUM
                        PIC X(2).
            02  HCN09V05-BIRTHDAY
                        PIC X(22).
            02  HCN09V05-JIYU
                        PIC X(20).
            02  HCN09V05-SEXN
                        PIC X(2).
            02  HCN09V05-TOKKI
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCN09V05-HOSPTEL
                        PIC X(15).
            02  HCN09V05-SRYKA
                        PIC X(30).
            02  HCN09V05-BEDSU
                        PIC X(4).
            02  HCN09V05-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCN09V05-SRYYMD
                        PIC X(24)       OCCURS 5 TIMES.
            02  HCN09V05-TENKI
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN09V05-JINISSU
                        PIC X(2).
            02  HCN09V05-JINISSU1
                        PIC X(2).
            02  HCN09V05-JINISSU2
                        PIC X(2).
            02  HCN09V05-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V05-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN09V05-NYUKHNTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V05-HOSPNAME
                        PIC X(80).
            02  HCN09V05-HOSPADRS
                        PIC X(100).
            02  HCN09V05-NAME
                        PIC X(50).
            02  HCN09V05-HEAD1-G.
              03  HCN09V05-HEAD1
                        PIC X(46)       OCCURS 2 TIMES.
            02  HCN09V05-HEAD2-G.
              03  HCN09V05-HEAD2
                        PIC X(2)        OCCURS 7 TIMES.
            02  HCN09V05-HEAD3-G.
              03  HCN09V05-HEAD3
                        PIC X(50)       OCCURS 2 TIMES.
            02  HCN09V05-HEAD4-G.
              03  HCN09V05-HEAD4
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCN09V05-BODY
                        PIC X(52).
            02  HCN09V05-SHOSHINTIME
                        PIC X(10).
            02  HCN09V05-SIDOTIME
                        PIC X(10).
            02  HCN09V05-ZAITAKUTIME
                        PIC X(10).
            02  HCN09V05-TENKIYMD
                        PIC X(6)        OCCURS 5 TIMES.
            02  HCN09V05-FTNMONEYSAI
                        PIC X(11).
            02  HCN09V05-FTNMONEYSAI2
                        PIC X(11).
            02  HCN09V05-RENNUM
                        PIC X(7).
            02  HCN09V05-SRYKA2
                        PIC X(60).
            02  HCN09V05-NYUINKBN
                        PIC X(42).
            02  HCN09V05-NYUINYMD.
              03  HCN09V05-NYUINYY
                        PIC X(2).
              03  HCN09V05-NYUINMM
                        PIC X(2).
              03  HCN09V05-NYUINDD
                        PIC X(2).
            02  HCN09V05-HOSPSBT
                        PIC X(2).
            02  HCN09V05-NYUSYUBETU
                        PIC X(12)       OCCURS 11 TIMES.
            02  HCN09V05-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V05-NICHI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V05-SYOKJN
                        PIC X(4)        OCCURS 3 TIMES.
            02  HCN09V05-SYOGAK
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN09V05-SYOKAISU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V05-SYOKJN-KHN
                        PIC X(2).
            02  HCN09V05-SYOKJN-KHNSEI
                        PIC X(2).
            02  HCN09V05-SYOGAK-SEI
                        PIC X(4)        OCCURS 2 TIMES.
            02  HCN09V05-SYOKAISU-SEI
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN09V05-FOOT
                        PIC X(28).
            02  HCN09V05-GENGAKUMEI
                        PIC X(30).
            02  HCN09V05-SYONISSU
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN09V05-SYOTOTALTEN
                        PIC X(7)        OCCURS 3 TIMES.
            02  HCN09V05-SYOFTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V05-SYOFTNMONEYSAI
                        PIC X(11).
            02  HCN09V05-NYUTENSUN-G
                                        OCCURS 10 TIMES.
              03  HCN09V05-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V05-NYUKOHTENN-G
                                        OCCURS 10 TIMES.
              03  HCN09V05-NYUKOHTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V05-NYUKHNTENN
                        PIC X(10)       OCCURS 10 TIMES.
            02  HCN09V05-NYUKAISU
                        PIC X(3)        OCCURS 10 TIMES.
            02  HCN09V05-NYUTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V05-NYUKOHTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V05-PAGE
                        PIC X(5).
            02  HCN09V05-MOJI1
                        PIC X(10).
            02  HCN09V05-MOJI2
                        PIC X(10).
            02  HCN09V05-TOTALTEN1
                        PIC X(10)       OCCURS 4 TIMES.
            02  HCN09V05-FTNMONEY1
                        PIC X(9)        OCCURS 4 TIMES.
            02  HCN09V05-SYOTOTALTEN1
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN09V05-SYOFTNMONEY1
                        PIC X(8)        OCCURS 4 TIMES.
            02  HCN09V05-SYONISSU1
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V05-FTNMONEY-GEN
                        PIC X(8).
            02  HCN09V05-GENMEN
                        PIC X(10).
            02  HCN09V05-GENMENKIN
                        PIC X(7).
            02  HCN09V05-GENMEN2
                        PIC X(2).
            02  HCN09V05-BTUNUM
                        PIC X(9).
            02  HCN09V05-LABEL
                        PIC X(14).
            02  HCN09V05-HEAD5
                        PIC X(20).
            02  HCN09V05-NYUSYUBETU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN09V05-NYUKHNTENN2
                        PIC X(14)       OCCURS 10 TIMES.
            02  HCN09V05-LOPTION
                        PIC X(100).

        01  HCN09V06.
            02  HCN09V06-TEISYUTUSAKI
                        PIC X(6).
            02  HCN09V06-PTNUM
                        PIC X(20).
            02  HCN09V06-SRYY
                        PIC X(2).
            02  HCN09V06-SRYM
                        PIC X(2).
            02  HCN09V06-PREFNUM
                        PIC X(2).
            02  HCN09V06-HOSPCDN
                        PIC X(20).
            02  HCN09V06-SRYKBN
                        PIC X(2)        OCCURS 35 TIMES.
            02  HCN09V06-TEKIYO
                        PIC X(52)       OCCURS 35 TIMES.
            02  HCN09V06-KOHTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V06-TOTALTEN
                        PIC X(10)       OCCURS 3 TIMES.
            02  HCN09V06-FTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V06-OCR1
                        PIC X(67).
            02  HCN09V06-OCR2
                        PIC X(67).
            02  HCN09V06-OCR3
                        PIC X(67).
            02  HCN09V06-RJNFTNJANUM.
              03  HCN09V06-RJNFTNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V06-KOH-TBL1.
              03  HCN09V06-KOHFTNJANUM1.
                04  HCN09V06-KOHFTNJANUM1-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V06-KOHFTNJANUM1N
                        PIC X(20).
              03  HCN09V06-KOHJKYSNUM1.
                04  HCN09V06-KOHJKYSNUM1-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V06-KOHJKYSNUM1N
                        PIC X(20).
            02  HCN09V06-KOH-TBL2.
              03  HCN09V06-KOHFTNJANUM2.
                04  HCN09V06-KOHFTNJANUM2-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCN09V06-KOHFTNJANUM2N
                        PIC X(20).
              03  HCN09V06-KOHJKYSNUM2.
                04  HCN09V06-KOHJKYSNUM2-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCN09V06-KOHJKYSNUM2N
                        PIC X(20).
            02  HCN09V06-HKNJANUM.
              03  HCN09V06-HKNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCN09V06-KYURATE
                        PIC X(8).
            02  HCN09V06-RJNJKYSNUM.
              03  HCN09V06-RJNJKYSNUM-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN09V06-HKNSBT1
                        PIC X(6).
            02  HCN09V06-HKNSBT2
                        PIC X(6).
            02  HCN09V06-HONKZK
                        PIC X(10).
            02  HCN09V06-KIGO
                        PIC X(34).
            02  HCN09V06-NUM
                        PIC X(34).
            02  HCN09V06-KANANAME
                        PIC X(40).
            02  HCN09V06-KANANAME2
                        PIC X(50).
            02  HCN09V06-SEXNUM
                        PIC X(2).
            02  HCN09V06-BIRTHDAY
                        PIC X(22).
            02  HCN09V06-JIYU
                        PIC X(20).
            02  HCN09V06-SEXN
                        PIC X(2).
            02  HCN09V06-TOKKI
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCN09V06-HOSPTEL
                        PIC X(15).
            02  HCN09V06-SRYKA
                        PIC X(30).
            02  HCN09V06-BEDSU
                        PIC X(4).
            02  HCN09V06-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCN09V06-SRYYMD
                        PIC X(24)       OCCURS 5 TIMES.
            02  HCN09V06-TENKI
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN09V06-JINISSU
                        PIC X(2).
            02  HCN09V06-JINISSU1
                        PIC X(2).
            02  HCN09V06-JINISSU2
                        PIC X(2).
            02  HCN09V06-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN09V06-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN09V06-NYUKHNTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V06-HOSPNAME
                        PIC X(80).
            02  HCN09V06-HOSPADRS
                        PIC X(100).
            02  HCN09V06-NAME
                        PIC X(50).
            02  HCN09V06-HEAD1-G.
              03  HCN09V06-HEAD1
                        PIC X(46)       OCCURS 2 TIMES.
            02  HCN09V06-HEAD2-G.
              03  HCN09V06-HEAD2
                        PIC X(2)        OCCURS 7 TIMES.
            02  HCN09V06-HEAD3-G.
              03  HCN09V06-HEAD3
                        PIC X(50)       OCCURS 2 TIMES.
            02  HCN09V06-HEAD4-G.
              03  HCN09V06-HEAD4
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCN09V06-BODY
                        PIC X(52).
            02  HCN09V06-SHOSHINTIME
                        PIC X(10).
            02  HCN09V06-SIDOTIME
                        PIC X(10).
            02  HCN09V06-ZAITAKUTIME
                        PIC X(10).
            02  HCN09V06-TENKIYMD
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCN09V06-FTNMONEYSAI
                        PIC X(11).
            02  HCN09V06-FTNMONEYSAI2
                        PIC X(11).
            02  HCN09V06-RENNUM
                        PIC X(7).
            02  HCN09V06-SRYKA2
                        PIC X(60).
            02  HCN09V06-NYUINKBN
                        PIC X(42).
            02  HCN09V06-NYUINYMD.
              03  HCN09V06-NYUINYY
                        PIC X(2).
              03  HCN09V06-NYUINMM
                        PIC X(2).
              03  HCN09V06-NYUINDD
                        PIC X(2).
            02  HCN09V06-HOSPSBT
                        PIC X(2).
            02  HCN09V06-NYUSYUBETU
                        PIC X(12)       OCCURS 11 TIMES.
            02  HCN09V06-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V06-NICHI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN09V06-SYOKJN
                        PIC X(4)        OCCURS 3 TIMES.
            02  HCN09V06-SYOGAK
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN09V06-SYOKAISU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V06-SYOKJN-KHN
                        PIC X(2).
            02  HCN09V06-SYOKJN-KHNSEI
                        PIC X(2).
            02  HCN09V06-SYOGAK-SEI
                        PIC X(4)        OCCURS 2 TIMES.
            02  HCN09V06-SYOKAISU-SEI
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN09V06-FOOT
                        PIC X(28).
            02  HCN09V06-GENGAKUMEI
                        PIC X(30).
            02  HCN09V06-SYONISSU
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN09V06-SYOTOTALTEN
                        PIC X(7)        OCCURS 3 TIMES.
            02  HCN09V06-SYOFTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCN09V06-SYOFTNMONEYSAI
                        PIC X(11).
            02  HCN09V06-NYUTENSUN-G
                                        OCCURS 10 TIMES.
              03  HCN09V06-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V06-NYUKOHTENN-G
                                        OCCURS 10 TIMES.
              03  HCN09V06-NYUKOHTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN09V06-NYUKHNTENN
                        PIC X(10)       OCCURS 10 TIMES.
            02  HCN09V06-NYUKAISU
                        PIC X(3)        OCCURS 10 TIMES.
            02  HCN09V06-NYUTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V06-NYUKOHTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN09V06-PAGE
                        PIC X(5).
            02  HCN09V06-MOJI1
                        PIC X(10).
            02  HCN09V06-MOJI2
                        PIC X(10).
            02  HCN09V06-TOTALTEN1
                        PIC X(10)       OCCURS 4 TIMES.
            02  HCN09V06-FTNMONEY1
                        PIC X(9)        OCCURS 4 TIMES.
            02  HCN09V06-SYOTOTALTEN1
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN09V06-SYOFTNMONEY1
                        PIC X(8)        OCCURS 4 TIMES.
            02  HCN09V06-SYONISSU1
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN09V06-FTNMONEY-GEN
                        PIC X(8).
            02  HCN09V06-GENMEN
                        PIC X(10).
            02  HCN09V06-GENMENKIN
                        PIC X(7).
            02  HCN09V06-GENMEN2
                        PIC X(2).
            02  HCN09V06-BTUNUM
                        PIC X(9).
            02  HCN09V06-LABEL
                        PIC X(14).
            02  HCN09V06-HEAD5
                        PIC X(20).
            02  HCN09V06-NYUSYUBETU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN09V06-NYUKHNTENN2
                        PIC X(14)       OCCURS 10 TIMES.
            02  HCN09V06-LOPTION
                        PIC X(100).

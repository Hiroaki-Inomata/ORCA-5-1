        01  HCN58V02.
            02  HCN58V02-PTNUM  PIC X(20).
            02  HCN58V02-PAGE   PIC X(5).
            02  HCN58V02-RENNUM PIC X(7).
            02  HCN58V02-SRYYM  PIC X(18).
            02  HCN58V02-HEAD1-G.
              03  HCN58V02-HEAD1
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCN58V02-KIGONUM    PIC X(24).
            02  HCN58V02-KIGONUM2-G.
              03  HCN58V02-KIGONUM2
                      PIC X(24)   OCCURS  2 TIMES.
            02  HCN58V02-KANANAME   PIC X(40).
            02  HCN58V02-NAME   PIC X(50).
            02  HCN58V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN58V02-BIRTHG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN58V02-BIRTHYY    PIC X(2).
            02  HCN58V02-HOSPADRS   PIC X(100).
            02  HCN58V02-HOSPNAME   PIC X(80).
            02  HCN58V02-HOSPTEL    PIC X(15).
            02  HCN58V02-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCN58V02-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCN58V02-TENKI-G        OCCURS  5 TIMES.
              03  HCN58V02-TENKI
                      PIC X(2)    OCCURS  3 TIMES.
            02  HCN58V02-JNISSU PIC X(2).
            02  HCN58V02-SHOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN58V02-SIDOTIME   PIC X(10).
            02  HCN58V02-ZAITAKUTIME    PIC X(10).
            02  HCN58V02-KHNTEN
                    PIC X(7)    OCCURS  18 TIMES.
            02  HCN58V02-KAISU
                    PIC X(3)    OCCURS  21 TIMES.
            02  HCN58V02-TENSU-G        OCCURS  2 TIMES.
              03  HCN58V02-TENSU
                      PIC X(7)    OCCURS  21 TIMES.
            02  HCN58V02-NYUING PIC X(4).
            02  HCN58V02-NYUINYY    PIC X(2).
            02  HCN58V02-NYUINMM    PIC X(2).
            02  HCN58V02-NYUINDD    PIC X(2).
            02  HCN58V02-HOSPSBT
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN58V02-NYUSYUBETU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN58V02-NYUKHNTEN
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN58V02-NYUKAISU
                    PIC X(3)    OCCURS  10 TIMES.
            02  HCN58V02-NYUTENSU-G     OCCURS  2 TIMES.
              03  HCN58V02-NYUTENSU
                      PIC X(7)    OCCURS  10 TIMES.
            02  HCN58V02-SYOUKEI
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCN58V02-SYOKJN-KHN PIC X(2).
            02  HCN58V02-SYOKJN PIC X(4).
            02  HCN58V02-SYOKHNTEN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN58V02-SYOKAKERU  PIC X(4).
            02  HCN58V02-SYOKAISU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN58V02-SYONICHI   PIC X(4).
            02  HCN58V02-SYOKINGAKU
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN58V02-SYOEN  PIC X(2).
            02  HCN58V02-GOKEI
                    PIC X(10)   OCCURS  4 TIMES.
            02  HCN58V02-SRYKBN
                    PIC X(2)    OCCURS  50 TIMES.
            02  HCN58V02-TEKIYO
                    PIC X(52)   OCCURS  50 TIMES.
            02  HCN58V02-LABEL  PIC X(14).
            02  HCN58V02-LOPTION    PIC X(100).

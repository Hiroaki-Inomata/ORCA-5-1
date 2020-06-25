        01  HCN58V03.
            02  HCN58V03-PTNUM
                        PIC X(20).
            02  HCN58V03-PAGE
                        PIC X(5).
            02  HCN58V03-RENNUM
                        PIC X(7).
            02  HCN58V03-SRYYM
                        PIC X(18).
            02  HCN58V03-HEAD1-G.
              03  HCN58V03-HEAD1
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCN58V03-KIGONUM
                        PIC X(24).
            02  HCN58V03-KIGONUM2-G.
              03  HCN58V03-KIGONUM2
                        PIC X(24)       OCCURS 2 TIMES.
            02  HCN58V03-KANANAME
                        PIC X(40).
            02  HCN58V03-NAME
                        PIC X(50).
            02  HCN58V03-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN58V03-BIRTHG
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN58V03-BIRTHYY
                        PIC X(2).
            02  HCN58V03-HOSPADRS
                        PIC X(100).
            02  HCN58V03-HOSPNAME
                        PIC X(80).
            02  HCN58V03-HOSPTEL
                        PIC X(15).
            02  HCN58V03-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCN58V03-SRYYMD
                        PIC X(24)       OCCURS 5 TIMES.
            02  HCN58V03-TENKI-G
                                        OCCURS 5 TIMES.
              03  HCN58V03-TENKI
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN58V03-JNISSU
                        PIC X(2).
            02  HCN58V03-SHOSHIN
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN58V03-SIDOTIME
                        PIC X(10).
            02  HCN58V03-ZAITAKUTIME
                        PIC X(10).
            02  HCN58V03-KHNTEN
                        PIC X(7)        OCCURS 18 TIMES.
            02  HCN58V03-KAISU
                        PIC X(3)        OCCURS 21 TIMES.
            02  HCN58V03-TENSU-G
                                        OCCURS 2 TIMES.
              03  HCN58V03-TENSU
                        PIC X(7)        OCCURS 21 TIMES.
            02  HCN58V03-NYUING
                        PIC X(4).
            02  HCN58V03-NYUINYY
                        PIC X(2).
            02  HCN58V03-NYUINMM
                        PIC X(2).
            02  HCN58V03-NYUINDD
                        PIC X(2).
            02  HCN58V03-HOSPSBT
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN58V03-NYUSYUBETU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN58V03-NYUKHNTEN
                        PIC X(7)        OCCURS 8 TIMES.
            02  HCN58V03-NYUKAISU
                        PIC X(3)        OCCURS 10 TIMES.
            02  HCN58V03-NYUTENSU-G
                                        OCCURS 2 TIMES.
              03  HCN58V03-NYUTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN58V03-SYOUKEI
                        PIC X(7)        OCCURS 3 TIMES.
            02  HCN58V03-SYOKJN-KHN
                        PIC X(2).
            02  HCN58V03-SYOKJN
                        PIC X(4).
            02  HCN58V03-SYOKHNTEN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN58V03-SYOKAKERU
                        PIC X(4).
            02  HCN58V03-SYOKAISU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN58V03-SYONICHI
                        PIC X(4).
            02  HCN58V03-SYOKINGAKU
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN58V03-SYOEN
                        PIC X(2).
            02  HCN58V03-GOKEI
                        PIC X(10)       OCCURS 4 TIMES.
            02  HCN58V03-SRYKBN
                        PIC X(2)        OCCURS 50 TIMES.
            02  HCN58V03-TEKIYO
                        PIC X(52)       OCCURS 50 TIMES.
            02  HCN58V03-LABEL
                        PIC X(14).
            02  HCN58V03-NYUSYUBETU2
                        PIC X(14)       OCCURS 5 TIMES.
            02  HCN58V03-LOPTION
                        PIC X(100).

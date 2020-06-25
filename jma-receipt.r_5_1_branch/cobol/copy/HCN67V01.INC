        01  HCN67V01.
            02  HCN67V01-SAIGAIKBN
                        PIC X(8).
            02  HCN67V01-PTNUM
                        PIC X(20).
            02  HCN67V01-RSIIRYOCD
                        PIC X(30).
            02  HCN67V01-HOSPNAME
                        PIC X(64).
            02  HCN67V01-RENNUM
                        PIC X(7).
            02  HCN67V01-SINKEI
                        PIC X(1).
            02  HCN67V01-TENKI
                        PIC X(1).
            02  HCN67V01-KOFUNUM.
              03  HCN67V01-KOFUNUM-S
                        PIC X(1)        OCCURS 9 TIMES.
            02  HCN67V01-RYOSTYMD.
              03  HCN67V01-RYOSTYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN67V01-RYOEDYMD.
              03  HCN67V01-RYOEDYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN67V01-NISSU.
              03  HCN67V01-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN67V01-GOKEI.
              03  HCN67V01-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN67V01-KANANAME
                        PIC X(50).
            02  HCN67V01-NAME
                        PIC X(26).
            02  HCN67V01-NAME2
                        PIC X(50).
            02  HCN67V01-NENREI
                        PIC X(3).
            02  HCN67V01-TANJYOG
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN67V01-TANJYOYMD.
              03  HCN67V01-TANJYOY
                        PIC X(2).
              03  HCN67V01-TANJYOM
                        PIC X(2).
              03  HCN67V01-TANJYOD
                        PIC X(2).
            02  HCN67V01-SYOBYOMEI
                        PIC X(44)       OCCURS 5 TIMES.
            02  HCN67V01-KEIKA
                        PIC X(54)       OCCURS 8 TIMES.
            02  HCN67V01-SYOSHIN1
                        PIC X(2).
            02  HCN67V01-SYOSHIN2
                        PIC X(2).
            02  HCN67V01-SYOSHIN3
                        PIC X(2).
            02  HCN67V01-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN67V01-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN67V01-NYUINYMD.
              03  HCN67V01-NYUINYY
                        PIC X(2).
              03  HCN67V01-NYUINMM
                        PIC X(2).
              03  HCN67V01-NYUINDD
                        PIC X(2).
            02  HCN67V01-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN67V01-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN67V01-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN67V01-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN67V01-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN67V01-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN67V01-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN67V01-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN67V01-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN67V01-NICHI
                        PIC X(4)        OCCURS 5 TIMES.
            02  HCN67V01-TOTALTEN
                        PIC X(10).
            02  HCN67V01-TOTALMONEY
                        PIC X(10).
            02  HCN67V01-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN67V01-KINGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCN67V01-SYOKEI
                        PIC X(9).
            02  HCN67V01-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN67V01-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN67V01-SYOKAKERU
                        PIC X(4).
            02  HCN67V01-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN67V01-SYONICHI
                        PIC X(4).
            02  HCN67V01-SYONISSU
                        PIC X(2).
            02  HCN67V01-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN67V01-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN67V01-SYOMONEY
                        PIC X(10).
            02  HCN67V01-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN67V01-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN67V01-KAI
                        PIC X(3).
            02  HCN67V01-PAGE
                        PIC X(5).
            02  HCN67V01-LABEL
                        PIC X(14).
            02  HCN67V01-SYOCD
                        PIC X(5).
            02  HCN67V01-HOSPNAME2
                        PIC X(94).
            02  HCN67V01-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN67V01-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN67V01-LOPTION
                        PIC X(100).
            02  HCN67V01-TANJYOG2
                        PIC X(2).
            02  HCN67V01-HEAD-G.
              03  HCN67V01-HEAD
                        PIC X(62)       OCCURS 2 TIMES.

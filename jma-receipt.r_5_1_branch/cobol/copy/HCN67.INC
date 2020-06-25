        01  HCN67.
            02  HCN67-SAIGAIKBN
                        PIC X(8).
            02  HCN67-PTNUM
                        PIC X(20).
            02  HCN67-RSIIRYOCD
                        PIC X(30).
            02  HCN67-HOSPNAME
                        PIC X(64).
            02  HCN67-RENNUM
                        PIC X(7).
            02  HCN67-SINKEI
                        PIC X(1).
            02  HCN67-TENKI
                        PIC X(1).
            02  HCN67-KOFUNUM.
              03  HCN67-KOFUNUM-S
                        PIC X(1)        OCCURS 9 TIMES.
            02  HCN67-RYOSTYMD.
              03  HCN67-RYOSTYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN67-RYOEDYMD.
              03  HCN67-RYOEDYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN67-NISSU.
              03  HCN67-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN67-GOKEI.
              03  HCN67-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN67-KANANAME
                        PIC X(50).
            02  HCN67-NAME
                        PIC X(26).
            02  HCN67-NAME2
                        PIC X(50).
            02  HCN67-NENREI
                        PIC X(3).
            02  HCN67-TANJYOG
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN67-TANJYOYMD.
              03  HCN67-TANJYOY
                        PIC X(2).
              03  HCN67-TANJYOM
                        PIC X(2).
              03  HCN67-TANJYOD
                        PIC X(2).
            02  HCN67-SYOBYOMEI
                        PIC X(44)       OCCURS 5 TIMES.
            02  HCN67-KEIKA
                        PIC X(54)       OCCURS 8 TIMES.
            02  HCN67-SYOSHIN1
                        PIC X(2).
            02  HCN67-SYOSHIN2
                        PIC X(2).
            02  HCN67-SYOSHIN3
                        PIC X(2).
            02  HCN67-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN67-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN67-NYUINYMD.
              03  HCN67-NYUINYY
                        PIC X(2).
              03  HCN67-NYUINMM
                        PIC X(2).
              03  HCN67-NYUINDD
                        PIC X(2).
            02  HCN67-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN67-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN67-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN67-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN67-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN67-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN67-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN67-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN67-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN67-NICHI
                        PIC X(4)        OCCURS 5 TIMES.
            02  HCN67-TOTALTEN
                        PIC X(10).
            02  HCN67-TOTALMONEY
                        PIC X(10).
            02  HCN67-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN67-KINGAKU
                        PIC X(7)        OCCURS 5 TIMES.
            02  HCN67-SYOKEI
                        PIC X(7).
            02  HCN67-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN67-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN67-SYOKAKERU
                        PIC X(4).
            02  HCN67-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN67-SYONICHI
                        PIC X(4).
            02  HCN67-SYONISSU
                        PIC X(2).
            02  HCN67-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN67-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN67-SYOMONEY
                        PIC X(10).
            02  HCN67-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN67-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN67-KAI
                        PIC X(3).
            02  HCN67-PAGE
                        PIC X(5).
            02  HCN67-LABEL
                        PIC X(14).
            02  HCN67-SYOCD
                        PIC X(5).
            02  HCN67-HOSPNAME2
                        PIC X(94).
            02  HCN67-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN67-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN67-LOPTION
                        PIC X(100).

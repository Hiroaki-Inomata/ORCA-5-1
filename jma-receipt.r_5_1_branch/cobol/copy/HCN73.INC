        01  HCN73.
            02  HCN73-SAIGAIKBN
                        PIC X(8).
            02  HCN73-PTNUM
                        PIC X(20).
            02  HCN73-RSIIRYOCD
                        PIC X(30).
            02  HCN73-HOSPNAME
                        PIC X(90).
            02  HCN73-RENNUM
                        PIC X(7).
            02  HCN73-SINKEI
                        PIC X(1).
            02  HCN73-TENKI
                        PIC X(1).
            02  HCN73-KOFUNUM.
              03  HCN73-KOFUNUM-S
                        PIC X(1)        OCCURS 9 TIMES.
            02  HCN73-RYOSTYMD.
              03  HCN73-RYOSTYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN73-RYOEDYMD.
              03  HCN73-RYOEDYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN73-NISSU.
              03  HCN73-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN73-GOKEI.
              03  HCN73-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN73-KANANAME
                        PIC X(50).
            02  HCN73-NAME
                        PIC X(26).
            02  HCN73-NAME2
                        PIC X(50).
            02  HCN73-NENREI
                        PIC X(3).
            02  HCN73-TANJYOG
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN73-TANJYOYMD.
              03  HCN73-TANJYOY
                        PIC X(2).
              03  HCN73-TANJYOM
                        PIC X(2).
              03  HCN73-TANJYOD
                        PIC X(2).
            02  HCN73-SYOBYOMEI
                        PIC X(44)       OCCURS 5 TIMES.
            02  HCN73-KEIKA
                        PIC X(54)       OCCURS 8 TIMES.
            02  HCN73-SYOSHIN1
                        PIC X(2).
            02  HCN73-SYOSHIN2
                        PIC X(2).
            02  HCN73-SYOSHIN3
                        PIC X(2).
            02  HCN73-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN73-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN73-NYUINGG
                        PIC X(4).
            02  HCN73-NYUINYMD.
              03  HCN73-NYUINYY
                        PIC X(2).
              03  HCN73-NYUINMM
                        PIC X(2).
              03  HCN73-NYUINDD
                        PIC X(2).
            02  HCN73-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN73-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN73-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN73-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN73-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN73-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN73-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN73-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN73-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN73-NICHI
                        PIC X(4)        OCCURS 5 TIMES.
            02  HCN73-TOTALTEN
                        PIC X(10).
            02  HCN73-TOTALMONEY
                        PIC X(10).
            02  HCN73-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN73-KINGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCN73-SYOKEI
                        PIC X(9).
            02  HCN73-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN73-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN73-SYOKAKERU
                        PIC X(4).
            02  HCN73-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN73-SYONICHI
                        PIC X(4).
            02  HCN73-SYONISSU
                        PIC X(2).
            02  HCN73-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN73-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN73-SYOMONEY
                        PIC X(10).
            02  HCN73-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN73-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN73-KAI
                        PIC X(3).
            02  HCN73-PAGE
                        PIC X(5).
            02  HCN73-LABEL
                        PIC X(14).
            02  HCN73-SYOCD
                        PIC X(5).
            02  HCN73-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN73-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN73-LOPTION
                        PIC X(100).
            02  HCN73-HEAD-G.
              03  HCN73-HEAD
                        PIC X(62)       OCCURS 2 TIMES.

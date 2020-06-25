        01  HCN50V04.
            02  HCN50V04-SAIGAIKBN
                        PIC X(8).
            02  HCN50V04-PTNUM
                        PIC X(20).
            02  HCN50V04-RSIIRYOCD
                        PIC X(30).
            02  HCN50V04-HOSPNAME
                        PIC X(90).
            02  HCN50V04-RENNUM
                        PIC X(7).
            02  HCN50V04-SINKEI
                        PIC X(1).
            02  HCN50V04-TENKI
                        PIC X(1).
            02  HCN50V04-KOFUNUM.
              03  HCN50V04-KOFUNUM-S
                        PIC X(1)        OCCURS 14 TIMES.
            02  HCN50V04-TANJYOYMD.
              03  HCN50V04-TANJYOYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN50V04-SYOBYOYMD.
              03  HCN50V04-SYOBYOYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V04-STRYMD.
              03  HCN50V04-STRYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V04-ENDYMD.
              03  HCN50V04-ENDYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V04-NISSU.
              03  HCN50V04-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN50V04-GOKEI.
              03  HCN50V04-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN50V04-KANANAME
                        PIC X(50).
            02  HCN50V04-NAME
                        PIC X(26).
            02  HCN50V04-NAME2
                        PIC X(50).
            02  HCN50V04-NENREI
                        PIC X(3).
            02  HCN50V04-JIGYOUNAME
                        PIC X(80).
            02  HCN50V04-TODOFUKEN
                        PIC X(6).
            02  HCN50V04-TODOFUKENKBN1
                        PIC X(2).
            02  HCN50V04-TODOFUKENKBN2
                        PIC X(2).
            02  HCN50V04-TODOFUKENKBN3
                        PIC X(2).
            02  HCN50V04-TODOFUKENKBN4
                        PIC X(2).
            02  HCN50V04-GUNSIKU
                        PIC X(20).
            02  HCN50V04-GUNSIKUKBN1
                        PIC X(2).
            02  HCN50V04-GUNSIKUKBN2
                        PIC X(2).
            02  HCN50V04-GUNSIKUKBN3
                        PIC X(2).
            02  HCN50V04-SYOBYOMEI
                        PIC X(44)       OCCURS 3 TIMES.
            02  HCN50V04-KEIKA
                        PIC X(54)       OCCURS 4 TIMES.
            02  HCN50V04-SYOSHIN1
                        PIC X(2).
            02  HCN50V04-SYOSHIN2
                        PIC X(2).
            02  HCN50V04-SYOSHIN3
                        PIC X(2).
            02  HCN50V04-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN50V04-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN50V04-NYUINYMD.
              03  HCN50V04-NYUINYY
                        PIC X(2).
              03  HCN50V04-NYUINMM
                        PIC X(2).
              03  HCN50V04-NYUINDD
                        PIC X(2).
            02  HCN50V04-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN50V04-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN50V04-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN50V04-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN50V04-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN50V04-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN50V04-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN50V04-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN50V04-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN50V04-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN50V04-TOTALTEN
                        PIC X(10).
            02  HCN50V04-TOTALMONEY
                        PIC X(10).
            02  HCN50V04-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN50V04-KINGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCN50V04-SYOKEI
                        PIC X(9).
            02  HCN50V04-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN50V04-SYOKAKERU
                        PIC X(4).
            02  HCN50V04-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN50V04-SYONICHI
                        PIC X(4).
            02  HCN50V04-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN50V04-SYONISSU
                        PIC X(2).
            02  HCN50V04-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN50V04-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN50V04-SYOMONEY
                        PIC X(10).
            02  HCN50V04-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN50V04-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN50V04-KAI
                        PIC X(3).
            02  HCN50V04-PAGE
                        PIC X(5).
            02  HCN50V04-LABEL
                        PIC X(14).
            02  HCN50V04-SYOCD
                        PIC X(5).
            02  HCN50V04-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN50V04-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN50V04-LOPTION
                        PIC X(100).
            02  HCN50V04-HEAD-G.
              03  HCN50V04-HEAD
                        PIC X(62)       OCCURS 2 TIMES.

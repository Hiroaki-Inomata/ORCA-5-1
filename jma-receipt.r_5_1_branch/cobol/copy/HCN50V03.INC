        01  HCN50V03.
            02  HCN50V03-SAIGAIKBN
                        PIC X(8).
            02  HCN50V03-PTNUM
                        PIC X(20).
            02  HCN50V03-RSIIRYOCD
                        PIC X(30).
            02  HCN50V03-HOSPNAME
                        PIC X(90).
            02  HCN50V03-RENNUM
                        PIC X(7).
            02  HCN50V03-SINKEI
                        PIC X(1).
            02  HCN50V03-TENKI
                        PIC X(1).
            02  HCN50V03-KOFUNUM.
              03  HCN50V03-KOFUNUM-S
                        PIC X(1)        OCCURS 14 TIMES.
            02  HCN50V03-TANJYOYMD.
              03  HCN50V03-TANJYOYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN50V03-SYOBYOYMD.
              03  HCN50V03-SYOBYOYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V03-STRYMD.
              03  HCN50V03-STRYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V03-ENDYMD.
              03  HCN50V03-ENDYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN50V03-NISSU.
              03  HCN50V03-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN50V03-GOKEI.
              03  HCN50V03-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN50V03-KANANAME
                        PIC X(50).
            02  HCN50V03-NAME
                        PIC X(26).
            02  HCN50V03-NAME2
                        PIC X(50).
            02  HCN50V03-NENREI
                        PIC X(3).
            02  HCN50V03-JIGYOUNAME
                        PIC X(80).
            02  HCN50V03-TODOFUKEN
                        PIC X(6).
            02  HCN50V03-TODOFUKENKBN1
                        PIC X(2).
            02  HCN50V03-TODOFUKENKBN2
                        PIC X(2).
            02  HCN50V03-TODOFUKENKBN3
                        PIC X(2).
            02  HCN50V03-TODOFUKENKBN4
                        PIC X(2).
            02  HCN50V03-GUNSIKU
                        PIC X(20).
            02  HCN50V03-GUNSIKUKBN1
                        PIC X(2).
            02  HCN50V03-GUNSIKUKBN2
                        PIC X(2).
            02  HCN50V03-GUNSIKUKBN3
                        PIC X(2).
            02  HCN50V03-SYOBYOMEI
                        PIC X(44)       OCCURS 3 TIMES.
            02  HCN50V03-KEIKA
                        PIC X(54)       OCCURS 4 TIMES.
            02  HCN50V03-SYOSHIN1
                        PIC X(2).
            02  HCN50V03-SYOSHIN2
                        PIC X(2).
            02  HCN50V03-SYOSHIN3
                        PIC X(2).
            02  HCN50V03-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN50V03-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN50V03-NYUINYMD.
              03  HCN50V03-NYUINYY
                        PIC X(2).
              03  HCN50V03-NYUINMM
                        PIC X(2).
              03  HCN50V03-NYUINDD
                        PIC X(2).
            02  HCN50V03-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN50V03-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN50V03-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN50V03-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN50V03-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN50V03-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN50V03-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN50V03-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN50V03-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN50V03-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN50V03-TOTALTEN
                        PIC X(10).
            02  HCN50V03-TOTALMONEY
                        PIC X(10).
            02  HCN50V03-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN50V03-KINGAKU
                        PIC X(7)        OCCURS 5 TIMES.
            02  HCN50V03-SYOKEI
                        PIC X(7).
            02  HCN50V03-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN50V03-SYOKAKERU
                        PIC X(4).
            02  HCN50V03-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN50V03-SYONICHI
                        PIC X(4).
            02  HCN50V03-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN50V03-SYONISSU
                        PIC X(2).
            02  HCN50V03-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN50V03-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN50V03-SYOMONEY
                        PIC X(10).
            02  HCN50V03-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN50V03-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN50V03-KAI
                        PIC X(3).
            02  HCN50V03-PAGE
                        PIC X(5).
            02  HCN50V03-LABEL
                        PIC X(14).
            02  HCN50V03-SYOCD
                        PIC X(5).
            02  HCN50V03-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN50V03-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN50V03-LOPTION
                        PIC X(100).

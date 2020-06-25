        01  HCN66.
            02  HCN66-SAIGAIKBN
                        PIC X(8).
            02  HCN66-PTNUM
                        PIC X(20).
            02  HCN66-RSIIRYOCD
                        PIC X(30).
            02  HCN66-HOSPNAME
                        PIC X(64).
            02  HCN66-RENNUM
                        PIC X(7).
            02  HCN66-SINKEI
                        PIC X(1).
            02  HCN66-TENKI
                        PIC X(1).
            02  HCN66-KOFUNUM.
              03  HCN66-KOFUNUM-S
                        PIC X(1)        OCCURS 14 TIMES.
            02  HCN66-TANJYOYMD.
              03  HCN66-TANJYOYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN66-SYOBYOYMD.
              03  HCN66-SYOBYOYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN66-STRYMD.
              03  HCN66-STRYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN66-ENDYMD.
              03  HCN66-ENDYMD-S
                        PIC X(1)        OCCURS 6 TIMES.
            02  HCN66-NISSU.
              03  HCN66-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN66-GOKEI.
              03  HCN66-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN66-KANANAME
                        PIC X(50).
            02  HCN66-NAME
                        PIC X(26).
            02  HCN66-NAME2
                        PIC X(50).
            02  HCN66-NENREI
                        PIC X(3).
            02  HCN66-JIGYOUNAME
                        PIC X(80).
            02  HCN66-TODOFUKEN
                        PIC X(6).
            02  HCN66-TODOFUKENKBN1
                        PIC X(2).
            02  HCN66-TODOFUKENKBN2
                        PIC X(2).
            02  HCN66-TODOFUKENKBN3
                        PIC X(2).
            02  HCN66-TODOFUKENKBN4
                        PIC X(2).
            02  HCN66-GUNSIKU
                        PIC X(20).
            02  HCN66-GUNSIKUKBN1
                        PIC X(2).
            02  HCN66-GUNSIKUKBN2
                        PIC X(2).
            02  HCN66-GUNSIKUKBN3
                        PIC X(2).
            02  HCN66-SYOBYOMEI
                        PIC X(44)       OCCURS 3 TIMES.
            02  HCN66-KEIKA
                        PIC X(54)       OCCURS 4 TIMES.
            02  HCN66-SYOSHIN1
                        PIC X(2).
            02  HCN66-SYOSHIN2
                        PIC X(2).
            02  HCN66-SYOSHIN3
                        PIC X(2).
            02  HCN66-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN66-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN66-NYUINYMD.
              03  HCN66-NYUINYY
                        PIC X(2).
              03  HCN66-NYUINMM
                        PIC X(2).
              03  HCN66-NYUINDD
                        PIC X(2).
            02  HCN66-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN66-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN66-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN66-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN66-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN66-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN66-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN66-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN66-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN66-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN66-TOTALTEN
                        PIC X(10).
            02  HCN66-TOTALMONEY
                        PIC X(10).
            02  HCN66-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN66-KINGAKU
                        PIC X(7)        OCCURS 5 TIMES.
            02  HCN66-SYOKEI
                        PIC X(7).
            02  HCN66-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN66-SYOKAKERU
                        PIC X(4).
            02  HCN66-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN66-SYONICHI
                        PIC X(4).
            02  HCN66-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN66-SYONISSU
                        PIC X(2).
            02  HCN66-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN66-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN66-SYOMONEY
                        PIC X(10).
            02  HCN66-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN66-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN66-KAI
                        PIC X(3).
            02  HCN66-PAGE
                        PIC X(5).
            02  HCN66-LABEL
                        PIC X(14).
            02  HCN66-SYOCD
                        PIC X(5).
            02  HCN66-HOSPNAME2
                        PIC X(94).
            02  HCN66-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN66-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN66-LOPTION
                        PIC X(100).

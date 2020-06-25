        01  HCN72.
            02  HCN72-SAIGAIKBN
                        PIC X(8).
            02  HCN72-PTNUM
                        PIC X(20).
            02  HCN72-RSIIRYOCD
                        PIC X(30).
            02  HCN72-HOSPNAME
                        PIC X(90).
            02  HCN72-RENNUM
                        PIC X(7).
            02  HCN72-SINKEI
                        PIC X(1).
            02  HCN72-TENKI
                        PIC X(1).
            02  HCN72-KOFUNUM.
              03  HCN72-KOFUNUM-S
                        PIC X(1)        OCCURS 14 TIMES.
            02  HCN72-TANJYOYMD.
              03  HCN72-TANJYOYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN72-SYOBYOYMD.
              03  HCN72-SYOBYOYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN72-STRYMD.
              03  HCN72-STRYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN72-ENDYMD.
              03  HCN72-ENDYMD-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN72-NISSU.
              03  HCN72-NISSU-S
                        PIC X(1)        OCCURS 3 TIMES.
            02  HCN72-GOKEI.
              03  HCN72-GOKEI-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCN72-KANANAME
                        PIC X(50).
            02  HCN72-NAME
                        PIC X(26).
            02  HCN72-NAME2
                        PIC X(50).
            02  HCN72-NENREI
                        PIC X(3).
            02  HCN72-JIGYOUNAME
                        PIC X(80).
            02  HCN72-TODOFUKEN
                        PIC X(6).
            02  HCN72-TODOFUKENKBN1
                        PIC X(2).
            02  HCN72-TODOFUKENKBN2
                        PIC X(2).
            02  HCN72-TODOFUKENKBN3
                        PIC X(2).
            02  HCN72-TODOFUKENKBN4
                        PIC X(2).
            02  HCN72-GUNSIKU
                        PIC X(20).
            02  HCN72-GUNSIKUKBN1
                        PIC X(2).
            02  HCN72-GUNSIKUKBN2
                        PIC X(2).
            02  HCN72-GUNSIKUKBN3
                        PIC X(2).
            02  HCN72-SYOBYOMEI
                        PIC X(44)       OCCURS 3 TIMES.
            02  HCN72-KEIKA
                        PIC X(54)       OCCURS 4 TIMES.
            02  HCN72-SYOSHIN1
                        PIC X(2).
            02  HCN72-SYOSHIN2
                        PIC X(2).
            02  HCN72-SYOSHIN3
                        PIC X(2).
            02  HCN72-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN72-TENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN72-NYUINGG
                        PIC X(4).
            02  HCN72-NYUINYMD.
              03  HCN72-NYUINYY
                        PIC X(2).
              03  HCN72-NYUINMM
                        PIC X(2).
              03  HCN72-NYUINDD
                        PIC X(2).
            02  HCN72-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN72-NYUSYUBETSU
                        PIC X(10)       OCCURS 11 TIMES.
            02  HCN72-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN72-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN72-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN72-NYUTENSU
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN72-NYUTENSUN-G
                                        OCCURS 9 TIMES.
              03  HCN72-NYUTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN72-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN72-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN72-TOTALTEN
                        PIC X(10).
            02  HCN72-TOTALMONEY
                        PIC X(10).
            02  HCN72-SONOTA
                        PIC X(12)       OCCURS 4 TIMES.
            02  HCN72-KINGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCN72-SYOKEI
                        PIC X(9).
            02  HCN72-SYOKJN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN72-SYOKAKERU
                        PIC X(4).
            02  HCN72-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN72-SYONICHI
                        PIC X(4).
            02  HCN72-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN72-SYONISSU
                        PIC X(2).
            02  HCN72-TEKIYO1
                        PIC X(28)       OCCURS 7 TIMES.
            02  HCN72-BIKOU
                        PIC X(28)       OCCURS 5 TIMES.
            02  HCN72-SYOMONEY
                        PIC X(10).
            02  HCN72-SRYKBN
                        PIC X(2)        OCCURS 22 TIMES.
            02  HCN72-TEKIYO2
                        PIC X(52)       OCCURS 22 TIMES.
            02  HCN72-KAI
                        PIC X(3).
            02  HCN72-PAGE
                        PIC X(5).
            02  HCN72-LABEL
                        PIC X(14).
            02  HCN72-SYOCD
                        PIC X(5).
            02  HCN72-NYUSYUBETSU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN72-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN72-LOPTION
                        PIC X(100).
            02  HCN72-HEAD-G.
              03  HCN72-HEAD
                        PIC X(62)       OCCURS 2 TIMES.

        01  HCN50.
            02  HCN50-SAIGAIKBN PIC X(8).
            02  HCN50-PTNUM PIC X(20).
            02  HCN50-RSIIRYOCD PIC X(30).
            02  HCN50-HOSPNAME  PIC X(90).
            02  HCN50-RENNUM    PIC X(7).
            02  HCN50-SINKEI    PIC X(1).
            02  HCN50-TENKI PIC X(1).
            02  HCN50-KOFUNUM.
              03  HCN50-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCN50-TANJYOYMD.
              03  HCN50-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN50-SYOBYOYMD.
              03  HCN50-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN50-STRYMD.
              03  HCN50-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN50-ENDYMD.
              03  HCN50-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN50-NISSU.
              03  HCN50-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCN50-GOKEI.
              03  HCN50-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN50-KANANAME  PIC X(50).
            02  HCN50-NAME  PIC X(26).
            02  HCN50-NAME2 PIC X(50).
            02  HCN50-NENREI    PIC X(3).
            02  HCN50-JIGYOUNAME    PIC X(80).
            02  HCN50-TODOFUKEN PIC X(6).
            02  HCN50-TODOFUKENKBN1 PIC X(2).
            02  HCN50-TODOFUKENKBN2 PIC X(2).
            02  HCN50-TODOFUKENKBN3 PIC X(2).
            02  HCN50-TODOFUKENKBN4 PIC X(2).
            02  HCN50-GUNSIKU   PIC X(20).
            02  HCN50-GUNSIKUKBN1   PIC X(2).
            02  HCN50-GUNSIKUKBN2   PIC X(2).
            02  HCN50-GUNSIKUKBN3   PIC X(2).
            02  HCN50-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCN50-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCN50-SYOSHIN1  PIC X(2).
            02  HCN50-SYOSHIN2  PIC X(2).
            02  HCN50-SYOSHIN3  PIC X(2).
            02  HCN50-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN50-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN50-NYUINYMD.
              03  HCN50-NYUINYY   PIC X(2).
              03  HCN50-NYUINMM   PIC X(2).
              03  HCN50-NYUINDD   PIC X(2).
            02  HCN50-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN50-NYUSYUBETSU
                    PIC X(10)   OCCURS  11 TIMES.
            02  HCN50-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN50-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN50-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN50-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN50-NYUTENSUN-G       OCCURS  9 TIMES.
              03  HCN50-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN50-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN50-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN50-TOTALTEN  PIC X(10).
            02  HCN50-TOTALMONEY    PIC X(10).
            02  HCN50-SONOTA
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN50-KINGAKU
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCN50-SYOKEI    PIC X(7).
            02  HCN50-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN50-SYOKAKERU PIC X(4).
            02  HCN50-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN50-SYONICHI  PIC X(4).
            02  HCN50-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN50-SYONISSU  PIC X(2).
            02  HCN50-TEKIYO1
                    PIC X(28)   OCCURS  7 TIMES.
            02  HCN50-BIKOU
                    PIC X(28)   OCCURS  5 TIMES.
            02  HCN50-SYOMONEY  PIC X(10).
            02  HCN50-SRYKBN
                    PIC X(2)    OCCURS  22 TIMES.
            02  HCN50-TEKIYO2
                    PIC X(52)   OCCURS  22 TIMES.
            02  HCN50-KAI   PIC X(3).
            02  HCN50-PAGE  PIC X(5).

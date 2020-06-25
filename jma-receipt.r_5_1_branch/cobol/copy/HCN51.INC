        01  HCN51.
            02  HCN51-SAIGAIKBN PIC X(8).
            02  HCN51-PTNUM PIC X(20).
            02  HCN51-RSIIRYOCD PIC X(30).
            02  HCN51-HOSPNAME  PIC X(90).
            02  HCN51-RENNUM    PIC X(7).
            02  HCN51-SINKEI    PIC X(1).
            02  HCN51-TENKI PIC X(1).
            02  HCN51-KOFUNUM.
              03  HCN51-KOFUNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCN51-RYOSTYMD.
              03  HCN51-RYOSTYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN51-RYOEDYMD.
              03  HCN51-RYOEDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN51-NISSU.
              03  HCN51-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCN51-GOKEI.
              03  HCN51-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN51-KANANAME  PIC X(50).
            02  HCN51-NAME  PIC X(26).
            02  HCN51-NAME2 PIC X(50).
            02  HCN51-NENREI    PIC X(3).
            02  HCN51-TANJYOG
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN51-TANJYOYMD.
              03  HCN51-TANJYOY   PIC X(2).
              03  HCN51-TANJYOM   PIC X(2).
              03  HCN51-TANJYOD   PIC X(2).
            02  HCN51-SYOBYOMEI
                    PIC X(44)   OCCURS  5 TIMES.
            02  HCN51-KEIKA
                    PIC X(54)   OCCURS  8 TIMES.
            02  HCN51-SYOSHIN1  PIC X(2).
            02  HCN51-SYOSHIN2  PIC X(2).
            02  HCN51-SYOSHIN3  PIC X(2).
            02  HCN51-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN51-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN51-NYUINYMD.
              03  HCN51-NYUINYY   PIC X(2).
              03  HCN51-NYUINMM   PIC X(2).
              03  HCN51-NYUINDD   PIC X(2).
            02  HCN51-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN51-NYUSYUBETSU
                    PIC X(10)   OCCURS  11 TIMES.
            02  HCN51-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN51-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN51-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN51-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN51-NYUTENSUN-G       OCCURS  9 TIMES.
              03  HCN51-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN51-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN51-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN51-TOTALTEN  PIC X(10).
            02  HCN51-TOTALMONEY    PIC X(10).
            02  HCN51-SONOTA
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN51-KINGAKU
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCN51-SYOKEI    PIC X(7).
            02  HCN51-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN51-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN51-SYOKAKERU PIC X(4).
            02  HCN51-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN51-SYONICHI  PIC X(4).
            02  HCN51-SYONISSU  PIC X(2).
            02  HCN51-TEKIYO1
                    PIC X(28)   OCCURS  7 TIMES.
            02  HCN51-BIKOU
                    PIC X(28)   OCCURS  5 TIMES.
            02  HCN51-SYOMONEY  PIC X(10).
            02  HCN51-SRYKBN
                    PIC X(2)    OCCURS  22 TIMES.
            02  HCN51-TEKIYO2
                    PIC X(52)   OCCURS  22 TIMES.
            02  HCN51-KAI   PIC X(3).
            02  HCN51-PAGE  PIC X(5).

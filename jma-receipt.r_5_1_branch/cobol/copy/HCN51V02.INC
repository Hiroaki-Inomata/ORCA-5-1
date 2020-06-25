        01  HCN51V02.
            02  HCN51V02-SAIGAIKBN  PIC X(8).
            02  HCN51V02-PTNUM  PIC X(20).
            02  HCN51V02-RSIIRYOCD  PIC X(30).
            02  HCN51V02-HOSPNAME   PIC X(90).
            02  HCN51V02-RENNUM PIC X(7).
            02  HCN51V02-SINKEI PIC X(1).
            02  HCN51V02-TENKI  PIC X(1).
            02  HCN51V02-KOFUNUM.
              03  HCN51V02-KOFUNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCN51V02-RYOSTYMD.
              03  HCN51V02-RYOSTYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN51V02-RYOEDYMD.
              03  HCN51V02-RYOEDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN51V02-NISSU.
              03  HCN51V02-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCN51V02-GOKEI.
              03  HCN51V02-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN51V02-KANANAME   PIC X(50).
            02  HCN51V02-NAME   PIC X(26).
            02  HCN51V02-NAME2  PIC X(50).
            02  HCN51V02-NENREI PIC X(3).
            02  HCN51V02-TANJYOG
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN51V02-TANJYOYMD.
              03  HCN51V02-TANJYOY    PIC X(2).
              03  HCN51V02-TANJYOM    PIC X(2).
              03  HCN51V02-TANJYOD    PIC X(2).
            02  HCN51V02-SYOBYOMEI
                    PIC X(44)   OCCURS  5 TIMES.
            02  HCN51V02-KEIKA
                    PIC X(54)   OCCURS  8 TIMES.
            02  HCN51V02-SYOSHIN1   PIC X(2).
            02  HCN51V02-SYOSHIN2   PIC X(2).
            02  HCN51V02-SYOSHIN3   PIC X(2).
            02  HCN51V02-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN51V02-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN51V02-NYUINYMD.
              03  HCN51V02-NYUINYY    PIC X(2).
              03  HCN51V02-NYUINMM    PIC X(2).
              03  HCN51V02-NYUINDD    PIC X(2).
            02  HCN51V02-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN51V02-NYUSYUBETSU
                    PIC X(10)   OCCURS  11 TIMES.
            02  HCN51V02-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN51V02-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN51V02-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN51V02-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN51V02-NYUTENSUN-G            OCCURS  9 TIMES.
              03  HCN51V02-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN51V02-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN51V02-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN51V02-TOTALTEN   PIC X(10).
            02  HCN51V02-TOTALMONEY PIC X(10).
            02  HCN51V02-SONOTA
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN51V02-KINGAKU
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCN51V02-SYOKEI PIC X(7).
            02  HCN51V02-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN51V02-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN51V02-SYOKAKERU  PIC X(4).
            02  HCN51V02-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN51V02-SYONICHI   PIC X(4).
            02  HCN51V02-SYONISSU   PIC X(2).
            02  HCN51V02-TEKIYO1
                    PIC X(28)   OCCURS  7 TIMES.
            02  HCN51V02-BIKOU
                    PIC X(28)   OCCURS  5 TIMES.
            02  HCN51V02-SYOMONEY   PIC X(10).
            02  HCN51V02-SRYKBN
                    PIC X(2)    OCCURS  22 TIMES.
            02  HCN51V02-TEKIYO2
                    PIC X(52)   OCCURS  22 TIMES.
            02  HCN51V02-KAI    PIC X(3).
            02  HCN51V02-PAGE   PIC X(5).
            02  HCN51V02-LABEL  PIC X(14).
            02  HCN51V02-SYOCD  PIC X(5).
            02  HCN51V02-LOPTION    PIC X(100).

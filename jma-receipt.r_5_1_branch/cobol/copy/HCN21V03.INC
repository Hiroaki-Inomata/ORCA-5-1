        01  HCN21V03.
            02  HCN21V03-SAIGAIKBN  PIC X(8).
            02  HCN21V03-PTNUM  PIC X(20).
            02  HCN21V03-RSIIRYOCD  PIC X(30).
            02  HCN21V03-HOSPNAME   PIC X(90).
            02  HCN21V03-RENNUM PIC X(7).
            02  HCN21V03-SINKEI PIC X(1).
            02  HCN21V03-TENKI  PIC X(1).
            02  HCN21V03-KOFUNUM.
              03  HCN21V03-KOFUNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCN21V03-RYOSTYMD.
              03  HCN21V03-RYOSTYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN21V03-RYOEDYMD.
              03  HCN21V03-RYOEDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN21V03-NISSU.
              03  HCN21V03-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCN21V03-GOKEI.
              03  HCN21V03-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN21V03-KANANAME   PIC X(50).
            02  HCN21V03-NAME   PIC X(26).
            02  HCN21V03-NAME2  PIC X(50).
            02  HCN21V03-NENREI PIC X(3).
            02  HCN21V03-TANJYOG
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN21V03-TANJYOYMD.
              03  HCN21V03-TANJYOY    PIC X(2).
              03  HCN21V03-TANJYOM    PIC X(2).
              03  HCN21V03-TANJYOD    PIC X(2).
            02  HCN21V03-SYOBYOMEI
                    PIC X(44)   OCCURS  5 TIMES.
            02  HCN21V03-KEIKA
                    PIC X(54)   OCCURS  8 TIMES.
            02  HCN21V03-SYOSHIN1   PIC X(2).
            02  HCN21V03-SYOSHIN2   PIC X(2).
            02  HCN21V03-SYOSHIN3   PIC X(2).
            02  HCN21V03-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN21V03-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN21V03-NYUINYMD.
              03  HCN21V03-NYUINYY    PIC X(2).
              03  HCN21V03-NYUINMM    PIC X(2).
              03  HCN21V03-NYUINDD    PIC X(2).
            02  HCN21V03-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN21V03-NYUSYUBETSU
                    PIC X(10)   OCCURS  11 TIMES.
            02  HCN21V03-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN21V03-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN21V03-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN21V03-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN21V03-NYUTENSUN-G            OCCURS  9 TIMES.
              03  HCN21V03-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN21V03-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN21V03-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN21V03-TOTALTEN   PIC X(10).
            02  HCN21V03-TOTALMONEY PIC X(10).
            02  HCN21V03-SONOTA
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN21V03-KINGAKU
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCN21V03-SYOKEI PIC X(7).
            02  HCN21V03-SYOKJN PIC X(48).
            02  HCN21V03-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN21V03-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN21V03-SYOKAKERU  PIC X(4).
            02  HCN21V03-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN21V03-SYONICHI   PIC X(4).
            02  HCN21V03-SYONISSU   PIC X(2).
            02  HCN21V03-TEKIYO1
                    PIC X(28)   OCCURS  7 TIMES.
            02  HCN21V03-BIKOU
                    PIC X(28)   OCCURS  5 TIMES.
            02  HCN21V03-SYOMONEY   PIC X(10).
            02  HCN21V03-SRYKBN
                    PIC X(2)    OCCURS  22 TIMES.
            02  HCN21V03-TEKIYO2
                    PIC X(52)   OCCURS  22 TIMES.
            02  HCN21V03-KAI    PIC X(3).

        01  HCN22V01.
            02  HCN22V01-SAIGAIKBN  PIC X(8).
            02  HCN22V01-PTNUM  PIC X(20).
            02  HCN22V01-RSIIRYOCD  PIC X(30).
            02  HCN22V01-HOSPNAME   PIC X(60).
            02  HCN22V01-RENNUM PIC X(7).
            02  HCN22V01-SINKEI PIC X(1).
            02  HCN22V01-TENKI  PIC X(1).
            02  HCN22V01-KOFUNUM.
              03  HCN22V01-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCN22V01-TANJYOYMD.
              03  HCN22V01-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN22V01-SYOBYOYMD.
              03  HCN22V01-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN22V01-STRYMD.
              03  HCN22V01-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN22V01-ENDYMD.
              03  HCN22V01-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCN22V01-NISSU.
              03  HCN22V01-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCN22V01-GOKEI.
              03  HCN22V01-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCN22V01-KANANAME   PIC X(50).
            02  HCN22V01-NAME   PIC X(26).
            02  HCN22V01-NAME2  PIC X(50).
            02  HCN22V01-NENREI PIC X(3).
            02  HCN22V01-JIGYOUNAME PIC X(80).
            02  HCN22V01-TODOFUKEN  PIC X(6).
            02  HCN22V01-TODOFUKENKBN1  PIC X(2).
            02  HCN22V01-TODOFUKENKBN2  PIC X(2).
            02  HCN22V01-TODOFUKENKBN3  PIC X(2).
            02  HCN22V01-TODOFUKENKBN4  PIC X(2).
            02  HCN22V01-GUNSIKU    PIC X(20).
            02  HCN22V01-GUNSIKUKBN1    PIC X(2).
            02  HCN22V01-GUNSIKUKBN2    PIC X(2).
            02  HCN22V01-GUNSIKUKBN3    PIC X(2).
            02  HCN22V01-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCN22V01-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCN22V01-SYOSHIN1   PIC X(2).
            02  HCN22V01-SYOSHIN2   PIC X(2).
            02  HCN22V01-SYOSHIN3   PIC X(2).
            02  HCN22V01-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN22V01-TENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN22V01-NYUINYMD.
              03  HCN22V01-NYUINYY    PIC X(2).
              03  HCN22V01-NYUINMM    PIC X(2).
              03  HCN22V01-NYUINDD    PIC X(2).
            02  HCN22V01-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN22V01-NYUSYUBETSU
                    PIC X(10)   OCCURS  11 TIMES.
            02  HCN22V01-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN22V01-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN22V01-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN22V01-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN22V01-NYUTENSUN-G            OCCURS  9 TIMES.
              03  HCN22V01-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN22V01-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN22V01-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN22V01-TOTALTEN   PIC X(7).
            02  HCN22V01-TOTALMONEY PIC X(10).
            02  HCN22V01-SONOTA
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCN22V01-KINGAKU
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCN22V01-SYOKEI PIC X(7).
            02  HCN22V01-SYOKJN PIC X(48).
            02  HCN22V01-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN22V01-SYOKAKERU  PIC X(4).
            02  HCN22V01-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN22V01-SYONICHI   PIC X(4).
            02  HCN22V01-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN22V01-SYONISSU   PIC X(2).
            02  HCN22V01-TEKIYO1
                    PIC X(28)   OCCURS  7 TIMES.
            02  HCN22V01-BIKOU
                    PIC X(28)   OCCURS  5 TIMES.
            02  HCN22V01-SYOMONEY   PIC X(10).
            02  HCN22V01-SRYKBN
                    PIC X(2)    OCCURS  22 TIMES.
            02  HCN22V01-TEKIYO2
                    PIC X(52)   OCCURS  22 TIMES.

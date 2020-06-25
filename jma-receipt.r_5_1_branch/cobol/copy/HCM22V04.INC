        01  HCM22V04.
            02  HCM22V04-TOTALTEN   PIC X(6).
            02  HCM22V04-TOTALMONEY PIC X(10).
            02  HCM22V04-TENKI  PIC X(1).
            02  HCM22V04-SINKEI PIC X(1).
            02  HCM22V04-JIGYOUNAME PIC X(80).
            02  HCM22V04-KANANAME   PIC X(50).
            02  HCM22V04-NAME   PIC X(26).
            02  HCM22V04-NAME2  PIC X(50).
            02  HCM22V04-NENREI PIC X(3).
            02  HCM22V04-TODOFUKEN  PIC X(6).
            02  HCM22V04-GUNSIKU    PIC X(20).
            02  HCM22V04-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCM22V04-SAISINKAISU    PIC X(3).
            02  HCM22V04-SHIDOKAISU PIC X(3).
            02  HCM22V04-TEKIYO1
                    PIC X(28)   OCCURS  9 TIMES.
            02  HCM22V04-SRYKBN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM22V04-TEKIYO2
                    PIC X(52)   OCCURS  30 TIMES.
            02  HCM22V04-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCM22V04-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM22V04-KHNTEN
                    PIC X(7)    OCCURS  19 TIMES.
            02  HCM22V04-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM22V04-RSIIRYOCD  PIC X(30).
            02  HCM22V04-HOSPNAME   PIC X(90).
            02  HCM22V04-GUNSIKUKBN1    PIC X(2).
            02  HCM22V04-GUNSIKUKBN2    PIC X(2).
            02  HCM22V04-GUNSIKUKBN3    PIC X(2).
            02  HCM22V04-TODOFUKENKBN1  PIC X(2).
            02  HCM22V04-TODOFUKENKBN3  PIC X(2).
            02  HCM22V04-TODOFUKENKBN2  PIC X(2).
            02  HCM22V04-TODOFUKENKBN4  PIC X(2).
            02  HCM22V04-SAIGAIKBN  PIC X(8).
            02  HCM22V04-SYOSHIN1   PIC X(2).
            02  HCM22V04-SYOSHIN2   PIC X(2).
            02  HCM22V04-SYOSHIN3   PIC X(2).
            02  HCM22V04-KEIZOKUKANRI   PIC X(16).
            02  HCM22V04-SYOKEI PIC X(7).
            02  HCM22V04-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM22V04-PTNUM  PIC X(20).
            02  HCM22V04-TANJYOYMD.
              03  HCM22V04-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V04-SYOBYOYMD.
              03  HCM22V04-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V04-NISSU.
              03  HCM22V04-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCM22V04-GOKEI.
              03  HCM22V04-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V04-ENDYMD.
              03  HCM22V04-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V04-STRYMD.
              03  HCM22V04-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V04-KOFUNUM.
              03  HCM22V04-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM22V04-RENNUM PIC X(7).
            02  HCM22V04-KAI    PIC X(3).
            02  HCM22V04-PAGE   PIC X(5).

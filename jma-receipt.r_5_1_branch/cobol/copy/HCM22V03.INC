        01  HCM22V03.
            02  HCM22V03-TOTALTEN   PIC X(6).
            02  HCM22V03-TOTALMONEY PIC X(10).
            02  HCM22V03-TENKI  PIC X(1).
            02  HCM22V03-SINKEI PIC X(1).
            02  HCM22V03-JIGYOUNAME PIC X(80).
            02  HCM22V03-KANANAME   PIC X(50).
            02  HCM22V03-NAME   PIC X(26).
            02  HCM22V03-NAME2  PIC X(50).
            02  HCM22V03-NENREI PIC X(3).
            02  HCM22V03-TODOFUKEN  PIC X(6).
            02  HCM22V03-GUNSIKU    PIC X(20).
            02  HCM22V03-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCM22V03-SAISINKAISU    PIC X(3).
            02  HCM22V03-SHIDOKAISU PIC X(3).
            02  HCM22V03-TEKIYO1
                    PIC X(28)   OCCURS  9 TIMES.
            02  HCM22V03-SRYKBN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM22V03-TEKIYO2
                    PIC X(52)   OCCURS  30 TIMES.
            02  HCM22V03-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCM22V03-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM22V03-KHNTEN
                    PIC X(7)    OCCURS  19 TIMES.
            02  HCM22V03-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM22V03-RSIIRYOCD  PIC X(30).
            02  HCM22V03-HOSPNAME   PIC X(90).
            02  HCM22V03-GUNSIKUKBN1    PIC X(2).
            02  HCM22V03-GUNSIKUKBN2    PIC X(2).
            02  HCM22V03-GUNSIKUKBN3    PIC X(2).
            02  HCM22V03-TODOFUKENKBN1  PIC X(2).
            02  HCM22V03-TODOFUKENKBN3  PIC X(2).
            02  HCM22V03-TODOFUKENKBN2  PIC X(2).
            02  HCM22V03-TODOFUKENKBN4  PIC X(2).
            02  HCM22V03-SAIGAIKBN  PIC X(8).
            02  HCM22V03-SYOSHIN1   PIC X(2).
            02  HCM22V03-SYOSHIN2   PIC X(2).
            02  HCM22V03-SYOSHIN3   PIC X(2).
            02  HCM22V03-KEIZOKUKANRI   PIC X(16).
            02  HCM22V03-SYOKEI PIC X(7).
            02  HCM22V03-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM22V03-PTNUM  PIC X(20).
            02  HCM22V03-TANJYOYMD.
              03  HCM22V03-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V03-SYOBYOYMD.
              03  HCM22V03-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V03-NISSU.
              03  HCM22V03-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCM22V03-GOKEI.
              03  HCM22V03-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V03-ENDYMD.
              03  HCM22V03-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V03-STRYMD.
              03  HCM22V03-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V03-KOFUNUM.
              03  HCM22V03-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM22V03-RENNUM PIC X(7).
            02  HCM22V03-KAI    PIC X(3).

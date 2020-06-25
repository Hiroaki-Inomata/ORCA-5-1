        01  HCM22V02.
            02  HCM22V02-TOTALTEN   PIC X(6).
            02  HCM22V02-TOTALMONEY PIC X(10).
            02  HCM22V02-TENKI  PIC X(1).
            02  HCM22V02-SINKEI PIC X(1).
            02  HCM22V02-JIGYOUNAME PIC X(80).
            02  HCM22V02-KANANAME   PIC X(50).
            02  HCM22V02-NAME   PIC X(26).
            02  HCM22V02-NAME2  PIC X(50).
            02  HCM22V02-NENREI PIC X(3).
            02  HCM22V02-TODOFUKEN  PIC X(6).
            02  HCM22V02-GUNSIKU    PIC X(20).
            02  HCM22V02-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCM22V02-SAISINKAISU    PIC X(3).
            02  HCM22V02-SHIDOKAISU PIC X(3).
            02  HCM22V02-TEKIYO1
                    PIC X(28)   OCCURS  9 TIMES.
            02  HCM22V02-SRYKBN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM22V02-TEKIYO2
                    PIC X(52)   OCCURS  30 TIMES.
            02  HCM22V02-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCM22V02-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM22V02-KHNTEN
                    PIC X(7)    OCCURS  19 TIMES.
            02  HCM22V02-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM22V02-RSIIRYOCD  PIC X(30).
            02  HCM22V02-HOSPNAME   PIC X(60).
            02  HCM22V02-GUNSIKUKBN1    PIC X(2).
            02  HCM22V02-GUNSIKUKBN2    PIC X(2).
            02  HCM22V02-GUNSIKUKBN3    PIC X(2).
            02  HCM22V02-TODOFUKENKBN1  PIC X(2).
            02  HCM22V02-TODOFUKENKBN3  PIC X(2).
            02  HCM22V02-TODOFUKENKBN2  PIC X(2).
            02  HCM22V02-TODOFUKENKBN4  PIC X(2).
            02  HCM22V02-SAIGAIKBN  PIC X(8).
            02  HCM22V02-SYOSHIN1   PIC X(2).
            02  HCM22V02-SYOSHIN2   PIC X(2).
            02  HCM22V02-SYOSHIN3   PIC X(2).
            02  HCM22V02-KEIZOKUKANRI   PIC X(16).
            02  HCM22V02-SYOKEI PIC X(7).
            02  HCM22V02-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM22V02-PTNUM  PIC X(20).
            02  HCM22V02-TANJYOYMD.
              03  HCM22V02-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V02-SYOBYOYMD.
              03  HCM22V02-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V02-NISSU.
              03  HCM22V02-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCM22V02-GOKEI.
              03  HCM22V02-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V02-ENDYMD.
              03  HCM22V02-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V02-STRYMD.
              03  HCM22V02-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V02-KOFUNUM.
              03  HCM22V02-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM22V02-RENNUM PIC X(7).

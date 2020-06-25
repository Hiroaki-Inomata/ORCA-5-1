        01  HCM22V05.
            02  HCM22V05-TOTALTEN   PIC X(6).
            02  HCM22V05-TOTALMONEY PIC X(10).
            02  HCM22V05-TENKI  PIC X(1).
            02  HCM22V05-SINKEI PIC X(1).
            02  HCM22V05-JIGYOUNAME PIC X(80).
            02  HCM22V05-KANANAME   PIC X(50).
            02  HCM22V05-NAME   PIC X(26).
            02  HCM22V05-NAME2  PIC X(50).
            02  HCM22V05-NENREI PIC X(3).
            02  HCM22V05-TODOFUKEN  PIC X(6).
            02  HCM22V05-GUNSIKU    PIC X(20).
            02  HCM22V05-SYOBYOMEI
                    PIC X(44)   OCCURS  3 TIMES.
            02  HCM22V05-SAISINKAISU    PIC X(3).
            02  HCM22V05-SHIDOKAISU PIC X(3).
            02  HCM22V05-TEKIYO1
                    PIC X(28)   OCCURS  9 TIMES.
            02  HCM22V05-SRYKBN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM22V05-TEKIYO2
                    PIC X(52)   OCCURS  30 TIMES.
            02  HCM22V05-KEIKA
                    PIC X(54)   OCCURS  4 TIMES.
            02  HCM22V05-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM22V05-KHNTEN
                    PIC X(7)    OCCURS  19 TIMES.
            02  HCM22V05-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM22V05-RSIIRYOCD  PIC X(30).
            02  HCM22V05-HOSPNAME   PIC X(90).
            02  HCM22V05-GUNSIKUKBN1    PIC X(2).
            02  HCM22V05-GUNSIKUKBN2    PIC X(2).
            02  HCM22V05-GUNSIKUKBN3    PIC X(2).
            02  HCM22V05-TODOFUKENKBN1  PIC X(2).
            02  HCM22V05-TODOFUKENKBN3  PIC X(2).
            02  HCM22V05-TODOFUKENKBN2  PIC X(2).
            02  HCM22V05-TODOFUKENKBN4  PIC X(2).
            02  HCM22V05-SAIGAIKBN  PIC X(8).
            02  HCM22V05-SYOSHIN1   PIC X(2).
            02  HCM22V05-SYOSHIN2   PIC X(2).
            02  HCM22V05-SYOSHIN3   PIC X(2).
            02  HCM22V05-KEIZOKUKANRI   PIC X(16).
            02  HCM22V05-SYOKEI PIC X(7).
            02  HCM22V05-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM22V05-PTNUM  PIC X(20).
            02  HCM22V05-TANJYOYMD.
              03  HCM22V05-TANJYOYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V05-SYOBYOYMD.
              03  HCM22V05-SYOBYOYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V05-NISSU.
              03  HCM22V05-NISSU-S
                      PIC X(1)    OCCURS  3 TIMES.
            02  HCM22V05-GOKEI.
              03  HCM22V05-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V05-ENDYMD.
              03  HCM22V05-ENDYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V05-STRYMD.
              03  HCM22V05-STRYMD-S
                      PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V05-KOFUNUM.
              03  HCM22V05-KOFUNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM22V05-RENNUM PIC X(7).
            02  HCM22V05-KAI    PIC X(3).
            02  HCM22V05-PAGE   PIC X(5).
            02  HCM22V05-LABEL  PIC X(14).

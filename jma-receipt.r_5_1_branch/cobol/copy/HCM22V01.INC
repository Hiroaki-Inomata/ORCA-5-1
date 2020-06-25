        01  HCM22V01.
            02  HCM22V01-TOTALTEN   PIC X(6).
            02  HCM22V01-TOTALMONEY PIC X(10).
            02  HCM22V01-TENKI  PIC X(1).
            02  HCM22V01-SINKEI PIC X(1).
            02  HCM22V01-JIGYOUNAME PIC X(80).
            02  HCM22V01-KANANAME   PIC X(30).
            02  HCM22V01-NAME   PIC X(26).
            02  HCM22V01-NENREI PIC X(3).
            02  HCM22V01-TODOFUKEN  PIC X(6).
            02  HCM22V01-GUNSIKU    PIC X(20).
            02  HCM22V01-SYOBYOMEI  PIC X(44)   OCCURS  3 TIMES.
            02  HCM22V01-SAISINKAISU    PIC X(3).
            02  HCM22V01-SHIDOKAISU PIC X(3).
            02  HCM22V01-TEKIYO1    PIC X(28)   OCCURS  9 TIMES.
            02  HCM22V01-SRYKBN PIC X(2)        OCCURS  30 TIMES.
            02  HCM22V01-TEKIYO2    PIC X(52)   OCCURS  30 TIMES.
            02  HCM22V01-KEIKA  PIC X(54)       OCCURS  4 TIMES.
            02  HCM22V01-KOUTEN PIC X(7)        OCCURS  35 TIMES.
            02  HCM22V01-KHNTEN PIC X(7)        OCCURS  19 TIMES.
            02  HCM22V01-KAISU  PIC X(3)        OCCURS  35 TIMES.
            02  HCM22V01-RSIIRYOCD  PIC X(30).
            02  HCM22V01-HOSPNAME   PIC X(60).
            02  HCM22V01-GUNSIKUKBN1    PIC X(2).
            02  HCM22V01-GUNSIKUKBN2    PIC X(2).
            02  HCM22V01-GUNSIKUKBN3    PIC X(2).
            02  HCM22V01-TODOFUKENKBN1  PIC X(2).
            02  HCM22V01-TODOFUKENKBN3  PIC X(2).
            02  HCM22V01-TODOFUKENKBN2  PIC X(2).
            02  HCM22V01-TODOFUKENKBN4  PIC X(2).
            02  HCM22V01-SAIGAIKBN  PIC X(8).
            02  HCM22V01-SYOSHIN1   PIC X(2).
            02  HCM22V01-SYOSHIN2   PIC X(2).
            02  HCM22V01-SYOSHIN3   PIC X(2).
            02  HCM22V01-KEIZOKUKANRI   PIC X(16).
            02  HCM22V01-SYOKEI PIC X(7).
            02  HCM22V01-KINGAKU    PIC X(7)      OCCURS  6 TIMES.
            02  HCM22V01-PTNUM  PIC X(20).
            02  HCM22V01-TANJYOYMD.
              03  HCM22V01-TANJYOYMD-S    PIC X(1)
                                                  OCCURS  7 TIMES.
            02  HCM22V01-SYOBYOYMD.
              03  HCM22V01-SYOBYOYMD-S    PIC X(1)
                                                  OCCURS  6 TIMES.
            02  HCM22V01-NISSU.
              03  HCM22V01-NISSU-S    PIC X(1)    OCCURS  3 TIMES.
            02  HCM22V01-GOKEI.
              03  HCM22V01-GOKEI-S    PIC X(1)    OCCURS  7 TIMES.
            02  HCM22V01-ENDYMD.
              03  HCM22V01-ENDYMD-S   PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V01-STRYMD.
              03  HCM22V01-STRYMD-S   PIC X(1)    OCCURS  6 TIMES.
            02  HCM22V01-KOFUNUM.
              03  HCM22V01-KOFUNUM-S  PIC X(1)    OCCURS  14 TIMES.
            02  HCM22V01-RENNUM PIC X(7).

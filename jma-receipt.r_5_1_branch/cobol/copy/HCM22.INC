        01  HCM22.
            02  HCM22-TOTALTEN  PIC X(6).
            02  HCM22-TOTALMONEY    PIC X(10).
            02  HCM22-TENKI PIC X(1).
            02  HCM22-SINKEI    PIC X(1).
            02  HCM22-JIGYOUNAME    PIC X(30).
            02  HCM22-KANANAME  PIC X(30).
            02  HCM22-NAME  PIC X(26).
            02  HCM22-NENREI    PIC X(3).
            02  HCM22-TODOFUKEN PIC X(6).
            02  HCM22-GUNSIKU   PIC X(20).
            02  HCM22-SYOBYOMEI PIC X(44)       OCCURS  3 TIMES.
            02  HCM22-SAISINKAISU   PIC X(3).
            02  HCM22-SHIDOKAISU    PIC X(3).
            02  HCM22-TEKIYO1   PIC X(28)       OCCURS  9 TIMES.
            02  HCM22-SRYKBN    PIC X(2)        OCCURS  30 TIMES.
            02  HCM22-TEKIYO2   PIC X(52)       OCCURS  30 TIMES.
            02  HCM22-KEIKA PIC X(54)   OCCURS  4 TIMES.
            02  HCM22-KOUTEN    PIC X(7)        OCCURS  35 TIMES.
            02  HCM22-KHNTEN    PIC X(7)        OCCURS  19 TIMES.
            02  HCM22-KAISU PIC X(3)    OCCURS  35 TIMES.
            02  HCM22-RSIIRYOCD PIC X(30).
            02  HCM22-HOSPNAME  PIC X(30).
            02  HCM22-GUNSIKUKBN1   PIC X(2).
            02  HCM22-GUNSIKUKBN2   PIC X(2).
            02  HCM22-GUNSIKUKBN3   PIC X(2).
            02  HCM22-TODOFUKENKBN1 PIC X(2).
            02  HCM22-TODOFUKENKBN3 PIC X(2).
            02  HCM22-TODOFUKENKBN2 PIC X(2).
            02  HCM22-TODOFUKENKBN4 PIC X(2).
            02  HCM22-SAIGAIKBN PIC X(8).
            02  HCM22-SYOSHIN1  PIC X(2).
            02  HCM22-SYOSHIN2  PIC X(2).
            02  HCM22-SYOSHIN3  PIC X(2).
            02  HCM22-KEIZOKUKANRI  PIC X(16).
            02  HCM22-SYOKEI    PIC X(7).
            02  HCM22-KINGAKU   PIC X(7)        OCCURS  6 TIMES.
            02  HCM22-PTNUM PIC X(20).
            02  HCM22-TANJYOYMD.
              03  HCM22-TANJYOYMD-S   PIC X(1)    OCCURS  7 TIMES.
            02  HCM22-SYOBYOYMD.
              03  HCM22-SYOBYOYMD-S   PIC X(1)    OCCURS  6 TIMES.
            02  HCM22-NISSU.
              03  HCM22-NISSU-S   PIC X(1)        OCCURS  3 TIMES.
            02  HCM22-GOKEI.
              03  HCM22-GOKEI-S   PIC X(1)        OCCURS  7 TIMES.
            02  HCM22-ENDYMD.
              03  HCM22-ENDYMD-S  PIC X(1)        OCCURS  6 TIMES.
            02  HCM22-STRYMD.
              03  HCM22-STRYMD-S  PIC X(1)        OCCURS  6 TIMES.
            02  HCM22-KOFUNUM.
              03  HCM22-KOFUNUM-S PIC X(1)        OCCURS  14 TIMES.
            02  HCM22-RENNUM    PIC X(7).

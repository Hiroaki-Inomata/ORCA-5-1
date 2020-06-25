        01  HCM21.
            02  HCM21-TANJYOG   PIC X(2)        OCCURS  3 TIMES.
            02  HCM21-NENREI    PIC X(3).
            02  HCM21-KANANAME  PIC X(30).
            02  HCM21-NAME  PIC X(26).
            02  HCM21-SYOBYOMEI PIC X(44)       OCCURS  5 TIMES.
            02  HCM21-KEIKA PIC X(54)   OCCURS  8 TIMES.
            02  HCM21-TEKIYO1   PIC X(28)       OCCURS  9 TIMES.
            02  HCM21-SRYKBN    PIC X(2)        OCCURS  30 TIMES.
            02  HCM21-TEKIYO2   PIC X(52)       OCCURS  30 TIMES.
            02  HCM21-TOTALTEN  PIC X(6).
            02  HCM21-TOTALMONEY    PIC X(10).
            02  HCM21-SYOKEI    PIC X(7).
            02  HCM21-SAISINKAISU   PIC X(3).
            02  HCM21-SHIDOKAISU    PIC X(3).
            02  HCM21-KOUTEN    PIC X(7)        OCCURS  35 TIMES.
            02  HCM21-KAISU PIC X(3)    OCCURS  35 TIMES.
            02  HCM21-KHNTEN    PIC X(7)        OCCURS  19 TIMES.
            02  HCM21-RSIIRYOCD PIC X(30).
            02  HCM21-HOSPNAME  PIC X(30).
            02  HCM21-SAIGAIKBN PIC X(8).
            02  HCM21-TANJYOY   PIC X(2).
            02  HCM21-TANJYOM   PIC X(2).
            02  HCM21-TANJYOD   PIC X(2).
            02  HCM21-SYOSHIN1  PIC X(2).
            02  HCM21-SYOSHIN2  PIC X(2).
            02  HCM21-SYOSHIN3  PIC X(2).
            02  HCM21-KEIZOKUKANRI  PIC X(16).
            02  HCM21-KINGAKU   PIC X(7)        OCCURS  6 TIMES.
            02  HCM21-TENKI PIC X(1).
            02  HCM21-SINKEI    PIC X(1).
            02  HCM21-PTNUM PIC X(20).
            02  HCM21-KOFUNUM.
              03  HCM21-KOFUNUM-S PIC X(1)        OCCURS  9 TIMES.
            02  HCM21-RYOSTYMD.
              03  HCM21-RYOSTYMD-S    PIC X(1)    OCCURS  6 TIMES.
            02  HCM21-RYOEDYMD.
              03  HCM21-RYOEDYMD-S    PIC X(1)    OCCURS  6 TIMES.
            02  HCM21-NISSU.
              03  HCM21-NISSU-S   PIC X(1)        OCCURS  3 TIMES.
            02  HCM21-GOKEI.
              03  HCM21-GOKEI-S   PIC X(1)        OCCURS  7 TIMES.
            02  HCM21-RENNUM    PIC X(7).

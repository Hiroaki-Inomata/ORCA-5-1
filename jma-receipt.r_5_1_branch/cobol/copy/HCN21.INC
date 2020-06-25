        01  HCN21.
            02  HCN21-SAIGAIKBN PIC X(8).
            02  HCN21-PTNUM PIC X(20).
            02  HCN21-RSIIRYOCD PIC X(30).
            02  HCN21-HOSPNAME  PIC X(60).
            02  HCN21-RENNUM    PIC X(7).
            02  HCN21-SINKEI    PIC X(1).
            02  HCN21-TENKI PIC X(1).
            02  HCN21-KOFUNUM.
              03  HCN21-KOFUNUM-S PIC X(1)        OCCURS  9 TIMES.
            02  HCN21-RYOSTYMD.
              03  HCN21-RYOSTYMD-S    PIC X(1)    OCCURS  6 TIMES.
            02  HCN21-RYOEDYMD.
              03  HCN21-RYOEDYMD-S    PIC X(1)    OCCURS  6 TIMES.
            02  HCN21-NISSU.
              03  HCN21-NISSU-S   PIC X(1)        OCCURS  3 TIMES.
            02  HCN21-GOKEI.
              03  HCN21-GOKEI-S   PIC X(1)        OCCURS  7 TIMES.
            02  HCN21-KANANAME  PIC X(30).
            02  HCN21-NAME  PIC X(26).
            02  HCN21-NENREI    PIC X(3).
            02  HCN21-TANJYOG   PIC X(2)        OCCURS  3 TIMES.
            02  HCN21-TANJYOYMD.
              03  HCN21-TANJYOY   PIC X(2).
              03  HCN21-TANJYOM   PIC X(2).
              03  HCN21-TANJYOD   PIC X(2).
            02  HCN21-SYOBYOMEI PIC X(44)       OCCURS  5 TIMES.
            02  HCN21-KEIKA PIC X(54)   OCCURS  8 TIMES.
            02  HCN21-SYOSHIN1  PIC X(2).
            02  HCN21-SYOSHIN2  PIC X(2).
            02  HCN21-SYOSHIN3  PIC X(2).
            02  HCN21-KAISU PIC X(3)    OCCURS  22 TIMES.
            02  HCN21-TENSU PIC X(7)    OCCURS  22 TIMES.
            02  HCN21-NYUINYMD.
              03  HCN21-NYUINYY   PIC X(2).
              03  HCN21-NYUINMM   PIC X(2).
              03  HCN21-NYUINDD   PIC X(2).
            02  HCN21-HOSPSBT   PIC X(2)        OCCURS  3 TIMES.
            02  HCN21-NYUSYUBETSU   PIC X(10)   OCCURS  11 TIMES.
            02  HCN21-NYUKHNTEN PIC X(7)        OCCURS  9 TIMES.
            02  HCN21-NYUKHNTENN    PIC X(10)   OCCURS  9 TIMES.
            02  HCN21-NYUKAISU  PIC X(3)        OCCURS  9 TIMES.
            02  HCN21-NYUTENSU  PIC X(7)        OCCURS  9 TIMES.
            02  HCN21-NYUTENSUN-G       OCCURS  9 TIMES.
              03  HCN21-NYUTENSUN PIC X(2)        OCCURS  6 TIMES.
            02  HCN21-KAKERU    PIC X(2)        OCCURS  5 TIMES.
            02  HCN21-NICHI PIC X(4)    OCCURS  5 TIMES.
            02  HCN21-TOTALTEN  PIC X(7).
            02  HCN21-TOTALMONEY    PIC X(10).
            02  HCN21-SONOTA    PIC X(12)       OCCURS  4 TIMES.
            02  HCN21-KINGAKU   PIC X(7)        OCCURS  5 TIMES.
            02  HCN21-SYOKEI    PIC X(7).
            02  HCN21-SYOKJN    PIC X(48).
            02  HCN21-SYOKBN    PIC X(2)        OCCURS  4 TIMES.
            02  HCN21-SYOKHNTEN PIC X(7)        OCCURS  4 TIMES.
            02  HCN21-SYOKAKERU PIC X(4).
            02  HCN21-SYOKAISU  PIC X(3)        OCCURS  4 TIMES.
            02  HCN21-SYONICHI  PIC X(4).
            02  HCN21-SYONISSU  PIC X(2).
            02  HCN21-TEKIYO1   PIC X(28)       OCCURS  7 TIMES.
            02  HCN21-BIKOU PIC X(28)   OCCURS  5 TIMES.
            02  HCN21-SYOMONEY  PIC X(10).
            02  HCN21-SRYKBN    PIC X(2)        OCCURS  22 TIMES.
            02  HCN21-TEKIYO2   PIC X(52)       OCCURS  22 TIMES.

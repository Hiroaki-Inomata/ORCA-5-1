        01  HCN22.
            02  HCN22-SAIGAIKBN PIC X(8).
            02  HCN22-PTNUM PIC X(20).
            02  HCN22-RSIIRYOCD PIC X(30).
            02  HCN22-HOSPNAME  PIC X(60).
            02  HCN22-RENNUM    PIC X(7).
            02  HCN22-SINKEI    PIC X(1).
            02  HCN22-TENKI PIC X(1).
            02  HCN22-KOFUNUM.
              03  HCN22-KOFUNUM-S PIC X(1)        OCCURS  14 TIMES.
            02  HCN22-TANJYOYMD.
              03  HCN22-TANJYOYMD-S   PIC X(1)    OCCURS  7 TIMES.
            02  HCN22-SYOBYOYMD.
              03  HCN22-SYOBYOYMD-S   PIC X(1)    OCCURS  6 TIMES.
            02  HCN22-STRYMD.
              03  HCN22-STRYMD-S  PIC X(1)        OCCURS  6 TIMES.
            02  HCN22-ENDYMD.
              03  HCN22-ENDYMD-S  PIC X(1)        OCCURS  6 TIMES.
            02  HCN22-NISSU.
              03  HCN22-NISSU-S   PIC X(1)        OCCURS  3 TIMES.
            02  HCN22-GOKEI.
              03  HCN22-GOKEI-S   PIC X(1)        OCCURS  7 TIMES.
            02  HCN22-KANANAME  PIC X(30).
            02  HCN22-NAME  PIC X(26).
            02  HCN22-NENREI    PIC X(3).
            02  HCN22-JIGYOUNAME    PIC X(80).
            02  HCN22-TODOFUKEN PIC X(6).
            02  HCN22-TODOFUKENKBN1 PIC X(2).
            02  HCN22-TODOFUKENKBN2 PIC X(2).
            02  HCN22-TODOFUKENKBN3 PIC X(2).
            02  HCN22-TODOFUKENKBN4 PIC X(2).
            02  HCN22-GUNSIKU   PIC X(20).
            02  HCN22-GUNSIKUKBN1   PIC X(2).
            02  HCN22-GUNSIKUKBN2   PIC X(2).
            02  HCN22-GUNSIKUKBN3   PIC X(2).
            02  HCN22-SYOBYOMEI PIC X(44)       OCCURS  3 TIMES.
            02  HCN22-KEIKA PIC X(54)   OCCURS  4 TIMES.
            02  HCN22-SYOSHIN1  PIC X(2).
            02  HCN22-SYOSHIN2  PIC X(2).
            02  HCN22-SYOSHIN3  PIC X(2).
            02  HCN22-KAISU PIC X(3)    OCCURS  22 TIMES.
            02  HCN22-TENSU PIC X(7)    OCCURS  22 TIMES.
            02  HCN22-NYUINYMD.
              03  HCN22-NYUINYY   PIC X(2).
              03  HCN22-NYUINMM   PIC X(2).
              03  HCN22-NYUINDD   PIC X(2).
            02  HCN22-HOSPSBT   PIC X(2)        OCCURS  3 TIMES.
            02  HCN22-NYUSYUBETSU   PIC X(10)   OCCURS  11 TIMES.
            02  HCN22-NYUKHNTEN PIC X(7)        OCCURS  9 TIMES.
            02  HCN22-NYUKHNTENN    PIC X(10)   OCCURS  9 TIMES.
            02  HCN22-NYUKAISU  PIC X(3)        OCCURS  9 TIMES.
            02  HCN22-NYUTENSU  PIC X(7)        OCCURS  9 TIMES.
            02  HCN22-NYUTENSUN-G       OCCURS  9 TIMES.
              03  HCN22-NYUTENSUN PIC X(2)        OCCURS  6 TIMES.
            02  HCN22-KAKERU    PIC X(2)        OCCURS  4 TIMES.
            02  HCN22-NICHI PIC X(4)    OCCURS  4 TIMES.
            02  HCN22-TOTALTEN  PIC X(7).
            02  HCN22-TOTALMONEY    PIC X(10).
            02  HCN22-SONOTA    PIC X(12)       OCCURS  4 TIMES.
            02  HCN22-KINGAKU   PIC X(7)        OCCURS  5 TIMES.
            02  HCN22-SYOKEI    PIC X(7).
            02  HCN22-SYOKJN    PIC X(48).
            02  HCN22-SYOKBN    PIC X(2)        OCCURS  4 TIMES.
            02  HCN22-SYOKAKERU PIC X(4).
            02  HCN22-SYOKHNTEN PIC X(7)        OCCURS  4 TIMES.
            02  HCN22-SYONICHI  PIC X(4).
            02  HCN22-SYOKAISU  PIC X(3)        OCCURS  4 TIMES.
            02  HCN22-SYONISSU  PIC X(2).
            02  HCN22-TEKIYO1   PIC X(28)       OCCURS  7 TIMES.
            02  HCN22-BIKOU PIC X(28)   OCCURS  5 TIMES.
            02  HCN22-SYOMONEY  PIC X(10).
            02  HCN22-SRYKBN    PIC X(2)        OCCURS  22 TIMES.
            02  HCN22-TEKIYO2   PIC X(52)       OCCURS  22 TIMES.

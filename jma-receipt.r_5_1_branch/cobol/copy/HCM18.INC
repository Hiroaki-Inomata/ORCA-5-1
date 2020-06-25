        01  HCM18.
            02  HCM18-KANANAME  PIC X(30).
            02  HCM18-NAME  PIC X(26).
            02  HCM18-HOSPNAME  PIC X(30).
            02  HCM18-BYOSYOSU  PIC X(4).
            02  HCM18-TOTALTEN  PIC X(6).
            02  HCM18-TOTALMONEY    PIC X(10).
            02  HCM18-KOFUNUM.
              03  HCM18-KOFUNUM-S PIC X(1)        OCCURS  13 TIMES.
            02  HCM18-SRYYMD.
              03  HCM18-SRYYMD-S  PIC X(1)        OCCURS  7 TIMES.
            02  HCM18-KENSAYMD.
              03  HCM18-KENSAYMD-S    PIC X(1)    OCCURS  7 TIMES.
            02  HCM18-GOKEI.
              03  HCM18-GOKEI-S   PIC X(1)        OCCURS  7 TIMES.
            02  HCM18-KEIKA-G           OCCURS  3 TIMES.
              03  HCM18-KEIKA PIC X(52).
            02  HCM18-ZENKAIY   PIC X(2).
            02  HCM18-ZENKAIM   PIC X(2).
            02  HCM18-ZENKAID   PIC X(2).
            02  HCM18-SYOSHINY  PIC X(2).
            02  HCM18-SYOSHINM  PIC X(2).
            02  HCM18-SYOSHIND  PIC X(2).
            02  HCM18-SAISHINY  PIC X(2).
            02  HCM18-SAISHINM  PIC X(2).
            02  HCM18-SAISHIND  PIC X(2).
            02  HCM18-JIKANGAI1 PIC X(2).
            02  HCM18-JIKANGAI2 PIC X(2).
            02  HCM18-JIKANGAI3 PIC X(2).
            02  HCM18-KAISU-G           OCCURS  32 TIMES.
              03  HCM18-KAISU PIC X(3).
            02  HCM18-KOUTEN-G          OCCURS  32 TIMES.
              03  HCM18-KOUTEN    PIC X(7).
            02  HCM18-SRYKBN-G          OCCURS  30 TIMES.
              03  HCM18-SRYKBN    PIC X(4).
            02  HCM18-TEKIYO-G          OCCURS  30 TIMES.
              03  HCM18-TEKIYO    PIC X(52).
            02  HCM18-SYOBYOEDA PIC X(1).
            02  HCM18-SYOBYO    PIC X(2).
            02  HCM18-SYOSHINMONEY  PIC X(7).
            02  HCM18-SAISHINMONEY  PIC X(7).
            02  HCM18-SYOKEI    PIC X(7).
            02  HCM18-PTNUM PIC X(20).
            02  HCM18-JIKANGAI11    PIC X(4).
            02  HCM18-JIKANGAI21    PIC X(4).
            02  HCM18-JIKANGAI31    PIC X(4).
            02  HCM18-RENNUM    PIC X(7).

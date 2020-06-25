        01  HCM71.
            02  HCM71-KANANAME  PIC X(50).
            02  HCM71-NAME  PIC X(50).
            02  HCM71-HOSPNAME  PIC X(90).
            02  HCM71-BYOSYOSU  PIC X(4).
            02  HCM71-TOTALTEN  PIC X(6).
            02  HCM71-TOTALMONEY    PIC X(10).
            02  HCM71-KOFUNUM.
              03  HCM71-KOFUNUM-S
                      PIC X(1)    OCCURS  13 TIMES.
            02  HCM71-SRYYMD.
              03  HCM71-SRYYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM71-KENSAYMD.
              03  HCM71-KENSAYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM71-GOKEI.
              03  HCM71-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM71-KEIKA-G           OCCURS  3 TIMES.
              03  HCM71-KEIKA PIC X(52).
            02  HCM71-ZENKAIY   PIC X(2).
            02  HCM71-ZENKAIM   PIC X(2).
            02  HCM71-ZENKAID   PIC X(2).
            02  HCM71-SYOSHINY  PIC X(2).
            02  HCM71-SYOSHINM  PIC X(2).
            02  HCM71-SYOSHIND  PIC X(2).
            02  HCM71-SAISHINY  PIC X(2).
            02  HCM71-SAISHINM  PIC X(2).
            02  HCM71-SAISHIND  PIC X(2).
            02  HCM71-JIKANGAI1 PIC X(2).
            02  HCM71-JIKANGAI2 PIC X(2).
            02  HCM71-JIKANGAI3 PIC X(2).
            02  HCM71-KAISU-G           OCCURS  32 TIMES.
              03  HCM71-KAISU PIC X(3).
            02  HCM71-KOUTEN-G          OCCURS  32 TIMES.
              03  HCM71-KOUTEN    PIC X(7).
            02  HCM71-SRYKBN-G          OCCURS  30 TIMES.
              03  HCM71-SRYKBN    PIC X(4).
            02  HCM71-TEKIYO-G          OCCURS  30 TIMES.
              03  HCM71-TEKIYO    PIC X(52).
            02  HCM71-SYOBYOEDA PIC X(1).
            02  HCM71-SYOBYO    PIC X(2).
            02  HCM71-SYOSHINMONEY  PIC X(7).
            02  HCM71-SAISHINMONEY  PIC X(7).
            02  HCM71-SYOKEI    PIC X(7).
            02  HCM71-PTNUM PIC X(20).
            02  HCM71-JIKANGAI11    PIC X(4).
            02  HCM71-JIKANGAI21    PIC X(4).
            02  HCM71-JIKANGAI31    PIC X(4).
            02  HCM71-RENNUM    PIC X(7).
            02  HCM71-PAGE  PIC X(5).
            02  HCM71-LABEL PIC X(14).
            02  HCM71-LOPTION   PIC X(100).

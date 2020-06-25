        01  HCM18V02.
            02  HCM18V02-KANANAME   PIC X(50).
            02  HCM18V02-NAME   PIC X(50).
            02  HCM18V02-HOSPNAME   PIC X(60).
            02  HCM18V02-BYOSYOSU   PIC X(4).
            02  HCM18V02-TOTALTEN   PIC X(6).
            02  HCM18V02-TOTALMONEY PIC X(10).
            02  HCM18V02-KOFUNUM.
              03  HCM18V02-KOFUNUM-S
                      PIC X(1)    OCCURS  13 TIMES.
            02  HCM18V02-SRYYMD.
              03  HCM18V02-SRYYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V02-KENSAYMD.
              03  HCM18V02-KENSAYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V02-GOKEI.
              03  HCM18V02-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V02-KEIKA-G        OCCURS  3 TIMES.
              03  HCM18V02-KEIKA  PIC X(52).
            02  HCM18V02-ZENKAIY    PIC X(2).
            02  HCM18V02-ZENKAIM    PIC X(2).
            02  HCM18V02-ZENKAID    PIC X(2).
            02  HCM18V02-SYOSHINY   PIC X(2).
            02  HCM18V02-SYOSHINM   PIC X(2).
            02  HCM18V02-SYOSHIND   PIC X(2).
            02  HCM18V02-SAISHINY   PIC X(2).
            02  HCM18V02-SAISHINM   PIC X(2).
            02  HCM18V02-SAISHIND   PIC X(2).
            02  HCM18V02-JIKANGAI1  PIC X(2).
            02  HCM18V02-JIKANGAI2  PIC X(2).
            02  HCM18V02-JIKANGAI3  PIC X(2).
            02  HCM18V02-KAISU-G        OCCURS  32 TIMES.
              03  HCM18V02-KAISU  PIC X(3).
            02  HCM18V02-KOUTEN-G       OCCURS  32 TIMES.
              03  HCM18V02-KOUTEN PIC X(7).
            02  HCM18V02-SRYKBN-G       OCCURS  30 TIMES.
              03  HCM18V02-SRYKBN PIC X(4).
            02  HCM18V02-TEKIYO-G       OCCURS  30 TIMES.
              03  HCM18V02-TEKIYO PIC X(52).
            02  HCM18V02-SYOBYOEDA  PIC X(1).
            02  HCM18V02-SYOBYO PIC X(2).
            02  HCM18V02-SYOSHINMONEY   PIC X(7).
            02  HCM18V02-SAISHINMONEY   PIC X(7).
            02  HCM18V02-SYOKEI PIC X(7).
            02  HCM18V02-PTNUM  PIC X(20).
            02  HCM18V02-JIKANGAI11 PIC X(4).
            02  HCM18V02-JIKANGAI21 PIC X(4).
            02  HCM18V02-JIKANGAI31 PIC X(4).
            02  HCM18V02-RENNUM PIC X(7).

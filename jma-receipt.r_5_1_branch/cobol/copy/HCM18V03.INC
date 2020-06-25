        01  HCM18V03.
            02  HCM18V03-KANANAME   PIC X(50).
            02  HCM18V03-NAME   PIC X(50).
            02  HCM18V03-HOSPNAME   PIC X(90).
            02  HCM18V03-BYOSYOSU   PIC X(4).
            02  HCM18V03-TOTALTEN   PIC X(6).
            02  HCM18V03-TOTALMONEY PIC X(10).
            02  HCM18V03-KOFUNUM.
              03  HCM18V03-KOFUNUM-S
                      PIC X(1)    OCCURS  13 TIMES.
            02  HCM18V03-SRYYMD.
              03  HCM18V03-SRYYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V03-KENSAYMD.
              03  HCM18V03-KENSAYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V03-GOKEI.
              03  HCM18V03-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V03-KEIKA-G        OCCURS  3 TIMES.
              03  HCM18V03-KEIKA  PIC X(52).
            02  HCM18V03-ZENKAIY    PIC X(2).
            02  HCM18V03-ZENKAIM    PIC X(2).
            02  HCM18V03-ZENKAID    PIC X(2).
            02  HCM18V03-SYOSHINY   PIC X(2).
            02  HCM18V03-SYOSHINM   PIC X(2).
            02  HCM18V03-SYOSHIND   PIC X(2).
            02  HCM18V03-SAISHINY   PIC X(2).
            02  HCM18V03-SAISHINM   PIC X(2).
            02  HCM18V03-SAISHIND   PIC X(2).
            02  HCM18V03-JIKANGAI1  PIC X(2).
            02  HCM18V03-JIKANGAI2  PIC X(2).
            02  HCM18V03-JIKANGAI3  PIC X(2).
            02  HCM18V03-KAISU-G        OCCURS  32 TIMES.
              03  HCM18V03-KAISU  PIC X(3).
            02  HCM18V03-KOUTEN-G       OCCURS  32 TIMES.
              03  HCM18V03-KOUTEN PIC X(7).
            02  HCM18V03-SRYKBN-G       OCCURS  30 TIMES.
              03  HCM18V03-SRYKBN PIC X(4).
            02  HCM18V03-TEKIYO-G       OCCURS  30 TIMES.
              03  HCM18V03-TEKIYO PIC X(52).
            02  HCM18V03-SYOBYOEDA  PIC X(1).
            02  HCM18V03-SYOBYO PIC X(2).
            02  HCM18V03-SYOSHINMONEY   PIC X(7).
            02  HCM18V03-SAISHINMONEY   PIC X(7).
            02  HCM18V03-SYOKEI PIC X(7).
            02  HCM18V03-PTNUM  PIC X(20).
            02  HCM18V03-JIKANGAI11 PIC X(4).
            02  HCM18V03-JIKANGAI21 PIC X(4).
            02  HCM18V03-JIKANGAI31 PIC X(4).
            02  HCM18V03-RENNUM PIC X(7).

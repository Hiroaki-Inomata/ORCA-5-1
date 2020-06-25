        01  HCM18V05.
            02  HCM18V05-KANANAME   PIC X(50).
            02  HCM18V05-NAME   PIC X(50).
            02  HCM18V05-HOSPNAME   PIC X(90).
            02  HCM18V05-BYOSYOSU   PIC X(4).
            02  HCM18V05-TOTALTEN   PIC X(6).
            02  HCM18V05-TOTALMONEY PIC X(10).
            02  HCM18V05-KOFUNUM.
              03  HCM18V05-KOFUNUM-S
                      PIC X(1)    OCCURS  13 TIMES.
            02  HCM18V05-SRYYMD.
              03  HCM18V05-SRYYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V05-KENSAYMD.
              03  HCM18V05-KENSAYMD-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V05-GOKEI.
              03  HCM18V05-GOKEI-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM18V05-KEIKA-G        OCCURS  3 TIMES.
              03  HCM18V05-KEIKA  PIC X(52).
            02  HCM18V05-ZENKAIY    PIC X(2).
            02  HCM18V05-ZENKAIM    PIC X(2).
            02  HCM18V05-ZENKAID    PIC X(2).
            02  HCM18V05-SYOSHINY   PIC X(2).
            02  HCM18V05-SYOSHINM   PIC X(2).
            02  HCM18V05-SYOSHIND   PIC X(2).
            02  HCM18V05-SAISHINY   PIC X(2).
            02  HCM18V05-SAISHINM   PIC X(2).
            02  HCM18V05-SAISHIND   PIC X(2).
            02  HCM18V05-JIKANGAI1  PIC X(2).
            02  HCM18V05-JIKANGAI2  PIC X(2).
            02  HCM18V05-JIKANGAI3  PIC X(2).
            02  HCM18V05-KAISU-G        OCCURS  32 TIMES.
              03  HCM18V05-KAISU  PIC X(3).
            02  HCM18V05-KOUTEN-G       OCCURS  32 TIMES.
              03  HCM18V05-KOUTEN PIC X(7).
            02  HCM18V05-SRYKBN-G       OCCURS  30 TIMES.
              03  HCM18V05-SRYKBN PIC X(4).
            02  HCM18V05-TEKIYO-G       OCCURS  30 TIMES.
              03  HCM18V05-TEKIYO PIC X(52).
            02  HCM18V05-SYOBYOEDA  PIC X(1).
            02  HCM18V05-SYOBYO PIC X(2).
            02  HCM18V05-SYOSHINMONEY   PIC X(7).
            02  HCM18V05-SAISHINMONEY   PIC X(7).
            02  HCM18V05-SYOKEI PIC X(7).
            02  HCM18V05-PTNUM  PIC X(20).
            02  HCM18V05-JIKANGAI11 PIC X(4).
            02  HCM18V05-JIKANGAI21 PIC X(4).
            02  HCM18V05-JIKANGAI31 PIC X(4).
            02  HCM18V05-RENNUM PIC X(7).
            02  HCM18V05-PAGE   PIC X(5).
            02  HCM18V05-LABEL  PIC X(14).

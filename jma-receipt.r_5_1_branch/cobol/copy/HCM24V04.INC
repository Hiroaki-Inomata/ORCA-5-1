        01  HCM24V04.
            02  HCM24V04-PTKANANAME PIC X(50).
            02  HCM24V04-PTNAME PIC X(50).
            02  HCM24V04-HOSPNAME   PIC X(90).
            02  HCM24V04-HOSPNUM    PIC X(30).
            02  HCM24V04-PTAGE  PIC X(3).
            02  HCM24V04-TEKIYO
                    PIC X(52)   OCCURS  118 TIMES.
            02  HCM24V04-SRYKBN
                    PIC X(2)    OCCURS  118 TIMES.
            02  HCM24V04-SAIGAIKBN  PIC X(8).
            02  HCM24V04-PTNUM  PIC X(30).
            02  HCM24V04-NENNUM.
              03  HCM24V04-NENNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCM24V04-HKNNUM.
              03  HCM24V04-HKNNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM24V04-RENNUM PIC X(7).
            02  HCM24V04-PAGE   PIC X(5).

        01  HCM24V03.
            02  HCM24V03-PTKANANAME PIC X(50).
            02  HCM24V03-PTNAME PIC X(50).
            02  HCM24V03-HOSPNAME   PIC X(90).
            02  HCM24V03-HOSPNUM    PIC X(30).
            02  HCM24V03-PTAGE  PIC X(3).
            02  HCM24V03-TEKIYO
                    PIC X(52)   OCCURS  118 TIMES.
            02  HCM24V03-SRYKBN
                    PIC X(2)    OCCURS  118 TIMES.
            02  HCM24V03-SAIGAIKBN  PIC X(8).
            02  HCM24V03-PTNUM  PIC X(30).
            02  HCM24V03-NENNUM.
              03  HCM24V03-NENNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCM24V03-HKNNUM.
              03  HCM24V03-HKNNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM24V03-RENNUM PIC X(7).

        01  HCM24V02.
            02  HCM24V02-PTKANANAME PIC X(50).
            02  HCM24V02-PTNAME PIC X(50).
            02  HCM24V02-HOSPNAME   PIC X(60).
            02  HCM24V02-HOSPNUM    PIC X(30).
            02  HCM24V02-PTAGE  PIC X(3).
            02  HCM24V02-TEKIYO
                    PIC X(52)   OCCURS  118 TIMES.
            02  HCM24V02-SRYKBN
                    PIC X(2)    OCCURS  118 TIMES.
            02  HCM24V02-SAIGAIKBN  PIC X(8).
            02  HCM24V02-PTNUM  PIC X(30).
            02  HCM24V02-NENNUM.
              03  HCM24V02-NENNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCM24V02-HKNNUM.
              03  HCM24V02-HKNNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM24V02-RENNUM PIC X(7).

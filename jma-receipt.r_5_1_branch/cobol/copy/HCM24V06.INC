        01  HCM24V06.
            02  HCM24V06-PTKANANAME PIC X(50).
            02  HCM24V06-PTNAME PIC X(50).
            02  HCM24V06-HOSPNAME   PIC X(90).
            02  HCM24V06-HOSPNUM    PIC X(30).
            02  HCM24V06-PTAGE  PIC X(3).
            02  HCM24V06-TEKIYO
                    PIC X(52)   OCCURS  118 TIMES.
            02  HCM24V06-SRYKBN
                    PIC X(2)    OCCURS  118 TIMES.
            02  HCM24V06-SAIGAIKBN  PIC X(8).
            02  HCM24V06-PTNUM  PIC X(30).
            02  HCM24V06-NENNUM.
              03  HCM24V06-NENNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCM24V06-HKNNUM.
              03  HCM24V06-HKNNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM24V06-RENNUM PIC X(7).
            02  HCM24V06-PAGE   PIC X(5).
            02  HCM24V06-LABEL  PIC X(14).
            02  HCM24V06-LOPTION    PIC X(100).

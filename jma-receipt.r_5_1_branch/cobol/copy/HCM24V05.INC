        01  HCM24V05.
            02  HCM24V05-PTKANANAME PIC X(50).
            02  HCM24V05-PTNAME PIC X(50).
            02  HCM24V05-HOSPNAME   PIC X(90).
            02  HCM24V05-HOSPNUM    PIC X(30).
            02  HCM24V05-PTAGE  PIC X(3).
            02  HCM24V05-TEKIYO
                    PIC X(52)   OCCURS  118 TIMES.
            02  HCM24V05-SRYKBN
                    PIC X(2)    OCCURS  118 TIMES.
            02  HCM24V05-SAIGAIKBN  PIC X(8).
            02  HCM24V05-PTNUM  PIC X(30).
            02  HCM24V05-NENNUM.
              03  HCM24V05-NENNUM-S
                      PIC X(1)    OCCURS  9 TIMES.
            02  HCM24V05-HKNNUM.
              03  HCM24V05-HKNNUM-S
                      PIC X(1)    OCCURS  14 TIMES.
            02  HCM24V05-RENNUM PIC X(7).
            02  HCM24V05-PAGE   PIC X(5).
            02  HCM24V05-LABEL  PIC X(14).

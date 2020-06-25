        01  HCM24.
            02  HCM24-PTKANANAME    PIC X(30).
            02  HCM24-PTNAME    PIC X(26).
            02  HCM24-HOSPNAME  PIC X(30).
            02  HCM24-HOSPNUM   PIC X(30).
            02  HCM24-PTAGE PIC X(3).
            02  HCM24-TEKIYO    PIC X(52)       OCCURS  118 TIMES.
            02  HCM24-SRYKBN    PIC X(2)        OCCURS  118 TIMES.
            02  HCM24-SAIGAIKBN PIC X(8).
            02  HCM24-PTNUM PIC X(30).
            02  HCM24-NENNUM.
              03  HCM24-NENNUM-S  PIC X(1)        OCCURS  9 TIMES.
            02  HCM24-HKNNUM.
              03  HCM24-HKNNUM-S  PIC X(1)        OCCURS  14 TIMES.
            02  HCM24-RENNUM    PIC X(7).

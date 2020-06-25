        01  HCM32V01.
            02  HCM32V01-STRSRYYMD  PIC X(22).
            02  HCM32V01-ENDSRYYMD  PIC X(22).
            02  HCM32V01-PTNUM  PIC X(20).
            02  HCM32V01-PTNAME PIC X(20).
            02  HCM32V01-HAKKOYMD   PIC X(22).
            02  HCM32V01-SRYYYMM
                    PIC X(16)   OCCURS  12 TIMES.
            02  HCM32V01-LABELYM    PIC X(10).
            02  HCM32V01-HKNTEN
                    PIC X(7)    OCCURS  12 TIMES.
            02  HCM32V01-JIHIMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32V01-SKYMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32V01-BUN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32V01-TEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32V01-JYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32V01-YEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32V01-GHKNTEN    PIC X(7).
            02  HCM32V01-GJIHIMONEY PIC X(11).
            02  HCM32V01-GSKYMONEY  PIC X(11).
            02  HCM32V01-HOSPADRS   PIC X(60).
            02  HCM32V01-HOSPADRS2  PIC X(100).
            02  HCM32V01-HOSPNAME   PIC X(60).
            02  HCM32V01-HOSPTEL    PIC X(15).

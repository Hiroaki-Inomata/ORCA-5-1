        01  HCM32.
            02  HCM32-STRSRYYMD PIC X(22).
            02  HCM32-ENDSRYYMD PIC X(22).
            02  HCM32-PTNUM PIC X(20).
            02  HCM32-PTNAME    PIC X(20).
            02  HCM32-HAKKOYMD  PIC X(22).
            02  HCM32-SRYYYMM
                    PIC X(16)   OCCURS  12 TIMES.
            02  HCM32-LABELYM   PIC X(10).
            02  HCM32-HKNTEN
                    PIC X(7)    OCCURS  12 TIMES.
            02  HCM32-JIHIMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32-SKYMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32-BUN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32-TEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32-JYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32-YEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32-GHKNTEN   PIC X(7).
            02  HCM32-GJIHIMONEY    PIC X(11).
            02  HCM32-GSKYMONEY PIC X(11).
            02  HCM32-HOSPADRS  PIC X(60).
            02  HCM32-HOSPNAME  PIC X(40).
            02  HCM32-HOSPTEL   PIC X(15).

        01  HCM32N.
            02  HCM32N-STRSRYYMD    PIC X(22).
            02  HCM32N-ENDSRYYMD    PIC X(22).
            02  HCM32N-PTNUM    PIC X(20).
            02  HCM32N-PTNAME   PIC X(20).
            02  HCM32N-HAKKOYMD PIC X(22).
            02  HCM32N-SRYYYMM
                    PIC X(6)    OCCURS  12 TIMES.
            02  HCM32N-LABELYM  PIC X(10).
            02  HCM32N-HKNTEN
                    PIC X(9)    OCCURS  12 TIMES.
            02  HCM32N-FTNMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32N-SKJMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32N-JIHIMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32N-SKYMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32N-TEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32N-HYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32N-SYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32N-JYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32N-YEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32N-GHKNTEN  PIC X(9).
            02  HCM32N-GFTNMONEY    PIC X(11).
            02  HCM32N-GSKJMONEY    PIC X(11).
            02  HCM32N-GJIHIMONEY   PIC X(11).
            02  HCM32N-GSKYMONEY    PIC X(11).
            02  HCM32N-HOSPADRS PIC X(60).
            02  HCM32N-HOSPADRS2    PIC X(100).
            02  HCM32N-HOSPNAME PIC X(60).
            02  HCM32N-HOSPTEL  PIC X(15).
            02  HCM32N-VER040300        OCCURS  12 TIMES.
              03  HCM32N-SRYYY    PIC X(10).
              03  HCM32N-SRYMM    PIC X(10).
              03  HCM32N-BFNYUKINMSG  PIC X(20).
              03  HCM32N-BFNYUKIN PIC X(11).
              03  HCM32N-BFYEN    PIC X(2).
            02  HCM32N-PAGE PIC X(10).

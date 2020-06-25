        01  HCM32G.
            02  HCM32G-STRSRYYMD    PIC X(22).
            02  HCM32G-ENDSRYYMD    PIC X(22).
            02  HCM32G-PTNUM    PIC X(20).
            02  HCM32G-PTNAME   PIC X(20).
            02  HCM32G-HAKKOYMD PIC X(22).
            02  HCM32G-SRYYYMM
                    PIC X(6)    OCCURS  12 TIMES.
            02  HCM32G-LABELYM  PIC X(10).
            02  HCM32G-HKNTEN
                    PIC X(9)    OCCURS  12 TIMES.
            02  HCM32G-FTNMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32G-JIHIMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32G-SKYMONEY
                    PIC X(11)   OCCURS  12 TIMES.
            02  HCM32G-TEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32G-HYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32G-JYEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32G-YEN
                    PIC X(2)    OCCURS  12 TIMES.
            02  HCM32G-GHKNTEN  PIC X(9).
            02  HCM32G-GJIHIMONEY   PIC X(11).
            02  HCM32G-GSKYMONEY    PIC X(11).
            02  HCM32G-GFTNMONEY    PIC X(11).
            02  HCM32G-HOSPADRS PIC X(60).
            02  HCM32G-HOSPADRS2    PIC X(100).
            02  HCM32G-HOSPNAME PIC X(60).
            02  HCM32G-HOSPTEL  PIC X(15).
            02  HCM32G-VER040300        OCCURS  12 TIMES.
              03  HCM32G-SRYYY    PIC X(10).
              03  HCM32G-SRYMM    PIC X(10).
              03  HCM32G-BFNYUKINMSG  PIC X(20).
              03  HCM32G-BFNYUKIN PIC X(11).
              03  HCM32G-BFYEN    PIC X(2).
            02  HCM32G-PAGE PIC X(10).

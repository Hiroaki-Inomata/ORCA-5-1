        01  HCM31V01.
            02  HCM31V01-STRSRYYMD  PIC X(22).
            02  HCM31V01-ENDSRYYMD  PIC X(22).
            02  HCM31V01-PTNUM  PIC X(20).
            02  HCM31V01-PTNAME PIC X(20).
            02  HCM31V01-HAKKOYMD   PIC X(22).
            02  HCM31V01-PAGE   PIC X(3).
            02  HCM31V01-LABELYMD   PIC X(10).
            02  HCM31V01-SRYYMD
                    PIC X(12)   OCCURS  30 TIMES.
            02  HCM31V01-HKNTEN
                    PIC X(7)    OCCURS  30 TIMES.
            02  HCM31V01-TEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V01-JIHIMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31V01-JYEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V01-SKYMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31V01-YEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V01-GHKNTEN    PIC X(7).
            02  HCM31V01-GJIHIMONEY PIC X(11).
            02  HCM31V01-GSKYMONEY  PIC X(11).
            02  HCM31V01-HOSPADRS   PIC X(60).
            02  HCM31V01-HOSPADRS2  PIC X(100).
            02  HCM31V01-HOSPNAME   PIC X(60).
            02  HCM31V01-HOSPTEL    PIC X(15).

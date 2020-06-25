        01  HCM31.
            02  HCM31-STRSRYYMD PIC X(22).
            02  HCM31-ENDSRYYMD PIC X(22).
            02  HCM31-PTNUM PIC X(20).
            02  HCM31-PTNAME    PIC X(20).
            02  HCM31-HAKKOYMD  PIC X(22).
            02  HCM31-PAGE  PIC X(3).
            02  HCM31-LABELYMD  PIC X(10).
            02  HCM31-SRYYMD
                    PIC X(12)   OCCURS  30 TIMES.
            02  HCM31-HKNTEN
                    PIC X(7)    OCCURS  30 TIMES.
            02  HCM31-TEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31-JIHIMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31-JYEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31-SKYMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31-YEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31-GHKNTEN   PIC X(7).
            02  HCM31-GJIHIMONEY    PIC X(11).
            02  HCM31-GSKYMONEY PIC X(11).
            02  HCM31-HOSPADRS  PIC X(60).
            02  HCM31-HOSPNAME  PIC X(40).
            02  HCM31-HOSPTEL   PIC X(15).

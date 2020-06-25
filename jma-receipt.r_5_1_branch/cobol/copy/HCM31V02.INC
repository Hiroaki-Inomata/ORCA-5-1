        01  HCM31V02.
            02  HCM31V02-STRSRYYMD  PIC X(22).
            02  HCM31V02-ENDSRYYMD  PIC X(22).
            02  HCM31V02-PTNUM  PIC X(20).
            02  HCM31V02-PTNAME PIC X(20).
            02  HCM31V02-HAKKOYMD   PIC X(22).
            02  HCM31V02-PAGE   PIC X(3).
            02  HCM31V02-LABELYMD   PIC X(10).
            02  HCM31V02-SRYYMD
                    PIC X(12)   OCCURS  30 TIMES.
            02  HCM31V02-HKNTEN
                    PIC X(7)    OCCURS  30 TIMES.
            02  HCM31V02-TEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V02-JIHIMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31V02-JYEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V02-SKYMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31V02-YEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31V02-GHKNTEN    PIC X(7).
            02  HCM31V02-GJIHIMONEY PIC X(11).
            02  HCM31V02-GSKYMONEY  PIC X(11).
            02  HCM31V02-HOSPADRS   PIC X(60).
            02  HCM31V02-HOSPADRS2  PIC X(100).
            02  HCM31V02-HOSPNAME   PIC X(60).
            02  HCM31V02-HOSPTEL    PIC X(15).
            02  HCM31V02-NYUGAI PIC X(8).

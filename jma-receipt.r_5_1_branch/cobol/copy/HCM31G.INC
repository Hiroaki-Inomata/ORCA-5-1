        01  HCM31G.
            02  HCM31G-STRSRYYMD    PIC X(22).
            02  HCM31G-ENDSRYYMD    PIC X(22).
            02  HCM31G-PTNUM    PIC X(20).
            02  HCM31G-PTNAME   PIC X(20).
            02  HCM31G-HAKKOYMD PIC X(22).
            02  HCM31G-PAGE PIC X(3).
            02  HCM31G-LABELYMD PIC X(10).
            02  HCM31G-SRYYMD
                    PIC X(12)   OCCURS  30 TIMES.
            02  HCM31G-HKNTEN
                    PIC X(9)    OCCURS  30 TIMES.
            02  HCM31G-TEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31G-FTNMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31G-HYEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31G-JIHIMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31G-JYEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31G-SKYMONEY
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM31G-YEN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM31G-GHKNTEN  PIC X(9).
            02  HCM31G-GFTNMONEY    PIC X(11).
            02  HCM31G-GJIHIMONEY   PIC X(11).
            02  HCM31G-GSKYMONEY    PIC X(11).
            02  HCM31G-HOSPADRS PIC X(60).
            02  HCM31G-HOSPADRS2    PIC X(100).
            02  HCM31G-HOSPNAME PIC X(60).
            02  HCM31G-HOSPTEL  PIC X(15).
            02  HCM31G-NYUGAI   PIC X(8).
            02  HCM31G-VER040300        OCCURS  30 TIMES.
              03  HCM31G-SRYYY    PIC X(10).
              03  HCM31G-SRYMMDD  PIC X(16).
              03  HCM31G-NEW  PIC X(10).
              03  HCM31G-BFNYUKINMSG  PIC X(20).
              03  HCM31G-BFNYUKIN PIC X(11).
              03  HCM31G-BFYEN    PIC X(10).

        01  HCM36.
            02  HCM36-PTNAME    PIC X(20).
            02  HCM36-SHINRYOYM PIC X(16).
            02  HCM36-HOSPADRS  PIC X(100).
            02  HCM36-HOSPNAME  PIC X(50).
            02  HCM36-DRNAME    PIC X(30).
            02  HCM36-USETBL            OCCURS  21 TIMES.
              03  HCM36-USENAME   PIC X(62).
              03  HCM36-USETANI   PIC X(8).
              03  HCM36-USERYO    PIC X(2)        OCCURS  31 TIMES.
              03  HCM36-USETEN    PIC X(1)        OCCURS  31 TIMES.

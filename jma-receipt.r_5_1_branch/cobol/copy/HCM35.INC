        01  HCM35.
            02  HCM35-SHINRYOYM PIC X(16).
            02  HCM35-HOSPCD    PIC X(20).
            02  HCM35-PTNAME    PIC X(20).
            02  HCM35-USETBL            OCCURS  31 TIMES.
              03  HCM35-USENAME   PIC X(46).
              03  HCM35-USERYO-G          OCCURS  31 TIMES.
                04  HCM35-USERYO    PIC X(1)        OCCURS  4 TIMES.
              03  HCM35-USETEN-G          OCCURS  31 TIMES.
                04  HCM35-USETEN    PIC X(1)        OCCURS  3 TIMES.
            02  HCM35-KBN   PIC X(2)    OCCURS  4 TIMES.

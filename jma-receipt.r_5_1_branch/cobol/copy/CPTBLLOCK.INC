        01  TBL-LOCK.
            02  LOCK-HOSPNUM PIC 9(2).
            02  LOCK-TERMID  PIC X(64).
            02  LOCK-OPID    PIC X(16).
            02  LOCK-KARTE-UID  PIC X(36).
            02  LOCKTABLE.
              03  LOCK-LDNAME  PIC X(20)       OCCURS  50 TIMES.
              03  LOCK-PTID    PIC X(10)      OCCURS  50 TIMES.
              03  LOCK-SRYYM       PIC X(8)    OCCURS  50 TIMES.
              03  LOCK-STARTDATE   PIC X(8)    OCCURS  50 TIMES.
              03  LOCK-ENDDATE PIC X(8)        OCCURS  50 TIMES.
              03  LOCK-STARTTIME   PIC X(8)    OCCURS  50 TIMES.
              03  LOCK-ENDTIME PIC X(8)        OCCURS  50 TIMES.
              03  LOCK-FLG PIC X(1)   OCCURS  50 TIMES.

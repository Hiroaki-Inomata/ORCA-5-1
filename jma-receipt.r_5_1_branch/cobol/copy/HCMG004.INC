        01  HCMG004.
            02  HCMG004-KJNYMD  PIC X(22).
            02  HCMG004-SYORIYMD    PIC X(22).
            02  HCMG004-PAGE    PIC X(3).
            02  HCMG004-NAIYO-TBL       OCCURS  30 TIMES.
              03  HCMG004-BTUNAME PIC X(10).
              03  HCMG004-BRMNUM  PIC X(6).
              03  HCMG004-SRYKA   PIC X(8).
              03  HCMG004-PTNUM   PIC X(10).
              03  HCMG004-PTNUM2  PIC X(20).
              03  HCMG004-NAME    PIC X(30).
              03  HCMG004-SEX PIC X(2).
              03  HCMG004-BIRTHDAY    PIC X(9).
              03  HCMG004-NYUINYMD    PIC X(9).
              03  HCMG004-TUSAN   PIC X(9).
              03  HCMG004-DAY-91  PIC X(9).
              03  HCMG004-DAY-180 PIC X(9).
            02  HCMG004-TAIINYMD
                    PIC X(9)    OCCURS  30 TIMES.
            02  HCMG004-NO
                    PIC X(4)    OCCURS  30 TIMES.

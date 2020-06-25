        01  HCMG005.
            02  HCMG005-KJNYMD  PIC X(22).
            02  HCMG005-SYORIYMD    PIC X(22).
            02  HCMG005-PAGE    PIC X(3).
            02  HCMG005-NAIYO-TBL       OCCURS  30 TIMES.
              03  HCMG005-BTUNAME PIC X(10).
              03  HCMG005-BRMNUM  PIC X(6).
              03  HCMG005-SRYKA   PIC X(8).
              03  HCMG005-PTNUM   PIC X(10).
              03  HCMG005-PTNUM2  PIC X(20).
              03  HCMG005-NAME    PIC X(30).
              03  HCMG005-SEX PIC X(2).
              03  HCMG005-BIRTHDAY    PIC X(9).
              03  HCMG005-NYUINYMD    PIC X(9).
              03  HCMG005-AST-91  PIC X(2).
              03  HCMG005-DAY-91  PIC X(9).
              03  HCMG005-AST-180 PIC X(2).
              03  HCMG005-DAY-180 PIC X(9).
            02  HCMG005-TAIINYMD
                    PIC X(9)    OCCURS  30 TIMES.

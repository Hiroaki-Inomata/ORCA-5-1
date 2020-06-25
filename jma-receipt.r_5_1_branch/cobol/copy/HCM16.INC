        01  HCM16.
            02  HCM16-SRYYMD    PIC X(22).
            02  HCM16-SAKUSEI   PIC X(14).
            02  HCM16-PAGE  PIC X(3).
            02  HCM16-A-TBL     OCCURS  35 TIMES.
              03  HCM16-NUM   PIC X(4).
              03  HCM16-PTNUM PIC X(20).
              03  HCM16-NAME  PIC X(50).
              03  HCM16-SRYKA PIC X(20).
              03  HCM16-NAIYO PIC X(20).
              03  HCM16-DRNAME    PIC X(30).
              03  HCM16-UKETUKE   PIC X(5).
              03  HCM16-KAIKEI    PIC X(5).

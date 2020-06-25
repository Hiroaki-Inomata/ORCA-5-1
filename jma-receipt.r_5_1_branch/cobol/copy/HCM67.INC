        01  HCM67.
            02  HCM67-PTNUM PIC X(20).
            02  HCM67-NAME  PIC X(30).
            02  HCM67-PRTYMD    PIC X(22).
            02  HCM67-PAGE  PIC X(2).
            02  HCM67-NAIYO-TBL         OCCURS  12 TIMES.
              03  HCM67-YYKYMD    PIC X(22).
              03  HCM67-YYKTIME   PIC X(5).
              03  HCM67-SRYKA PIC X(10).
              03  HCM67-DRNAME    PIC X(18).
              03  HCM67-BIKOH PIC X(48).
            02  HCM67-MSG   PIC X(100).
            02  HCM67-HOSPADRS  PIC X(80).
            02  HCM67-HOSPNAME  PIC X(80).
            02  HCM67-HOSPTEL   PIC X(15).

        01  HCM20.
            02  HCM20-KANANAME  PIC X(30).
            02  HCM20-NAME  PIC X(26).
            02  HCM20-HOSPNAME  PIC X(30).
            02  HCM20-RISIRYOCD PIC X(30).
            02  HCM20-KOFUNUM   PIC X(13).
            02  HCM20-SRYYMDY   PIC X(2).
            02  HCM20-SRYYMDM   PIC X(2).
            02  HCM20-SRYYMDD   PIC X(2).
            02  HCM20-SRYYMDH   PIC X(2).
            02  HCM20-SRYKBN-G          OCCURS  100 TIMES.
              03  HCM20-SRYKBN    PIC X(4).
            02  HCM20-TEKIYO-G          OCCURS  100 TIMES.
              03  HCM20-TEKIYO    PIC X(52).
            02  HCM20-PTNUM PIC X(20).
            02  HCM20-RENNUM    PIC X(7).

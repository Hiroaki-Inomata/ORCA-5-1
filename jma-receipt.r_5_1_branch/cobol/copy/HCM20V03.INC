        01  HCM20V03.
            02  HCM20V03-KANANAME   PIC X(50).
            02  HCM20V03-NAME   PIC X(50).
            02  HCM20V03-HOSPNAME   PIC X(90).
            02  HCM20V03-RISIRYOCD  PIC X(30).
            02  HCM20V03-KOFUNUM    PIC X(13).
            02  HCM20V03-SRYYMDY    PIC X(2).
            02  HCM20V03-SRYYMDM    PIC X(2).
            02  HCM20V03-SRYYMDD    PIC X(2).
            02  HCM20V03-SRYYMDH    PIC X(2).
            02  HCM20V03-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V03-SRYKBN PIC X(4).
            02  HCM20V03-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V03-TEKIYO PIC X(52).
            02  HCM20V03-PTNUM  PIC X(20).
            02  HCM20V03-RENNUM PIC X(7).

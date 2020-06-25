        01  HCM20V06.
            02  HCM20V06-KANANAME   PIC X(50).
            02  HCM20V06-NAME   PIC X(50).
            02  HCM20V06-HOSPNAME   PIC X(90).
            02  HCM20V06-RISIRYOCD  PIC X(30).
            02  HCM20V06-KOFUNUM    PIC X(13).
            02  HCM20V06-SRYYMDY    PIC X(2).
            02  HCM20V06-SRYYMDM    PIC X(2).
            02  HCM20V06-SRYYMDD    PIC X(2).
            02  HCM20V06-SRYYMDH    PIC X(2).
            02  HCM20V06-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V06-SRYKBN PIC X(4).
            02  HCM20V06-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V06-TEKIYO PIC X(52).
            02  HCM20V06-PTNUM  PIC X(20).
            02  HCM20V06-RENNUM PIC X(7).
            02  HCM20V06-PAGE   PIC X(5).
            02  HCM20V06-LABEL  PIC X(14).
            02  HCM20V06-LOPTION    PIC X(100).

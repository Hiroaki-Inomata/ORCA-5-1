        01  HCM20V05.
            02  HCM20V05-KANANAME   PIC X(50).
            02  HCM20V05-NAME   PIC X(50).
            02  HCM20V05-HOSPNAME   PIC X(90).
            02  HCM20V05-RISIRYOCD  PIC X(30).
            02  HCM20V05-KOFUNUM    PIC X(13).
            02  HCM20V05-SRYYMDY    PIC X(2).
            02  HCM20V05-SRYYMDM    PIC X(2).
            02  HCM20V05-SRYYMDD    PIC X(2).
            02  HCM20V05-SRYYMDH    PIC X(2).
            02  HCM20V05-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V05-SRYKBN PIC X(4).
            02  HCM20V05-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V05-TEKIYO PIC X(52).
            02  HCM20V05-PTNUM  PIC X(20).
            02  HCM20V05-RENNUM PIC X(7).
            02  HCM20V05-PAGE   PIC X(5).
            02  HCM20V05-LABEL  PIC X(14).

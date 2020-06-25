        01  HCM20V04.
            02  HCM20V04-KANANAME   PIC X(50).
            02  HCM20V04-NAME   PIC X(50).
            02  HCM20V04-HOSPNAME   PIC X(90).
            02  HCM20V04-RISIRYOCD  PIC X(30).
            02  HCM20V04-KOFUNUM    PIC X(13).
            02  HCM20V04-SRYYMDY    PIC X(2).
            02  HCM20V04-SRYYMDM    PIC X(2).
            02  HCM20V04-SRYYMDD    PIC X(2).
            02  HCM20V04-SRYYMDH    PIC X(2).
            02  HCM20V04-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V04-SRYKBN PIC X(4).
            02  HCM20V04-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V04-TEKIYO PIC X(52).
            02  HCM20V04-PTNUM  PIC X(20).
            02  HCM20V04-RENNUM PIC X(7).
            02  HCM20V04-PAGE   PIC X(5).

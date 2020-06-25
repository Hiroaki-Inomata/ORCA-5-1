        01  HCM20V01.
            02  HCM20V01-KANANAME   PIC X(30).
            02  HCM20V01-NAME   PIC X(26).
            02  HCM20V01-HOSPNAME   PIC X(60).
            02  HCM20V01-RISIRYOCD  PIC X(30).
            02  HCM20V01-KOFUNUM    PIC X(13).
            02  HCM20V01-SRYYMDY    PIC X(2).
            02  HCM20V01-SRYYMDM    PIC X(2).
            02  HCM20V01-SRYYMDD    PIC X(2).
            02  HCM20V01-SRYYMDH    PIC X(2).
            02  HCM20V01-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V01-SRYKBN PIC X(4).
            02  HCM20V01-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V01-TEKIYO PIC X(52).
            02  HCM20V01-PTNUM  PIC X(20).
            02  HCM20V01-RENNUM PIC X(7).

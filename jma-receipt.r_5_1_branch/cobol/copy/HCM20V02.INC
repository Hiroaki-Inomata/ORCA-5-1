        01  HCM20V02.
            02  HCM20V02-KANANAME   PIC X(50).
            02  HCM20V02-NAME   PIC X(50).
            02  HCM20V02-HOSPNAME   PIC X(60).
            02  HCM20V02-RISIRYOCD  PIC X(30).
            02  HCM20V02-KOFUNUM    PIC X(13).
            02  HCM20V02-SRYYMDY    PIC X(2).
            02  HCM20V02-SRYYMDM    PIC X(2).
            02  HCM20V02-SRYYMDD    PIC X(2).
            02  HCM20V02-SRYYMDH    PIC X(2).
            02  HCM20V02-SRYKBN-G       OCCURS  100 TIMES.
              03  HCM20V02-SRYKBN PIC X(4).
            02  HCM20V02-TEKIYO-G       OCCURS  100 TIMES.
              03  HCM20V02-TEKIYO PIC X(52).
            02  HCM20V02-PTNUM  PIC X(20).
            02  HCM20V02-RENNUM PIC X(7).

      01  HC20.
              03  HC20-KANANAME    PIC  X(30).
              03  HC20-NAME    PIC  X(26).
              03  HC20-HOSPNAME    PIC  X(30).
              03  HC20-RISIRYOCD    PIC  X(30).
              03  HC20-KOFUNUM    PIC  X(13).
              03  HC20-SRYYMDY    PIC  X(2).
              03  HC20-SRYYMDM    PIC  X(2).
              03  HC20-SRYYMDD    PIC  X(2).
              03  HC20-SRYYMDH    PIC  X(2).
              03  HC20-SRYKBN-G   OCCURS  100.
                  05  HC20-SRYKBN PIC  X(4).
              03  HC20-TEKIYO-G   OCCURS  100.
                  05  HC20-TEKIYO PIC  X(52).
              03  HC20-PTNUM     PIC X(20).

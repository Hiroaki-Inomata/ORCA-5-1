        01  HCM41.
            02  HCM41-SYSYMD    PIC X(22).
            02  HCM41-SRYYMD    PIC X(22).
            02  HCM41-PAGE  PIC X(3).
            02  HCM41-MIDASI    PIC X(10).
            02  HCM41-NYUGAIKBN PIC X(8).
            02  HCM41-GOKEI-G           OCCURS  10 TIMES.
              03  HCM41-GMONEY    PIC X(9).
              03  HCM41-GHKNTEN   PIC X(8).
              03  HCM41-GKENSU    PIC X(8).
            02  HCM41-MEISAI-G          OCCURS  30 TIMES.
              03  HCM41-RENNUM    PIC X(3).
              03  HCM41-PTNUM PIC X(20).
              03  HCM41-NAME  PIC X(26).
              03  HCM41-HKNTEN    PIC X(8).
            02  HCM41-MONEY-G           OCCURS  9 TIMES.
              03  HCM41-MONEY-OCC         OCCURS  30 TIMES.
                04  HCM41-MONEY PIC X(9).

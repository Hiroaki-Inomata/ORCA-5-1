        01  HCM17.
            02  HCM17-SYSYMD    PIC X(22).
            02  HCM17-SRYYMD    PIC X(22).
            02  HCM17-PAGE  PIC X(3).
            02  HCM17-MIDASI    PIC X(10).
            02  HCM17-GOKEI-G           OCCURS  10 TIMES.
              03  HCM17-GMONEY    PIC X(9).
              03  HCM17-GHKNTEN   PIC X(8).
              03  HCM17-GKENSU    PIC X(8).
            02  HCM17-MEISAI-G          OCCURS  30 TIMES.
              03  HCM17-RENNUM    PIC X(3).
              03  HCM17-PTNUM PIC X(20).
              03  HCM17-NAME  PIC X(26).
              03  HCM17-HKNTEN    PIC X(8).
            02  HCM17-MONEY-G           OCCURS  9 TIMES.
              03  HCM17-MONEY-OCC         OCCURS  30 TIMES.
                04  HCM17-MONEY PIC X(9).

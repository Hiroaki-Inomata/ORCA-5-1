        01  HCM41V01.
            02  HCM41V01-SYSYMD PIC X(22).
            02  HCM41V01-SRYYMD PIC X(22).
            02  HCM41V01-PAGE   PIC X(3).
            02  HCM41V01-MIDASI PIC X(10).
            02  HCM41V01-NYUGAIKBN  PIC X(8).
            02  HCM41V01-GOKEI-G        OCCURS  11 TIMES.
              03  HCM41V01-GMONEY PIC X(9).
              03  HCM41V01-GHKNTEN    PIC X(8).
              03  HCM41V01-GKENSU PIC X(8).
            02  HCM41V01-MEISAI-G       OCCURS  30 TIMES.
              03  HCM41V01-RENNUM PIC X(3).
              03  HCM41V01-PTNUM  PIC X(10).
              03  HCM41V01-PTNUM2 PIC X(20).
              03  HCM41V01-NAME   PIC X(26).
              03  HCM41V01-HKNTEN PIC X(8).
            02  HCM41V01-MONEY-G        OCCURS  10 TIMES.
              03  HCM41V01-MONEY-OCC      OCCURS  30 TIMES.
                04  HCM41V01-MONEY  PIC X(9).

        01  HCM41V02.
            02  HCM41V02-SYSYMD PIC X(22).
            02  HCM41V02-SRYYMD PIC X(22).
            02  HCM41V02-PAGE   PIC X(3).
            02  HCM41V02-MIDASI PIC X(10).
            02  HCM41V02-NYUGAIKBN  PIC X(8).
            02  HCM41V02-GOKEI-G        OCCURS  11 TIMES.
              03  HCM41V02-GMONEY PIC X(9).
              03  HCM41V02-GHKNTEN    PIC X(8).
              03  HCM41V02-GKENSU PIC X(8).
            02  HCM41V02-MEISAI-G       OCCURS  30 TIMES.
              03  HCM41V02-RENNUM PIC X(3).
              03  HCM41V02-PTNUM  PIC X(10).
              03  HCM41V02-PTNUM2 PIC X(20).
              03  HCM41V02-NAME   PIC X(26).
              03  HCM41V02-HKNTEN PIC X(8).
            02  HCM41V02-MONEY-G        OCCURS  10 TIMES.
              03  HCM41V02-HKNNAME    PIC X(8).
              03  HCM41V02-MONEY-OCC      OCCURS  30 TIMES.
                04  HCM41V02-MONEY  PIC X(9).

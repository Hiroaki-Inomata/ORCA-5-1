        01  HCM58V02.
            02  HCM58V02-PTNUM  PIC X(20).
            02  HCM58V02-PAGE   PIC X(5).
            02  HCM58V02-RENNUM PIC X(7).
            02  HCM58V02-SRYYM  PIC X(18).
            02  HCM58V02-HEAD1-G.
              03  HCM58V02-HEAD1
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM58V02-KIGONUM    PIC X(24).
            02  HCM58V02-KIGONUM2-G.
              03  HCM58V02-KIGONUM2
                      PIC X(24)   OCCURS  2 TIMES.
            02  HCM58V02-KANANAME   PIC X(40).
            02  HCM58V02-NAME   PIC X(50).
            02  HCM58V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM58V02-BIRTHG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM58V02-BIRTHYY    PIC X(2).
            02  HCM58V02-HOSPADRS   PIC X(100).
            02  HCM58V02-HOSPNAME   PIC X(80).
            02  HCM58V02-HOSPTEL    PIC X(15).
            02  HCM58V02-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM58V02-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM58V02-TENKI-G        OCCURS  5 TIMES.
              03  HCM58V02-TENKI
                      PIC X(2)    OCCURS  3 TIMES.
            02  HCM58V02-JNISSU PIC X(2).
            02  HCM58V02-SHOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM58V02-KHNTEN
                    PIC X(7)    OCCURS  37 TIMES.
            02  HCM58V02-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM58V02-TENSU-G        OCCURS  2 TIMES.
              03  HCM58V02-TENSU
                      PIC X(7)    OCCURS  38 TIMES.
            02  HCM58V02-SYOUKEI
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM58V02-GOKEI
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM58V02-SRYKBN
                    PIC X(2)    OCCURS  50 TIMES.
            02  HCM58V02-TEKIYO
                    PIC X(52)   OCCURS  50 TIMES.
            02  HCM58V02-LABEL  PIC X(14).
            02  HCM58V02-LOPTION    PIC X(100).

        01  HCM58V01.
            02  HCM58V01-PTNUM  PIC X(20).
            02  HCM58V01-PAGE   PIC X(5).
            02  HCM58V01-RENNUM PIC X(7).
            02  HCM58V01-SRYYM  PIC X(18).
            02  HCM58V01-HEAD1-G.
              03  HCM58V01-HEAD1
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM58V01-KIGONUM    PIC X(24).
            02  HCM58V01-KIGONUM2-G.
              03  HCM58V01-KIGONUM2
                      PIC X(24)   OCCURS  2 TIMES.
            02  HCM58V01-KANANAME   PIC X(40).
            02  HCM58V01-NAME   PIC X(50).
            02  HCM58V01-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM58V01-BIRTHG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM58V01-BIRTHYY    PIC X(2).
            02  HCM58V01-HOSPADRS   PIC X(100).
            02  HCM58V01-HOSPNAME   PIC X(80).
            02  HCM58V01-HOSPTEL    PIC X(15).
            02  HCM58V01-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM58V01-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM58V01-TENKI-G        OCCURS  5 TIMES.
              03  HCM58V01-TENKI
                      PIC X(2)    OCCURS  3 TIMES.
            02  HCM58V01-JNISSU PIC X(2).
            02  HCM58V01-SHOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM58V01-KHNTEN
                    PIC X(7)    OCCURS  37 TIMES.
            02  HCM58V01-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM58V01-TENSU-G        OCCURS  2 TIMES.
              03  HCM58V01-TENSU
                      PIC X(7)    OCCURS  38 TIMES.
            02  HCM58V01-SYOUKEI
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM58V01-GOKEI
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM58V01-SRYKBN
                    PIC X(2)    OCCURS  50 TIMES.
            02  HCM58V01-TEKIYO
                    PIC X(52)   OCCURS  50 TIMES.
            02  HCM58V01-LABEL  PIC X(14).

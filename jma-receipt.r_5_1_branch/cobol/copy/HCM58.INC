        01  HCM58.
            02  HCM58-PTNUM PIC X(20).
            02  HCM58-PAGE  PIC X(5).
            02  HCM58-RENNUM    PIC X(7).
            02  HCM58-SRYYM PIC X(18).
            02  HCM58-HEAD1-G.
              03  HCM58-HEAD1
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM58-KIGONUM   PIC X(20).
            02  HCM58-KANANAME  PIC X(40).
            02  HCM58-NAME  PIC X(50).
            02  HCM58-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM58-BIRTHG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM58-BIRTHYY   PIC X(2).
            02  HCM58-HOSPADRS  PIC X(100).
            02  HCM58-HOSPNAME  PIC X(80).
            02  HCM58-HOSPTEL   PIC X(15).
            02  HCM58-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM58-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM58-TENKI-G           OCCURS  5 TIMES.
              03  HCM58-TENKI
                      PIC X(2)    OCCURS  3 TIMES.
            02  HCM58-JNISSU    PIC X(2).
            02  HCM58-SHOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM58-KHNTEN
                    PIC X(7)    OCCURS  37 TIMES.
            02  HCM58-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM58-TENSU-G           OCCURS  2 TIMES.
              03  HCM58-TENSU
                      PIC X(7)    OCCURS  38 TIMES.
            02  HCM58-SYOUKEI
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM58-GOKEI
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM58-SRYKBN
                    PIC X(2)    OCCURS  50 TIMES.
            02  HCM58-TEKIYO
                    PIC X(52)   OCCURS  50 TIMES.

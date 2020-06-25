        01  HCM56V02.
            02  HCM56V02-PTNUM  PIC X(20).
            02  HCM56V02-PAGE   PIC X(5).
            02  HCM56V02-RENNUM PIC X(7).
            02  HCM56V02-KANANAME   PIC X(50).
            02  HCM56V02-NAME   PIC X(40).
            02  HCM56V02-SYOBYO
                    PIC X(54)   OCCURS  3 TIMES.
            02  HCM56V02-SYOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56V02-KHNTEN
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56V02-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM56V02-TENSU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56V02-TOUYAKUNAME    PIC X(4).
            02  HCM56V02-TOUYAKUTANI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM56V02-SONOTANAME     OCCURS  2 TIMES.
              03  HCM56V02-SONOTANAME-S
                      PIC X(2)    OCCURS  4 TIMES.
            02  HCM56V02-SONOTATANI PIC X(2).
            02  HCM56V02-NYUINYMD   PIC X(22).
            02  HCM56V02-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56V02-NYUSYUBETSU
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCM56V02-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56V02-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCM56V02-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCM56V02-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56V02-NYUTENSUN-G            OCCURS  9 TIMES.
              03  HCM56V02-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCM56V02-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM56V02-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V02-SRYYMD
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCM56V02-STRYMD PIC X(26).
            02  HCM56V02-ENDYMD PIC X(26).
            02  HCM56V02-JITSUNISSU PIC X(3).
            02  HCM56V02-KEIKA
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM56V02-TENKIYMD   PIC X(22).
            02  HCM56V02-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM56V02-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM56V02-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM56V02-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V02-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM56V02-SYOKAKERU
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V02-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCM56V02-SYONICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V02-KOMUTEN    PIC X(9).
            02  HCM56V02-KOMUTENTANKA   PIC X(4).
            02  HCM56V02-KOMUKINGAKU
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM56V02-SEIKYUYMD  PIC X(22).
            02  HCM56V02-HOSPJYUSYO PIC X(100).
            02  HCM56V02-HOSPNAME   PIC X(120).
            02  HCM56V02-ISYA   PIC X(40).
            02  HCM56V02-LABEL  PIC X(14).
            02  HCM56V02-LOPTION    PIC X(100).

        01  HCM56.
            02  HCM56-PTNUM PIC X(20).
            02  HCM56-PAGE  PIC X(5).
            02  HCM56-RENNUM    PIC X(7).
            02  HCM56-KANANAME  PIC X(50).
            02  HCM56-NAME  PIC X(40).
            02  HCM56-SYOBYO
                    PIC X(54)   OCCURS  3 TIMES.
            02  HCM56-SYOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56-KHNTEN
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM56-TENSU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56-TOUYAKUNAME   PIC X(4).
            02  HCM56-TOUYAKUTANI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM56-SONOTANAME        OCCURS  2 TIMES.
              03  HCM56-SONOTANAME-S
                      PIC X(2)    OCCURS  4 TIMES.
            02  HCM56-SONOTATANI    PIC X(2).
            02  HCM56-NYUINYMD  PIC X(22).
            02  HCM56-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56-NYUSYUBETSU
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCM56-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCM56-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCM56-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56-NYUTENSUN-G       OCCURS  9 TIMES.
              03  HCM56-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCM56-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM56-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56-SRYYMD
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCM56-STRYMD    PIC X(26).
            02  HCM56-ENDYMD    PIC X(26).
            02  HCM56-JITSUNISSU    PIC X(3).
            02  HCM56-KEIKA
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM56-TENKIYMD  PIC X(22).
            02  HCM56-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM56-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM56-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM56-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM56-SYOKAKERU
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCM56-SYONICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56-KOMUTEN   PIC X(9).
            02  HCM56-KOMUTENTANKA  PIC X(4).
            02  HCM56-KOMUKINGAKU
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM56-SEIKYUYMD PIC X(22).
            02  HCM56-HOSPJYUSYO    PIC X(100).
            02  HCM56-HOSPNAME  PIC X(120).
            02  HCM56-ISYA  PIC X(40).

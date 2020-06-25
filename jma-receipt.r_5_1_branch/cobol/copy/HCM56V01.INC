        01  HCM56V01.
            02  HCM56V01-PTNUM  PIC X(20).
            02  HCM56V01-PAGE   PIC X(5).
            02  HCM56V01-RENNUM PIC X(7).
            02  HCM56V01-KANANAME   PIC X(50).
            02  HCM56V01-NAME   PIC X(40).
            02  HCM56V01-SYOBYO
                    PIC X(54)   OCCURS  3 TIMES.
            02  HCM56V01-SYOSHIN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56V01-KHNTEN
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56V01-KAISU
                    PIC X(3)    OCCURS  38 TIMES.
            02  HCM56V01-TENSU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM56V01-TOUYAKUNAME    PIC X(4).
            02  HCM56V01-TOUYAKUTANI
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM56V01-SONOTANAME     OCCURS  2 TIMES.
              03  HCM56V01-SONOTANAME-S
                      PIC X(2)    OCCURS  4 TIMES.
            02  HCM56V01-SONOTATANI PIC X(2).
            02  HCM56V01-NYUINYMD   PIC X(22).
            02  HCM56V01-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM56V01-NYUSYUBETSU
                    PIC X(10)   OCCURS  10 TIMES.
            02  HCM56V01-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56V01-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCM56V01-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCM56V01-NYUTENSU
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCM56V01-NYUTENSUN-G            OCCURS  9 TIMES.
              03  HCM56V01-NYUTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCM56V01-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM56V01-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V01-SRYYMD
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCM56V01-STRYMD PIC X(26).
            02  HCM56V01-ENDYMD PIC X(26).
            02  HCM56V01-JITSUNISSU PIC X(3).
            02  HCM56V01-KEIKA
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM56V01-TENKIYMD   PIC X(22).
            02  HCM56V01-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM56V01-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM56V01-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM56V01-SYOKJN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V01-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM56V01-SYOKAKERU
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V01-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCM56V01-SYONICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCM56V01-KOMUTEN    PIC X(9).
            02  HCM56V01-KOMUTENTANKA   PIC X(4).
            02  HCM56V01-KOMUKINGAKU
                    PIC X(10)   OCCURS  3 TIMES.
            02  HCM56V01-SEIKYUYMD  PIC X(22).
            02  HCM56V01-HOSPJYUSYO PIC X(100).
            02  HCM56V01-HOSPNAME   PIC X(120).
            02  HCM56V01-ISYA   PIC X(40).
            02  HCM56V01-LABEL  PIC X(14).

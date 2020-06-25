        01  HCM53V02.
            02  HCM53V02-HAKKOUYY   PIC X(2).
            02  HCM53V02-HAKKOUMM   PIC X(2).
            02  HCM53V02-PTNUM  PIC X(20).
            02  HCM53V02-NAME   PIC X(50).
            02  HCM53V02-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM53V02-TANJYOYY   PIC X(2).
            02  HCM53V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM53V02-NENREI PIC X(3).
            02  HCM53V02-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM53V02-JYUSYOUBI-G.
              03  HCM53V02-JYUSYOUBIYY    PIC X(2).
              03  HCM53V02-JYUSYOUBIMM    PIC X(2).
              03  HCM53V02-JYUSYOUBIDD    PIC X(2).
            02  HCM53V02-SYOSHINBI-G.
              03  HCM53V02-SYOSHINBIYY    PIC X(2).
              03  HCM53V02-SYOSHINBIMM    PIC X(2).
              03  HCM53V02-SYOSHINBIDD    PIC X(2).
            02  HCM53V02-STR-G.
              03  HCM53V02-STRYY  PIC X(2).
              03  HCM53V02-STRMM  PIC X(2).
              03  HCM53V02-STRDD  PIC X(2).
            02  HCM53V02-END-G.
              03  HCM53V02-ENDYY  PIC X(2).
              03  HCM53V02-ENDMM  PIC X(2).
              03  HCM53V02-ENDDD  PIC X(2).
            02  HCM53V02-JITSUNISSU PIC X(3).
            02  HCM53V02-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM53V02-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM53V02-KHNTEN
                    PIC X(7)    OCCURS  34 TIMES.
            02  HCM53V02-KAISU
                    PIC X(3)    OCCURS  34 TIMES.
            02  HCM53V02-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM53V02-KOUTENG    PIC X(7).
            02  HCM53V02-YKZTENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM53V02-YKZTENSUG  PIC X(7).
            02  HCM53V02-SAISINKAISU    PIC X(3).
            02  HCM53V02-SHIDOKAISU PIC X(2).
            02  HCM53V02-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM53V02-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM53V02-SHINDANSYOSUU  PIC X(3).
            02  HCM53V02-MEISAISYOSUU   PIC X(3).
            02  HCM53V02-SHINDANSYOMONEY    PIC X(7).
            02  HCM53V02-MEISAISYOMONEY PIC X(7).
            02  HCM53V02-SRYKBN
                    PIC X(2)    OCCURS  28 TIMES.
            02  HCM53V02-TEKIYO2
                    PIC X(52)   OCCURS  28 TIMES.
            02  HCM53V02-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM53V02-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM53V02-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM53V02-TSUIN-TBL.
              03  HCM53V02-TSUIN-G        OCCURS  2 TIMES.
                04  HCM53V02-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM53V02-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM53V02-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM53V02-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM53V02-SEIKYUG.
              03  HCM53V02-SEIKYUYY   PIC X(2).
              03  HCM53V02-SEIKYUMM   PIC X(2).
              03  HCM53V02-SEIKYUDD   PIC X(2).
            02  HCM53V02-HOSPJYUSYO PIC X(100).
            02  HCM53V02-HOSPNAME   PIC X(80).
            02  HCM53V02-ISYA   PIC X(40).
            02  HCM53V02-TEL    PIC X(15).
            02  HCM53V02-BYOSYOSU   PIC X(4).
            02  HCM53V02-KANANAME   PIC X(50).
            02  HCM53V02-RENNUM PIC X(7).
            02  HCM53V02-HOKENNAME  PIC X(60).
            02  HCM53V02-PAGE   PIC X(5).
            02  HCM53V02-LABEL  PIC X(14).
            02  HCM53V02-LOPTION    PIC X(100).

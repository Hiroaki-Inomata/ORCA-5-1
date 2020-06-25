        01  HCM23V02.
            02  HCM23V02-HAKKOUYY   PIC X(2).
            02  HCM23V02-HAKKOUMM   PIC X(2).
            02  HCM23V02-PTNUM  PIC X(20).
            02  HCM23V02-NAME   PIC X(50).
            02  HCM23V02-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V02-TANJYOYY   PIC X(2).
            02  HCM23V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V02-NENREI PIC X(3).
            02  HCM23V02-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM23V02-JYUSYOUBI-G.
              03  HCM23V02-JYUSYOUBIYY    PIC X(2).
              03  HCM23V02-JYUSYOUBIMM    PIC X(2).
              03  HCM23V02-JYUSYOUBIDD    PIC X(2).
            02  HCM23V02-SYOSHINBI-G.
              03  HCM23V02-SYOSHINBIYY    PIC X(2).
              03  HCM23V02-SYOSHINBIMM    PIC X(2).
              03  HCM23V02-SYOSHINBIDD    PIC X(2).
            02  HCM23V02-STR-G.
              03  HCM23V02-STRYY  PIC X(2).
              03  HCM23V02-STRMM  PIC X(2).
              03  HCM23V02-STRDD  PIC X(2).
            02  HCM23V02-END-G.
              03  HCM23V02-ENDYY  PIC X(2).
              03  HCM23V02-ENDMM  PIC X(2).
              03  HCM23V02-ENDDD  PIC X(2).
            02  HCM23V02-JITSUNISSU PIC X(3).
            02  HCM23V02-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM23V02-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V02-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM23V02-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM23V02-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V02-KOUTENG    PIC X(7).
            02  HCM23V02-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V02-YKZTENSUG  PIC X(7).
            02  HCM23V02-SAISINKAISU    PIC X(2).
            02  HCM23V02-SHIDOKAISU PIC X(2).
            02  HCM23V02-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM23V02-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM23V02-SHINDANSYOSUU  PIC X(3).
            02  HCM23V02-MEISAISYOSUU   PIC X(3).
            02  HCM23V02-SHINDANSYOMONEY    PIC X(7).
            02  HCM23V02-MEISAISYOMONEY PIC X(7).
            02  HCM23V02-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM23V02-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM23V02-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V02-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM23V02-TSUIN-TBL.
              03  HCM23V02-TSUIN-G        OCCURS  2 TIMES.
                04  HCM23V02-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM23V02-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM23V02-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V02-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM23V02-SEIKYU-G.
              03  HCM23V02-SEIKYUYY   PIC X(2).
              03  HCM23V02-SEIKYUMM   PIC X(2).
              03  HCM23V02-SEIKYUDD   PIC X(2).
            02  HCM23V02-HOSPJYUSYO PIC X(80).
            02  HCM23V02-HOSPNAME   PIC X(80).
            02  HCM23V02-ISYA   PIC X(40).
            02  HCM23V02-TEL    PIC X(15).
            02  HCM23V02-BYOSYOSU   PIC X(4).
            02  HCM23V02-KANANAME   PIC X(50).
            02  HCM23V02-RENNUM PIC X(7).

        01  HCM48V01.
            02  HCM48V01-HAKKOUYY   PIC X(2).
            02  HCM48V01-HAKKOUMM   PIC X(2).
            02  HCM48V01-PTNUM  PIC X(20).
            02  HCM48V01-NAME   PIC X(50).
            02  HCM48V01-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM48V01-TANJYOYY   PIC X(2).
            02  HCM48V01-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM48V01-NENREI PIC X(3).
            02  HCM48V01-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM48V01-JYUSYOUBI-G.
              03  HCM48V01-JYUSYOUBIYY    PIC X(2).
              03  HCM48V01-JYUSYOUBIMM    PIC X(2).
              03  HCM48V01-JYUSYOUBIDD    PIC X(2).
            02  HCM48V01-SYOSHINBI-G.
              03  HCM48V01-SYOSHINBIYY    PIC X(2).
              03  HCM48V01-SYOSHINBIMM    PIC X(2).
              03  HCM48V01-SYOSHINBIDD    PIC X(2).
            02  HCM48V01-STR-G.
              03  HCM48V01-STRYY  PIC X(2).
              03  HCM48V01-STRMM  PIC X(2).
              03  HCM48V01-STRDD  PIC X(2).
            02  HCM48V01-END-G.
              03  HCM48V01-ENDYY  PIC X(2).
              03  HCM48V01-ENDMM  PIC X(2).
              03  HCM48V01-ENDDD  PIC X(2).
            02  HCM48V01-JITSUNISSU PIC X(3).
            02  HCM48V01-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM48V01-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM48V01-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM48V01-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM48V01-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM48V01-KOUTENG    PIC X(7).
            02  HCM48V01-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM48V01-YKZTENSUG  PIC X(7).
            02  HCM48V01-SAISINKAISU    PIC X(2).
            02  HCM48V01-SHIDOKAISU PIC X(2).
            02  HCM48V01-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM48V01-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM48V01-SHINDANSYOSUU  PIC X(3).
            02  HCM48V01-MEISAISYOSUU   PIC X(3).
            02  HCM48V01-SHINDANSYOMONEY    PIC X(7).
            02  HCM48V01-MEISAISYOMONEY PIC X(7).
            02  HCM48V01-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM48V01-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM48V01-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM48V01-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM48V01-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM48V01-TSUIN-TBL.
              03  HCM48V01-TSUIN-G        OCCURS  2 TIMES.
                04  HCM48V01-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM48V01-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM48V01-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM48V01-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM48V01-SEIKYU-G.
              03  HCM48V01-SEIKYUYY   PIC X(2).
              03  HCM48V01-SEIKYUMM   PIC X(2).
              03  HCM48V01-SEIKYUDD   PIC X(2).
            02  HCM48V01-HOSPJYUSYO PIC X(100).
            02  HCM48V01-HOSPNAME   PIC X(80).
            02  HCM48V01-ISYA   PIC X(40).
            02  HCM48V01-TEL    PIC X(15).
            02  HCM48V01-BYOSYOSU   PIC X(4).
            02  HCM48V01-KANANAME   PIC X(50).
            02  HCM48V01-RENNUM PIC X(7).
            02  HCM48V01-HOKENNAME  PIC X(60).
            02  HCM48V01-PAGE   PIC X(5).
            02  HCM48V01-LABEL  PIC X(14).

        01  HCM23V06.
            02  HCM23V06-HAKKOUYY   PIC X(2).
            02  HCM23V06-HAKKOUMM   PIC X(2).
            02  HCM23V06-PTNUM  PIC X(20).
            02  HCM23V06-NAME   PIC X(50).
            02  HCM23V06-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V06-TANJYOYY   PIC X(2).
            02  HCM23V06-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V06-NENREI PIC X(3).
            02  HCM23V06-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM23V06-JYUSYOUBI-G.
              03  HCM23V06-JYUSYOUBIYY    PIC X(2).
              03  HCM23V06-JYUSYOUBIMM    PIC X(2).
              03  HCM23V06-JYUSYOUBIDD    PIC X(2).
            02  HCM23V06-SYOSHINBI-G.
              03  HCM23V06-SYOSHINBIYY    PIC X(2).
              03  HCM23V06-SYOSHINBIMM    PIC X(2).
              03  HCM23V06-SYOSHINBIDD    PIC X(2).
            02  HCM23V06-STR-G.
              03  HCM23V06-STRYY  PIC X(2).
              03  HCM23V06-STRMM  PIC X(2).
              03  HCM23V06-STRDD  PIC X(2).
            02  HCM23V06-END-G.
              03  HCM23V06-ENDYY  PIC X(2).
              03  HCM23V06-ENDMM  PIC X(2).
              03  HCM23V06-ENDDD  PIC X(2).
            02  HCM23V06-JITSUNISSU PIC X(3).
            02  HCM23V06-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM23V06-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V06-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM23V06-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM23V06-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V06-KOUTENG    PIC X(7).
            02  HCM23V06-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V06-YKZTENSUG  PIC X(7).
            02  HCM23V06-SAISINKAISU    PIC X(2).
            02  HCM23V06-SHIDOKAISU PIC X(2).
            02  HCM23V06-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM23V06-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM23V06-SHINDANSYOSUU  PIC X(3).
            02  HCM23V06-MEISAISYOSUU   PIC X(3).
            02  HCM23V06-SHINDANSYOMONEY    PIC X(7).
            02  HCM23V06-MEISAISYOMONEY PIC X(7).
            02  HCM23V06-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM23V06-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM23V06-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM23V06-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V06-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM23V06-TSUIN-TBL.
              03  HCM23V06-TSUIN-G        OCCURS  2 TIMES.
                04  HCM23V06-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM23V06-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM23V06-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V06-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM23V06-SEIKYU-G.
              03  HCM23V06-SEIKYUYY   PIC X(2).
              03  HCM23V06-SEIKYUMM   PIC X(2).
              03  HCM23V06-SEIKYUDD   PIC X(2).
            02  HCM23V06-HOSPJYUSYO PIC X(100).
            02  HCM23V06-HOSPNAME   PIC X(80).
            02  HCM23V06-ISYA   PIC X(40).
            02  HCM23V06-TEL    PIC X(15).
            02  HCM23V06-BYOSYOSU   PIC X(4).
            02  HCM23V06-KANANAME   PIC X(50).
            02  HCM23V06-RENNUM PIC X(7).
            02  HCM23V06-HOKENNAME  PIC X(60).
            02  HCM23V06-PAGE   PIC X(5).
            02  HCM23V06-LABEL  PIC X(14).
            02  HCM23V06-LOPTION    PIC X(100).

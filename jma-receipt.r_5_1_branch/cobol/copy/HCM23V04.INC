        01  HCM23V04.
            02  HCM23V04-HAKKOUYY   PIC X(2).
            02  HCM23V04-HAKKOUMM   PIC X(2).
            02  HCM23V04-PTNUM  PIC X(20).
            02  HCM23V04-NAME   PIC X(50).
            02  HCM23V04-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V04-TANJYOYY   PIC X(2).
            02  HCM23V04-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V04-NENREI PIC X(3).
            02  HCM23V04-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM23V04-JYUSYOUBI-G.
              03  HCM23V04-JYUSYOUBIYY    PIC X(2).
              03  HCM23V04-JYUSYOUBIMM    PIC X(2).
              03  HCM23V04-JYUSYOUBIDD    PIC X(2).
            02  HCM23V04-SYOSHINBI-G.
              03  HCM23V04-SYOSHINBIYY    PIC X(2).
              03  HCM23V04-SYOSHINBIMM    PIC X(2).
              03  HCM23V04-SYOSHINBIDD    PIC X(2).
            02  HCM23V04-STR-G.
              03  HCM23V04-STRYY  PIC X(2).
              03  HCM23V04-STRMM  PIC X(2).
              03  HCM23V04-STRDD  PIC X(2).
            02  HCM23V04-END-G.
              03  HCM23V04-ENDYY  PIC X(2).
              03  HCM23V04-ENDMM  PIC X(2).
              03  HCM23V04-ENDDD  PIC X(2).
            02  HCM23V04-JITSUNISSU PIC X(3).
            02  HCM23V04-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM23V04-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V04-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM23V04-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM23V04-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V04-KOUTENG    PIC X(7).
            02  HCM23V04-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V04-YKZTENSUG  PIC X(7).
            02  HCM23V04-SAISINKAISU    PIC X(2).
            02  HCM23V04-SHIDOKAISU PIC X(2).
            02  HCM23V04-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM23V04-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM23V04-SHINDANSYOSUU  PIC X(3).
            02  HCM23V04-MEISAISYOSUU   PIC X(3).
            02  HCM23V04-SHINDANSYOMONEY    PIC X(7).
            02  HCM23V04-MEISAISYOMONEY PIC X(7).
            02  HCM23V04-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM23V04-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM23V04-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM23V04-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V04-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM23V04-TSUIN-TBL.
              03  HCM23V04-TSUIN-G        OCCURS  2 TIMES.
                04  HCM23V04-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM23V04-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM23V04-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V04-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM23V04-SEIKYU-G.
              03  HCM23V04-SEIKYUYY   PIC X(2).
              03  HCM23V04-SEIKYUMM   PIC X(2).
              03  HCM23V04-SEIKYUDD   PIC X(2).
            02  HCM23V04-HOSPJYUSYO PIC X(100).
            02  HCM23V04-HOSPNAME   PIC X(80).
            02  HCM23V04-ISYA   PIC X(40).
            02  HCM23V04-TEL    PIC X(15).
            02  HCM23V04-BYOSYOSU   PIC X(4).
            02  HCM23V04-KANANAME   PIC X(50).
            02  HCM23V04-RENNUM PIC X(7).
            02  HCM23V04-HOKENNAME  PIC X(60).
            02  HCM23V04-PAGE   PIC X(5).

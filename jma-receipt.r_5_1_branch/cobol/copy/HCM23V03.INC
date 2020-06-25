        01  HCM23V03.
            02  HCM23V03-HAKKOUYY   PIC X(2).
            02  HCM23V03-HAKKOUMM   PIC X(2).
            02  HCM23V03-PTNUM  PIC X(20).
            02  HCM23V03-NAME   PIC X(50).
            02  HCM23V03-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V03-TANJYOYY   PIC X(2).
            02  HCM23V03-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V03-NENREI PIC X(3).
            02  HCM23V03-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM23V03-JYUSYOUBI-G.
              03  HCM23V03-JYUSYOUBIYY    PIC X(2).
              03  HCM23V03-JYUSYOUBIMM    PIC X(2).
              03  HCM23V03-JYUSYOUBIDD    PIC X(2).
            02  HCM23V03-SYOSHINBI-G.
              03  HCM23V03-SYOSHINBIYY    PIC X(2).
              03  HCM23V03-SYOSHINBIMM    PIC X(2).
              03  HCM23V03-SYOSHINBIDD    PIC X(2).
            02  HCM23V03-STR-G.
              03  HCM23V03-STRYY  PIC X(2).
              03  HCM23V03-STRMM  PIC X(2).
              03  HCM23V03-STRDD  PIC X(2).
            02  HCM23V03-END-G.
              03  HCM23V03-ENDYY  PIC X(2).
              03  HCM23V03-ENDMM  PIC X(2).
              03  HCM23V03-ENDDD  PIC X(2).
            02  HCM23V03-JITSUNISSU PIC X(3).
            02  HCM23V03-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM23V03-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM23V03-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM23V03-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM23V03-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V03-KOUTENG    PIC X(7).
            02  HCM23V03-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM23V03-YKZTENSUG  PIC X(7).
            02  HCM23V03-SAISINKAISU    PIC X(2).
            02  HCM23V03-SHIDOKAISU PIC X(2).
            02  HCM23V03-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM23V03-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM23V03-SHINDANSYOSUU  PIC X(3).
            02  HCM23V03-MEISAISYOSUU   PIC X(3).
            02  HCM23V03-SHINDANSYOMONEY    PIC X(7).
            02  HCM23V03-MEISAISYOMONEY PIC X(7).
            02  HCM23V03-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM23V03-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM23V03-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V03-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM23V03-TSUIN-TBL.
              03  HCM23V03-TSUIN-G        OCCURS  2 TIMES.
                04  HCM23V03-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM23V03-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM23V03-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM23V03-SEIKYUGAKUTOTAL    PIC X(9).
            02  HCM23V03-SEIKYU-G.
              03  HCM23V03-SEIKYUYY   PIC X(2).
              03  HCM23V03-SEIKYUMM   PIC X(2).
              03  HCM23V03-SEIKYUDD   PIC X(2).
            02  HCM23V03-HOSPJYUSYO PIC X(100).
            02  HCM23V03-HOSPNAME   PIC X(80).
            02  HCM23V03-ISYA   PIC X(40).
            02  HCM23V03-TEL    PIC X(15).
            02  HCM23V03-BYOSYOSU   PIC X(4).
            02  HCM23V03-KANANAME   PIC X(50).
            02  HCM23V03-RENNUM PIC X(7).
            02  HCM23V03-HOKENNAME  PIC X(60).

        01  HCM48.
            02  HCM48-HAKKOUYY  PIC X(2).
            02  HCM48-HAKKOUMM  PIC X(2).
            02  HCM48-PTNUM PIC X(20).
            02  HCM48-NAME  PIC X(50).
            02  HCM48-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM48-TANJYOYY  PIC X(2).
            02  HCM48-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM48-NENREI    PIC X(3).
            02  HCM48-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM48-JYUSYOUBI-G.
              03  HCM48-JYUSYOUBIYY   PIC X(2).
              03  HCM48-JYUSYOUBIMM   PIC X(2).
              03  HCM48-JYUSYOUBIDD   PIC X(2).
            02  HCM48-SYOSHINBI-G.
              03  HCM48-SYOSHINBIYY   PIC X(2).
              03  HCM48-SYOSHINBIMM   PIC X(2).
              03  HCM48-SYOSHINBIDD   PIC X(2).
            02  HCM48-STR-G.
              03  HCM48-STRYY PIC X(2).
              03  HCM48-STRMM PIC X(2).
              03  HCM48-STRDD PIC X(2).
            02  HCM48-END-G.
              03  HCM48-ENDYY PIC X(2).
              03  HCM48-ENDMM PIC X(2).
              03  HCM48-ENDDD PIC X(2).
            02  HCM48-JITSUNISSU    PIC X(3).
            02  HCM48-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM48-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM48-KHNTEN
                    PIC X(7)    OCCURS  32 TIMES.
            02  HCM48-KAISU
                    PIC X(3)    OCCURS  32 TIMES.
            02  HCM48-KOUTEN
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM48-KOUTENG   PIC X(7).
            02  HCM48-YKZTENSU
                    PIC X(7)    OCCURS  33 TIMES.
            02  HCM48-YKZTENSUG PIC X(7).
            02  HCM48-SAISINKAISU   PIC X(2).
            02  HCM48-SHIDOKAISU    PIC X(2).
            02  HCM48-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM48-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM48-SHINDANSYOSUU PIC X(3).
            02  HCM48-MEISAISYOSUU  PIC X(3).
            02  HCM48-SHINDANSYOMONEY   PIC X(7).
            02  HCM48-MEISAISYOMONEY    PIC X(7).
            02  HCM48-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCM48-TEKIYO2
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCM48-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM48-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM48-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM48-TSUIN-TBL.
              03  HCM48-TSUIN-G           OCCURS  2 TIMES.
                04  HCM48-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM48-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM48-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM48-SEIKYUGAKUTOTAL   PIC X(9).
            02  HCM48-SEIKYU-G.
              03  HCM48-SEIKYUYY  PIC X(2).
              03  HCM48-SEIKYUMM  PIC X(2).
              03  HCM48-SEIKYUDD  PIC X(2).
            02  HCM48-HOSPJYUSYO    PIC X(100).
            02  HCM48-HOSPNAME  PIC X(80).
            02  HCM48-ISYA  PIC X(40).
            02  HCM48-TEL   PIC X(15).
            02  HCM48-BYOSYOSU  PIC X(4).
            02  HCM48-KANANAME  PIC X(50).
            02  HCM48-RENNUM    PIC X(7).
            02  HCM48-HOKENNAME PIC X(60).
            02  HCM48-PAGE  PIC X(5).

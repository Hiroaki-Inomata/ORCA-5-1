        01  HCM54.
            02  HCM54-HAKKOUYY  PIC X(2).
            02  HCM54-HAKKOUMM  PIC X(2).
            02  HCM54-PTNUM PIC X(20).
            02  HCM54-NAME  PIC X(50).
            02  HCM54-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM54-TANJYOYY  PIC X(2).
            02  HCM54-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM54-NENREI    PIC X(3).
            02  HCM54-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM54-JYUSYOUBI-G.
              03  HCM54-JYUSYOUBIYY   PIC X(2).
              03  HCM54-JYUSYOUBIMM   PIC X(2).
              03  HCM54-JYUSYOUBIDD   PIC X(2).
            02  HCM54-SYOSHINBI-G.
              03  HCM54-SYOSHINBIYY   PIC X(2).
              03  HCM54-SYOSHINBIMM   PIC X(2).
              03  HCM54-SYOSHINBIDD   PIC X(2).
            02  HCM54-STR-G.
              03  HCM54-STRYY PIC X(2).
              03  HCM54-STRMM PIC X(2).
              03  HCM54-STRDD PIC X(2).
            02  HCM54-END-G.
              03  HCM54-ENDYY PIC X(2).
              03  HCM54-ENDMM PIC X(2).
              03  HCM54-ENDDD PIC X(2).
            02  HCM54-JITSUNISSU    PIC X(3).
            02  HCM54-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM54-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM54-KHNTEN
                    PIC X(7)    OCCURS  34 TIMES.
            02  HCM54-KAISU
                    PIC X(3)    OCCURS  34 TIMES.
            02  HCM54-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM54-KOUTENG   PIC X(7).
            02  HCM54-YKZTENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM54-YKZTENSUG PIC X(7).
            02  HCM54-SAISINKAISU   PIC X(3).
            02  HCM54-SHIDOKAISU    PIC X(2).
            02  HCM54-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM54-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM54-SHINDANSYOSUU PIC X(3).
            02  HCM54-MEISAISYOSUU  PIC X(3).
            02  HCM54-SHINDANSYOMONEY   PIC X(7).
            02  HCM54-MEISAISYOMONEY    PIC X(7).
            02  HCM54-SRYKBN
                    PIC X(2)    OCCURS  28 TIMES.
            02  HCM54-TEKIYO2
                    PIC X(52)   OCCURS  28 TIMES.
            02  HCM54-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM54-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM54-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM54-TSUIN-TBL.
              03  HCM54-TSUIN-G           OCCURS  2 TIMES.
                04  HCM54-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM54-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM54-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM54-SEIKYUGAKUTOTAL   PIC X(9).
            02  HCM54-SEIKYUG.
              03  HCM54-SEIKYUYY  PIC X(2).
              03  HCM54-SEIKYUMM  PIC X(2).
              03  HCM54-SEIKYUDD  PIC X(2).
            02  HCM54-HOSPJYUSYO    PIC X(100).
            02  HCM54-HOSPNAME  PIC X(80).
            02  HCM54-ISYA  PIC X(40).
            02  HCM54-TEL   PIC X(15).
            02  HCM54-BYOSYOSU  PIC X(4).
            02  HCM54-KANANAME  PIC X(50).
            02  HCM54-RENNUM    PIC X(7).
            02  HCM54-HOKENNAME PIC X(60).
            02  HCM54-PAGE  PIC X(5).

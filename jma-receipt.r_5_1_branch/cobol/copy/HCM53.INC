        01  HCM53.
            02  HCM53-HAKKOUYY  PIC X(2).
            02  HCM53-HAKKOUMM  PIC X(2).
            02  HCM53-PTNUM PIC X(20).
            02  HCM53-NAME  PIC X(50).
            02  HCM53-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM53-TANJYOYY  PIC X(2).
            02  HCM53-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM53-NENREI    PIC X(3).
            02  HCM53-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM53-JYUSYOUBI-G.
              03  HCM53-JYUSYOUBIYY   PIC X(2).
              03  HCM53-JYUSYOUBIMM   PIC X(2).
              03  HCM53-JYUSYOUBIDD   PIC X(2).
            02  HCM53-SYOSHINBI-G.
              03  HCM53-SYOSHINBIYY   PIC X(2).
              03  HCM53-SYOSHINBIMM   PIC X(2).
              03  HCM53-SYOSHINBIDD   PIC X(2).
            02  HCM53-STR-G.
              03  HCM53-STRYY PIC X(2).
              03  HCM53-STRMM PIC X(2).
              03  HCM53-STRDD PIC X(2).
            02  HCM53-END-G.
              03  HCM53-ENDYY PIC X(2).
              03  HCM53-ENDMM PIC X(2).
              03  HCM53-ENDDD PIC X(2).
            02  HCM53-JITSUNISSU    PIC X(3).
            02  HCM53-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM53-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM53-KHNTEN
                    PIC X(7)    OCCURS  34 TIMES.
            02  HCM53-KAISU
                    PIC X(3)    OCCURS  34 TIMES.
            02  HCM53-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM53-KOUTENG   PIC X(7).
            02  HCM53-YKZTENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM53-YKZTENSUG PIC X(7).
            02  HCM53-SAISINKAISU   PIC X(3).
            02  HCM53-SHIDOKAISU    PIC X(2).
            02  HCM53-KINGAKU
                    PIC X(7)    OCCURS  6 TIMES.
            02  HCM53-TEKIYO1
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCM53-SHINDANSYOSUU PIC X(3).
            02  HCM53-MEISAISYOSUU  PIC X(3).
            02  HCM53-SHINDANSYOMONEY   PIC X(7).
            02  HCM53-MEISAISYOMONEY    PIC X(7).
            02  HCM53-SRYKBN
                    PIC X(2)    OCCURS  28 TIMES.
            02  HCM53-TEKIYO2
                    PIC X(52)   OCCURS  28 TIMES.
            02  HCM53-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCM53-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM53-SEIKYUGAKU
                    PIC X(9)    OCCURS  5 TIMES.
            02  HCM53-TSUIN-TBL.
              03  HCM53-TSUIN-G           OCCURS  2 TIMES.
                04  HCM53-TSUIN
                        PIC X(2)    OCCURS  31 TIMES.
              03  HCM53-TSUINMM
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCM53-TSUINSUU
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM53-SEIKYUGAKUTOTAL   PIC X(9).
            02  HCM53-SEIKYUG.
              03  HCM53-SEIKYUYY  PIC X(2).
              03  HCM53-SEIKYUMM  PIC X(2).
              03  HCM53-SEIKYUDD  PIC X(2).
            02  HCM53-HOSPJYUSYO    PIC X(100).
            02  HCM53-HOSPNAME  PIC X(80).
            02  HCM53-ISYA  PIC X(40).
            02  HCM53-TEL   PIC X(15).
            02  HCM53-BYOSYOSU  PIC X(4).
            02  HCM53-KANANAME  PIC X(50).
            02  HCM53-RENNUM    PIC X(7).
            02  HCM53-HOKENNAME PIC X(60).
            02  HCM53-PAGE  PIC X(5).

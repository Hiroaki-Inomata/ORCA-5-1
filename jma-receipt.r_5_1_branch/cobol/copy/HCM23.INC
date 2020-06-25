        01  HCM23.
            02  HCM23-HAKKOUYY  PIC X(2).
            02  HCM23-HAKKOUMM  PIC X(2).
            02  HCM23-PTNUM PIC X(20).
            02  HCM23-NAME  PIC X(26).
            02  HCM23-TANJYOG   PIC X(2)        OCCURS  4 TIMES.
            02  HCM23-TANJYOYY  PIC X(2).
            02  HCM23-SEX   PIC X(2)    OCCURS  2 TIMES.
            02  HCM23-NENREI    PIC X(3).
            02  HCM23-SYOBYO    PIC X(50)       OCCURS  3 TIMES.
            02  HCM23-JYUSYOUBI-G.
              03  HCM23-JYUSYOUBIYY   PIC X(2).
              03  HCM23-JYUSYOUBIMM   PIC X(2).
              03  HCM23-JYUSYOUBIDD   PIC X(2).
            02  HCM23-SYOSHINBI-G.
              03  HCM23-SYOSHINBIYY   PIC X(2).
              03  HCM23-SYOSHINBIMM   PIC X(2).
              03  HCM23-SYOSHINBIDD   PIC X(2).
            02  HCM23-STR-G.
              03  HCM23-STRYY PIC X(2).
              03  HCM23-STRMM PIC X(2).
              03  HCM23-STRDD PIC X(2).
            02  HCM23-END-G.
              03  HCM23-ENDYY PIC X(2).
              03  HCM23-ENDMM PIC X(2).
              03  HCM23-ENDDD PIC X(2).
            02  HCM23-JITSUNISSU    PIC X(3).
            02  HCM23-TENKI PIC X(2)    OCCURS  5 TIMES.
            02  HCM23-SYOSHIN   PIC X(2)        OCCURS  4 TIMES.
            02  HCM23-KHNTEN    PIC X(7)        OCCURS  32 TIMES.
            02  HCM23-KAISU PIC X(3)    OCCURS  32 TIMES.
            02  HCM23-KOUTEN    PIC X(7)        OCCURS  33 TIMES.
            02  HCM23-KOUTENG   PIC X(7).
            02  HCM23-YKZTENSU  PIC X(7)        OCCURS  33 TIMES.
            02  HCM23-YKZTENSUG PIC X(7).
            02  HCM23-SAISINKAISU   PIC X(2).
            02  HCM23-SHIDOKAISU    PIC X(2).
            02  HCM23-KINGAKU   PIC X(7)        OCCURS  6 TIMES.
            02  HCM23-TEKIYO1   PIC X(24)       OCCURS  6 TIMES.
            02  HCM23-SHINDANSYOSUU PIC X(3).
            02  HCM23-MEISAISYOSUU  PIC X(3).
            02  HCM23-SHINDANSYOMONEY   PIC X(7).
            02  HCM23-MEISAISYOMONEY    PIC X(7).
            02  HCM23-SRYKBN    PIC X(2)        OCCURS  26 TIMES.
            02  HCM23-TEKIYO2   PIC X(52)       OCCURS  26 TIMES.
            02  HCM23-SEIKYU    PIC X(2)        OCCURS  2 TIMES.
            02  HCM23-SEIKYUGAKU    PIC X(9)    OCCURS  5 TIMES.
            02  HCM23-TSUIN-TBL.
              03  HCM23-TSUIN-G           OCCURS  2 TIMES.
                04  HCM23-TSUIN PIC X(2)    OCCURS  31 TIMES.
              03  HCM23-TSUINMM   PIC X(2)        OCCURS  2 TIMES.
              03  HCM23-TSUINSUU  PIC X(2)        OCCURS  2 TIMES.
            02  HCM23-SEIKYUGAKUTOTAL   PIC X(9).
            02  HCM23-SEIKYU-G.
              03  HCM23-SEIKYUYY  PIC X(2).
              03  HCM23-SEIKYUMM  PIC X(2).
              03  HCM23-SEIKYUDD  PIC X(2).
            02  HCM23-HOSPJYUSYO    PIC X(40).
            02  HCM23-HOSPNAME  PIC X(30).
            02  HCM23-ISYA  PIC X(40).
            02  HCM23-TEL   PIC X(15).
            02  HCM23-BYOSYOSU  PIC X(4).
            02  HCM23-KANANAME  PIC X(30).
            02  HCM23-RENNUM    PIC X(7).

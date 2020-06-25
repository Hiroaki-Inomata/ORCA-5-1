        01  HCN23.
            02  HCN23-HAKKOUYY  PIC X(2).
            02  HCN23-HAKKOUMM  PIC X(2).
            02  HCN23-PTNUM PIC X(20).
            02  HCN23-RENNUM    PIC X(7).
            02  HCN23-KANANAME  PIC X(30).
            02  HCN23-NAME  PIC X(26).
            02  HCN23-TANJYOG   PIC X(2)        OCCURS  4 TIMES.
            02  HCN23-TANJYOYY  PIC X(2).
            02  HCN23-SEX   PIC X(2)    OCCURS  2 TIMES.
            02  HCN23-NENREI    PIC X(3).
            02  HCN23-SYOBYO    PIC X(50)       OCCURS  3 TIMES.
            02  HCN23-JYUSYOUBI-G.
              03  HCN23-JYUSYOUBIYY   PIC X(2).
              03  HCN23-JYUSYOUBIMM   PIC X(2).
              03  HCN23-JYUSYOUBIDD   PIC X(2).
            02  HCN23-SYOSHINBI-G.
              03  HCN23-SYOSHINBIYY   PIC X(2).
              03  HCN23-SYOSHINBIMM   PIC X(2).
              03  HCN23-SYOSHINBIDD   PIC X(2).
            02  HCN23-STR-G.
              03  HCN23-STRYY PIC X(2).
              03  HCN23-STRMM PIC X(2).
              03  HCN23-STRDD PIC X(2).
            02  HCN23-END-G.
              03  HCN23-ENDYY PIC X(2).
              03  HCN23-ENDMM PIC X(2).
              03  HCN23-ENDDD PIC X(2).
            02  HCN23-JITSUNISSU    PIC X(3).
            02  HCN23-TENKI PIC X(2)    OCCURS  5 TIMES.
            02  HCN23-SYOSHIN   PIC X(2)        OCCURS  4 TIMES.
            02  HCN23-KHNTEN    PIC X(7)        OCCURS  21 TIMES.
            02  HCN23-KAISU PIC X(3)    OCCURS  21 TIMES.
            02  HCN23-KOUTEN    PIC X(7)        OCCURS  22 TIMES.
            02  HCN23-YKZTENSU  PIC X(7)        OCCURS  22 TIMES.
            02  HCN23-NYUYMD.
              03  HCN23-NYUY  PIC X(2).
              03  HCN23-NYUM  PIC X(2).
              03  HCN23-NYUD  PIC X(2).
            02  HCN23-HOSPSBT   PIC X(2)        OCCURS  3 TIMES.
            02  HCN23-NYUINSYUBETU  PIC X(10)   OCCURS  9 TIMES.
            02  HCN23-NYUKHNTEN PIC X(7)        OCCURS  9 TIMES.
            02  HCN23-NYUKHNTENN    PIC X(10)   OCCURS  9 TIMES.
            02  HCN23-KAKERU    PIC X(2)        OCCURS  5 TIMES.
            02  HCN23-NYUKAISU  PIC X(3)        OCCURS  9 TIMES.
            02  HCN23-NICHI PIC X(4)    OCCURS  5 TIMES.
            02  HCN23-NYUKOUTEN PIC X(7)        OCCURS  11 TIMES.
            02  HCN23-NYUKOUTENN-G      OCCURS  11 TIMES.
              03  HCN23-NYUKOUTENN    PIC X(2)    OCCURS  6 TIMES.
            02  HCN23-NYUYKZTENSU   PIC X(7)    OCCURS  11 TIMES.
            02  HCN23-NYUYKZTENSUN-G            OCCURS  11 TIMES.
              03  HCN23-NYUYKZTENSUN  PIC X(2)    OCCURS  6 TIMES.
            02  HCN23-KOUTENG   PIC X(7).
            02  HCN23-YKZTENSUG PIC X(7).
            02  HCN23-SONOTANIN PIC X(2).
            02  HCN23-SONOTAKAISU   PIC X(2).
            02  HCN23-SONOTA    PIC X(16)       OCCURS  2 TIMES.
            02  HCN23-KINGAKU   PIC X(7)        OCCURS  8 TIMES.
            02  HCN23-KIJYUN    PIC X(56).
            02  HCN23-SYOKBN    PIC X(2)        OCCURS  4 TIMES.
            02  HCN23-SYOKAKERU PIC X(4).
            02  HCN23-SYONICHI  PIC X(4).
            02  HCN23-SYOKHNTEN PIC X(7)        OCCURS  4 TIMES.
            02  HCN23-SYOKAISU  PIC X(3)        OCCURS  4 TIMES.
            02  HCN23-SYOKINGAKU    PIC X(7).
            02  HCN23-TEKIYO1   PIC X(24)       OCCURS  8 TIMES.
            02  HCN23-BIKOU PIC X(24)   OCCURS  6 TIMES.
            02  HCN23-SHINDANSYOSUU PIC X(3).
            02  HCN23-MEISAISYOSUU  PIC X(3).
            02  HCN23-SHINDANSYOMONEY   PIC X(7).
            02  HCN23-MEISAISYOMONEY    PIC X(7).
            02  HCN23-SRYKBN    PIC X(2)        OCCURS  20 TIMES.
            02  HCN23-TEKIYO2   PIC X(52)       OCCURS  20 TIMES.
            02  HCN23-SEIKYU    PIC X(2)        OCCURS  2 TIMES.
            02  HCN23-SEIKYUGAKU    PIC X(9)    OCCURS  5 TIMES.
            02  HCN23-SEIKYUGAKUTOTAL   PIC X(9).
            02  HCN23-SEIKYU-G.
              03  HCN23-SEIKYUYY  PIC X(2).
              03  HCN23-SEIKYUMM  PIC X(2).
              03  HCN23-SEIKYUDD  PIC X(2).
            02  HCN23-HOSPJYUSYO    PIC X(80).
            02  HCN23-HOSPNAME  PIC X(80).
            02  HCN23-ISYA  PIC X(40).
            02  HCN23-TEL   PIC X(15).
            02  HCN23-BYOSYOSU  PIC X(4).

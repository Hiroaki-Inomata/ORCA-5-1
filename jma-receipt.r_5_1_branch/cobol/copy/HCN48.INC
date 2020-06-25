        01  HCN48.
            02  HCN48-HAKKOUYY  PIC X(2).
            02  HCN48-HAKKOUMM  PIC X(2).
            02  HCN48-PTNUM PIC X(20).
            02  HCN48-RENNUM    PIC X(7).
            02  HCN48-KANANAME  PIC X(50).
            02  HCN48-NAME  PIC X(50).
            02  HCN48-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN48-TANJYOYY  PIC X(2).
            02  HCN48-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN48-NENREI    PIC X(3).
            02  HCN48-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN48-JYUSYOUBI-G.
              03  HCN48-JYUSYOUBIYY   PIC X(2).
              03  HCN48-JYUSYOUBIMM   PIC X(2).
              03  HCN48-JYUSYOUBIDD   PIC X(2).
            02  HCN48-SYOSHINBI-G.
              03  HCN48-SYOSHINBIYY   PIC X(2).
              03  HCN48-SYOSHINBIMM   PIC X(2).
              03  HCN48-SYOSHINBIDD   PIC X(2).
            02  HCN48-STR-G.
              03  HCN48-STRYY PIC X(2).
              03  HCN48-STRMM PIC X(2).
              03  HCN48-STRDD PIC X(2).
            02  HCN48-END-G.
              03  HCN48-ENDYY PIC X(2).
              03  HCN48-ENDMM PIC X(2).
              03  HCN48-ENDDD PIC X(2).
            02  HCN48-JITSUNISSU    PIC X(3).
            02  HCN48-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN48-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN48-KHNTEN
                    PIC X(7)    OCCURS  21 TIMES.
            02  HCN48-KAISU
                    PIC X(3)    OCCURS  21 TIMES.
            02  HCN48-KOUTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN48-YKZTENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN48-NYUYMD.
              03  HCN48-NYUY  PIC X(2).
              03  HCN48-NYUM  PIC X(2).
              03  HCN48-NYUD  PIC X(2).
            02  HCN48-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN48-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN48-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN48-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN48-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN48-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN48-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN48-NYUKOUTEN
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN48-NYUKOUTENN-G      OCCURS  11 TIMES.
              03  HCN48-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN48-NYUYKZTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN48-NYUYKZTENSUN-G            OCCURS  11 TIMES.
              03  HCN48-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN48-KOUTENG   PIC X(9).
            02  HCN48-YKZTENSUG PIC X(9).
            02  HCN48-SONOTANIN PIC X(2).
            02  HCN48-SONOTAKAISU   PIC X(2).
            02  HCN48-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN48-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN48-KIJYUN    PIC X(56).
            02  HCN48-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN48-SYOKAKERU PIC X(4).
            02  HCN48-SYONICHI  PIC X(4).
            02  HCN48-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN48-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN48-SYOKINGAKU    PIC X(7).
            02  HCN48-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN48-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN48-SHINDANSYOSUU PIC X(3).
            02  HCN48-MEISAISYOSUU  PIC X(3).
            02  HCN48-SHINDANSYOMONEY   PIC X(7).
            02  HCN48-MEISAISYOMONEY    PIC X(7).
            02  HCN48-SRYKBN
                    PIC X(2)    OCCURS  20 TIMES.
            02  HCN48-TEKIYO2
                    PIC X(52)   OCCURS  20 TIMES.
            02  HCN48-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN48-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN48-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN48-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCN48-SEIKYU-G.
              03  HCN48-SEIKYUYY  PIC X(2).
              03  HCN48-SEIKYUMM  PIC X(2).
              03  HCN48-SEIKYUDD  PIC X(2).
            02  HCN48-HOSPJYUSYO    PIC X(100).
            02  HCN48-HOSPNAME  PIC X(80).
            02  HCN48-ISYA  PIC X(40).
            02  HCN48-TEL   PIC X(15).
            02  HCN48-BYOSYOSU  PIC X(4).
            02  HCN48-HOKENNAME PIC X(60).
            02  HCN48-PAGE  PIC X(5).

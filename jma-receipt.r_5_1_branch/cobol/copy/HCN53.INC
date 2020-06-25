        01  HCN53.
            02  HCN53-HAKKOUYY  PIC X(2).
            02  HCN53-HAKKOUMM  PIC X(2).
            02  HCN53-PTNUM PIC X(20).
            02  HCN53-RENNUM    PIC X(7).
            02  HCN53-KANANAME  PIC X(50).
            02  HCN53-NAME  PIC X(50).
            02  HCN53-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53-TANJYOYY  PIC X(2).
            02  HCN53-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53-NENREI    PIC X(3).
            02  HCN53-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN53-JYUSYOUBI-G.
              03  HCN53-JYUSYOUBIYY   PIC X(2).
              03  HCN53-JYUSYOUBIMM   PIC X(2).
              03  HCN53-JYUSYOUBIDD   PIC X(2).
            02  HCN53-SYOSHINBI-G.
              03  HCN53-SYOSHINBIYY   PIC X(2).
              03  HCN53-SYOSHINBIMM   PIC X(2).
              03  HCN53-SYOSHINBIDD   PIC X(2).
            02  HCN53-STR-G.
              03  HCN53-STRYY PIC X(2).
              03  HCN53-STRMM PIC X(2).
              03  HCN53-STRDD PIC X(2).
            02  HCN53-END-G.
              03  HCN53-ENDYY PIC X(2).
              03  HCN53-ENDMM PIC X(2).
              03  HCN53-ENDDD PIC X(2).
            02  HCN53-JITSUNISSU    PIC X(3).
            02  HCN53-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53-SYOSHIN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53-KHNTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN53-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN53-KOUTEN
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53-YKZTENSU
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53-NYUYMD.
              03  HCN53-NYUY  PIC X(2).
              03  HCN53-NYUM  PIC X(2).
              03  HCN53-NYUD  PIC X(2).
            02  HCN53-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN53-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN53-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN53-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53-NYUKOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53-NYUKOUTEN-G       OCCURS  10 TIMES.
              03  HCN53-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53-NYUYKZTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53-NYUYKZTENSU-G     OCCURS  10 TIMES.
              03  HCN53-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53-KOUTENG   PIC X(9).
            02  HCN53-YKZTENSUG PIC X(9).
            02  HCN53-SONOTANIN PIC X(2).
            02  HCN53-SONOTAKAISU   PIC X(2).
            02  HCN53-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN53-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN53-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53-SYOKAKERU PIC X(4).
            02  HCN53-SYONICHI  PIC X(4).
            02  HCN53-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN53-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN53-SYOKINGAKU    PIC X(7).
            02  HCN53-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN53-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN53-SHINDANSYOSUU PIC X(3).
            02  HCN53-MEISAISYOSUU  PIC X(3).
            02  HCN53-SHINDANSYOMONEY   PIC X(7).
            02  HCN53-MEISAISYOMONEY    PIC X(7).
            02  HCN53-SRYKBN
                    PIC X(2)    OCCURS  21 TIMES.
            02  HCN53-TEKIYO2
                    PIC X(52)   OCCURS  21 TIMES.
            02  HCN53-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN53-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN53-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCN53-SEIKYU-G.
              03  HCN53-SEIKYUYY  PIC X(2).
              03  HCN53-SEIKYUMM  PIC X(2).
              03  HCN53-SEIKYUDD  PIC X(2).
            02  HCN53-HOSPJYUSYO    PIC X(100).
            02  HCN53-HOSPNAME  PIC X(80).
            02  HCN53-ISYA  PIC X(40).
            02  HCN53-TEL   PIC X(15).
            02  HCN53-BYOSYOSU  PIC X(4).
            02  HCN53-HOKENNAME PIC X(60).
            02  HCN53-PAGE  PIC X(5).

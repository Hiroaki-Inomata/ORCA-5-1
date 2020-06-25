        01  HCN54.
            02  HCN54-HAKKOUYY  PIC X(2).
            02  HCN54-HAKKOUMM  PIC X(2).
            02  HCN54-PTNUM PIC X(20).
            02  HCN54-RENNUM    PIC X(7).
            02  HCN54-KANANAME  PIC X(50).
            02  HCN54-NAME  PIC X(50).
            02  HCN54-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN54-TANJYOYY  PIC X(2).
            02  HCN54-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN54-NENREI    PIC X(3).
            02  HCN54-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN54-JYUSYOUBI-G.
              03  HCN54-JYUSYOUBIYY   PIC X(2).
              03  HCN54-JYUSYOUBIMM   PIC X(2).
              03  HCN54-JYUSYOUBIDD   PIC X(2).
            02  HCN54-SYOSHINBI-G.
              03  HCN54-SYOSHINBIYY   PIC X(2).
              03  HCN54-SYOSHINBIMM   PIC X(2).
              03  HCN54-SYOSHINBIDD   PIC X(2).
            02  HCN54-STR-G.
              03  HCN54-STRYY PIC X(2).
              03  HCN54-STRMM PIC X(2).
              03  HCN54-STRDD PIC X(2).
            02  HCN54-END-G.
              03  HCN54-ENDYY PIC X(2).
              03  HCN54-ENDMM PIC X(2).
              03  HCN54-ENDDD PIC X(2).
            02  HCN54-JITSUNISSU    PIC X(3).
            02  HCN54-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN54-SYOSHIN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN54-KHNTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN54-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN54-KOUTEN
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN54-YKZTENSU
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN54-NYUYMD.
              03  HCN54-NYUY  PIC X(2).
              03  HCN54-NYUM  PIC X(2).
              03  HCN54-NYUD  PIC X(2).
            02  HCN54-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN54-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN54-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN54-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN54-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN54-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN54-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN54-NYUKOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN54-NYUKOUTEN-G       OCCURS  10 TIMES.
              03  HCN54-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN54-NYUYKZTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN54-NYUYKZTENSU-G     OCCURS  10 TIMES.
              03  HCN54-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN54-KOUTENG   PIC X(9).
            02  HCN54-YKZTENSUG PIC X(9).
            02  HCN54-SONOTANIN PIC X(2).
            02  HCN54-SONOTAKAISU   PIC X(2).
            02  HCN54-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN54-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN54-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN54-SYOKAKERU PIC X(4).
            02  HCN54-SYONICHI  PIC X(4).
            02  HCN54-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN54-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN54-SYOKINGAKU    PIC X(7).
            02  HCN54-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN54-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN54-SHINDANSYOSUU PIC X(3).
            02  HCN54-MEISAISYOSUU  PIC X(3).
            02  HCN54-SHINDANSYOMONEY   PIC X(7).
            02  HCN54-MEISAISYOMONEY    PIC X(7).
            02  HCN54-SRYKBN
                    PIC X(2)    OCCURS  21 TIMES.
            02  HCN54-TEKIYO2
                    PIC X(52)   OCCURS  21 TIMES.
            02  HCN54-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN54-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN54-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN54-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCN54-SEIKYU-G.
              03  HCN54-SEIKYUYY  PIC X(2).
              03  HCN54-SEIKYUMM  PIC X(2).
              03  HCN54-SEIKYUDD  PIC X(2).
            02  HCN54-HOSPJYUSYO    PIC X(100).
            02  HCN54-HOSPNAME  PIC X(80).
            02  HCN54-ISYA  PIC X(40).
            02  HCN54-TEL   PIC X(15).
            02  HCN54-BYOSYOSU  PIC X(4).
            02  HCN54-HOKENNAME PIC X(60).
            02  HCN54-PAGE  PIC X(5).

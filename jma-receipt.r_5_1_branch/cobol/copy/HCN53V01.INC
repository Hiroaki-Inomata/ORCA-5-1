        01  HCN53V01.
            02  HCN53V01-HAKKOUYY   PIC X(2).
            02  HCN53V01-HAKKOUMM   PIC X(2).
            02  HCN53V01-PTNUM  PIC X(20).
            02  HCN53V01-RENNUM PIC X(7).
            02  HCN53V01-KANANAME   PIC X(50).
            02  HCN53V01-NAME   PIC X(50).
            02  HCN53V01-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53V01-TANJYOYY   PIC X(2).
            02  HCN53V01-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53V01-NENREI PIC X(3).
            02  HCN53V01-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN53V01-JYUSYOUBI-G.
              03  HCN53V01-JYUSYOUBIYY    PIC X(2).
              03  HCN53V01-JYUSYOUBIMM    PIC X(2).
              03  HCN53V01-JYUSYOUBIDD    PIC X(2).
            02  HCN53V01-SYOSHINBI-G.
              03  HCN53V01-SYOSHINBIYY    PIC X(2).
              03  HCN53V01-SYOSHINBIMM    PIC X(2).
              03  HCN53V01-SYOSHINBIDD    PIC X(2).
            02  HCN53V01-STR-G.
              03  HCN53V01-STRYY  PIC X(2).
              03  HCN53V01-STRMM  PIC X(2).
              03  HCN53V01-STRDD  PIC X(2).
            02  HCN53V01-END-G.
              03  HCN53V01-ENDYY  PIC X(2).
              03  HCN53V01-ENDMM  PIC X(2).
              03  HCN53V01-ENDDD  PIC X(2).
            02  HCN53V01-JITSUNISSU PIC X(3).
            02  HCN53V01-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53V01-SYOSHIN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53V01-KHNTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN53V01-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN53V01-KOUTEN
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53V01-YKZTENSU
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53V01-NYUYMD.
              03  HCN53V01-NYUY   PIC X(2).
              03  HCN53V01-NYUM   PIC X(2).
              03  HCN53V01-NYUD   PIC X(2).
            02  HCN53V01-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN53V01-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53V01-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN53V01-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53V01-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53V01-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN53V01-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53V01-NYUKOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53V01-NYUKOUTEN-G            OCCURS  10 TIMES.
              03  HCN53V01-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53V01-NYUYKZTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53V01-NYUYKZTENSU-G          OCCURS  10 TIMES.
              03  HCN53V01-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53V01-KOUTENG    PIC X(9).
            02  HCN53V01-YKZTENSUG  PIC X(9).
            02  HCN53V01-SONOTANIN  PIC X(2).
            02  HCN53V01-SONOTAKAISU    PIC X(2).
            02  HCN53V01-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN53V01-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN53V01-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53V01-SYOKAKERU  PIC X(4).
            02  HCN53V01-SYONICHI   PIC X(4).
            02  HCN53V01-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN53V01-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN53V01-SYOKINGAKU PIC X(7).
            02  HCN53V01-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN53V01-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN53V01-SHINDANSYOSUU  PIC X(3).
            02  HCN53V01-MEISAISYOSUU   PIC X(3).
            02  HCN53V01-SHINDANSYOMONEY    PIC X(7).
            02  HCN53V01-MEISAISYOMONEY PIC X(7).
            02  HCN53V01-SRYKBN
                    PIC X(2)    OCCURS  21 TIMES.
            02  HCN53V01-TEKIYO2
                    PIC X(52)   OCCURS  21 TIMES.
            02  HCN53V01-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN53V01-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53V01-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN53V01-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN53V01-SEIKYU-G.
              03  HCN53V01-SEIKYUYY   PIC X(2).
              03  HCN53V01-SEIKYUMM   PIC X(2).
              03  HCN53V01-SEIKYUDD   PIC X(2).
            02  HCN53V01-HOSPJYUSYO PIC X(100).
            02  HCN53V01-HOSPNAME   PIC X(80).
            02  HCN53V01-ISYA   PIC X(40).
            02  HCN53V01-TEL    PIC X(15).
            02  HCN53V01-BYOSYOSU   PIC X(4).
            02  HCN53V01-HOKENNAME  PIC X(60).
            02  HCN53V01-PAGE   PIC X(5).
            02  HCN53V01-LABEL  PIC X(14).

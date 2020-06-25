        01  HCN53V02.
            02  HCN53V02-HAKKOUYY   PIC X(2).
            02  HCN53V02-HAKKOUMM   PIC X(2).
            02  HCN53V02-PTNUM  PIC X(20).
            02  HCN53V02-RENNUM PIC X(7).
            02  HCN53V02-KANANAME   PIC X(50).
            02  HCN53V02-NAME   PIC X(50).
            02  HCN53V02-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53V02-TANJYOYY   PIC X(2).
            02  HCN53V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53V02-NENREI PIC X(3).
            02  HCN53V02-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN53V02-JYUSYOUBI-G.
              03  HCN53V02-JYUSYOUBIYY    PIC X(2).
              03  HCN53V02-JYUSYOUBIMM    PIC X(2).
              03  HCN53V02-JYUSYOUBIDD    PIC X(2).
            02  HCN53V02-SYOSHINBI-G.
              03  HCN53V02-SYOSHINBIYY    PIC X(2).
              03  HCN53V02-SYOSHINBIMM    PIC X(2).
              03  HCN53V02-SYOSHINBIDD    PIC X(2).
            02  HCN53V02-STR-G.
              03  HCN53V02-STRYY  PIC X(2).
              03  HCN53V02-STRMM  PIC X(2).
              03  HCN53V02-STRDD  PIC X(2).
            02  HCN53V02-END-G.
              03  HCN53V02-ENDYY  PIC X(2).
              03  HCN53V02-ENDMM  PIC X(2).
              03  HCN53V02-ENDDD  PIC X(2).
            02  HCN53V02-JITSUNISSU PIC X(3).
            02  HCN53V02-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53V02-SYOSHIN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN53V02-KHNTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN53V02-KAISU
                    PIC X(3)    OCCURS  22 TIMES.
            02  HCN53V02-KOUTEN
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53V02-YKZTENSU
                    PIC X(7)    OCCURS  23 TIMES.
            02  HCN53V02-NYUYMD.
              03  HCN53V02-NYUY   PIC X(2).
              03  HCN53V02-NYUM   PIC X(2).
              03  HCN53V02-NYUD   PIC X(2).
            02  HCN53V02-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN53V02-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53V02-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN53V02-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN53V02-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN53V02-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN53V02-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53V02-NYUKOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53V02-NYUKOUTEN-G            OCCURS  10 TIMES.
              03  HCN53V02-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53V02-NYUYKZTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN53V02-NYUYKZTENSU-G          OCCURS  10 TIMES.
              03  HCN53V02-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN53V02-KOUTENG    PIC X(9).
            02  HCN53V02-YKZTENSUG  PIC X(9).
            02  HCN53V02-SONOTANIN  PIC X(2).
            02  HCN53V02-SONOTAKAISU    PIC X(2).
            02  HCN53V02-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN53V02-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN53V02-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN53V02-SYOKAKERU  PIC X(4).
            02  HCN53V02-SYONICHI   PIC X(4).
            02  HCN53V02-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN53V02-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN53V02-SYOKINGAKU PIC X(7).
            02  HCN53V02-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN53V02-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN53V02-SHINDANSYOSUU  PIC X(3).
            02  HCN53V02-MEISAISYOSUU   PIC X(3).
            02  HCN53V02-SHINDANSYOMONEY    PIC X(7).
            02  HCN53V02-MEISAISYOMONEY PIC X(7).
            02  HCN53V02-SRYKBN
                    PIC X(2)    OCCURS  21 TIMES.
            02  HCN53V02-TEKIYO2
                    PIC X(52)   OCCURS  21 TIMES.
            02  HCN53V02-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN53V02-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN53V02-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN53V02-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN53V02-SEIKYU-G.
              03  HCN53V02-SEIKYUYY   PIC X(2).
              03  HCN53V02-SEIKYUMM   PIC X(2).
              03  HCN53V02-SEIKYUDD   PIC X(2).
            02  HCN53V02-HOSPJYUSYO PIC X(100).
            02  HCN53V02-HOSPNAME   PIC X(80).
            02  HCN53V02-ISYA   PIC X(40).
            02  HCN53V02-TEL    PIC X(15).
            02  HCN53V02-BYOSYOSU   PIC X(4).
            02  HCN53V02-HOKENNAME  PIC X(60).
            02  HCN53V02-PAGE   PIC X(5).
            02  HCN53V02-LABEL  PIC X(14).
            02  HCN53V02-LOPTION    PIC X(100).

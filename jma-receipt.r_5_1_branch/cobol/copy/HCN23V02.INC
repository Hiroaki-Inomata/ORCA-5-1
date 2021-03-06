        01  HCN23V02.
            02  HCN23V02-HAKKOUYY   PIC X(2).
            02  HCN23V02-HAKKOUMM   PIC X(2).
            02  HCN23V02-PTNUM  PIC X(20).
            02  HCN23V02-RENNUM PIC X(7).
            02  HCN23V02-KANANAME   PIC X(50).
            02  HCN23V02-NAME   PIC X(50).
            02  HCN23V02-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V02-TANJYOYY   PIC X(2).
            02  HCN23V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN23V02-NENREI PIC X(3).
            02  HCN23V02-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN23V02-JYUSYOUBI-G.
              03  HCN23V02-JYUSYOUBIYY    PIC X(2).
              03  HCN23V02-JYUSYOUBIMM    PIC X(2).
              03  HCN23V02-JYUSYOUBIDD    PIC X(2).
            02  HCN23V02-SYOSHINBI-G.
              03  HCN23V02-SYOSHINBIYY    PIC X(2).
              03  HCN23V02-SYOSHINBIMM    PIC X(2).
              03  HCN23V02-SYOSHINBIDD    PIC X(2).
            02  HCN23V02-STR-G.
              03  HCN23V02-STRYY  PIC X(2).
              03  HCN23V02-STRMM  PIC X(2).
              03  HCN23V02-STRDD  PIC X(2).
            02  HCN23V02-END-G.
              03  HCN23V02-ENDYY  PIC X(2).
              03  HCN23V02-ENDMM  PIC X(2).
              03  HCN23V02-ENDDD  PIC X(2).
            02  HCN23V02-JITSUNISSU PIC X(3).
            02  HCN23V02-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN23V02-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V02-KHNTEN
                    PIC X(7)    OCCURS  21 TIMES.
            02  HCN23V02-KAISU
                    PIC X(3)    OCCURS  21 TIMES.
            02  HCN23V02-KOUTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN23V02-YKZTENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN23V02-NYUYMD.
              03  HCN23V02-NYUY   PIC X(2).
              03  HCN23V02-NYUM   PIC X(2).
              03  HCN23V02-NYUD   PIC X(2).
            02  HCN23V02-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN23V02-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN23V02-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN23V02-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN23V02-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN23V02-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN23V02-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN23V02-NYUKOUTEN
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN23V02-NYUKOUTENN-G           OCCURS  11 TIMES.
              03  HCN23V02-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN23V02-NYUYKZTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN23V02-NYUYKZTENSUN-G         OCCURS  11 TIMES.
              03  HCN23V02-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN23V02-KOUTENG    PIC X(9).
            02  HCN23V02-YKZTENSUG  PIC X(9).
            02  HCN23V02-SONOTANIN  PIC X(2).
            02  HCN23V02-SONOTAKAISU    PIC X(2).
            02  HCN23V02-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN23V02-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN23V02-KIJYUN PIC X(56).
            02  HCN23V02-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V02-SYOKAKERU  PIC X(4).
            02  HCN23V02-SYONICHI   PIC X(4).
            02  HCN23V02-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN23V02-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN23V02-SYOKINGAKU PIC X(7).
            02  HCN23V02-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN23V02-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN23V02-SHINDANSYOSUU  PIC X(3).
            02  HCN23V02-MEISAISYOSUU   PIC X(3).
            02  HCN23V02-SHINDANSYOMONEY    PIC X(7).
            02  HCN23V02-MEISAISYOMONEY PIC X(7).
            02  HCN23V02-SRYKBN
                    PIC X(2)    OCCURS  20 TIMES.
            02  HCN23V02-TEKIYO2
                    PIC X(52)   OCCURS  20 TIMES.
            02  HCN23V02-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN23V02-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN23V02-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN23V02-SEIKYU-G.
              03  HCN23V02-SEIKYUYY   PIC X(2).
              03  HCN23V02-SEIKYUMM   PIC X(2).
              03  HCN23V02-SEIKYUDD   PIC X(2).
            02  HCN23V02-HOSPJYUSYO PIC X(80).
            02  HCN23V02-HOSPNAME   PIC X(80).
            02  HCN23V02-ISYA   PIC X(40).
            02  HCN23V02-TEL    PIC X(15).
            02  HCN23V02-BYOSYOSU   PIC X(4).

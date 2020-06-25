        01  HCN23V05.
            02  HCN23V05-HAKKOUYY   PIC X(2).
            02  HCN23V05-HAKKOUMM   PIC X(2).
            02  HCN23V05-PTNUM  PIC X(20).
            02  HCN23V05-RENNUM PIC X(7).
            02  HCN23V05-KANANAME   PIC X(50).
            02  HCN23V05-NAME   PIC X(50).
            02  HCN23V05-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V05-TANJYOYY   PIC X(2).
            02  HCN23V05-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN23V05-NENREI PIC X(3).
            02  HCN23V05-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN23V05-JYUSYOUBI-G.
              03  HCN23V05-JYUSYOUBIYY    PIC X(2).
              03  HCN23V05-JYUSYOUBIMM    PIC X(2).
              03  HCN23V05-JYUSYOUBIDD    PIC X(2).
            02  HCN23V05-SYOSHINBI-G.
              03  HCN23V05-SYOSHINBIYY    PIC X(2).
              03  HCN23V05-SYOSHINBIMM    PIC X(2).
              03  HCN23V05-SYOSHINBIDD    PIC X(2).
            02  HCN23V05-STR-G.
              03  HCN23V05-STRYY  PIC X(2).
              03  HCN23V05-STRMM  PIC X(2).
              03  HCN23V05-STRDD  PIC X(2).
            02  HCN23V05-END-G.
              03  HCN23V05-ENDYY  PIC X(2).
              03  HCN23V05-ENDMM  PIC X(2).
              03  HCN23V05-ENDDD  PIC X(2).
            02  HCN23V05-JITSUNISSU PIC X(3).
            02  HCN23V05-TENKI
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN23V05-SYOSHIN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V05-KHNTEN
                    PIC X(7)    OCCURS  21 TIMES.
            02  HCN23V05-KAISU
                    PIC X(3)    OCCURS  21 TIMES.
            02  HCN23V05-KOUTEN
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN23V05-YKZTENSU
                    PIC X(7)    OCCURS  22 TIMES.
            02  HCN23V05-NYUYMD.
              03  HCN23V05-NYUY   PIC X(2).
              03  HCN23V05-NYUM   PIC X(2).
              03  HCN23V05-NYUD   PIC X(2).
            02  HCN23V05-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN23V05-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN23V05-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN23V05-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN23V05-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN23V05-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN23V05-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN23V05-NYUKOUTEN
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN23V05-NYUKOUTENN-G           OCCURS  11 TIMES.
              03  HCN23V05-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN23V05-NYUYKZTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN23V05-NYUYKZTENSUN-G         OCCURS  11 TIMES.
              03  HCN23V05-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN23V05-KOUTENG    PIC X(9).
            02  HCN23V05-YKZTENSUG  PIC X(9).
            02  HCN23V05-SONOTANIN  PIC X(2).
            02  HCN23V05-SONOTAKAISU    PIC X(2).
            02  HCN23V05-SONOTA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCN23V05-KINGAKU
                    PIC X(7)    OCCURS  8 TIMES.
            02  HCN23V05-KIJYUN PIC X(56).
            02  HCN23V05-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN23V05-SYOKAKERU  PIC X(4).
            02  HCN23V05-SYONICHI   PIC X(4).
            02  HCN23V05-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN23V05-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN23V05-SYOKINGAKU PIC X(7).
            02  HCN23V05-TEKIYO1
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN23V05-BIKOU
                    PIC X(24)   OCCURS  6 TIMES.
            02  HCN23V05-SHINDANSYOSUU  PIC X(3).
            02  HCN23V05-MEISAISYOSUU   PIC X(3).
            02  HCN23V05-SHINDANSYOMONEY    PIC X(7).
            02  HCN23V05-MEISAISYOMONEY PIC X(7).
            02  HCN23V05-SRYKBN
                    PIC X(2)    OCCURS  20 TIMES.
            02  HCN23V05-TEKIYO2
                    PIC X(52)   OCCURS  20 TIMES.
            02  HCN23V05-SEIKYU-SEISU
                    PIC X(1)    OCCURS  2 TIMES.
            02  HCN23V05-SEIKYU
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN23V05-SEIKYUGAKU
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCN23V05-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN23V05-SEIKYU-G.
              03  HCN23V05-SEIKYUYY   PIC X(2).
              03  HCN23V05-SEIKYUMM   PIC X(2).
              03  HCN23V05-SEIKYUDD   PIC X(2).
            02  HCN23V05-HOSPJYUSYO PIC X(100).
            02  HCN23V05-HOSPNAME   PIC X(80).
            02  HCN23V05-ISYA   PIC X(40).
            02  HCN23V05-TEL    PIC X(15).
            02  HCN23V05-BYOSYOSU   PIC X(4).
            02  HCN23V05-HOKENNAME  PIC X(60).
            02  HCN23V05-PAGE   PIC X(5).
            02  HCN23V05-LABEL  PIC X(14).

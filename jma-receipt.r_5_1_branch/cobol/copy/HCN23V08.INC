        01  HCN23V08.
            02  HCN23V08-HAKKOUYY
                        PIC X(2).
            02  HCN23V08-HAKKOUMM
                        PIC X(2).
            02  HCN23V08-PTNUM
                        PIC X(20).
            02  HCN23V08-RENNUM
                        PIC X(7).
            02  HCN23V08-KANANAME
                        PIC X(50).
            02  HCN23V08-NAME
                        PIC X(50).
            02  HCN23V08-TANJYOG
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V08-TANJYOYY
                        PIC X(2).
            02  HCN23V08-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN23V08-NENREI
                        PIC X(3).
            02  HCN23V08-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN23V08-JYUSYOUBI-G.
              03  HCN23V08-JYUSYOUBIYY
                        PIC X(2).
              03  HCN23V08-JYUSYOUBIMM
                        PIC X(2).
              03  HCN23V08-JYUSYOUBIDD
                        PIC X(2).
            02  HCN23V08-SYOSHINBI-G.
              03  HCN23V08-SYOSHINBIYY
                        PIC X(2).
              03  HCN23V08-SYOSHINBIMM
                        PIC X(2).
              03  HCN23V08-SYOSHINBIDD
                        PIC X(2).
            02  HCN23V08-STR-G.
              03  HCN23V08-STRYY
                        PIC X(2).
              03  HCN23V08-STRMM
                        PIC X(2).
              03  HCN23V08-STRDD
                        PIC X(2).
            02  HCN23V08-END-G.
              03  HCN23V08-ENDYY
                        PIC X(2).
              03  HCN23V08-ENDMM
                        PIC X(2).
              03  HCN23V08-ENDDD
                        PIC X(2).
            02  HCN23V08-JITSUNISSU
                        PIC X(3).
            02  HCN23V08-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN23V08-SYOSHIN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V08-KHNTEN
                        PIC X(7)        OCCURS 21 TIMES.
            02  HCN23V08-KAISU
                        PIC X(3)        OCCURS 21 TIMES.
            02  HCN23V08-KOUTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN23V08-YKZTENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN23V08-NYUYMD.
              03  HCN23V08-NYUY
                        PIC X(2).
              03  HCN23V08-NYUM
                        PIC X(2).
              03  HCN23V08-NYUD
                        PIC X(2).
            02  HCN23V08-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN23V08-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN23V08-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN23V08-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN23V08-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN23V08-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN23V08-NICHI
                        PIC X(4)        OCCURS 5 TIMES.
            02  HCN23V08-NYUKOUTEN
                        PIC X(7)        OCCURS 11 TIMES.
            02  HCN23V08-NYUKOUTENN-G
                                        OCCURS 11 TIMES.
              03  HCN23V08-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN23V08-NYUYKZTENSU
                        PIC X(7)        OCCURS 11 TIMES.
            02  HCN23V08-NYUYKZTENSUN-G
                                        OCCURS 11 TIMES.
              03  HCN23V08-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN23V08-KOUTENG
                        PIC X(9).
            02  HCN23V08-YKZTENSUG
                        PIC X(9).
            02  HCN23V08-SONOTANIN
                        PIC X(2).
            02  HCN23V08-SONOTAKAISU
                        PIC X(2).
            02  HCN23V08-SONOTA
                        PIC X(16)       OCCURS 2 TIMES.
            02  HCN23V08-KINGAKU
                        PIC X(9)        OCCURS 8 TIMES.
            02  HCN23V08-KIJYUN
                        PIC X(56).
            02  HCN23V08-SYOKBN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V08-SYOKAKERU
                        PIC X(4).
            02  HCN23V08-SYONICHI
                        PIC X(4).
            02  HCN23V08-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN23V08-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN23V08-SYOKINGAKU
                        PIC X(7).
            02  HCN23V08-TEKIYO1
                        PIC X(24)       OCCURS 8 TIMES.
            02  HCN23V08-BIKOU
                        PIC X(24)       OCCURS 6 TIMES.
            02  HCN23V08-SHINDANSYOSUU
                        PIC X(3).
            02  HCN23V08-MEISAISYOSUU
                        PIC X(3).
            02  HCN23V08-SHINDANSYOMONEY
                        PIC X(7).
            02  HCN23V08-MEISAISYOMONEY
                        PIC X(7).
            02  HCN23V08-SRYKBN
                        PIC X(2)        OCCURS 20 TIMES.
            02  HCN23V08-TEKIYO2
                        PIC X(52)       OCCURS 20 TIMES.
            02  HCN23V08-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN23V08-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN23V08-SEIKYUGAKU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN23V08-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN23V08-SEIKYU-G.
              03  HCN23V08-SEIKYUYY
                        PIC X(2).
              03  HCN23V08-SEIKYUMM
                        PIC X(2).
              03  HCN23V08-SEIKYUDD
                        PIC X(2).
            02  HCN23V08-HOSPJYUSYO
                        PIC X(100).
            02  HCN23V08-HOSPNAME
                        PIC X(80).
            02  HCN23V08-ISYA
                        PIC X(40).
            02  HCN23V08-TEL
                        PIC X(15).
            02  HCN23V08-BYOSYOSU
                        PIC X(4).
            02  HCN23V08-HOKENNAME
                        PIC X(60).
            02  HCN23V08-PAGE
                        PIC X(5).
            02  HCN23V08-LABEL
                        PIC X(14).
            02  HCN23V08-NYUINSYUBETU2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN23V08-LOPTION
                        PIC X(100).

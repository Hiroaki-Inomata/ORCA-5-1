        01  HCN23V07.
            02  HCN23V07-HAKKOUYY
                        PIC X(2).
            02  HCN23V07-HAKKOUMM
                        PIC X(2).
            02  HCN23V07-PTNUM
                        PIC X(20).
            02  HCN23V07-RENNUM
                        PIC X(7).
            02  HCN23V07-KANANAME
                        PIC X(50).
            02  HCN23V07-NAME
                        PIC X(50).
            02  HCN23V07-TANJYOG
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V07-TANJYOYY
                        PIC X(2).
            02  HCN23V07-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN23V07-NENREI
                        PIC X(3).
            02  HCN23V07-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN23V07-JYUSYOUBI-G.
              03  HCN23V07-JYUSYOUBIYY
                        PIC X(2).
              03  HCN23V07-JYUSYOUBIMM
                        PIC X(2).
              03  HCN23V07-JYUSYOUBIDD
                        PIC X(2).
            02  HCN23V07-SYOSHINBI-G.
              03  HCN23V07-SYOSHINBIYY
                        PIC X(2).
              03  HCN23V07-SYOSHINBIMM
                        PIC X(2).
              03  HCN23V07-SYOSHINBIDD
                        PIC X(2).
            02  HCN23V07-STR-G.
              03  HCN23V07-STRYY
                        PIC X(2).
              03  HCN23V07-STRMM
                        PIC X(2).
              03  HCN23V07-STRDD
                        PIC X(2).
            02  HCN23V07-END-G.
              03  HCN23V07-ENDYY
                        PIC X(2).
              03  HCN23V07-ENDMM
                        PIC X(2).
              03  HCN23V07-ENDDD
                        PIC X(2).
            02  HCN23V07-JITSUNISSU
                        PIC X(3).
            02  HCN23V07-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN23V07-SYOSHIN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V07-KHNTEN
                        PIC X(7)        OCCURS 21 TIMES.
            02  HCN23V07-KAISU
                        PIC X(3)        OCCURS 21 TIMES.
            02  HCN23V07-KOUTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN23V07-YKZTENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN23V07-NYUYMD.
              03  HCN23V07-NYUY
                        PIC X(2).
              03  HCN23V07-NYUM
                        PIC X(2).
              03  HCN23V07-NYUD
                        PIC X(2).
            02  HCN23V07-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN23V07-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN23V07-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN23V07-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN23V07-KAKERU
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN23V07-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN23V07-NICHI
                        PIC X(4)        OCCURS 5 TIMES.
            02  HCN23V07-NYUKOUTEN
                        PIC X(7)        OCCURS 11 TIMES.
            02  HCN23V07-NYUKOUTENN-G
                                        OCCURS 11 TIMES.
              03  HCN23V07-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN23V07-NYUYKZTENSU
                        PIC X(7)        OCCURS 11 TIMES.
            02  HCN23V07-NYUYKZTENSUN-G
                                        OCCURS 11 TIMES.
              03  HCN23V07-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN23V07-KOUTENG
                        PIC X(9).
            02  HCN23V07-YKZTENSUG
                        PIC X(9).
            02  HCN23V07-SONOTANIN
                        PIC X(2).
            02  HCN23V07-SONOTAKAISU
                        PIC X(2).
            02  HCN23V07-SONOTA
                        PIC X(16)       OCCURS 2 TIMES.
            02  HCN23V07-KINGAKU
                        PIC X(7)        OCCURS 8 TIMES.
            02  HCN23V07-KIJYUN
                        PIC X(56).
            02  HCN23V07-SYOKBN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN23V07-SYOKAKERU
                        PIC X(4).
            02  HCN23V07-SYONICHI
                        PIC X(4).
            02  HCN23V07-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN23V07-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN23V07-SYOKINGAKU
                        PIC X(7).
            02  HCN23V07-TEKIYO1
                        PIC X(24)       OCCURS 8 TIMES.
            02  HCN23V07-BIKOU
                        PIC X(24)       OCCURS 6 TIMES.
            02  HCN23V07-SHINDANSYOSUU
                        PIC X(3).
            02  HCN23V07-MEISAISYOSUU
                        PIC X(3).
            02  HCN23V07-SHINDANSYOMONEY
                        PIC X(7).
            02  HCN23V07-MEISAISYOMONEY
                        PIC X(7).
            02  HCN23V07-SRYKBN
                        PIC X(2)        OCCURS 20 TIMES.
            02  HCN23V07-TEKIYO2
                        PIC X(52)       OCCURS 20 TIMES.
            02  HCN23V07-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN23V07-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN23V07-SEIKYUGAKU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN23V07-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN23V07-SEIKYU-G.
              03  HCN23V07-SEIKYUYY
                        PIC X(2).
              03  HCN23V07-SEIKYUMM
                        PIC X(2).
              03  HCN23V07-SEIKYUDD
                        PIC X(2).
            02  HCN23V07-HOSPJYUSYO
                        PIC X(100).
            02  HCN23V07-HOSPNAME
                        PIC X(80).
            02  HCN23V07-ISYA
                        PIC X(40).
            02  HCN23V07-TEL
                        PIC X(15).
            02  HCN23V07-BYOSYOSU
                        PIC X(4).
            02  HCN23V07-HOKENNAME
                        PIC X(60).
            02  HCN23V07-PAGE
                        PIC X(5).
            02  HCN23V07-LABEL
                        PIC X(14).
            02  HCN23V07-NYUINSYUBETU2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN23V07-LOPTION
                        PIC X(100).

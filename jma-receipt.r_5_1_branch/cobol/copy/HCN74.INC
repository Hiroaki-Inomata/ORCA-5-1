        01  HCN74.
            02  HCN74-HAKKOUGG
                        PIC X(4).
            02  HCN74-HAKKOUYY
                        PIC X(2).
            02  HCN74-HAKKOUMM
                        PIC X(2).
            02  HCN74-PTNUM
                        PIC X(20).
            02  HCN74-RENNUM
                        PIC X(7).
            02  HCN74-KANANAME
                        PIC X(50).
            02  HCN74-NAME
                        PIC X(50).
            02  HCN74-TANJYOG
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN74-TANJYOYY
                        PIC X(2).
            02  HCN74-TANJYOG-LBL
                        PIC X(2).
            02  HCN74-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN74-NENREI
                        PIC X(3).
            02  HCN74-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN74-JYUSYOUBIGG
                        PIC X(4).
            02  HCN74-JYUSYOUBI-G.
              03  HCN74-JYUSYOUBIYY
                        PIC X(2).
              03  HCN74-JYUSYOUBIMM
                        PIC X(2).
              03  HCN74-JYUSYOUBIDD
                        PIC X(2).
            02  HCN74-SYOSHINBIGG
                        PIC X(4).
            02  HCN74-SYOSHINBI-G.
              03  HCN74-SYOSHINBIYY
                        PIC X(2).
              03  HCN74-SYOSHINBIMM
                        PIC X(2).
              03  HCN74-SYOSHINBIDD
                        PIC X(2).
            02  HCN74-STRGG
                        PIC X(4).
            02  HCN74-STR-G.
              03  HCN74-STRYY
                        PIC X(2).
              03  HCN74-STRMM
                        PIC X(2).
              03  HCN74-STRDD
                        PIC X(2).
            02  HCN74-ENDGG
                        PIC X(4).
            02  HCN74-END-G.
              03  HCN74-ENDYY
                        PIC X(2).
              03  HCN74-ENDMM
                        PIC X(2).
              03  HCN74-ENDDD
                        PIC X(2).
            02  HCN74-JITSUNISSU
                        PIC X(3).
            02  HCN74-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN74-SYOSHIN
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN74-KHNTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN74-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN74-KOUTEN
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN74-YKZTENSU
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN74-NYUG
                        PIC X(2).
            02  HCN74-NYUYMD.
              03  HCN74-NYUY
                        PIC X(2).
              03  HCN74-NYUM
                        PIC X(2).
              03  HCN74-NYUD
                        PIC X(2).
            02  HCN74-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN74-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN74-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN74-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN74-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN74-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN74-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN74-NYUKOUTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN74-NYUKOUTEN-G
                                        OCCURS 10 TIMES.
              03  HCN74-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN74-NYUYKZTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN74-NYUYKZTENSU-G
                                        OCCURS 10 TIMES.
              03  HCN74-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN74-KOUTENG
                        PIC X(9).
            02  HCN74-YKZTENSUG
                        PIC X(9).
            02  HCN74-SONOTANIN
                        PIC X(2).
            02  HCN74-SONOTAKAISU
                        PIC X(2).
            02  HCN74-SONOTA
                        PIC X(16)       OCCURS 2 TIMES.
            02  HCN74-KINGAKU
                        PIC X(9)        OCCURS 8 TIMES.
            02  HCN74-KIJYUN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN74-SYOKAKERU
                        PIC X(4).
            02  HCN74-SYONICHI
                        PIC X(4).
            02  HCN74-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN74-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN74-SYOKINGAKU
                        PIC X(7).
            02  HCN74-TEKIYO1
                        PIC X(24)       OCCURS 8 TIMES.
            02  HCN74-BIKOU
                        PIC X(24)       OCCURS 6 TIMES.
            02  HCN74-SHINDANSYOSUU
                        PIC X(3).
            02  HCN74-MEISAISYOSUU
                        PIC X(3).
            02  HCN74-SHINDANSYOMONEY
                        PIC X(7).
            02  HCN74-MEISAISYOMONEY
                        PIC X(7).
            02  HCN74-SRYKBN
                        PIC X(2)        OCCURS 21 TIMES.
            02  HCN74-TEKIYO2
                        PIC X(52)       OCCURS 21 TIMES.
            02  HCN74-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN74-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN74-SEIKYUGAKU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN74-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN74-SEIKYUGG
                        PIC X(4).
            02  HCN74-SEIKYU-G.
              03  HCN74-SEIKYUYY
                        PIC X(2).
              03  HCN74-SEIKYUMM
                        PIC X(2).
              03  HCN74-SEIKYUDD
                        PIC X(2).
            02  HCN74-HOSPJYUSYO
                        PIC X(100).
            02  HCN74-HOSPNAME
                        PIC X(80).
            02  HCN74-ISYA
                        PIC X(40).
            02  HCN74-TEL
                        PIC X(15).
            02  HCN74-BYOSYOSU
                        PIC X(4).
            02  HCN74-HOKENNAME
                        PIC X(60).
            02  HCN74-PAGE
                        PIC X(5).
            02  HCN74-LABEL
                        PIC X(14).
            02  HCN74-NYUINSYUBETU2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN74-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN74-LOPTION
                        PIC X(100).

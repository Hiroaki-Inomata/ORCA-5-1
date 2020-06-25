        01  HCN53V03.
            02  HCN53V03-HAKKOUYY
                        PIC X(2).
            02  HCN53V03-HAKKOUMM
                        PIC X(2).
            02  HCN53V03-PTNUM
                        PIC X(20).
            02  HCN53V03-RENNUM
                        PIC X(7).
            02  HCN53V03-KANANAME
                        PIC X(50).
            02  HCN53V03-NAME
                        PIC X(50).
            02  HCN53V03-TANJYOG
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN53V03-TANJYOYY
                        PIC X(2).
            02  HCN53V03-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN53V03-NENREI
                        PIC X(3).
            02  HCN53V03-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN53V03-JYUSYOUBI-G.
              03  HCN53V03-JYUSYOUBIYY
                        PIC X(2).
              03  HCN53V03-JYUSYOUBIMM
                        PIC X(2).
              03  HCN53V03-JYUSYOUBIDD
                        PIC X(2).
            02  HCN53V03-SYOSHINBI-G.
              03  HCN53V03-SYOSHINBIYY
                        PIC X(2).
              03  HCN53V03-SYOSHINBIMM
                        PIC X(2).
              03  HCN53V03-SYOSHINBIDD
                        PIC X(2).
            02  HCN53V03-STR-G.
              03  HCN53V03-STRYY
                        PIC X(2).
              03  HCN53V03-STRMM
                        PIC X(2).
              03  HCN53V03-STRDD
                        PIC X(2).
            02  HCN53V03-END-G.
              03  HCN53V03-ENDYY
                        PIC X(2).
              03  HCN53V03-ENDMM
                        PIC X(2).
              03  HCN53V03-ENDDD
                        PIC X(2).
            02  HCN53V03-JITSUNISSU
                        PIC X(3).
            02  HCN53V03-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN53V03-SYOSHIN
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN53V03-KHNTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN53V03-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN53V03-KOUTEN
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN53V03-YKZTENSU
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN53V03-NYUYMD.
              03  HCN53V03-NYUY
                        PIC X(2).
              03  HCN53V03-NYUM
                        PIC X(2).
              03  HCN53V03-NYUD
                        PIC X(2).
            02  HCN53V03-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN53V03-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN53V03-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN53V03-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN53V03-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN53V03-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN53V03-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN53V03-NYUKOUTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN53V03-NYUKOUTEN-G
                                        OCCURS 10 TIMES.
              03  HCN53V03-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN53V03-NYUYKZTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN53V03-NYUYKZTENSU-G
                                        OCCURS 10 TIMES.
              03  HCN53V03-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN53V03-KOUTENG
                        PIC X(9).
            02  HCN53V03-YKZTENSUG
                        PIC X(9).
            02  HCN53V03-SONOTANIN
                        PIC X(2).
            02  HCN53V03-SONOTAKAISU
                        PIC X(2).
            02  HCN53V03-SONOTA
                        PIC X(16)       OCCURS 2 TIMES.
            02  HCN53V03-KINGAKU
                        PIC X(7)        OCCURS 8 TIMES.
            02  HCN53V03-KIJYUN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN53V03-SYOKAKERU
                        PIC X(4).
            02  HCN53V03-SYONICHI
                        PIC X(4).
            02  HCN53V03-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN53V03-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN53V03-SYOKINGAKU
                        PIC X(7).
            02  HCN53V03-TEKIYO1
                        PIC X(24)       OCCURS 8 TIMES.
            02  HCN53V03-BIKOU
                        PIC X(24)       OCCURS 6 TIMES.
            02  HCN53V03-SHINDANSYOSUU
                        PIC X(3).
            02  HCN53V03-MEISAISYOSUU
                        PIC X(3).
            02  HCN53V03-SHINDANSYOMONEY
                        PIC X(7).
            02  HCN53V03-MEISAISYOMONEY
                        PIC X(7).
            02  HCN53V03-SRYKBN
                        PIC X(2)        OCCURS 21 TIMES.
            02  HCN53V03-TEKIYO2
                        PIC X(52)       OCCURS 21 TIMES.
            02  HCN53V03-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN53V03-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN53V03-SEIKYUGAKU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN53V03-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN53V03-SEIKYU-G.
              03  HCN53V03-SEIKYUYY
                        PIC X(2).
              03  HCN53V03-SEIKYUMM
                        PIC X(2).
              03  HCN53V03-SEIKYUDD
                        PIC X(2).
            02  HCN53V03-HOSPJYUSYO
                        PIC X(100).
            02  HCN53V03-HOSPNAME
                        PIC X(80).
            02  HCN53V03-ISYA
                        PIC X(40).
            02  HCN53V03-TEL
                        PIC X(15).
            02  HCN53V03-BYOSYOSU
                        PIC X(4).
            02  HCN53V03-HOKENNAME
                        PIC X(60).
            02  HCN53V03-PAGE
                        PIC X(5).
            02  HCN53V03-LABEL
                        PIC X(14).
            02  HCN53V03-NYUINSYUBETU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN53V03-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN53V03-LOPTION
                        PIC X(100).

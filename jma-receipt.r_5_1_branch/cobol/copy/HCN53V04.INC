        01  HCN53V04.
            02  HCN53V04-HAKKOUYY
                        PIC X(2).
            02  HCN53V04-HAKKOUMM
                        PIC X(2).
            02  HCN53V04-PTNUM
                        PIC X(20).
            02  HCN53V04-RENNUM
                        PIC X(7).
            02  HCN53V04-KANANAME
                        PIC X(50).
            02  HCN53V04-NAME
                        PIC X(50).
            02  HCN53V04-TANJYOG
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN53V04-TANJYOYY
                        PIC X(2).
            02  HCN53V04-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN53V04-NENREI
                        PIC X(3).
            02  HCN53V04-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN53V04-JYUSYOUBI-G.
              03  HCN53V04-JYUSYOUBIYY
                        PIC X(2).
              03  HCN53V04-JYUSYOUBIMM
                        PIC X(2).
              03  HCN53V04-JYUSYOUBIDD
                        PIC X(2).
            02  HCN53V04-SYOSHINBI-G.
              03  HCN53V04-SYOSHINBIYY
                        PIC X(2).
              03  HCN53V04-SYOSHINBIMM
                        PIC X(2).
              03  HCN53V04-SYOSHINBIDD
                        PIC X(2).
            02  HCN53V04-STR-G.
              03  HCN53V04-STRYY
                        PIC X(2).
              03  HCN53V04-STRMM
                        PIC X(2).
              03  HCN53V04-STRDD
                        PIC X(2).
            02  HCN53V04-END-G.
              03  HCN53V04-ENDYY
                        PIC X(2).
              03  HCN53V04-ENDMM
                        PIC X(2).
              03  HCN53V04-ENDDD
                        PIC X(2).
            02  HCN53V04-JITSUNISSU
                        PIC X(3).
            02  HCN53V04-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN53V04-SYOSHIN
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN53V04-KHNTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN53V04-KAISU
                        PIC X(3)        OCCURS 22 TIMES.
            02  HCN53V04-KOUTEN
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN53V04-YKZTENSU
                        PIC X(7)        OCCURS 23 TIMES.
            02  HCN53V04-NYUYMD.
              03  HCN53V04-NYUY
                        PIC X(2).
              03  HCN53V04-NYUM
                        PIC X(2).
              03  HCN53V04-NYUD
                        PIC X(2).
            02  HCN53V04-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN53V04-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN53V04-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN53V04-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN53V04-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN53V04-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN53V04-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN53V04-NYUKOUTEN
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN53V04-NYUKOUTEN-G
                                        OCCURS 10 TIMES.
              03  HCN53V04-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN53V04-NYUYKZTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN53V04-NYUYKZTENSU-G
                                        OCCURS 10 TIMES.
              03  HCN53V04-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN53V04-KOUTENG
                        PIC X(9).
            02  HCN53V04-YKZTENSUG
                        PIC X(9).
            02  HCN53V04-SONOTANIN
                        PIC X(2).
            02  HCN53V04-SONOTAKAISU
                        PIC X(2).
            02  HCN53V04-SONOTA
                        PIC X(16)       OCCURS 2 TIMES.
            02  HCN53V04-KINGAKU
                        PIC X(9)        OCCURS 8 TIMES.
            02  HCN53V04-KIJYUN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN53V04-SYOKAKERU
                        PIC X(4).
            02  HCN53V04-SYONICHI
                        PIC X(4).
            02  HCN53V04-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN53V04-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN53V04-SYOKINGAKU
                        PIC X(7).
            02  HCN53V04-TEKIYO1
                        PIC X(24)       OCCURS 8 TIMES.
            02  HCN53V04-BIKOU
                        PIC X(24)       OCCURS 6 TIMES.
            02  HCN53V04-SHINDANSYOSUU
                        PIC X(3).
            02  HCN53V04-MEISAISYOSUU
                        PIC X(3).
            02  HCN53V04-SHINDANSYOMONEY
                        PIC X(7).
            02  HCN53V04-MEISAISYOMONEY
                        PIC X(7).
            02  HCN53V04-SRYKBN
                        PIC X(2)        OCCURS 21 TIMES.
            02  HCN53V04-TEKIYO2
                        PIC X(52)       OCCURS 21 TIMES.
            02  HCN53V04-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN53V04-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN53V04-SEIKYUGAKU
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCN53V04-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN53V04-SEIKYU-G.
              03  HCN53V04-SEIKYUYY
                        PIC X(2).
              03  HCN53V04-SEIKYUMM
                        PIC X(2).
              03  HCN53V04-SEIKYUDD
                        PIC X(2).
            02  HCN53V04-HOSPJYUSYO
                        PIC X(100).
            02  HCN53V04-HOSPNAME
                        PIC X(80).
            02  HCN53V04-ISYA
                        PIC X(40).
            02  HCN53V04-TEL
                        PIC X(15).
            02  HCN53V04-BYOSYOSU
                        PIC X(4).
            02  HCN53V04-HOKENNAME
                        PIC X(60).
            02  HCN53V04-PAGE
                        PIC X(5).
            02  HCN53V04-LABEL
                        PIC X(14).
            02  HCN53V04-NYUINSYUBETU2
                        PIC X(14)       OCCURS 11 TIMES.
            02  HCN53V04-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN53V04-LOPTION
                        PIC X(100).

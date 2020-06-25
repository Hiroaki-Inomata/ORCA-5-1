        01  HCN70.
            02  HCN70-HAKKOUG
                        PIC X(1).
            02  HCN70-HAKKOUYY
                        PIC X(2).
            02  HCN70-HAKKOUMM
                        PIC X(2).
            02  HCN70-PTNUM
                        PIC X(20).
            02  HCN70-RENNUM
                        PIC X(7).
            02  HCN70-NAME
                        PIC X(48).
            02  HCN70-SYOBYO.
              03  HCN70-SYOBYO-S
                        PIC X(48)       OCCURS 4 TIMES.
            02  HCN70-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN70-TANJYO-G.
              03  HCN70-TANJYOG
                        PIC X(1).
              03  HCN70-TANJYOYY
                        PIC X(2).
              03  HCN70-TANJYOMM
                        PIC X(2).
              03  HCN70-TANJYODD
                        PIC X(2).
            02  HCN70-NENREI
                        PIC X(3).
            02  HCN70-JITSUNISSU
                        PIC X(3).
            02  HCN70-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN70-JYUSYOUBI-G.
              03  HCN70-JYUSYOUBIDD
                        PIC X(2).
              03  HCN70-JYUSYOUBIMM
                        PIC X(2).
              03  HCN70-JYUSYOUBIYY
                        PIC X(2).
              03  HCN70-JYUSYOUBIG
                        PIC X(1).
            02  HCN70-SYOSHINBI-G.
              03  HCN70-SYOSHINBIG
                        PIC X(1).
              03  HCN70-SYOSHINBIYY
                        PIC X(2).
              03  HCN70-SYOSHINBIMM
                        PIC X(2).
              03  HCN70-SYOSHINBIDD
                        PIC X(2).
            02  HCN70-STR-G.
              03  HCN70-STRG
                        PIC X(1).
              03  HCN70-STRYY
                        PIC X(2).
              03  HCN70-STRMM
                        PIC X(2).
              03  HCN70-STRDD
                        PIC X(2).
            02  HCN70-END-G.
              03  HCN70-ENDG
                        PIC X(1).
              03  HCN70-ENDYY
                        PIC X(2).
              03  HCN70-ENDMM
                        PIC X(2).
              03  HCN70-ENDDD
                        PIC X(2).
            02  HCN70-SYOSHIN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN70-KHNTEN
                        PIC X(2)        OCCURS 21 TIMES.
            02  HCN70-KHNKAISU
                        PIC X(4)        OCCURS 21 TIMES.
            02  HCN70-KOUTEN
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN70-YKZTENSU
                        PIC X(7)        OCCURS 22 TIMES.
            02  HCN70-NYUIN-G.
              03  HCN70-NYUING
                        PIC X(1).
              03  HCN70-NYUINY
                        PIC X(2).
              03  HCN70-NYUINM
                        PIC X(2).
              03  HCN70-NYUIND
                        PIC X(2).
            02  HCN70-HOSPSBT
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN70-NYUINSYUBETU.
              03  HCN70-NYUINSYUBETU-S
                        PIC X(10)       OCCURS 13 TIMES.
            02  HCN70-NYUINSYUBETU2.
              03  HCN70-NYUINSYUBETU2-S
                        PIC X(14)       OCCURS 13 TIMES.
            02  HCN70-NYUKHNTEN
                        PIC X(6)        OCCURS 10 TIMES.
            02  HCN70-NYUKHNTENN
                        PIC X(10)       OCCURS 10 TIMES.
            02  HCN70-NYUKHNTENN2
                        PIC X(14)       OCCURS 10 TIMES.
            02  HCN70-KAKERU
                        PIC X(2)        OCCURS 10 TIMES.
            02  HCN70-NYUKAISU
                        PIC X(2)        OCCURS 10 TIMES.
            02  HCN70-NICHI
                        PIC X(4)        OCCURS 10 TIMES.
            02  HCN70-NYUKOUTEN
                        PIC X(7)        OCCURS 11 TIMES.
            02  HCN70-KOUTENG
                        PIC X(7).
            02  HCN70-YKZTENSUG
                        PIC X(7).
            02  HCN70-SONOTANIN
                        PIC X(2).
            02  HCN70-SONOTAKAISU
                        PIC X(2).
            02  HCN70-SONOTA
                        PIC X(20)       OCCURS 2 TIMES.
            02  HCN70-KINGAKU
                        PIC X(8)        OCCURS 7 TIMES.
            02  HCN70-KIJYUN
                        PIC X(2).
            02  HCN70-SYOKBN
                        PIC X(6)        OCCURS 3 TIMES.
            02  HCN70-SYOKHNTEN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN70-SYOYEN
                        PIC X(2).
            02  HCN70-SYOKAKERU
                        PIC X(2).
            02  HCN70-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN70-SYONICHI
                        PIC X(2).
            02  HCN70-SYOKINGAKU
                        PIC X(6).
            02  HCN70-SHINDANSYOSUU
                        PIC X(2).
            02  HCN70-SHINDANSYOMONEY
                        PIC X(6).
            02  HCN70-MEISAISYOSUU
                        PIC X(2).
            02  HCN70-MEISAISYOMONEY
                        PIC X(6).
            02  HCN70-SRYKBN
                        PIC X(2)        OCCURS 26 TIMES.
            02  HCN70-MARK
                        PIC X(2)        OCCURS 26 TIMES.
            02  HCN70-TEKIYO
                        PIC X(30)       OCCURS 26 TIMES.
            02  HCN70-TENSU
                        PIC X(7)        OCCURS 26 TIMES.
            02  HCN70-MARK2
                        PIC X(2)        OCCURS 26 TIMES.
            02  HCN70-KAISU
                        PIC X(3)        OCCURS 26 TIMES.
            02  HCN70-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCN70-SEIKYU
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN70-SEIKYUGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCN70-SEIKYUGAKUTOTAL
                        PIC X(9)        OCCURS 2 TIMES.
            02  HCN70-HOKENNAME
                        PIC X(30)       OCCURS 2 TIMES.
            02  HCN70-SEIKYU-G.
              03  HCN70-SEIKYUG
                        PIC X(1).
              03  HCN70-SEIKYUYY
                        PIC X(2).
              03  HCN70-SEIKYUMM
                        PIC X(2).
              03  HCN70-SEIKYUDD
                        PIC X(2).
            02  HCN70-BYOSYOSU
                        PIC X(5).
            02  HCN70-HOSPPOST-G.
              03  HCN70-HOSPPOST1
                        PIC X(3).
              03  HCN70-HOSPPOST2
                        PIC X(4).
            02  HCN70-HOSPJYUSYO
                        PIC X(68).
            02  HCN70-HOSPNAME
                        PIC X(40).
            02  HCN70-ISYA
                        PIC X(40).
            02  HCN70-TEL
                        PIC X(16).
            02  HCN70-HOSPCD
                        PIC X(7).
            02  HCN70-PAGE1
                        PIC X(2).
            02  HCN70-PAGE2
                        PIC X(2).
            02  HCN70-LABEL
                        PIC X(14).
            02  HCN70-LOPTION
                        PIC X(100).

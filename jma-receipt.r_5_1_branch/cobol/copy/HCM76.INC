        01  HCM76.
            02  HCM76-HAKKOUG
                        PIC X(1).
            02  HCM76-HAKKOUYY
                        PIC X(2).
            02  HCM76-HAKKOUMM
                        PIC X(2).
            02  HCM76-PTNUM
                        PIC X(20).
            02  HCM76-RENNUM
                        PIC X(7).
            02  HCM76-NAME
                        PIC X(48).
            02  HCM76-SYOBYO.
              03  HCM76-SYOBYO-S
                        PIC X(48)       OCCURS 4 TIMES.
            02  HCM76-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCM76-TANJYO-G.
              03  HCM76-TANJYOG
                        PIC X(1).
              03  HCM76-TANJYOYY
                        PIC X(2).
              03  HCM76-TANJYOMM
                        PIC X(2).
              03  HCM76-TANJYODD
                        PIC X(2).
            02  HCM76-NENREI
                        PIC X(3).
            02  HCM76-JITSUNISSU
                        PIC X(3).
            02  HCM76-TENKI
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCM76-JYUSYOU-G.
              03  HCM76-JYUSYOUBIG
                        PIC X(1).
              03  HCM76-JYUSYOUBIYY
                        PIC X(2).
              03  HCM76-JYUSYOUBIMM
                        PIC X(2).
              03  HCM76-JYUSYOUBIDD
                        PIC X(2).
            02  HCM76-SYOSHIN-G.
              03  HCM76-SYOSHINBIG
                        PIC X(1).
              03  HCM76-SYOSHINBIYY
                        PIC X(2).
              03  HCM76-SYOSHINBIMM
                        PIC X(2).
              03  HCM76-SYOSHINBIDD
                        PIC X(2).
            02  HCM76-STR-G.
              03  HCM76-STRG
                        PIC X(1).
              03  HCM76-STRYY
                        PIC X(2).
              03  HCM76-STRMM
                        PIC X(2).
              03  HCM76-STRDD
                        PIC X(2).
            02  HCM76-END-G.
              03  HCM76-ENDG
                        PIC X(1).
              03  HCM76-ENDYY
                        PIC X(2).
              03  HCM76-ENDMM
                        PIC X(2).
              03  HCM76-ENDDD
                        PIC X(2).
            02  HCM76-SYOSHIN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCM76-KHNTEN
                        PIC X(2)        OCCURS 32 TIMES.
            02  HCM76-KHNKAISU
                        PIC X(4)        OCCURS 32 TIMES.
            02  HCM76-KOUTEN
                        PIC X(7)        OCCURS 33 TIMES.
            02  HCM76-KOUTENG
                        PIC X(7).
            02  HCM76-YKZTENSU
                        PIC X(7)        OCCURS 33 TIMES.
            02  HCM76-YKZTENSUG
                        PIC X(7).
            02  HCM76-SAISINKAISU
                        PIC X(3).
            02  HCM76-SHIDOKAISU
                        PIC X(3).
            02  HCM76-KINGAKU
                        PIC X(6)        OCCURS 6 TIMES.
            02  HCM76-SHINDANSYOSUU
                        PIC X(2).
            02  HCM76-MEISAISYOSUU
                        PIC X(2).
            02  HCM76-SHINDANSYOMONEY
                        PIC X(6).
            02  HCM76-MEISAISYOMONEY
                        PIC X(6).
            02  HCM76-SRYKBN
                        PIC X(2)        OCCURS 32 TIMES.
            02  HCM76-MARK
                        PIC X(2)        OCCURS 32 TIMES.
            02  HCM76-TEKIYO
                        PIC X(30)       OCCURS 32 TIMES.
            02  HCM76-TENSU
                        PIC X(7)        OCCURS 32 TIMES.
            02  HCM76-MARK2
                        PIC X(2)        OCCURS 32 TIMES.
            02  HCM76-KAISU
                        PIC X(3)        OCCURS 32 TIMES.
            02  HCM76-SEIKYU-SEISU
                        PIC X(1)        OCCURS 2 TIMES.
            02  HCM76-SEIKYU
                        PIC X(9)        OCCURS 2 TIMES.
            02  HCM76-SEIKYUGAKU
                        PIC X(9)        OCCURS 5 TIMES.
            02  HCM76-TSUINMM
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCM76-TSUIN-G
                                        OCCURS 2 TIMES.
              03  HCM76-TSUIN
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCM76-TSUINSUU
                        PIC X(3)        OCCURS 2 TIMES.
            02  HCM76-SEIKYUGAKUTOTAL
                        PIC X(9)        OCCURS 2 TIMES.
            02  HCM76-HOKENNAME
                        PIC X(30)       OCCURS 2 TIMES.
            02  HCM76-SEIKYU-G.
              03  HCM76-SEIKYUG
                        PIC X(1).
              03  HCM76-SEIKYUYY
                        PIC X(2).
              03  HCM76-SEIKYUMM
                        PIC X(2).
              03  HCM76-SEIKYUDD
                        PIC X(2).
            02  HCM76-BYOSYOSU
                        PIC X(5).
            02  HCM76-HOSPPOST-G.
              03  HCM76-HOSPPOST1
                        PIC X(3).
              03  HCM76-HOSPPOST2
                        PIC X(4).
            02  HCM76-HOSPJYUSYO
                        PIC X(68).
            02  HCM76-HOSPNAME
                        PIC X(40).
            02  HCM76-ISYA
                        PIC X(40).
            02  HCM76-TEL
                        PIC X(16).
            02  HCM76-HOSPCD
                        PIC X(7).
            02  HCM76-PAGE1
                        PIC X(2).
            02  HCM76-PAGE2
                        PIC X(2).
            02  HCM76-LABEL
                        PIC X(14).
            02  HCM76-LOPTION
                        PIC X(100).

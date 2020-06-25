        01  HCN75.
            02  HCN75-HAKKOUGG
                        PIC X(4).
            02  HCN75-HAKOUYM.
              03  HCN75-HAKKOUYY
                        PIC X(2).
              03  HCN75-HAKKOUMM
                        PIC X(2).
            02  HCN75-PTNUM
                        PIC X(20).
            02  HCN75-RENNUM
                        PIC X(7).
            02  HCN75-KIGONUM
                        PIC X(50).
            02  HCN75-HKNJANAME
                        PIC X(60).
            02  HCN75-SINRYOKBN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN75-SYOBYOKBN
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCN75-KANANAME
                        PIC X(50).
            02  HCN75-NAME
                        PIC X(50).
            02  HCN75-TANJYOG
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN75-TANJYOYY
                        PIC X(2).
            02  HCN75-TANJYOG-LBL
                        PIC X(2).
            02  HCN75-SEX
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN75-NENREI
                        PIC X(3).
            02  HCN75-SYOBYO
                        PIC X(50)       OCCURS 3 TIMES.
            02  HCN75-JYUSYOUBIGG
                        PIC X(4).
            02  HCN75-JYUSYOUBI-G.
              03  HCN75-JYUSYOUBIYY
                        PIC X(2).
              03  HCN75-JYUSYOUBIMM
                        PIC X(2).
              03  HCN75-JYUSYOUBIDD
                        PIC X(2).
            02  HCN75-SYOSHINBIGG
                        PIC X(4).
            02  HCN75-SYOSHINBI-G.
              03  HCN75-SYOSHINBIYY
                        PIC X(2).
              03  HCN75-SYOSHINBIMM
                        PIC X(2).
              03  HCN75-SYOSHINBIDD
                        PIC X(2).
            02  HCN75-STRGG
                        PIC X(4).
            02  HCN75-STR-G.
              03  HCN75-STRYY
                        PIC X(2).
              03  HCN75-STRMM
                        PIC X(2).
              03  HCN75-STRDD
                        PIC X(2).
            02  HCN75-ENDGG
                        PIC X(4).
            02  HCN75-END-G.
              03  HCN75-ENDYY
                        PIC X(2).
              03  HCN75-ENDMM
                        PIC X(2).
              03  HCN75-ENDDD
                        PIC X(2).
            02  HCN75-JITSUNISSU
                        PIC X(3).
            02  HCN75-TENKIKBN
                        PIC X(2)        OCCURS 5 TIMES.
            02  HCN75-SYOSHINKBN
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN75-KHNTEN
                        PIC X(7)        OCCURS 28 TIMES.
            02  HCN75-KAISU
                        PIC X(3)        OCCURS 28 TIMES.
            02  HCN75-TENSU
                        PIC X(7)        OCCURS 29 TIMES.
            02  HCN75-KINGAKU
                        PIC X(7)        OCCURS 29 TIMES.
            02  HCN75-NYUG
                        PIC X(2).
            02  HCN75-NYUYMD.
              03  HCN75-NYUY
                        PIC X(2).
              03  HCN75-NYUM
                        PIC X(2).
              03  HCN75-NYUD
                        PIC X(2).
            02  HCN75-HOSPSBTKBN
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCN75-NYUINSYUBETU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN75-NYUKHNTEN
                        PIC X(7)        OCCURS 9 TIMES.
            02  HCN75-NYUKHNTENN
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN75-KAKERU
                        PIC X(2)        OCCURS 4 TIMES.
            02  HCN75-NYUKAISU
                        PIC X(3)        OCCURS 9 TIMES.
            02  HCN75-NICHI
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN75-NYUTENSU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN75-NYUTENSUN-G
                                        OCCURS 10 TIMES.
              03  HCN75-NYUKOUTENN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN75-NYUKINGAKU
                        PIC X(7)        OCCURS 10 TIMES.
            02  HCN75-NYUKINGAKUN-G
                                        OCCURS 10 TIMES.
              03  HCN75-NYUYKZTENSUN
                        PIC X(2)        OCCURS 6 TIMES.
            02  HCN75-TANKA
                        PIC X(4).
            02  HCN75-TENSUG
                        PIC X(7).
            02  HCN75-KINGAKUG
                        PIC X(7).
            02  HCN75-SRYKBN
                        PIC X(2)        OCCURS 26 TIMES.
            02  HCN75-TEKIYO
                        PIC X(52)       OCCURS 26 TIMES.
            02  HCN75-KIJYUN
                        PIC X(4)        OCCURS 4 TIMES.
            02  HCN75-SYOKHNTEN
                        PIC X(7)        OCCURS 4 TIMES.
            02  HCN75-SYOKAISU
                        PIC X(3)        OCCURS 4 TIMES.
            02  HCN75-KIJYUNNAME-SEI
                        PIC X(10).
            02  HCN75-KIJYUN-SEI
                        PIC X(2).
            02  HCN75-SYOKHNTEN-SEI
                        PIC X(7)        OCCURS 2 TIMES.
            02  HCN75-SYOKAISU-SEI
                        PIC X(3)        OCCURS 2 TIMES.
            02  HCN75-SYONISSU
                        PIC X(3).
            02  HCN75-SYOKINGAKU
                        PIC X(10).
            02  HCN75-SHINDANSYOSUU
                        PIC X(3).
            02  HCN75-FTNWARIAI
                        PIC X(3).
            02  HCN75-FTNNISSU
                        PIC X(3).
            02  HCN75-MEISAISYOSUU
                        PIC X(3).
            02  HCN75-SONOTANIN
                        PIC X(2).
            02  HCN75-SONOTAKAISU
                        PIC X(2).
            02  HCN75-SEIKYU
                        PIC X(10)       OCCURS 9 TIMES.
            02  HCN75-SEIKYUG
                        PIC X(10).
            02  HCN75-SEIKYUGAKUTOTAL
                        PIC X(10).
            02  HCN75-SEIKYUGG
                        PIC X(4).
            02  HCN75-SEIKYUYMD.
              03  HCN75-SEIKYUYY
                        PIC X(2).
              03  HCN75-SEIKYUMM
                        PIC X(2).
              03  HCN75-SEIKYUDD
                        PIC X(2).
            02  HCN75-HOSPJYUSYO
                        PIC X(100).
            02  HCN75-HOSPNAME
                        PIC X(80).
            02  HCN75-ISYA
                        PIC X(40).
            02  HCN75-TEL
                        PIC X(15).
            02  HCN75-BYOSYOSU
                        PIC X(4).
            02  HCN75-HOKENNAME
                        PIC X(60).
            02  HCN75-PAGE
                        PIC X(5).
            02  HCN75-LABEL
                        PIC X(14).
            02  HCN75-NYUINSYUBETU2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN75-NYUKHNTENN2
                        PIC X(14)       OCCURS 9 TIMES.
            02  HCN75-LOPTION
                        PIC X(100).
            02  HCN75-HKNJANUM
                        PIC X(8).

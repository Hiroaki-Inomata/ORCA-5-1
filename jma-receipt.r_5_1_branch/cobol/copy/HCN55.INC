        01  HCN55.
            02  HCN55-HAKOUYM.
              03  HCN55-HAKKOUYY  PIC X(2).
              03  HCN55-HAKKOUMM  PIC X(2).
            02  HCN55-PTNUM PIC X(20).
            02  HCN55-RENNUM    PIC X(7).
            02  HCN55-KIGONUM   PIC X(50).
            02  HCN55-HKNJANAME PIC X(60).
            02  HCN55-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN55-KANANAME  PIC X(50).
            02  HCN55-NAME  PIC X(50).
            02  HCN55-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55-TANJYOYY  PIC X(2).
            02  HCN55-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN55-NENREI    PIC X(3).
            02  HCN55-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN55-JYUSYOUBI-G.
              03  HCN55-JYUSYOUBIYY   PIC X(2).
              03  HCN55-JYUSYOUBIMM   PIC X(2).
              03  HCN55-JYUSYOUBIDD   PIC X(2).
            02  HCN55-SYOSHINBI-G.
              03  HCN55-SYOSHINBIYY   PIC X(2).
              03  HCN55-SYOSHINBIMM   PIC X(2).
              03  HCN55-SYOSHINBIDD   PIC X(2).
            02  HCN55-STR-G.
              03  HCN55-STRYY PIC X(2).
              03  HCN55-STRMM PIC X(2).
              03  HCN55-STRDD PIC X(2).
            02  HCN55-END-G.
              03  HCN55-ENDYY PIC X(2).
              03  HCN55-ENDMM PIC X(2).
              03  HCN55-ENDDD PIC X(2).
            02  HCN55-JITSUNISSU    PIC X(3).
            02  HCN55-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN55-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55-KHNTEN
                    PIC X(7)    OCCURS  28 TIMES.
            02  HCN55-KAISU
                    PIC X(3)    OCCURS  28 TIMES.
            02  HCN55-TENSU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN55-KINGAKU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN55-NYUYMD.
              03  HCN55-NYUY  PIC X(2).
              03  HCN55-NYUM  PIC X(2).
              03  HCN55-NYUD  PIC X(2).
            02  HCN55-HOSPSBTKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN55-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN55-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN55-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN55-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN55-NYUTENSUN-G       OCCURS  10 TIMES.
              03  HCN55-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN55-NYUKINGAKU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN55-NYUKINGAKUN-G     OCCURS  10 TIMES.
              03  HCN55-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN55-TANKA PIC X(4).
            02  HCN55-TENSUG    PIC X(7).
            02  HCN55-KINGAKUG  PIC X(7).
            02  HCN55-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCN55-TEKIYO
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCN55-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN55-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN55-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN55-KIJYUNNAME-SEI    PIC X(10).
            02  HCN55-KIJYUN-SEI    PIC X(2).
            02  HCN55-SYOKHNTEN-SEI
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCN55-SYOKAISU-SEI
                    PIC X(3)    OCCURS  2 TIMES.
            02  HCN55-SYONISSU  PIC X(3).
            02  HCN55-SYOKINGAKU    PIC X(10).
            02  HCN55-SHINDANSYOSUU PIC X(3).
            02  HCN55-FTNWARIAI PIC X(3).
            02  HCN55-FTNNISSU  PIC X(3).
            02  HCN55-MEISAISYOSUU  PIC X(3).
            02  HCN55-SONOTANIN PIC X(2).
            02  HCN55-SONOTAKAISU   PIC X(2).
            02  HCN55-SEIKYU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55-SEIKYUG   PIC X(10).
            02  HCN55-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCN55-SEIKYUYMD.
              03  HCN55-SEIKYUYY  PIC X(2).
              03  HCN55-SEIKYUMM  PIC X(2).
              03  HCN55-SEIKYUDD  PIC X(2).
            02  HCN55-HOSPJYUSYO    PIC X(100).
            02  HCN55-HOSPNAME  PIC X(80).
            02  HCN55-ISYA  PIC X(40).
            02  HCN55-TEL   PIC X(15).
            02  HCN55-BYOSYOSU  PIC X(4).
            02  HCN55-HOKENNAME PIC X(60).
            02  HCN55-PAGE  PIC X(5).

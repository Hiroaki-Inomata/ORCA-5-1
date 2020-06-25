        01  HCN25.
            02  HCN25-HAKOUYM.
              03  HCN25-HAKKOUYY  PIC X(2).
              03  HCN25-HAKKOUMM  PIC X(2).
            02  HCN25-PTNUM PIC X(20).
            02  HCN25-RENNUM    PIC X(7).
            02  HCN25-KIGONUM   PIC X(50).
            02  HCN25-HKNJANAME PIC X(60).
            02  HCN25-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25-KANANAME  PIC X(50).
            02  HCN25-NAME  PIC X(50).
            02  HCN25-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25-TANJYOYY  PIC X(2).
            02  HCN25-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN25-NENREI    PIC X(3).
            02  HCN25-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN25-JYUSYOUBI-G.
              03  HCN25-JYUSYOUBIYY   PIC X(2).
              03  HCN25-JYUSYOUBIMM   PIC X(2).
              03  HCN25-JYUSYOUBIDD   PIC X(2).
            02  HCN25-SYOSHINBI-G.
              03  HCN25-SYOSHINBIYY   PIC X(2).
              03  HCN25-SYOSHINBIMM   PIC X(2).
              03  HCN25-SYOSHINBIDD   PIC X(2).
            02  HCN25-STR-G.
              03  HCN25-STRYY PIC X(2).
              03  HCN25-STRMM PIC X(2).
              03  HCN25-STRDD PIC X(2).
            02  HCN25-END-G.
              03  HCN25-ENDYY PIC X(2).
              03  HCN25-ENDMM PIC X(2).
              03  HCN25-ENDDD PIC X(2).
            02  HCN25-JITSUNISSU    PIC X(3).
            02  HCN25-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25-KHNTEN
                    PIC X(7)    OCCURS  28 TIMES.
            02  HCN25-KAISU
                    PIC X(3)    OCCURS  28 TIMES.
            02  HCN25-TENSU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25-KINGAKU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25-NYUYMD.
              03  HCN25-NYUY  PIC X(2).
              03  HCN25-NYUM  PIC X(2).
              03  HCN25-NYUD  PIC X(2).
            02  HCN25-SBTNAME   PIC X(2).
            02  HCN25-HOSPSBTKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN25-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN25-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25-NYUTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25-NYUTENSUN-G       OCCURS  11 TIMES.
              03  HCN25-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25-NYUKINGAKU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25-NYUKINGAKUN-G     OCCURS  11 TIMES.
              03  HCN25-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25-TANKA PIC X(4).
            02  HCN25-TENSUG    PIC X(7).
            02  HCN25-KINGAKUG  PIC X(7).
            02  HCN25-SRYKBN
                    PIC X(2)    OCCURS  27 TIMES.
            02  HCN25-TEKIYO
                    PIC X(52)   OCCURS  27 TIMES.
            02  HCN25-KIJYUN
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN25-SYOKAKERU PIC X(4).
            02  HCN25-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN25-SYONICHI  PIC X(4).
            02  HCN25-BIKOU
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCN25-SYONISSU  PIC X(3).
            02  HCN25-SYOKINGAKU    PIC X(10).
            02  HCN25-SHINDANSYOSUU PIC X(3).
            02  HCN25-FTNWARIAI PIC X(3).
            02  HCN25-FTNNISSU  PIC X(3).
            02  HCN25-MEISAISYOSUU  PIC X(3).
            02  HCN25-SONOTANIN PIC X(2).
            02  HCN25-SONOTAKAISU   PIC X(2).
            02  HCN25-SEIKYU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25-SEIKYUG   PIC X(10).
            02  HCN25-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCN25-SEIKYUYMD.
              03  HCN25-SEIKYUYY  PIC X(2).
              03  HCN25-SEIKYUMM  PIC X(2).
              03  HCN25-SEIKYUDD  PIC X(2).
            02  HCN25-HOSPJYUSYO    PIC X(100).
            02  HCN25-HOSPNAME  PIC X(80).
            02  HCN25-ISYA  PIC X(40).
            02  HCN25-TEL   PIC X(15).
            02  HCN25-BYOSYOSU  PIC X(4).
            02  HCN25-HOKENNAME PIC X(60).
            02  HCN25-PAGE  PIC X(5).

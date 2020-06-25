        01  HCN25V02.
            02  HCN25V02-HAKOUYM.
              03  HCN25V02-HAKKOUYY   PIC X(2).
              03  HCN25V02-HAKKOUMM   PIC X(2).
            02  HCN25V02-PTNUM  PIC X(20).
            02  HCN25V02-RENNUM PIC X(7).
            02  HCN25V02-KIGONUM    PIC X(50).
            02  HCN25V02-HKNJANAME  PIC X(60).
            02  HCN25V02-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V02-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25V02-KANANAME   PIC X(50).
            02  HCN25V02-NAME   PIC X(50).
            02  HCN25V02-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V02-TANJYOYY   PIC X(2).
            02  HCN25V02-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN25V02-NENREI PIC X(3).
            02  HCN25V02-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN25V02-JYUSYOUBI-G.
              03  HCN25V02-JYUSYOUBIYY    PIC X(2).
              03  HCN25V02-JYUSYOUBIMM    PIC X(2).
              03  HCN25V02-JYUSYOUBIDD    PIC X(2).
            02  HCN25V02-SYOSHINBI-G.
              03  HCN25V02-SYOSHINBIYY    PIC X(2).
              03  HCN25V02-SYOSHINBIMM    PIC X(2).
              03  HCN25V02-SYOSHINBIDD    PIC X(2).
            02  HCN25V02-STR-G.
              03  HCN25V02-STRYY  PIC X(2).
              03  HCN25V02-STRMM  PIC X(2).
              03  HCN25V02-STRDD  PIC X(2).
            02  HCN25V02-END-G.
              03  HCN25V02-ENDYY  PIC X(2).
              03  HCN25V02-ENDMM  PIC X(2).
              03  HCN25V02-ENDDD  PIC X(2).
            02  HCN25V02-JITSUNISSU PIC X(3).
            02  HCN25V02-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25V02-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V02-KHNTEN
                    PIC X(7)    OCCURS  28 TIMES.
            02  HCN25V02-KAISU
                    PIC X(3)    OCCURS  28 TIMES.
            02  HCN25V02-TENSU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25V02-KINGAKU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25V02-NYUYMD.
              03  HCN25V02-NYUY   PIC X(2).
              03  HCN25V02-NYUM   PIC X(2).
              03  HCN25V02-NYUD   PIC X(2).
            02  HCN25V02-SBTNAME    PIC X(2).
            02  HCN25V02-HOSPSBTKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25V02-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V02-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN25V02-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V02-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25V02-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN25V02-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25V02-NYUTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25V02-NYUTENSUN-G            OCCURS  11 TIMES.
              03  HCN25V02-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25V02-NYUKINGAKU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25V02-NYUKINGAKUN-G          OCCURS  11 TIMES.
              03  HCN25V02-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25V02-TANKA  PIC X(4).
            02  HCN25V02-TENSUG PIC X(7).
            02  HCN25V02-KINGAKUG   PIC X(7).
            02  HCN25V02-SRYKBN
                    PIC X(2)    OCCURS  27 TIMES.
            02  HCN25V02-TEKIYO
                    PIC X(52)   OCCURS  27 TIMES.
            02  HCN25V02-KIJYUN
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25V02-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V02-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN25V02-SYOKAKERU  PIC X(4).
            02  HCN25V02-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN25V02-SYONICHI   PIC X(4).
            02  HCN25V02-BIKOU
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCN25V02-SYONISSU   PIC X(3).
            02  HCN25V02-SYOKINGAKU PIC X(10).
            02  HCN25V02-SHINDANSYOSUU  PIC X(3).
            02  HCN25V02-FTNWARIAI  PIC X(3).
            02  HCN25V02-FTNNISSU   PIC X(3).
            02  HCN25V02-MEISAISYOSUU   PIC X(3).
            02  HCN25V02-SONOTANIN  PIC X(2).
            02  HCN25V02-SONOTAKAISU    PIC X(2).
            02  HCN25V02-SEIKYU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V02-SEIKYUG    PIC X(10).
            02  HCN25V02-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN25V02-SEIKYUYMD.
              03  HCN25V02-SEIKYUYY   PIC X(2).
              03  HCN25V02-SEIKYUMM   PIC X(2).
              03  HCN25V02-SEIKYUDD   PIC X(2).
            02  HCN25V02-HOSPJYUSYO PIC X(100).
            02  HCN25V02-HOSPNAME   PIC X(80).
            02  HCN25V02-ISYA   PIC X(40).
            02  HCN25V02-TEL    PIC X(15).
            02  HCN25V02-BYOSYOSU   PIC X(4).
            02  HCN25V02-HOKENNAME  PIC X(60).
            02  HCN25V02-PAGE   PIC X(5).
            02  HCN25V02-LABEL  PIC X(14).
            02  HCN25V02-LOPTION    PIC X(100).

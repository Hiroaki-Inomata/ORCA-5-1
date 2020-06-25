        01  HCN25V01.
            02  HCN25V01-HAKOUYM.
              03  HCN25V01-HAKKOUYY   PIC X(2).
              03  HCN25V01-HAKKOUMM   PIC X(2).
            02  HCN25V01-PTNUM  PIC X(20).
            02  HCN25V01-RENNUM PIC X(7).
            02  HCN25V01-KIGONUM    PIC X(50).
            02  HCN25V01-HKNJANAME  PIC X(60).
            02  HCN25V01-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V01-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25V01-KANANAME   PIC X(50).
            02  HCN25V01-NAME   PIC X(50).
            02  HCN25V01-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V01-TANJYOYY   PIC X(2).
            02  HCN25V01-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN25V01-NENREI PIC X(3).
            02  HCN25V01-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN25V01-JYUSYOUBI-G.
              03  HCN25V01-JYUSYOUBIYY    PIC X(2).
              03  HCN25V01-JYUSYOUBIMM    PIC X(2).
              03  HCN25V01-JYUSYOUBIDD    PIC X(2).
            02  HCN25V01-SYOSHINBI-G.
              03  HCN25V01-SYOSHINBIYY    PIC X(2).
              03  HCN25V01-SYOSHINBIMM    PIC X(2).
              03  HCN25V01-SYOSHINBIDD    PIC X(2).
            02  HCN25V01-STR-G.
              03  HCN25V01-STRYY  PIC X(2).
              03  HCN25V01-STRMM  PIC X(2).
              03  HCN25V01-STRDD  PIC X(2).
            02  HCN25V01-END-G.
              03  HCN25V01-ENDYY  PIC X(2).
              03  HCN25V01-ENDMM  PIC X(2).
              03  HCN25V01-ENDDD  PIC X(2).
            02  HCN25V01-JITSUNISSU PIC X(3).
            02  HCN25V01-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25V01-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V01-KHNTEN
                    PIC X(7)    OCCURS  28 TIMES.
            02  HCN25V01-KAISU
                    PIC X(3)    OCCURS  28 TIMES.
            02  HCN25V01-TENSU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25V01-KINGAKU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN25V01-NYUYMD.
              03  HCN25V01-NYUY   PIC X(2).
              03  HCN25V01-NYUM   PIC X(2).
              03  HCN25V01-NYUD   PIC X(2).
            02  HCN25V01-SBTNAME    PIC X(2).
            02  HCN25V01-HOSPSBTKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN25V01-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V01-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN25V01-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V01-KAKERU
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN25V01-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN25V01-NICHI
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25V01-NYUTENSU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25V01-NYUTENSUN-G            OCCURS  11 TIMES.
              03  HCN25V01-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25V01-NYUKINGAKU
                    PIC X(7)    OCCURS  11 TIMES.
            02  HCN25V01-NYUKINGAKUN-G          OCCURS  11 TIMES.
              03  HCN25V01-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN25V01-TANKA  PIC X(4).
            02  HCN25V01-TENSUG PIC X(7).
            02  HCN25V01-KINGAKUG   PIC X(7).
            02  HCN25V01-SRYKBN
                    PIC X(2)    OCCURS  27 TIMES.
            02  HCN25V01-TEKIYO
                    PIC X(52)   OCCURS  27 TIMES.
            02  HCN25V01-KIJYUN
                    PIC X(4)    OCCURS  5 TIMES.
            02  HCN25V01-SYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN25V01-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN25V01-SYOKAKERU  PIC X(4).
            02  HCN25V01-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN25V01-SYONICHI   PIC X(4).
            02  HCN25V01-BIKOU
                    PIC X(24)   OCCURS  3 TIMES.
            02  HCN25V01-SYONISSU   PIC X(3).
            02  HCN25V01-SYOKINGAKU PIC X(10).
            02  HCN25V01-SHINDANSYOSUU  PIC X(3).
            02  HCN25V01-FTNWARIAI  PIC X(3).
            02  HCN25V01-FTNNISSU   PIC X(3).
            02  HCN25V01-MEISAISYOSUU   PIC X(3).
            02  HCN25V01-SONOTANIN  PIC X(2).
            02  HCN25V01-SONOTAKAISU    PIC X(2).
            02  HCN25V01-SEIKYU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN25V01-SEIKYUG    PIC X(10).
            02  HCN25V01-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN25V01-SEIKYUYMD.
              03  HCN25V01-SEIKYUYY   PIC X(2).
              03  HCN25V01-SEIKYUMM   PIC X(2).
              03  HCN25V01-SEIKYUDD   PIC X(2).
            02  HCN25V01-HOSPJYUSYO PIC X(100).
            02  HCN25V01-HOSPNAME   PIC X(80).
            02  HCN25V01-ISYA   PIC X(40).
            02  HCN25V01-TEL    PIC X(15).
            02  HCN25V01-BYOSYOSU   PIC X(4).
            02  HCN25V01-HOKENNAME  PIC X(60).
            02  HCN25V01-PAGE   PIC X(5).
            02  HCN25V01-LABEL  PIC X(14).

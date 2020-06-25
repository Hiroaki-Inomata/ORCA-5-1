        01  HCN55V01.
            02  HCN55V01-HAKOUYM.
              03  HCN55V01-HAKKOUYY   PIC X(2).
              03  HCN55V01-HAKKOUMM   PIC X(2).
            02  HCN55V01-PTNUM  PIC X(20).
            02  HCN55V01-RENNUM PIC X(7).
            02  HCN55V01-KIGONUM    PIC X(50).
            02  HCN55V01-HKNJANAME  PIC X(60).
            02  HCN55V01-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55V01-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN55V01-KANANAME   PIC X(50).
            02  HCN55V01-NAME   PIC X(50).
            02  HCN55V01-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55V01-TANJYOYY   PIC X(2).
            02  HCN55V01-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN55V01-NENREI PIC X(3).
            02  HCN55V01-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCN55V01-JYUSYOUBI-G.
              03  HCN55V01-JYUSYOUBIYY    PIC X(2).
              03  HCN55V01-JYUSYOUBIMM    PIC X(2).
              03  HCN55V01-JYUSYOUBIDD    PIC X(2).
            02  HCN55V01-SYOSHINBI-G.
              03  HCN55V01-SYOSHINBIYY    PIC X(2).
              03  HCN55V01-SYOSHINBIMM    PIC X(2).
              03  HCN55V01-SYOSHINBIDD    PIC X(2).
            02  HCN55V01-STR-G.
              03  HCN55V01-STRYY  PIC X(2).
              03  HCN55V01-STRMM  PIC X(2).
              03  HCN55V01-STRDD  PIC X(2).
            02  HCN55V01-END-G.
              03  HCN55V01-ENDYY  PIC X(2).
              03  HCN55V01-ENDMM  PIC X(2).
              03  HCN55V01-ENDDD  PIC X(2).
            02  HCN55V01-JITSUNISSU PIC X(3).
            02  HCN55V01-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCN55V01-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55V01-KHNTEN
                    PIC X(7)    OCCURS  28 TIMES.
            02  HCN55V01-KAISU
                    PIC X(3)    OCCURS  28 TIMES.
            02  HCN55V01-TENSU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN55V01-KINGAKU
                    PIC X(7)    OCCURS  29 TIMES.
            02  HCN55V01-NYUYMD.
              03  HCN55V01-NYUY   PIC X(2).
              03  HCN55V01-NYUM   PIC X(2).
              03  HCN55V01-NYUD   PIC X(2).
            02  HCN55V01-HOSPSBTKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN55V01-NYUINSYUBETU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55V01-NYUKHNTEN
                    PIC X(7)    OCCURS  9 TIMES.
            02  HCN55V01-NYUKHNTENN
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55V01-KAKERU
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCN55V01-NYUKAISU
                    PIC X(3)    OCCURS  9 TIMES.
            02  HCN55V01-NICHI
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN55V01-NYUTENSU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN55V01-NYUTENSUN-G            OCCURS  10 TIMES.
              03  HCN55V01-NYUKOUTENN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN55V01-NYUKINGAKU
                    PIC X(7)    OCCURS  10 TIMES.
            02  HCN55V01-NYUKINGAKUN-G          OCCURS  10 TIMES.
              03  HCN55V01-NYUYKZTENSUN
                      PIC X(2)    OCCURS  6 TIMES.
            02  HCN55V01-TANKA  PIC X(4).
            02  HCN55V01-TENSUG PIC X(7).
            02  HCN55V01-KINGAKUG   PIC X(7).
            02  HCN55V01-SRYKBN
                    PIC X(2)    OCCURS  26 TIMES.
            02  HCN55V01-TEKIYO
                    PIC X(52)   OCCURS  26 TIMES.
            02  HCN55V01-KIJYUN
                    PIC X(4)    OCCURS  4 TIMES.
            02  HCN55V01-SYOKHNTEN
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCN55V01-SYOKAISU
                    PIC X(3)    OCCURS  4 TIMES.
            02  HCN55V01-KIJYUNNAME-SEI PIC X(10).
            02  HCN55V01-KIJYUN-SEI PIC X(2).
            02  HCN55V01-SYOKHNTEN-SEI
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCN55V01-SYOKAISU-SEI
                    PIC X(3)    OCCURS  2 TIMES.
            02  HCN55V01-SYONISSU   PIC X(3).
            02  HCN55V01-SYOKINGAKU PIC X(10).
            02  HCN55V01-SHINDANSYOSUU  PIC X(3).
            02  HCN55V01-FTNWARIAI  PIC X(3).
            02  HCN55V01-FTNNISSU   PIC X(3).
            02  HCN55V01-MEISAISYOSUU   PIC X(3).
            02  HCN55V01-SONOTANIN  PIC X(2).
            02  HCN55V01-SONOTAKAISU    PIC X(2).
            02  HCN55V01-SEIKYU
                    PIC X(10)   OCCURS  9 TIMES.
            02  HCN55V01-SEIKYUG    PIC X(10).
            02  HCN55V01-SEIKYUGAKUTOTAL    PIC X(10).
            02  HCN55V01-SEIKYUYMD.
              03  HCN55V01-SEIKYUYY   PIC X(2).
              03  HCN55V01-SEIKYUMM   PIC X(2).
              03  HCN55V01-SEIKYUDD   PIC X(2).
            02  HCN55V01-HOSPJYUSYO PIC X(100).
            02  HCN55V01-HOSPNAME   PIC X(80).
            02  HCN55V01-ISYA   PIC X(40).
            02  HCN55V01-TEL    PIC X(15).
            02  HCN55V01-BYOSYOSU   PIC X(4).
            02  HCN55V01-HOKENNAME  PIC X(60).
            02  HCN55V01-PAGE   PIC X(5).
            02  HCN55V01-LABEL  PIC X(14).

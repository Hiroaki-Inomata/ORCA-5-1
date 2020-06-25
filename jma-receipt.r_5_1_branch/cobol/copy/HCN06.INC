        01  HCN06.
            02  HCN06-TEISYUTUSAKI  PIC X(6).
            02  HCN06-PTNUM PIC X(20).
            02  HCN06-SRYY  PIC X(2).
            02  HCN06-SRYM  PIC X(2).
            02  HCN06-PREFNUM   PIC X(2).
            02  HCN06-HOSPCDN   PIC X(20).
            02  HCN06-SRYKBN    PIC X(2)        OCCURS  35 TIMES.
            02  HCN06-TEKIYO    PIC X(52)       OCCURS  35 TIMES.
            02  HCN06-KOHTEN    PIC X(7)        OCCURS  22 TIMES.
            02  HCN06-TOTALTEN  PIC X(7)        OCCURS  3 TIMES.
            02  HCN06-FTNMONEY  PIC X(8)        OCCURS  3 TIMES.
            02  HCN06-RJNFTNJANUM   PIC X(8).
            02  HCN06-KOH-TBL1.
              03  HCN06-KOHFTNJANUM1  PIC X(8).
              03  HCN06-KOHFTNJANUM1N PIC X(20).
              03  HCN06-KOHJKYSNUM1   PIC X(7).
              03  HCN06-KOHJKYSNUM1N  PIC X(20).
            02  HCN06-KOH-TBL2.
              03  HCN06-KOHFTNJANUM2  PIC X(8).
              03  HCN06-KOHFTNJANUM2N PIC X(20).
              03  HCN06-KOHJKYSNUM2   PIC X(7).
              03  HCN06-KOHJKYSNUM2N  PIC X(20).
            02  HCN06-HKNJANUM  PIC X(8).
            02  HCN06-KYURATE   PIC X(8).
            02  HCN06-RJNJKYSNUM    PIC X(7).
            02  HCN06-HKNSBT1   PIC X(6).
            02  HCN06-HKNSBT2   PIC X(6).
            02  HCN06-HONKZK    PIC X(10).
            02  HCN06-KIGO  PIC X(34).
            02  HCN06-NUM   PIC X(34).
            02  HCN06-KANANAME  PIC X(30).
            02  HCN06-SEXNUM    PIC X(2).
            02  HCN06-BIRTHDAY  PIC X(22).
            02  HCN06-JIYU  PIC X(20).
            02  HCN06-SEXN  PIC X(2).
            02  HCN06-TOKKI PIC X(12)   OCCURS  4 TIMES.
            02  HCN06-HOSPTEL   PIC X(15).
            02  HCN06-SRYKA PIC X(30).
            02  HCN06-BEDSU PIC X(3).
            02  HCN06-BYOMEI    PIC X(54)       OCCURS  5 TIMES.
            02  HCN06-SRYYMD    PIC X(24)       OCCURS  5 TIMES.
            02  HCN06-TENKI PIC X(10)   OCCURS  5 TIMES.
            02  HCN06-JINISSU   PIC X(2).
            02  HCN06-JINISSU1  PIC X(2).
            02  HCN06-JINISSU2  PIC X(2).
            02  HCN06-TENSU PIC X(7)    OCCURS  22 TIMES.
            02  HCN06-KAISU PIC X(3)    OCCURS  22 TIMES.
            02  HCN06-NYUKHNTEN PIC X(7)        OCCURS  10 TIMES.
            02  HCN06-HOSPNAME  PIC X(80).
            02  HCN06-HOSPADRS  PIC X(100).
            02  HCN06-NAME  PIC X(50).
            02  HCN06-HEAD1-G.
              03  HCN06-HEAD1 PIC X(46)   OCCURS  2 TIMES.
            02  HCN06-HEAD2-G.
              03  HCN06-HEAD2 PIC X(2)    OCCURS  7 TIMES.
            02  HCN06-HEAD3-G.
              03  HCN06-HEAD3 PIC X(50)   OCCURS  2 TIMES.
            02  HCN06-BODY  PIC X(52).
            02  HCN06-SHOSHINTIME   PIC X(10).
            02  HCN06-SIDOTIME  PIC X(10).
            02  HCN06-ZAITAKUTIME   PIC X(10).
            02  HCN06-TENKIYMD  PIC X(6)        OCCURS  5 TIMES.
            02  HCN06-FTNMONEYSAI   PIC X(11).
            02  HCN06-RENNUM    PIC X(7).
            02  HCN06-SRYKA2    PIC X(60).
            02  HCN06-NYUINKBN  PIC X(42).
            02  HCN06-NYUINYMD.
              03  HCN06-NYUINYY   PIC X(2).
              03  HCN06-NYUINMM   PIC X(2).
              03  HCN06-NYUINDD   PIC X(2).
            02  HCN06-HOSPSBT   PIC X(2).
            02  HCN06-NYUSYUBETU    PIC X(12)   OCCURS  11 TIMES.
            02  HCN06-KAKERU    PIC X(2)        OCCURS  5 TIMES.
            02  HCN06-NICHI PIC X(2)    OCCURS  5 TIMES.
            02  HCN06-SYOKJN    PIC X(4)        OCCURS  3 TIMES.
            02  HCN06-SYOKBN    PIC X(2)        OCCURS  4 TIMES.
            02  HCN06-SYOGAK    PIC X(4)        OCCURS  4 TIMES.
            02  HCN06-SYOKAISU  PIC X(2)        OCCURS  4 TIMES.
            02  HCN06-FOOT  PIC X(28).
            02  HCN06-GAIITBFTN PIC X(7).
            02  HCN06-GENGAKUMEI    PIC X(30).
            02  HCN06-SYONISSU  PIC X(2)        OCCURS  3 TIMES.
            02  HCN06-SYOTOTALTEN   PIC X(7)    OCCURS  3 TIMES.
            02  HCN06-SYOFTNMONEY   PIC X(8)    OCCURS  3 TIMES.
            02  HCN06-SYOFTNMONEYSAI    PIC X(11).
            02  HCN06-NYUTENSUN-G       OCCURS  10 TIMES.
              03  HCN06-NYUTENSUN PIC X(2)        OCCURS  6 TIMES.
            02  HCN06-NYUKOHTENN-G      OCCURS  10 TIMES.
              03  HCN06-NYUKOHTENN    PIC X(2)    OCCURS  6 TIMES.
            02  HCN06-NYUKHNTENN    PIC X(10)   OCCURS  10 TIMES.
            02  HCN06-NYUKAISU  PIC X(3)        OCCURS  10 TIMES.
            02  HCN06-NYUTENSU  PIC X(7)        OCCURS  10 TIMES.
            02  HCN06-NYUKOHTEN PIC X(7)        OCCURS  10 TIMES.

        01  SC03.
            02  SC03-STSNAME    PIC X(14).
            02  SC03-PRTYM  PIC X(16).
            02  SC03-PRTYY  PIC X(4).
            02  SC03-PRTMM  PIC X(4).
            02  SC03-HOSPCDN    PIC X(40).
            02  SC03-SEIYMD PIC X(22).
            02  SC03-POST   PIC X(8).
            02  SC03-ADRS   PIC X(200).
            02  SC03-HOSPNAME   PIC X(120).
            02  SC03-KANRINAME  PIC X(120).
            02  SC03-TEL    PIC X(13).
            02  SC03-BANK.
              03  SC03-BANKNAME   PIC X(20).
              03  SC03-SITENNAME  PIC X(20).
              03  SC03-SYUBETU    PIC X(4).
              03  SC03-KOZANUM    PIC X(20).
              03  SC03-KANAMEIGI  PIC X(80).
              03  SC03-MEIGI  PIC X(80).
              03  SC03-KOUZA1 PIC X(2).
              03  SC03-KOUZA2 PIC X(2).
              03  SC03-HENKO1 PIC X(2).
              03  SC03-HENKO2 PIC X(2).
            02  SC03-PTENSUKBN-TBL      OCCURS  4 TIMES.
              03  SC03-PTENSUKBN  PIC X(2).
            02  SC03-MEISAI-TBL         OCCURS  20 TIMES.
              03  SC03-PTID   PIC X(10).
              03  SC03-NAME   PIC X(14).
              03  SC03-NAME2  PIC X(14).
              03  SC03-SEX.
                04  SC03-SEX-M  PIC X(2).
                04  SC03-SEX-F  PIC X(2).
              03  SC03-BIRTHDAY.
                04  SC03-BIRTHGNG   PIC X(2).
                04  SC03-BIRTHYY    PIC X(2).
                04  SC03-BIRTHMM    PIC X(2).
                04  SC03-BIRTHDD    PIC X(2).
              03  SC03-BIRTHDAYK  PIC X(44).
              03  SC03-HKNKBN.
                04  SC03-HKNKBN-KOK PIC X(2).
                04  SC03-HKNKBN-SYHH    PIC X(2).
                04  SC03-HKNKBN-SYHK    PIC X(2).
              03  SC03-HKNJANUM   PIC X(8).
              03  SC03-NKJANAME   PIC X(40).
              03  SC03-KIGO   PIC X(20).
              03  SC03-NUM    PIC X(20).
              03  SC03-FTNJANUM   PIC X(8).
              03  SC03-JKYSNUM    PIC X(20).
              03  SC03-JKYSNUM-ETC    PIC X(10).
              03  SC03-SINRYO-YYMM    PIC X(5).
              03  SC03-NYUGAI-KUBUN       OCCURS  2 TIMES.
                04  SC03-NYUGAI-KBN PIC X(2).
              03  SC03-HONKZKKBN-TBL      OCCURS  2 TIMES.
                04  SC03-HONKZKKBN  PIC X(2).
              03  SC03-FTNWARIAI1-TBL     OCCURS  2 TIMES.
                04  SC03-FTNWARIAI1 PIC X(2).
              03  SC03-FTNWARIAI2 PIC X(2).
              03  SC03-FTNWARIAI-TBL.
                04  SC03-FTNWARIAI-9    PIC X(2).
                04  SC03-FTNWARIAI-2    PIC X(2).
                04  SC03-FTNWARIAI-3    PIC X(2).
                04  SC03-FTNWARIAI-ETC  PIC X(2).
              03  SC03-HKNSYUBETU-TBL     OCCURS  7 TIMES.
                04  SC03-HKNSYUBETU PIC X(2).
              03  SC03-SRYM   PIC X(2).
              03  SC03-SRMM   PIC X(4).
              03  SC03-TENSUKBN-TBL       OCCURS  4 TIMES.
                04  SC03-TENSUKBN   PIC X(2).
              03  SC03-NYUNISSU   PIC X(2).
              03  SC03-GTENSU PIC X(6).
              03  SC03-GTENSUN    PIC X(6).
              03  SC03-GTEN-WARI  PIC X(7).
              03  SC03-YKZITBFTN  PIC X(6).
              03  SC03-KOHIITBFTN PIC X(7).
              03  SC03-GKINGK PIC X(7).
              03  SC03-GKINGKN    PIC X(7).
              03  SC03-SRNISU PIC X(2).
              03  SC03-BIKO   PIC X(10).
              03  SC03-SRNISU-FROM.
                04  SC03-SRNISU-FROM-YY PIC X(4).
                04  SC03-SRNISU-FROM-MM PIC X(4).
                04  SC03-SRNISU-FROM-DD PIC X(4).
              03  SC03-SRNISU-TO.
                04  SC03-SRNISU-TO-YY   PIC X(4).
                04  SC03-SRNISU-TO-MM   PIC X(4).
                04  SC03-SRNISU-TO-DD   PIC X(4).
              03  SC03-HIYOU  PIC X(10).
            02  SC03-GOKEI-TBL          OCCURS  2 TIMES.
              03  SC03-GAIRAI-GKEN    PIC X(4).
              03  SC03-GAIRAI-GSRNISU PIC X(4).
              03  SC03-GAIRAI-GTENSU  PIC X(7).
              03  SC03-GAIRAI-GYKZKEN PIC X(4).
              03  SC03-GAIRAI-GYKZFTN PIC X(7).
              03  SC03-GAIRAI-GITBFTN PIC X(7).
              03  SC03-NYUIN-GKEN PIC X(4).
              03  SC03-NYUIN-GSRNISU  PIC X(4).
              03  SC03-NYUIN-GTESNSU  PIC X(7).
              03  SC03-NYUIN-GITBFTN  PIC X(7).
            02  SC03-GOKEI.
              03  SC03-GOKEI-GKEN PIC X(4).
              03  SC03-GOKEI-GSRNISU  PIC X(4).
              03  SC03-GOKEI-GTENSU   PIC X(7).
              03  SC03-GOKEI-GYKZKEN  PIC X(4).
              03  SC03-GOKEI-GYKZFTN  PIC X(7).
              03  SC03-GOKEI-GITBFTN  PIC X(7).
            02  SC03-SYKEI.
              03  SC03-SKEN   PIC X(4).
              03  SC03-SKEN2  PIC X(4).
              03  SC03-SKENK  PIC X(4).
              03  SC03-SKENS  PIC X(4).
              03  SC03-STENSU PIC X(10).
              03  SC03-STENSUN    PIC X(10).
              03  SC03-SKIN   PIC X(10).
              03  SC03-SKINN  PIC X(10).
              03  SC03-SYKZFTN    PIC X(10).
              03  SC03-SNISU  PIC X(3).
              03  SC03-SNNISU PIC X(3).
              03  SC03-SKENNYU    PIC X(4).
              03  SC03-SKENGAI    PIC X(4).
            02  SC03-GOUKEI.
              03  SC03-GKEN   PIC X(4).
              03  SC03-GKEN2  PIC X(4).
              03  SC03-GKENK  PIC X(4).
              03  SC03-GKENS  PIC X(4).
              03  SC03-GITBFTN    PIC X(10).
              03  SC03-GGTENSU    PIC X(10).
              03  SC03-GKIN   PIC X(10).
              03  SC03-GYKZFTN    PIC X(10).
              03  SC03-GKIN2  PIC X(10).
              03  SC03-GTESU  PIC X(10).
              03  SC03-GKENNYU    PIC X(4).
              03  SC03-GKENGAI    PIC X(4).

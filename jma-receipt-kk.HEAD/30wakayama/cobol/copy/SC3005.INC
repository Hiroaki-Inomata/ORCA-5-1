        01  SC05.
            02  SC05-STSNAME    PIC X(14).
            02  SC05-PRTYM  PIC X(16).
            02  SC05-PRTYY  PIC X(4).
            02  SC05-PRTMM  PIC X(4).
            02  SC05-HOSPCDN    PIC X(40).
            02  SC05-SEIYMD PIC X(22).
            02  SC05-POST   PIC X(8).
            02  SC05-ADRS   PIC X(200).
            02  SC05-HOSPNAME   PIC X(120).
            02  SC05-KANRINAME  PIC X(120).
            02  SC05-TEL    PIC X(13).
            02  SC05-BANK.
              03  SC05-BANKNAME   PIC X(20).
              03  SC05-SITENNAME  PIC X(20).
              03  SC05-SYUBETU    PIC X(4).
              03  SC05-KOZANUM    PIC X(20).
              03  SC05-KANAMEIGI  PIC X(80).
              03  SC05-MEIGI  PIC X(80).
              03  SC05-KOUZA1 PIC X(2).
              03  SC05-KOUZA2 PIC X(2).
              03  SC05-HENKO1 PIC X(2).
              03  SC05-HENKO2 PIC X(2).
            02  SC05-PTENSUKBN-TBL      OCCURS  4 TIMES.
              03  SC05-PTENSUKBN  PIC X(2).
            02  SC05-GOKEI-TBL.
              03  SC05-GKENSU PIC X(3).
              03  SC05-GKOHFTN    PIC X(7).
            02  SC05-MEISAI-TBL         OCCURS  20 TIMES.
              03  SC05-PTID   PIC X(10).
              03  SC05-NAME   PIC X(14).
              03  SC05-SEX.
                04  SC05-SEX-M  PIC X(2).
                04  SC05-SEX-F  PIC X(2).
              03  SC05-BIRTHDAY   PIC X(44).
              03  SC05-HKNKBN.
                04  SC05-HKNKBN-KOK PIC X(2).
                04  SC05-HKNKBN-SYHH    PIC X(2).
                04  SC05-HKNKBN-SYHK    PIC X(2).
              03  SC05-HKNJANUM   PIC X(8).
              03  SC05-NKJANAME   PIC X(40).
              03  SC05-KIGO   PIC X(20).
              03  SC05-NUM    PIC X(20).
              03  SC05-FTNJANUM   PIC X(8).
              03  SC05-JKYSNUM    PIC X(7).
              03  SC05-JKYSNUM-ETC    PIC X(10).
              03  SC05-SINRYO-YYMM    PIC X(5).
              03  SC05-HONKZKKBN-TBL      OCCURS  2 TIMES.
                04  SC05-HONKZKKBN  PIC X(2).
              03  SC05-FTNWARIAI1-TBL     OCCURS  2 TIMES.
                04  SC05-FTNWARIAI1 PIC X(2).
              03  SC05-FTNWARIAI2 PIC X(1).
              03  SC05-FTNWARIAI-TBL.
                04  SC05-FTNWARIAI-9    PIC X(2).
                04  SC05-FTNWARIAI-8    PIC X(2).
                04  SC05-FTNWARIAI-7    PIC X(2).
                04  SC05-FTNWARIAI-ETC  PIC X(2).
              03  SC05-SRYM   PIC X(2).
              03  SC05-HKNSYUBETU-TBL     OCCURS  7 TIMES.
                04  SC05-HKNSYUBETU PIC X(2).
              03  SC05-TENSUKBN-TBL       OCCURS  4 TIMES.
                04  SC05-TENSUKBN   PIC X(2).
              03  SC05-NYUNISSU   PIC X(2).
              03  SC05-GTENSU PIC X(6).
              03  SC05-GTENSUN    PIC X(6).
              03  SC05-GTEN-WARI  PIC X(7).
              03  SC05-YKZITBFTN  PIC X(6).
              03  SC05-GKINGK PIC X(7).
              03  SC05-GKINGKN    PIC X(7).
              03  SC05-KOHIITBFTN PIC X(7).
              03  SC05-SRNISU PIC X(2).
              03  SC05-BIKO   PIC X(10).
              03  SC05-SRNISU-FROM.
                04  SC05-SRNISU-FROM-YY PIC X(4).
                04  SC05-SRNISU-FROM-MM PIC X(4).
                04  SC05-SRNISU-FROM-DD PIC X(4).
              03  SC05-SRNISU-TO.
                04  SC05-SRNISU-TO-YY   PIC X(4).
                04  SC05-SRNISU-TO-MM   PIC X(4).
                04  SC05-SRNISU-TO-DD   PIC X(4).
            02  SC05-GOKEI.
              03  SC05-GKEN   PIC X(4).
              03  SC05-GKENK  PIC X(4).
              03  SC05-GKENS  PIC X(4).
              03  SC05-GITBFTN    PIC X(7).
              03  SC05-GTENSU PIC X(10).
            02  SC05-SYKEI.
              03  SC05-SKEN   PIC X(4).
              03  SC05-SKEN2  PIC X(4).
              03  SC05-SKENK  PIC X(4).
              03  SC05-SKENS  PIC X(4).
              03  SC05-STENSU PIC X(10).
              03  SC05-STENSUN    PIC X(10).
              03  SC05-SKIN   PIC X(10).
              03  SC05-SKINN  PIC X(10).
              03  SC05-SYKZFTN    PIC X(10).
              03  SC05-SNISU  PIC X(3).
              03  SC05-SNNISU PIC X(3).

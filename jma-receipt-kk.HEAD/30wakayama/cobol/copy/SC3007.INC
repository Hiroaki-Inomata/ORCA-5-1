        01  SC07.
            02  SC07-STSNAME    PIC X(14).
            02  SC07-TENSUKBN-TBL       OCCURS  4 TIMES.
              03  SC07-TENSUKBN   PIC X(2).
            02  SC07-PRTYM  PIC X(16).
            02  SC07-PRTYY  PIC X(4).
            02  SC07-PRTMM  PIC X(4).
            02  SC07-HOSPCDN    PIC X(40).
            02  SC07-SEIYMD PIC X(22).
            02  SC07-POST   PIC X(8).
            02  SC07-ADRS   PIC X(200).
            02  SC07-HOSPNAME   PIC X(120).
            02  SC07-KANRINAME  PIC X(120).
            02  SC07-TEL    PIC X(13).
            02  SC07-BANK.
              03  SC07-BANKNAME   PIC X(20).
              03  SC07-SITENNAME  PIC X(20).
              03  SC07-SYUBETU    PIC X(4).
              03  SC07-KOZANUM    PIC X(20).
              03  SC07-KANAMEIGI  PIC X(80).
              03  SC07-MEIGI  PIC X(80).
              03  SC07-KOUZA1 PIC X(2).
              03  SC07-KOUZA2 PIC X(2).
              03  SC07-HENKO1 PIC X(2).
              03  SC07-HENKO2 PIC X(2).
            02  SC07-GOKEI-TBL.
              03  SC07-GKENSU PIC X(3).
              03  SC07-GKOHFTN    PIC X(7).
            02  SC07-MEISAI-TBL         OCCURS  20 TIMES.
              03  SC07-JKYSNUM    PIC X(20).
              03  SC07-NAME   PIC X(14).
              03  SC07-NAME2  PIC X(14).
              03  SC07-HKNJANUM   PIC X(8).
              03  SC07-KIGOBG PIC X(16).
              03  SC07-FTNWARIAI  PIC X(1).
              03  SC07-NISSU  PIC X(2).
              03  SC07-SRNISU PIC X(2).
              03  SC07-SRMM   PIC X(4).
              03  SC07-TENSU  PIC X(6).
              03  SC07-TENSUN PIC X(6).
              03  SC07-ETCFTN PIC X(6).
              03  SC07-YKZITBFTN  PIC X(6).
              03  SC07-KOHFTN PIC X(6).
              03  SC07-KOHFTNN    PIC X(6).
              03  SC07-HKNKBN.
                04  SC07-HKNKBN-KOK PIC X(2).
                04  SC07-HKNKBN-SYH PIC X(2).
              03  SC07-NYUGAI-KUBUN       OCCURS  2 TIMES.
                04  SC07-NYUGAI-KBN PIC X(2).
              03  SC07-FTNWARIAI-TBL.
                04  SC07-FTNWARIAI-9    PIC X(2).
                04  SC07-FTNWARIAI-2    PIC X(2).
                04  SC07-FTNWARIAI-3    PIC X(2).
                04  SC07-FTNWARIAI-ETC  PIC X(2).
              03  SC07-HKNSYUBETU-TBL     OCCURS  7 TIMES.
                04  SC07-HKNSYUBETU PIC X(2).
              03  SC07-SEX.
                04  SC07-SEX-M  PIC X(2).
                04  SC07-SEX-F  PIC X(2).
              03  SC07-BIRTHDAYK  PIC X(44).
              03  SC07-BIRTHDAY.
                04  SC07-BIRTHDAYYY PIC X(4).
                04  SC07-BIRTHDAYMM PIC X(4).
                04  SC07-BIRTHDAYDD PIC X(4).
              03  SC07-NKJANAME   PIC X(40).
              03  SC07-NUM    PIC X(20).
              03  SC07-SRNISU-FROM.
                04  SC07-SRNISU-FROM-YY PIC X(4).
                04  SC07-SRNISU-FROM-MM PIC X(4).
                04  SC07-SRNISU-FROM-DD PIC X(4).
              03  SC07-SRNISU-TO.
                04  SC07-SRNISU-TO-YY   PIC X(4).
                04  SC07-SRNISU-TO-MM   PIC X(4).
                04  SC07-SRNISU-TO-DD   PIC X(4).
              03  SC07-BIKOU1 PIC X(40).
              03  SC07-BIKOU2 PIC X(40).
              03  SC07-BIKOU3 PIC X(40).
              03  SC07-HIYOU  PIC X(10).
            02  SC07-SYKEI.
              03  SC07-SKEN   PIC X(4).
              03  SC07-SKEN2  PIC X(4).
              03  SC07-SKENK  PIC X(4).
              03  SC07-SKENS  PIC X(4).
              03  SC07-STENSU PIC X(10).
              03  SC07-STENSUN    PIC X(10).
              03  SC07-SKIN   PIC X(10).
              03  SC07-SKINN  PIC X(10).
              03  SC07-SNISU  PIC X(3).
              03  SC07-SNNISU PIC X(3).
              03  SC07-SKENNYU    PIC X(4).
              03  SC07-SKENGAI    PIC X(4).
            02  SC07-GOKEI.
              03  SC07-GKEN   PIC X(4).
              03  SC07-GKEN2  PIC X(4).
              03  SC07-GKENK  PIC X(4).
              03  SC07-GKENS  PIC X(4).
              03  SC07-GITBFTN    PIC X(7).
              03  SC07-GTENSU PIC X(10).
              03  SC07-GKIN   PIC X(10).
              03  SC07-GTESU  PIC X(10).
              03  SC07-GKENNYU    PIC X(4).
              03  SC07-GKENGAI    PIC X(4).

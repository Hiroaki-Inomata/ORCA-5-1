      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô    ÆüÉÕ        ÆâÍÆ
      *  01.00.01    ÇëÌî      2018/12/12  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
        01  SC11.
            02  SC11-STSNAME    PIC X(14).
            02  SC11-PRTYM  PIC X(16).
            02  SC11-PRTYY  PIC X(4).
            02  SC11-PRTMM  PIC X(4).
            02  SC11-HOSPCDN    PIC X(40).
            02  SC11-SEIYMD PIC X(22).
            02  SC11-POST   PIC X(8).
            02  SC11-ADRS   PIC X(200).
            02  SC11-HOSPNAME   PIC X(120).
            02  SC11-KANRINAME  PIC X(120).
            02  SC11-TEL    PIC X(13).
            02  SC11-BANK.
              03  SC11-BANKNAME   PIC X(20).
              03  SC11-SITENNAME  PIC X(20).
              03  SC11-SYUBETU    PIC X(4).
              03  SC11-KOZANUM    PIC X(20).
              03  SC11-KANAMEIGI  PIC X(80).
              03  SC11-MEIGI  PIC X(80).
              03  SC11-KOUZA1 PIC X(2).
              03  SC11-KOUZA2 PIC X(2).
              03  SC11-HENKO1 PIC X(2).
              03  SC11-HENKO2 PIC X(2).
            02  SC11-PTENSUKBN-TBL      OCCURS  4 TIMES.
              03  SC11-PTENSUKBN  PIC X(2).
            02  SC11-GOKEI-TBL.
              03  SC11-GKENSU PIC X(3).
              03  SC11-GKOHFTN    PIC X(7).
            02  SC11-MEISAI-TBL         OCCURS  20 TIMES.
              03  SC11-PTID   PIC X(10).
              03  SC11-NAME   PIC X(14).
              03  SC11-SEX.
                04  SC11-SEX-M  PIC X(2).
                04  SC11-SEX-F  PIC X(2).
              03  SC11-BIRTHDAY   PIC X(44).
              03  SC11-HKNKBN.
                04  SC11-HKNKBN-KOK PIC X(2).
                04  SC11-HKNKBN-SYHH    PIC X(2).
                04  SC11-HKNKBN-SYHK    PIC X(2).
              03  SC11-HKNJANUM   PIC X(8).
              03  SC11-NKJANAME   PIC X(40).
              03  SC11-KIGO   PIC X(20).
              03  SC11-NUM    PIC X(20).
              03  SC11-FTNJANUM   PIC X(8).
              03  SC11-JKYSNUM    PIC X(7).
              03  SC11-JKYSNUM-ETC    PIC X(10).
              03  SC11-SINRYO-YYMM    PIC X(5).
              03  SC11-HONKZKKBN-TBL      OCCURS  2 TIMES.
                04  SC11-HONKZKKBN  PIC X(2).
              03  SC11-FTNWARIAI1-TBL     OCCURS  2 TIMES.
                04  SC11-FTNWARIAI1 PIC X(2).
              03  SC11-FTNWARIAI2 PIC X(1).
              03  SC11-FTNWARIAI-TBL.
                04  SC11-FTNWARIAI-9    PIC X(2).
                04  SC11-FTNWARIAI-8    PIC X(2).
                04  SC11-FTNWARIAI-7    PIC X(2).
                04  SC11-FTNWARIAI-ETC  PIC X(2).
              03  SC11-SRYM   PIC X(2).
              03  SC11-SRMM   PIC X(4).
              03  SC11-HKNSYUBETU-TBL     OCCURS  7 TIMES.
                04  SC11-HKNSYUBETU PIC X(2).
              03  SC11-TENSUKBN-TBL       OCCURS  4 TIMES.
                04  SC11-TENSUKBN   PIC X(2).
              03  SC11-NYUNISSU   PIC X(2).
              03  SC11-GTENSU PIC X(6).
              03  SC11-GTEN-WARI  PIC X(7).
              03  SC11-YKZITBFTN  PIC X(6).
              03  SC11-GKINGK PIC X(7).
              03  SC11-KOHIITBFTN PIC X(7).
              03  SC11-SRNISU PIC X(2).
              03  SC11-BIKO   PIC X(10).
              03  SC11-SRNISU-FROM.
                04  SC11-SRNISU-FROM-YY PIC X(4).
                04  SC11-SRNISU-FROM-MM PIC X(4).
                04  SC11-SRNISU-FROM-DD PIC X(4).
              03  SC11-SRNISU-TO.
                04  SC11-SRNISU-TO-YY   PIC X(4).
                04  SC11-SRNISU-TO-MM   PIC X(4).
                04  SC11-SRNISU-TO-DD   PIC X(4).
              03  SC11-HIYOU  PIC X(10).
            02  SC11-GOKEI.
              03  SC11-GKEN   PIC X(4).
              03  SC11-GKENK  PIC X(4).
              03  SC11-GKENS  PIC X(4).
              03  SC11-GITBFTN    PIC X(7).

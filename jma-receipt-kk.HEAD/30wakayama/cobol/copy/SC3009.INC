        01  SC09.
            02  SC09-TTKBN-TBL          OCCURS  2 TIMES.
              03  SC09-TTKBN  PIC X(2).
            02  SC09-TENSUKBN-TBL       OCCURS  3 TIMES.
              03  SC09-TENSUKBN   PIC X(2).
            02  SC09-STSNAME    PIC X(14).
            02  SC09-HOSPCDN    PIC X(40).
            02  SC09-SEIYMD PIC X(22).
            02  SC09-ADRS   PIC X(200).
            02  SC09-HOSPNAME   PIC X(120).
            02  SC09-KANRINAME  PIC X(120).
            02  SC09-TEL    PIC X(13).
            02  SC09-PRTYM  PIC X(16).
            02  SC09-BANK.
              03  SC09-BANKNAME   PIC X(20).
              03  SC09-SITENNAME  PIC X(20).
              03  SC09-SYUBETU    PIC X(4).
              03  SC09-KOZANUM    PIC X(20).
              03  SC09-KANAMEIGI  PIC X(80).
              03  SC09-MEIGI  PIC X(80).
              03  SC09-KOUZA1 PIC X(2).
              03  SC09-KOUZA2 PIC X(2).
              03  SC09-HENKO1 PIC X(2).
              03  SC09-HENKO2 PIC X(2).
            02  SC09-PTENSUKBN-TBL      OCCURS  4 TIMES.
              03  SC09-PTENSUKBN  PIC X(2).
            02  SC09-GOKEI.
              03  SC09-GKENSU PIC X(3).
              03  SC09-GKOHFTN    PIC X(7).
            02  SC09-MEISAI-TBL         OCCURS  20 TIMES.
              03  SC09-JKYSNUM    PIC X(8).
              03  SC09-JKYSNUM-ETC    PIC X(10).
              03  SC09-NAME   PIC X(14).
              03  SC09-SRYM   PIC X(2).
              03  SC09-SEX.
                04  SC09-SEX-M  PIC X(2).
                04  SC09-SEX-F  PIC X(2).
              03  SC09-BIRTHDAY.
                04  SC09-BIRTHGNG   PIC X(2).
                04  SC09-BIRTHDAYYY PIC X(2).
                04  SC09-BIRTHDAYMM PIC X(2).
                04  SC09-BIRTHDAYDD PIC X(2).
              03  SC09-BIRTHDAYK  PIC X(44).
              03  SC09-NYUGAIKBN-TBL      OCCURS  2 TIMES.
                04  SC09-NISSU  PIC X(2).
                04  SC09-TENSU  PIC X(6).
                04  SC09-FTNMONEY   PIC X(7).
              03  SC09-SYOHOU PIC X(10).
              03  SC09-HKNKBN.
                04  SC09-HKNKBN-KOK PIC X(2).
                04  SC09-HKNKBN-SYH PIC X(2).
              03  SC09-HKNJANUM   PIC X(8).
              03  SC09-NKJANAME   PIC X(40).
              03  SC09-KIGO   PIC X(20).
              03  SC09-NUM    PIC X(20).
              03  SC09-FTNJANUM   PIC X(8).
              03  SC09-FTNWARIAI2 PIC X(2).
              03  SC09-FTNWARIAI-TBL.
                04  SC09-FTNWARIAI-9    PIC X(2).
                04  SC09-FTNWARIAI-2    PIC X(2).
                04  SC09-FTNWARIAI-3    PIC X(2).
                04  SC09-FTNWARIAI-ETC  PIC X(2).
            02  SC09-GOKE.
              03  SC09-GKEN   PIC X(4).
              03  SC09-GKENK  PIC X(4).
              03  SC09-GKENS  PIC X(4).
              03  SC09-GITBFTN    PIC X(7).
              03  SC09-GTENSU PIC X(10).
            02  SC09-SYKEI.
              03  SC09-SKEN   PIC X(4).
              03  SC09-SKEN2  PIC X(4).
              03  SC09-SKENK  PIC X(4).
              03  SC09-SKENS  PIC X(4).
              03  SC09-SKENN  PIC X(4).
              03  SC09-SKENG  PIC X(4).
              03  SC09-STENSU PIC X(10).
              03  SC09-STENSUN    PIC X(10).
              03  SC09-SKIN   PIC X(10).
              03  SC09-SKINN  PIC X(10).
              03  SC09-SKING  PIC X(10).
              03  SC09-SYKZFTN    PIC X(10).
              03  SC09-SNISU  PIC X(3).
              03  SC09-SNNISU PIC X(3).
            02  SC09-GOKEI-TBL.
              03  SC09-GGAINISSU  PIC X(2).
              03  SC09-GGAITENSU  PIC X(6).
              03  SC09-GGAIFTNMONEY   PIC X(9).
              03  SC09-GINNISSU   PIC X(2).
              03  SC09-GINTENSU   PIC X(6).
              03  SC09-GINFTNMONEY    PIC X(9).
            02  SC09-SOGOKEI    PIC X(9).

      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
            02  H01.
              03  H01-B10-STATE   PIC S9(09) BINARY.
              03  H01-B12-STATE   PIC S9(09) BINARY.
              03  H01-PTNUM.
                04  H01-VALUE   PIC X(20).
              03  H01-SRYYM.
                04  H01-VALUE   PIC X(7).
              03  H01-SRYKA.
                04  H01-SRYKA-STATE   PIC S9(9)   BINARY.
                04  H01-SRYKA-VALUE   PIC X(2).
              03  H01-SCROLL.
                04  H01-SCR-HPOS      PIC S9(9)   BINARY.
                04  H01-SCR-VPOS      PIC S9(9)   BINARY.
              03  H01-MIDASI-G  OCCURS  38.
                04  H01-MIDASI-STYLE   PIC X(20).
                04  H01-MIDASI         PIC X(20).
              03  H01-KHNTEN-G  OCCURS  38.
                04  H01-KHNTEN-STATE   PIC S9(9)   BINARY.
                04  H01-KHNTEN   PIC 9(8).
              03  H01-KAISU-G   OCCURS  38.
                04  H01-KAISU-STATE   PIC S9(9)   BINARY.
                04  H01-KAISU   PIC 9(5).
              03  H01-TENSU-G   OCCURS  3.
                04  H01-TENSU-TBL OCCURS  38.
                  05  H01-TENSU-STATE   PIC S9(9)   BINARY.
                  05  H01-TENSU   PIC S9(8).
              03  H01-TOTALTEN-G  OCCURS  5.
                04  H01-TOTALTEN-STATE   PIC S9(9)   BINARY.
                04  H01-TOTALTEN   PIC 9(8).
              03  H01-FTNMONEY-G  OCCURS  5.
                04  H01-FTNMONEY-STATE   PIC S9(9)   BINARY.
                04  H01-FTNMONEY   PIC 9(8).
              03  H01-YKZFTN-G  OCCURS  5.
                04  H01-YKZFTN-STATE   PIC S9(9)   BINARY.
                04  H01-YKZFTN   PIC 9(8).
              03  H01-JNISSU-G  OCCURS  5.
                04  H01-JNISSU-STATE   PIC S9(9)   BINARY.
                04  H01-JNISSU   PIC 9(8).
              03  H01-SHORYOYOHI-G  OCCURS  5.
                04  H01-SHORYOYOHI-STATE   PIC S9(9)   BINARY.
                04  H01-SHORYOYOHI   PIC 9(8).
              03  H01-SHOFTN-G  OCCURS  5.
                04  H01-SHOFTN-STATE   PIC S9(9)   BINARY.
                04  H01-SHOFTN   PIC 9(8).
              03  H01-SHONISSU-G  OCCURS  5.
                04  H01-SHONISSU-STATE   PIC S9(9)   BINARY.
                04  H01-SHONISSU   PIC 9(2).
              03  H01-NKHNTEN-G  OCCURS  38.
                04  H01-NKHNTEN-STATE   PIC S9(09) BINARY.
                04  H01-NKHNTEN   PIC 9(8).
              03  H01-NKAISU-G  OCCURS  38.
                04  H01-NKAISU-STATE   PIC S9(09) BINARY.
                04  H01-NKAISU   PIC 9(5).
              03  H01-NTENSU-G  OCCURS  3.
                04  H01-NTENSU-TBL  OCCURS  38.
                  05  H01-NTENSU-STATE   PIC S9(09) BINARY.
                  05  H01-NTENSU   PIC S9(8).
              03  H01-NTOTALTEN-G OCCURS  5.
                04  H01-NTOTALTEN-STATE   PIC S9(9)   BINARY.
                04  H01-NTOTALTEN   PIC 9(8).
              03  H01-NFTNMONEY-G OCCURS  5.
                04  H01-NFTNMONEY-STATE   PIC S9(09) BINARY.
                04  H01-NFTNMONEY   PIC 9(8).
              03  H01-NYKZFTN-G   OCCURS  5.
                04  H01-NYKZFTN-STATE   PIC S9(09) BINARY.
                04  H01-NYKZFTN   PIC 9(8).
              03  H01-NJNISSU-G   OCCURS  5.
                04  H01-NJNISSU-STATE   PIC S9(09) BINARY.
                04  H01-NJNISSU   PIC 9(8).
              03  H01-NSHORYOYOHI-G  OCCURS  5.
                04  H01-NSHORYOYOHI-STATE   PIC S9(9)   BINARY.
                04  H01-NSHORYOYOHI   PIC 9(8).
              03  H01-NSHOFTN-G  OCCURS  5.
                04  H01-NSHOFTN-STATE   PIC S9(9)   BINARY.
                04  H01-NSHOFTN   PIC 9(8).
              03  H01-NSHONISSU-G  OCCURS  5.
                04  H01-NSHONISSU-STATE   PIC S9(9)   BINARY.
                04  H01-NSHONISSU   PIC 9(2).
              03  H01-SHOMIDASI-G  OCCURS  12.
                04  H01-SHOMIDASI  PIC X(14).
              03  H01-TENMIDASI-G  OCCURS  16.
                04  H01-TENMIDASI  PIC X(14).
              03  H01-SKYKBN-COMBO.
                04  H01-SKYKBN.
                  05  H01-VALUE   PIC X(15).
                04  H01-SKYKBN-COUNT   PIC S9(9)   BINARY.
                04  H01-SKYKBNLST PIC X(14)   OCCURS  3 TIMES.
              03  H01-SKYYM.
                04  H01-VALUE   PIC X(7).
              03  H01-HENREIYM.
                04  H01-VALUE   PIC X(7).
              03  H01-HENREI-SIJI-FLG-STATE   PIC S9(09) BINARY.
              03  H01-HENREI-SIJI-FLG-STYLE   PIC X(20).
              03  H01-HENREI-SIJI-FLG.
                04  H01-VALUE   PIC X.
              03  H01-HENREI-SIJI-FLG-LABEL   PIC X(6).
              03  H01-LABEL-KOHSKY         PIC X(16).
              03  H01-SKYYM-KOHSKY-STATE   PIC S9(9)   BINARY.
              03  H01-SKYYM-KOHSKY         PIC X(7).
              03  H01-HENREIYM-KOHSKY-STATE   PIC S9(9)   BINARY.
              03  H01-HENREIYM-KOHSKY         PIC X(7).
              03  H01-MSG.
                04  H01-MSG-STYLE   PIC X(20).
                04  H01-MSG-VALUE   PIC X(50).
              03  H01-SRYKANAME.
                04  H01-SRYKANAME-STYLE   PIC X(20).
                04  H01-SRYKANAME-VALUE   PIC X(18).
              03  H01-NAME.
                04  H01-NAME-STYLE   PIC X(20).
                04  H01-NAME-VALUE   PIC X(24).
              03  H01-TEISEIKBNNM.
                04  H01-TEISEIKBNNM-STYLE   PIC X(20).
                04  H01-TEISEIKBNNM-VALUE   PIC X(14).
              03  H01-RECEDEN-CHK-MSG.
                04  H01-RECEDEN-CHK-MSG-STYLE   PIC X(20).
                04  H01-RECEDEN-CHK-MSG-VALUE   PIC X(30).
              03  H01-HNYUGAIKBN.
                04  H01-HNYUGAIKBN-STYLE   PIC X(20).
                04  H01-HNYUGAIKBN-VALUE   PIC X(6).
              03  H01-HTEISYUTUSAKI.
                04  H01-HTEISYUTUSAKI-STYLE   PIC X(20).
                04  H01-HTEISYUTUSAKI-VALUE   PIC X(10).
              03  H01-HRECESYUBETU.
                04  H01-HRECESYUBETU-STYLE   PIC X(20).
                04  H01-HRECESYUBETU-VALUE   PIC X(14).
              03  H01-HHKNJANUM.
                04  H01-HHKNJANUM-STYLE   PIC X(20).
                04  H01-HHKNJANUM-VALUE   PIC X(14).
              03  H01-HHOJOKBN.
                04  H01-HHOJOKBN-STYLE   PIC X(20).
                04  H01-HHOJOKBN-VALUE   PIC X(6).
              03  H01-HKIGO.
                04  H01-HKIGO-STYLE   PIC X(20).
                04  H01-HKIGO-VALUE   PIC X(20).
              03  H01-HRECEIPT-KBN.
                04  H01-HRECEIPT-KBN-STYLE   PIC X(20).
                04  H01-HRECEIPT-KBN-VALUE   PIC X(10).
              03  H01-HENREI-MSG.
                04  H01-HENREI-MSG-STYLE   PIC X(20).
                04  H01-HENREI-MSG-VALUE   PIC X(14).
              03  H01-HCREYMD.
                04  H01-HCREYMD-STYLE   PIC X(20).
                04  H01-HCREYMD-VALUE   PIC X(19).
              03  H01-AGE.
                04  H01-VALUE   PIC 9(3).
              03  H01-KYURATE.
                04  H01-VALUE   PIC 9(3).
              03  H01-ZAITAKU.
                04  H01-VALUE   PIC X(20).
              03  H01-HONKZKKBN.
                04  H01-VALUE   PIC X(4).
              03  H01-TITLE-HKNJANUM.
                04  H01-VALUE   PIC X(16).
              03  H01-TITLE-KIGO.
                04  H01-VALUE   PIC X(10).

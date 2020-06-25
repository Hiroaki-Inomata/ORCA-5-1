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
        02  W14.
                04  W14-FIXED2.
                  05  W14-B05-LABEL PIC X(06).
                  05  W14-MSG-STYLE PIC X(20).
                  05  W14-MSG.
                    06  W14-VALUE   PIC X(50).
                  05  W14-STYUKYMD.
                    06  W14-VALUE   PIC X(9).
                  05  W14-EDYUKYMD.
                    06  W14-VALUE   PIC X(9).
                04  W14-CD-G   OCCURS 25.
                  05  W14-CD.
                    06  W14-VALUE   PIC X(4).
                04  W14-SHELLID-G   OCCURS 25.
                  05  W14-SHELLID-STYLE PIC X(20).
                  05  W14-SHELLID.
                    06  W14-VALUE   PIC X(20).
                04  W14-SHORIMSG-G   OCCURS 25.
                  05  W14-SHORIMSG.
                    06  W14-VALUE   PIC X(40).
                04  W14-CHKBTN01-G   OCCURS 25.
                  05  W14-CHK.
                    06  W14-CHKG     OCCURS 4.
                      07  W14-CHKBTN   PIC X.
                04  W14-PRTNM-G   OCCURS 25.
                  05  W14-PRTNM.
                    06  W14-VALUE   PIC X(16).
                04  W14-PARAKBN-COMBO-G   OCCURS 25.
                  05  W14-PARAKBN-COMBO.
                    06  W14-PARAKBN.
                      07  W14-VALUE   PIC X(14).
                    06  W14-PARAKBN-COUNT   PIC S9(9)   BINARY.
                    06  W14-PARAKBN-ITEM    PIC X(14)   OCCURS  3 TIMES.
                04  W14-PRTKBN11     PIC X.
                04  W14-PRTKBN12     PIC X.
                04  W14-PRTKBN21     PIC X.
                04  W14-PRTKBN22     PIC X.
                04  W14-PRTKBN31     PIC X.
                04  W14-PRTKBN32     PIC X.
                04  W14-PRTKBN41-STATE   PIC S9(9)   BINARY.
                04  W14-PRTKBN41     PIC X.
                04  W14-PRTKBN42-STATE   PIC S9(9)   BINARY.
                04  W14-PRTKBN42     PIC X.
                04  W14-PRTKBN51-STATE   PIC S9(9)   BINARY.
                04  W14-PRTKBN51     PIC X.
                04  W14-PRTKBN52-STATE   PIC S9(9)   BINARY.
                04  W14-PRTKBN52     PIC X.
                04  W14-BD-G         OCCURS 25.
                    05  W14-BD-VISIBLE  PIC X(01).
                    05  W14-BD-STYLE    PIC X(20).
                    05  W14-BD-LABEL    PIC X(01).

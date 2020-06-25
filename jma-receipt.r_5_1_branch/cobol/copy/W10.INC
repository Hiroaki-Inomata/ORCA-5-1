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
          02  W10.
              03  W10-B03.
                04  W10-B03-STATE       PIC S9(9)   BINARY.
              03  W10-B06               PIC X(08).
              03  W10-B07               PIC X(08).
              03  W10-B12.
                04  W10-B12-STATE       PIC S9(9)   BINARY.
              03  W10-LBLTITLE          PIC X(20).
              03  W10-LBLKANRICD-G.
                04  W10-LBLKANRICD-STYLE
                                        PIC X(20).
                04  W10-LBLKANRICD      PIC X(08).
              03  W10-LBLKIKAN-G.
                04  W10-LBLKIKAN-STYLE  PIC X(20).
              03  W10-STYUKYMD          PIC X(9).
              03  W10-EDYUKYMD          PIC X(9).
              03  W10-CHK-SAVEPARA         PIC X(01).
              03  W10-CHK-SAVEPARA-STATE   PIC S9(9)   BINARY.
              03  W10-PROGNAME          PIC X(24).
              03  W10-LBLREPOS-NAME     PIC X(20).
              03  W10-REPOS-NAME        PIC X(128).
              03  W10-REPOS-NAME-VISIBLE   PIC X.
              03  W10-HCNAME            PIC X(80).
              03  W10-LBLHCNUM          PIC X(20).
              03  W10-HCNUM             PIC S9(3).
              03  W10-LBLPARA09            PIC X(20).
              03  W10-LBLPARA10            PIC X(20).
              03  W10-PARA-G        OCCURS  10.
                  04  W10-PARAHYOJI PIC X(20).
                  04  W10-PARAHYOJI-VISIBLE PIC X.
                  04  W10-PARA      PIC X(10).
                  04  W10-PARA-VISIBLE PIC X.
              03  W10-OPTHISSU-KBN-G        OCCURS  10.
                  04  W10-OPTHISSU  PIC X.
                  04  W10-OPTHISSU-VISIBLE PIC X.
                  04  W10-OPTNINI   PIC X.
                  04  W10-OPTNINI-VISIBLE PIC X.
              03  W10-SCROLLEDWINDOW1.
                  04  W10-MEMO        PIC X(6000).

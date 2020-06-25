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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  X02.
            03  X02-FIXED2.
              04  X02-NAME.
                05  X02-VALUE   PIC X(100).
              04  X02-B03.
                05  X02-B03-STATE   PIC S9(9)   BINARY.
              04  X02-NO-G          OCCURS  20.
                05  X02-NO-STYLE    PIC X(20).
                05  X02-NO          PIC S9(3).
              04  X02-TEKCD-G   OCCURS  20.
                05  X02-TEKCD-STYLE PIC X(20).
                05  X02-TEKCD       PIC X(20).
              04  X02-DSPNAME-G OCCURS  20.
                05  X02-DSPNAME-STYLE   PIC X(20).
                05  X02-DSPNAME PIC X(100).
              04  X02-NGBNDKBN-G OCCURS  20.
                05  X02-NGBNDKBN-STATE   PIC S9(9)   BINARY.
                05  X02-NGBNDKBN-STYLE   PIC X(20).
                05  X02-NGBNDKBN PIC S9(1).
              04  X02-CHKERR-G OCCURS  20.
                05  X02-CHKERR-STATE    PIC S9(9)   BINARY.
                05  X02-CHKERR-STYLE    PIC X(20).
                05  X02-CHKERR   PIC S9(1).
              04  X02-LBLCHKERR-G OCCURS  20.
                05  X02-LBLCHKERR-STYLE    PIC X(20).
                05  X02-LBLCHKERR   PIC X(1).
              04  X02-SRYCD.
                05  X02-VALUE   PIC X(9).
              04  X02-STYUKYMD.
                05  X02-VALUE   PIC X(9).
              04  X02-EDYUKYMD.
                05  X02-VALUE   PIC X(9).
              04  X02-CHKKBN.
                05  X02-VALUE   PIC X(50).
              04  X02-LBLPAGE.
                05  X02-VALUE   PIC X(10).
              04  X02-LBLCAUTION-G.
                05  X02-LBLCAUTION-STYLE
                                    PIC X(20).
                05  X02-LBLCAUTION  PIC X(200).

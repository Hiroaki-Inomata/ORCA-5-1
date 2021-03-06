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
      ************************************************************
      *    総括表一時ファイル                                    *
      *
      ************************************************************
       01  SKT2000TMP-REC.
           03  SKT2000TMP-KEY.
               05  SKT2000TMP-PREFKBN        PIC  X(01).
               05  SKT2000TMP-GOKEIKBN       PIC  X(01).
               05  SKT2000TMP-HKNJANUM       PIC  X(06).
               05  SKT2000TMP-RJNFLG         PIC  X(01).
           03  SKT2000TMP-DATA.
               05  SKT2000TMP-KENSU          PIC  9(06).
               05  SKT2000TMP-TYOKI          PIC  9(05).
      *

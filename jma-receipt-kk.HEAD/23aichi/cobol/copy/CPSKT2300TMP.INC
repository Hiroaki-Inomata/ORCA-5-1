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
      *                                                          *
      ************************************************************
       01  SKT2300TMP-REC.
           03  SKT2300TMP-KEY.
             05  SKT2300TMP-SYUTURYOKU  PIC  9(01).
             05  SKT2300TMP-HKNJANUM    PIC  X(08).
           03    SKT2300TMP-DATA.
             05  SKT2300TMP-HKNKBN  OCCURS 3.
               07  SKT2300TMP-NYUGAIKBN  OCCURS 2.
                 09  SKT2300TMP-KENSU   PIC  9(10).
                 09  SKT2300TMP-NISSU   PIC  9(10).
                 09  SKT2300TMP-TENSU   PIC  9(10).
               07  SKT2300TMP-SKJ-AREA OCCURS 2.
                 09  SKT2300TMP-SKJ  PIC  9(10).
             05  SKT2300TMP-KOHKENSU-AREA.
               07  SKT2300TMP-KOHKENSU  PIC  9(04).
               07  SKT2300TMP-FUKUKENSU PIC  9(04).

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
       01  SKT1400TMP-REC.
           03     SKT1400TMP-KEY.
             05   SKT1400TMP-HKNJANUM        PIC  X(06).
           03     SKT1400TMP-DATA.
             05   SKT1400TMP-NYUIN-AREA.
               07 SKT1400TMP-N-KENSU         PIC  9(06).
               07 SKT1400TMP-N-TENSU         PIC  9(10).
             05   SKT1400TMP-GAIRAI-AREA.
               07 SKT1400TMP-G-KENSU         PIC  9(06).
               07 SKT1400TMP-G-TENSU         PIC  9(10).
             05   SKT1400TMP-KOUKI-NYUIN-AREA.
               07 SKT1400TMP-KN-KENSU         PIC  9(06).
               07 SKT1400TMP-KN-TENSU         PIC  9(10).
             05   SKT1400TMP-KOUKI-GAIRAI-AREA.
               07 SKT1400TMP-KG-KENSU         PIC  9(06).
               07 SKT1400TMP-KG-TENSU         PIC  9(10).
             05   SKT1400TMP-SYOKUJI-AREA.
               07 SKT1400TMP-S-KENSU         PIC  9(06).
               07 SKT1400TMP-S-MONEY         PIC  9(10).
      *

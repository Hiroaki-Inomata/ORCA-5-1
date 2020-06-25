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
      *    点数マスタ退避ワーク
       01  TTNS-AREA.
           03  TTNS-FLG                        PIC 9(01).
           03  TTNS-IDX                        PIC 9(05).
           03  TTNS-CHK-SRYCD                  PIC X(09).
           03  TTNS-CHK-YMD                    PIC X(08).
           03  TTNS-G.
               05 TTNS-MAX                     PIC 9(05).
               05 TTNS-KEY-OCC                 OCCURS 2000.
                  07   TTNS-KEY-SRYCD          PIC X(09).
                  07   TTNS-KEY-YUKOSTYMD      PIC X(08).
                  07   TTNS-KEY-YUKOEDYMD      PIC X(08).
               05 TTNS-DAT                     PIC X(2000)
                                               OCCURS 2000.
       01  TTNS-CONST-MAX                      PIC 9(05)
                                               VALUE   2000.
      *

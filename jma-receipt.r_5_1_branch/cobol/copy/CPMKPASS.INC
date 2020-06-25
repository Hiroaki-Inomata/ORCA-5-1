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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    ディレクトリ位置指定                                        *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ORCSMKPASSAREA.
      *    入力項目
           03  MKPASS-IN-AREA.
               05  MKPASS-IN-01         PIC X(512).
               05  MKPASS-IN-02         PIC X(512).
               05  MKPASS-IN-03         PIC X(512).
               05  MKPASS-IN-04         PIC X(512).
               05  MKPASS-IN-05         PIC X(512).
               05  MKPASS-IN-06         PIC X(512).
               05  MKPASS-IN-07         PIC X(512).
               05  MKPASS-IN-08         PIC X(512).
               05  MKPASS-IN-09         PIC X(512).
               05  MKPASS-IN-10         PIC X(512).
           03  MKPASS-IN-AREA-R         REDEFINES    MKPASS-IN-AREA.
               05  MKPASS-IN            PIC X(512)   OCCURS   10.
      *
      *    出力項目
           03  MKPASS-OT-AREA.
               05  MKPASS-OT-01         PIC X(512).
               05  MKPASS-OT-02         PIC X(512).
               05  MKPASS-OT-03         PIC X(512).
               05  MKPASS-OT-04         PIC X(512).
               05  MKPASS-OT-05         PIC X(512).
               05  MKPASS-OT-06         PIC X(512).
               05  MKPASS-OT-07         PIC X(512).
               05  MKPASS-OT-08         PIC X(512).
               05  MKPASS-OT-09         PIC X(512).
               05  MKPASS-OT-10         PIC X(512).
           03  MKPASS-OT-AREA-R         REDEFINES    MKPASS-OT-AREA.
               05  MKPASS-OT            PIC X(512)   OCCURS   10.
      *
      *

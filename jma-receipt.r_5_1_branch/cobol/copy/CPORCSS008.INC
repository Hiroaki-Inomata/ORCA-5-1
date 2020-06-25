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
      *    医療区分取得　　COPY (CPORCSS008.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SS008-AREA.
           03  SS008-IN.
               05  SS008-SRYYM             PIC X(06).
           03  SS008-OT.
               05  SS008-MEDKBNDAY-G.
                   07  SS008-MEDKBNDAY     PIC X(01)
                                           OCCURS  31.
               05  SS008-RC                PIC 9(2).

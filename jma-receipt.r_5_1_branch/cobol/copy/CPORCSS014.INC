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
      *    施設基準取得サブ　COPY (CPORCSS014.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS014-AREA.
           03  SS014-SRYYMD                 PIC X(08).
           03  SS014-SSTKJN-AREA.
               05  SS014-SSTKJN-OCC     PIC X(500)
                                            OCCURS  20.
           03  SS014-SSTKJN-R    REDEFINES  SS014-SSTKJN-AREA.
               05  SS014-SSTKJN-CD          PIC X(01)
                                            OCCURS  10000.

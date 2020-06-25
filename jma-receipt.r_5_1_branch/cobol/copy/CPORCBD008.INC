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
      *----------------------------------------------------------*
      *    日計表パラメタ　     COPY (ORCBD008 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCBD008AREA.
      *    処理区分 (1:伝票発行日/ 2:診療年月日）
           03  ORCBD008-SYORIKBN           PIC X(01).
      *    日付 (1:伝票発行日/ 2:診療年月日）
           03  ORCBD008-SYORIYMD           PIC X(08).
      *    入外区分 (1:入院/ 2:外来）
           03  ORCBD008-NYUGAIKBN          PIC X(01).
      *    並び順
           03  ORCBD008-SORTKBN            PIC X(01).

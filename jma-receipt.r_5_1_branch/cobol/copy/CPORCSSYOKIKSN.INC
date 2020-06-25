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
      *    初期加算算定サブパラメタ　  COPY (CPORCSSYOKIKSN.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSSYOKIKSNAREA.
      *    入力項目
           03  SYOKIKSN-IN-AREA.
      *        自院算定済み通算日数
               05  SYOKIKSN-IN-TUSANNISSU      PIC 9(05).
      *        基本点数診療コード
               05  SYOKIKSN-IN-KHNSRYCD        PIC X(09).
      *        算定開始日
               05  SYOKIKSN-IN-SANTEISTYMD     PIC X(08).
      *    出力項目
           03  SYOKIKSN-OT-AREA.
               05  SYOKIKSN-OT-OCC             OCCURS  7.
      *            加減算診療コード
                   07  SYOKIKSN-OT-KGNSRYCD    PIC X(09).
      *            有効開始日
                   07  SYOKIKSN-OT-YUKOSTYMD   PIC X(08).
      *            有効終了日
                   07  SYOKIKSN-OT-YUKOEDYMD   PIC X(08).
      *        リターンコード
               05  SYOKIKSN-OT-RC              PIC 9(01).

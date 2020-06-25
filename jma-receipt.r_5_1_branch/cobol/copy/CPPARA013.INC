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
      *    照会APIパラメタ　  COPY  (CPPARA013.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  PARA013-REC.
      *    照会状態 01:検索中／02:検索終了
      *    印刷状態 01:印刷中／02:印刷終了／その他:異常終了
           03  PARA013-STATE               PIC X(02).
           03  PARA013-MESSAGE             PIC X(80).
           03  PARA013-NUMBER-OF-RECORDS   PIC X(10).
           03  PARA013-NUMBER-OF-SHEETS    PIC X(10).

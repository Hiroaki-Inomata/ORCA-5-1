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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  BI201.
      *    処理区分（0:ＣＳＶ出力／1:帳票印刷）
           03  BI201-PROC-FLG          PIC 9(01).
      *    ＣＳＶファイル名
           03  BI201-CSVFILENAME       PIC X(100).
      *    開始ページ数
           03  BI201-STPAGE            PIC 9(09).
      *    終了ページ数
           03  BI201-EDPAGE            PIC 9(09).
      *    開始件数
           03  BI201-STKNS             PIC 9(09).
      *    終了件数
           03  BI201-EDKNS             PIC 9(09).
      *    基準日
           03  BI201-KJYNYMD           PIC 9(08).
      *    SQL
           03  BI201-SQL               PIC X(15000).

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
      *         特定器材品目  COPY (CPMATERIAL-H-M.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MATERIAL-H-M-REC.
      *    レコード管理番号
           03  MATERH-M-KANRINO        PIC 9(08).
      *    ＪＡＮコード
           03  MATERH-M-JANCD          PIC X(13).
      *    製品名コード
           03  MATERH-M-SNAMECD        PIC 9(05).
      *    規格コード
           03  MATERH-M-KIKAKUCD       PIC 9(05).
      *    特定器材コード
           03  MATERH-M-SRYCD          PIC X(09).
      *    製造販売業者名コード
           03  MATERH-M-COMPANYCD1     PIC 9(05).
      *    国内総販売元業者名コード
           03  MATERH-M-COMPANYCD2     PIC 9(05).
      *    添付文書情報管理コード
           03  MATERH-M-DOCKANRICD     PIC X(30).
      *    機能区分番号
           03  MATERH-M-KINOKBNNO      PIC X(30).
      *    変更年月日
           03  MATERH-M-CHGYMD         PIC X(08).
      *    作成年月日
           03  MATERH-M-CREYMD         PIC X(08).
      *    更新年月日
           03  MATERH-M-UPYMD          PIC X(08).
      *    更新時間
           03  MATERH-M-UPHMS          PIC X(06).

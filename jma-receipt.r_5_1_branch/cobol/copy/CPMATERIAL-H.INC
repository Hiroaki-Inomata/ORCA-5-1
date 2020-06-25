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
      *         特定器材品目ビュー  COPY (CPMATERIAL-H.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MATERIAL-H-REC.
      *    レコード管理番号
           03  MATERH-KANRINO          PIC 9(08).
      *    ＪＡＮコード
           03  MATERH-JANCD            PIC X(13).
      *    製品名
           03  MATERH-SNAME            PIC X(200).
      *    規格
           03  MATERH-KIKAKU           PIC X(120).
      *    特定器材コード
           03  MATERH-SRYCD            PIC  X(09).
      *    製造販売業者名
           03  MATERH-COMPANY1         PIC  X(100).
      *    国内総販売元業者名
           03  MATERH-COMPANY2         PIC  X(100).
      *    添付文書情報管理コード
           03  MATERH-DOCKANRICD       PIC  X(30).
      *    機能区分番号
           03  MATERH-KINOKBNNO        PIC  X(30).
      *    変更年月日
           03  MATERH-CHGYMD           PIC  X(08).
      *
           03  MATERH-CREYMD           PIC  X(08).
           03  MATERH-UPYMD            PIC  X(08).
           03  MATERH-UPHMS            PIC  X(06).

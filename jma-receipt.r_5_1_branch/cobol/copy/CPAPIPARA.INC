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
      *    APIV3パラメタ　  COPY  (CPAPIPARA.INC)               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  APIPARA-REC.
           03  APIPARA-KEY.
               05  APIPARA-HOSPNUM           PIC 9(2).
      *業務ＩＤ
               05  APIPARA-GYOUMUID          PIC X(08).
      *端末ＩＤ（ORCA-UID)
               05  APIPARA-TERMID            PIC X(64).
      *ファイル名
               05  APIPARA-FILEMEI           PIC X(20).
      *枝番
               05  APIPARA-EDANUM            PIC 9(03).
      *カルテUID
           03  APIPARA-KARTE-UID             PIC X(36).
      *患者番号
           03  APIPARA-PTNUM                 PIC X(20).
      *データ領域
           03  APIPARA-DATA-REC              PIC X(5000).
      *更新年月日
           03  APIPARA-UPYMD                 PIC X(08).
      *更新時間
           03  APIPARA-UPHMS                 PIC X(06).

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
      *    ＤＢ管理情報　    　 COPY  (CPDBKANRI.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  DBKANRI-REC.
      *キー
           03  DBKANRI-KEY.
      *管理コード
               05  DBKANRI-KANRICD         PIC  X(08).
      *バージョン
           03  DBKANRI-VERSION             PIC  X(64).
      *ＤＢ構造バージョン１
           03  DBKANRI-DBSVERSION1         PIC  X(64).
      *ＤＢ構造バージョン２
           03  DBKANRI-DBSVERSION2         PIC  X(64).
      *端末ＩＤ
           03  DBKANRI-TERMID              PIC  X(32).
      *オペーレータＩＤ
           03  DBKANRI-OPID                PIC  X(16).
      *作成年月日
           03  DBKANRI-CREYMD              PIC  X(08).
      *更新年月日
           03  DBKANRI-UPYMD               PIC  X(08).
      *更新時間
           03  DBKANRI-UPHMS               PIC  X(06).

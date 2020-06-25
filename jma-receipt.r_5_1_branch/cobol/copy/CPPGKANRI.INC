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
      *    ＤＢ管理情報　    　 COPY  (CPPGKANRI.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PGKANRI-REC.
      *キー
           03  PGKANRI-KEY.
      *デビアンバージョン
               05  PGKANRI-DEBIANVERSION   PIC  X(24).
      *システムバージョン
               05  PGKANRI-SYSTEMVERSION   PIC  X(10).
      *プログラム番号
               05  PGKANRI-PGNO            PIC  X(03).
      *リリース年月日
           03  PGKANRI-RELEASEYMD          PIC  X(08).
      *プログラム名
           03  PGKANRI-PGNAME              PIC  X(64).
      *更新フラグ
           03  PGKANRI-UPDATEFLG           PIC  X(01).
      *再起動フラグ
           03  PGKANRI-RESTARTFLG          PIC  X(01).
      *インストール位置
           03  PGKANRI-INSTALLDIR          PIC  X(256).
      *内容
           03  PGKANRI-COMMENT             PIC  X(400).
      *端末ＩＤ
           03  PGKANRI-TERMID              PIC  X(32).
      *オペーレータＩＤ
           03  PGKANRI-OPID                PIC  X(16).
      *作成年月日
           03  PGKANRI-CREYMD              PIC  X(08).
      *更新年月日
           03  PGKANRI-UPYMD               PIC  X(08).
      *更新時間
           03  PGKANRI-UPHMS               PIC  X(06).

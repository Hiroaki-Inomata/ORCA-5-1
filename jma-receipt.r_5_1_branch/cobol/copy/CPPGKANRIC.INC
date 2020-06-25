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
      *    ＤＢ管理情報　    　 COPY  (CPPGKANRIC.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PGKANRIC-REC.
      *デビアンバージョン
           03  PGKANRIC-DEBIANVERSION       PIC  X(20).
      *システムバージョン
           03  PGKANRIC-SYSTEMVERSION       PIC  X(10).
      *プログラム番号
           03  PGKANRIC-PGNO                PIC  X(03).
           03  PGKANRIC-YOBI1               PIC  X(01).
      *リリース年月日
           03  PGKANRIC-RELEASEYMD          PIC  X(08).
           03  PGKANRIC-YOBI2               PIC  X(01).
      *プログラム名
           03  PGKANRIC-PGNAME              PIC  X(32).
      *再起動フラグ
           03  PGKANRIC-RESTARTFLG          PIC  X(01).
           03  PGKANRIC-YOBI3               PIC  X(01).
      *インストール位置
           03  PGKANRIC-INSTALLDIR          PIC  X(64).
      *内容
           03  PGKANRIC-COMMENT             PIC  X(200).

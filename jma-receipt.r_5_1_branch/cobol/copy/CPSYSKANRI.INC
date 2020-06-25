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
      *         システム管理    COPY   (CPSYSKANRI.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYSKANRI-REC.
      *キー
           03  SYS-KEY.
               05  SYS-HOSPNUM         PIC 9(2).
      *管理コード
               05  SYS-KANRICD          PIC  X(04).
      *区分コード
               05  SYS-KBNCD            PIC  X(08).
      *開始有効年月日
               05  SYS-STYUKYMD         PIC  X(08).
      *終了有効年月日
               05  SYS-EDYUKYMD         PIC  X(08).
      *管理テーブル
           03  SYS-KANRITBL             PIC  X(500).
      *端末ＩＤ
           03  SYS-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  SYS-OPID                 PIC  X(16).
      *作成年月日
           03  SYS-CREYMD               PIC  X(08).
      *更新年月日
           03  SYS-UPYMD                PIC  X(08).
      *更新時間
           03  SYS-UPHMS                PIC  X(06).
      *　　主キー更新用項目
           03  SYSUPD-KEY.
               05  SYSUP-HOSPNUM          PIC 9(2).
               05  SYSUP-KANRICD          PIC X(04).
               05  SYSUP-KBNCD            PIC X(08).
               05  SYSUP-STYUKYMD         PIC X(08).
               05  SYSUP-EDYUKYMD         PIC X(08).

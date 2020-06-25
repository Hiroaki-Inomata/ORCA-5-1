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
      *    ヘルプマスタ                  COPY (CPHELP.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HELP-REC.
           03  HELP-KEY.
               05  HELP-HOSPNUM          PIC 9(2).
      *ヘルプ区分（０ 標準、１ 自院）
               05  HELP-HELPKBN           PIC  X(01).
      *オペレータＩＤ
               05  HELP-KEY-OPID          PIC  X(16).
      *ＬＤ名
               05  HELP-LDNAME            PIC  X(20).
      *目次１
               05  HELP-MENUNUM1          PIC  9(04).
      *目次２
               05  HELP-MENUNUM2          PIC  9(04).
      *枝番
               05  HELP-EDANUM            PIC  9(04).
      *目次２フラグ（１＝目次２あり）
           03  HELP-MENU2FLG              PIC  9(01).
      *目次名称
           03  HELP-MENU                  PIC  X(60).
      *ヘルプ内容
           03  HELP-HELP                  PIC  X(100).
      *端末ＩＤ
           03  HELP-TERMID                PIC  X(64).
      *オペーレータＩＤ
           03  HELP-OPID                  PIC  X(16).
      *作成年月日
           03  HELP-CREYMD                PIC  X(08).
      *更新年月日
           03  HELP-UPYMD                 PIC  X(08).
      *更新時間
           03  HELP-UPHMS                 PIC  X(06).
      *

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
      ***************************************************************
      *    プログラムオプション          COPY  (CPPRGOPTION.INC)    *
      ***************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PRGOPTION-REC.
      *キー
           03  PRGOPTION-KEY.
      *  医療機関番号
               05  PRGOPTION-HOSPNUM       PIC  9(2).
      *  プログラム名
               05  PRGOPTION-PRGID         PIC  X(25).
      *  区分コード
               05  PRGOPTION-KBNCD         PIC  X(16).
      *オプション内容
           03  PRGOPTION-OPTION            PIC  X(44000).
      *登録区分（0:提供パラメタ、1：ユーザ登録）
           03  PRGOPTION-ADDKBN            PIC  X(01).
      *端末ＩＤ
           03  PRGOPTION-TERMID            PIC  X(16).
      *オペーレータＩＤ
           03  PRGOPTION-OPID              PIC  X(16).
      *作成年月日
           03  PRGOPTION-CREYMD            PIC  X(08).
      *更新年月日
           03  PRGOPTION-UPYMD             PIC  X(08).
      *更新時間
           03  PRGOPTION-UPHMS             PIC  X(06).

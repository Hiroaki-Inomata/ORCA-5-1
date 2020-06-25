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
      *******************************************************************
      ************************************************************
      *    労働基準監督署マスタ     COPY  (CPLABOR-SIO.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  LABOR-SIO-REC.
      *キー
           03  LABOR-SIO-KEY.
      *署コード
               05  LABOR-SIO-SYOCD          PIC  X(05).
      *有効開始年月日
               05  LABOR-SIO-YUKOSTYMD      PIC  X(08).
      *有効終了年月日
               05  LABOR-SIO-YUKOEDYMD      PIC  X(08).
      *名称
           03  LABOR-SIO-NAME               PIC  X(50).
      *端末ＩＤ
           03  LABOR-SIO-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  LABOR-SIO-OPID               PIC  X(16).
      *作成年月日
           03  LABOR-SIO-CREYMD             PIC  X(08).
      *更新年月日
           03  LABOR-SIO-UPYMD              PIC  X(08).
      *更新時間
           03  LABOR-SIO-UPHMS              PIC  X(06).
      *更新用
           03  LABOR-SIO-UP-KEY.
               05  LABOR-SIO-UPSYOCD      PIC  X(05).
               05  LABOR-SIO-UPYUKOSTYMD  PIC  X(08).
               05  LABOR-SIO-UPYUKOEDYMD  PIC  X(08).

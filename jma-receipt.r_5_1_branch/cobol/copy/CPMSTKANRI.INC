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
      *    マスタ管理情報　    　 COPY  (CPMSTKANRI.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MSTKANRI-REC.
      *キー
           03  MSTKANRI-KEY.
               05  MSTKANRI-HOSPNUM        PIC 9(2).
      *管理コード
               05  MSTKANRI-KANRICD        PIC  X(08).
      *名称
               05  MSTKANRI-NAME           PIC  X(64).
      *ＤＢレコードバージョン１
           03  MSTKANRI-DBRVERSION1        PIC  X(64).
      *ＤＢレコードバージョン２
           03  MSTKANRI-DBRVERSION2        PIC  X(64).
      *端末ＩＤ
           03  MSTKANRI-TERMID             PIC  X(32).
      *オペーレータＩＤ
           03  MSTKANRI-OPID               PIC  X(16).
      *作成年月日
           03  MSTKANRI-CREYMD             PIC  X(08).
      *更新年月日
           03  MSTKANRI-UPYMD              PIC  X(08).
      *更新時間
           03  MSTKANRI-UPHMS              PIC  X(06).

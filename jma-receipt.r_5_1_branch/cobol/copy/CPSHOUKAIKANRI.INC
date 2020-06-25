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
      *    患者照会検索条件管理情報    COPY  (CPSHOUKAIKANRI.INC)*
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SHOUKAIKANRI-REC.
      *キー
           03  SHOKANRI-KEY.
      *医療機関番号
               05  SHOKANRI-HOSPNUM    PIC  9(02).
      *UID
               05  SHOKANRI-UID        PIC  X(36).
      *連番
           03  SHOKANRI-RENNUM         PIC  S9(05).
      *検索条件名称
           03  SHOKANRI-NAME           PIC  X(100).
      *入外区分
           03  SHOKANRI-NYUGAIKBN      PIC  X(01).
      *表題区分
           03  SHOKANRI-TITLEKBN       PIC  X(01).
      *端末ＩＤ
           03  SHOKANRI-TERMID         PIC  X(32).
      *オペーレータＩＤ
           03  SHOKANRI-OPID           PIC  X(16).
      *作成年月日
           03  SHOKANRI-CREYMD         PIC  X(08).
      *更新年月日
           03  SHOKANRI-UPYMD          PIC  X(08).
      *更新時間
           03  SHOKANRI-UPHMS          PIC  X(06).
      *virtual項目
           03  SHOKANRI-RENNUM-FIRST   PIC  9(05).
           03  SHOKANRI-RENNUM-LAST    PIC  9(05).

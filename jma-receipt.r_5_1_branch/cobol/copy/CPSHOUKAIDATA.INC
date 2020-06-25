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
      *    患者照会検索条件データ情報  COPY  (CPSHOUKAIDATA.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SHOUKAIDATA-REC.
      *キー
           03  SHODATA-KEY.
      *医療機関番号
               05  SHODATA-HOSPNUM     PIC  9(02).
      *UID
               05  SHODATA-UID         PIC  X(36).
      *項目ID
           03  SHODATA-ITEMID          PIC  9(05).
      *項目
           03  SHODATA-ITEM            PIC  X(100).
      *端末ＩＤ
           03  SHODATA-TERMID          PIC  X(32).
      *オペーレータＩＤ
           03  SHODATA-OPID            PIC  X(16).
      *作成年月日
           03  SHODATA-CREYMD          PIC  X(08).
      *更新年月日
           03  SHODATA-UPYMD           PIC  X(08).
      *更新時間
           03  SHODATA-UPHMS           PIC  X(06).

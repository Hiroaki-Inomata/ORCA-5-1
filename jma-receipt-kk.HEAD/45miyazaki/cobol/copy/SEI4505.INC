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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/02/10  open-cobol1.0対応
      *  01.00.02    門間         19/02/01  修正履歴コメント削除
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *負担者番号
               05  MF100-CITYNUM         PIC  X(06).
      *公費番号
               05  MF100-KOHNUM          PIC  X(02).
      *負担割合
               05  MF100-FTNRATE         PIC  9(03).
      *請求データエリア
           03  MF100-SEIKYU-AREA.
               05  MF100-NYUGAIKBN-TBL OCCURS 2.
      *    件数
                   07  MF100-KENSU   PIC 9(05).
      *    日数
                   07  MF100-NISSU   PIC 9(05).
      *    点数
                   07  MF100-TENSU   PIC 9(10).
      *    一部負担金
                   07  MF100-ITBFTN  PIC 9(10).
      *    食事件数
               05  MF100-SKENSU      PIC 9(05).
      *    食事日数
               05  MF100-SNISSU      PIC 9(05).
      *    金額
               05  MF100-SKINGK      PIC 9(10).
      *    標準負担額
               05  MF100-SFTNGK      PIC 9(10).

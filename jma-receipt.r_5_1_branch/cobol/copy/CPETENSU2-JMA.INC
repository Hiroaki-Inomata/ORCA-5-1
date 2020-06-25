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
      *      電子点数表　包括・被包括テーブル（ＯＲＣＡプロジェクト）
      *                                     COPY (CPETENSU2-JMA.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-伊藤    16/03/13  特例条件追加
      ************************************************************
      *01  ETENSU2-JMA-REC.
      *グループ番号
           03  ETNSJMA-H-GROUP             PIC  X(07).
      *診療行為コード
           03  ETNSJMA-SRYCD               PIC  X(09).
      *有効開始日
           03  ETNSJMA-YUKOSTYMD           PIC  X(08).
      *有効終了日
           03  ETNSJMA-YUKOEDYMD           PIC  X(08).
      *作成年月日
           03  ETNSJMA-CHGYMD              PIC  X(08).
      *特例条件
           03  ETNSJMA-TOKUREI             PIC  9(01).
      *点数マスタ 検索用
           03  ETNSJMA-TK-KEY.
               05  ETNSJMA-TK-HOSPNUM           PIC  9(02).
               05  ETNSJMA-TK-SRYKBN            PIC  X(02).
               05  ETNSJMA-TK-SRYKBN-2          PIC  X(02).
               05  ETNSJMA-TK-SRYSYUKBN         PIC  X(03).
               05  ETNSJMA-TK-SRYSYUKBN-2       PIC  X(03).

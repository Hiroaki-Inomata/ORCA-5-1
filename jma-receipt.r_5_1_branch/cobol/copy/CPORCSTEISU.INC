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
      *    定数超過情報取得パラメタ　COPY (CPORCSTEISU.INC)
      *                                使用プログラム (ORCSTEISU.CBL)
      *    設定項目
      *      算定年月
      *      算定入院料
      *      特定入院料区分
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTEISU-AREA.
      *
      *算定年月
           03  LNK-TEISU-SANTEIYM                 PIC  X(06).
      *算定入院料
           03  LNK-TEISU-SANTEISRYCD              PIC  X(09).
      *特定入院料区分（０：一般入院料　１：特定入院料）
           03  LNK-TEISU-TOKUTEINYUIN-KBN         PIC  X(01).
      *返却項目
           03  LNK-TEISU-HENKYAKU-AREA.
      *定数超過返却テーブル
             04  LNK-TEISU-HENKYAKU-TBL           OCCURS 2.
      *定数超過コード
               05  LNK-TEISU-SRYCD                PIC  X(09).
      *定数超過開始年月
               05  LNK-TEISU-STARTYM              PIC  X(06).
      *定数超過終了年月
               05  LNK-TEISU-ENDYM                PIC  X(06).
      *復帰コード（１：算定年月エラー　２：システム管理情報無し
      *            ３：定数超過対象外）
           03  LNK-TEISU-RC                       PIC S9(9) BINARY.
      *

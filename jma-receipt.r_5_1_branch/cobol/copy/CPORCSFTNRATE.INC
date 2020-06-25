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
      ************************************************************
      *    患者所得情報編集パラメタ
      *    COPY  (CPORCSFTNRATE.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSFTNRATEAREA.
      *入力値
      *    医療機関ＩＤ
           03  LNK-FTNRATE-HOSPNUM         PIC  9(02).
      *    患者ＩＤ
           03  LNK-FTNRATE-PTID            PIC  9(10).
      *    処理年月日
           03  LNK-FTNRATE-SRYYMD          PIC  X(08).
      *    保険組合せ番号
           03  LNK-FTNRATE-HKNCOMBINUM     PIC  X(04).
      *    保険組合せ検索済み（1:検索済み）
           03  LNK-FTNRATE-KBN             PIC  X(01).
      *
      *返却項目
           03  LNK-FTNRATE-HENKYAKU-AREA.
               05  LNK-FTNRATE-NYUIN       PIC  9(03).
               05  LNK-FTNRATE-GAIRAI      PIC  9(03).
      *
      *復帰コード
           03  LNK-FTNRATE-RC              PIC S9(09) BINARY.
      *

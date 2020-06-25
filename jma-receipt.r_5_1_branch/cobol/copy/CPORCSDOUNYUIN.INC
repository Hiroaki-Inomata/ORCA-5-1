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
      *    同日再入院判定パラメタ　　　COPY (CPORCSDOUNYUIN.INC)
      *                                使用プログラム (ORCSDOUNYUIN)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      入院履歴番号
      *      入院日（又は転入日）
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSDOUNYUIN-AREA.
      *
      *医療機関ＩＤ
           03  LNK-DOUNYUIN-HOSPNUM               PIC  9(02).
      *患者ＩＤ
           03  LNK-DOUNYUIN-PTID                  PIC  9(10).
      *入院履歴番号
           03  LNK-DOUNYUIN-RRKNUM                PIC  9(03).
      *再入院日
           03  LNK-DOUNYUIN-SAINYUINYMD           PIC  X(08).
      *返却エリア
           03  LNK-DOUNYUIN-HENKYAKU-AREA.
      *入院料算定開始日（復帰コードが１の時のみ返却）
             05  LNK-DOUNYUIN-SANTEIYMD           PIC  X(08).
      *
      *復帰コード
      *     ０：前歴からの同日再入院以外
      *     １：前歴からの同日再入院
      *     ８：前入院歴無し
      *     ９：患者マスター検索エラー
           03  LNK-DOUNYUIN-RC                    PIC S9(9) BINARY.
      *

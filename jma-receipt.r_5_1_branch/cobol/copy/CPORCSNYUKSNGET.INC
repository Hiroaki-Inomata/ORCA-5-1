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
      *    入院料加算取得パラメタ　　COPY (CPORCSNYUKSNGET.INC)
      *                                使用プログラム (ORCSNYUKSNGET)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定開始日
      *      入院料コード
      *    返却項目
      *      入院料加算区分
      *      入院料加算コード
      *      特別入院料判定フラグ
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYUKSNGET-AREA.
      *
      *医療機関ＩＤ
           03  LNK-NYUKSNGET-HOSPNUM                 PIC  9(02).
      *患者ＩＤ
           03  LNK-NYUKSNGET-PTID                    PIC  9(10).
      *算定開始日
           03  LNK-NYUKSNGET-SANTEIYM                PIC  X(06).
      *入院病棟番号
           03  LNK-NYUKSNGET-BTUNUM                  PIC  X(02).
      *入院病室番号
           03  LNK-NYUKSNGET-BRMNUM                  PIC  X(08).
      *入院料コード
           03  LNK-NYUKSNGET-NYUIN-SRYCD             PIC  X(09).
      *特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に入院料加算
      *を特別判定する場合の判定区分
           03  LNK-NYUKSNGET-GAI-NYUKSNKBN           PIC  X(01).
      *入退院登録画面設定の加算領域
           03  LNK-NYUKSNGET-I01-KSN-SRYCD           PIC  X(09)
                                                     OCCURS  3.
           03  LNK-NYUKSNGET-FLG-IPPAN               PIC  9(01).
           03  LNK-NYUKSNGET-FLG-IPPAN2              PIC  9(01).
      *返却項目
           03  LNK-NYUKSNGET-HENKYAKU-AREA.
      *入院料加算コード返却領域
             05  LNK-NYUKSNGET-HENKYAKU-OCC          OCCURS  20.
      *入院料加算区分
                 07  LNK-NYUKSNGET-NYUKSNKBN         PIC  X(01).
      *入院料加算コード
                 07  LNK-NYUKSNGET-KSNSRYCD          PIC  X(09).
      *特別入院料区分
           03  LNK-NYUKSNGET-TOKUBETSU-NYUIN         PIC  X(01).
      *復帰コード（０：入院料加算返却あり　１：入院料加算返却なし）
           03  LNK-NYUKSNGET-RC                      PIC S9(9) BINARY.
      *

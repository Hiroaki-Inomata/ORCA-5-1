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
      *    特定入院料算定期間取得パラメタ　　COPY (CPORCSTOKNYUIN2.INC)
      *                                使用プログラム (ORCSTOKNYUIN2)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定開始日
      *      特定入院料コード
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTOKNYUIN2-AREA.
      *
      *医療機関ＩＤ
           03  LNK-TOKNYUIN2-HOSPNUM                 PIC  9(02).
      *患者ＩＤ
           03  LNK-TOKNYUIN2-PTID                    PIC  9(10).
      *異動区分（１：２ヵ月分会計作成　２：１ヵ月分会計作成）
           03  LNK-TOKNYUIN2-IDOKBN                  PIC  X(01).
      *入院日
           03  LNK-TOKNYUIN2-NYUINYMD                PIC  X(08).
      *算定開始日
           03  LNK-TOKNYUIN2-SANTEIYMD               PIC  X(08).
      *特定入院料コード
           03  LNK-TOKNYUIN2-SET-SRYCD               PIC  X(09).
      *特定入院料通算日数（STUSAN-NISSU08をセット）
           03  LNK-TOKNYUIN2-NISSU08                 PIC  9(08).
      *特定入院料区分
           03  LNK-TOKNYUIN2-TOKUTEI-NYUIN           PIC X(02).
      *特別処理フラグ領域
      *    １：平成２４年３月３１日に亜急性期入院医療管理料１算定時は
      *        上限日数を９０日とする
      *    ２：療養病棟、療養病床入院時は特定入院料算定上限日数を超えた
      *        日以降の入院会計を作成しない（異動処理で作成する為）
      *    ３から５は未使用
           03  LNK-TOKNYUIN2-TOKUBETSU-FLG           PIC X(01)
                                                     OCCURS  5.
      *初回番号（歴作成で登録された入院履歴登録分について特定入院−通算
      *          対象日数を取得する為にセット）
           03  LNK-TOKNYUIN2-SHONUM                  PIC 9(03).
      *返却項目
           03  LNK-TOKNYUIN2-HENKYAKU-AREA.
      *特定入院料コード算定期間返却領域
             05  LNK-TOKNYUIN2-HENKYAKU-OCC          OCCURS  5.
      *特定入院料コード
                 07  LNK-TOKNYUIN2-SRYCD             PIC  X(09).
      *特定入院開始日
                 07  LNK-TOKNYUIN2-STYMD             PIC  X(08).
      *特定入院終了日
                 07  LNK-TOKNYUIN2-EDYMD             PIC  X(08).
      *一般入院会計作成フラグ（特定入院料算定期限後の入院会計作成）
      *    （空白：作成しない　１：作成する）
             05  LNK-TOKNYUIN2-IPN-KAIKEI-FLG        PIC  X(01).
      *一般入院会計作成開始日
             05  LNK-TOKNYUIN2-IPN-KAIKEI-YMD        PIC  X(08).
      *復帰コード（０：特定入院料返却あり　１：特定入院料上限超え返却なし
      *            ２：特定入院料対象なし）
           03  LNK-TOKNYUIN2-RC                      PIC S9(9) BINARY.
      *

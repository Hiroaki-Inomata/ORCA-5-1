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
      *    療養レセコメント取得パラメタ　　　COPY (CPORCSRYOCOM.INC)
      *                                使用プログラム (ORCSRYOCOM)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *      保険区分
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSRYOCOM-AREA.
      *
      *医療機関ＩＤ
           03  LNK-RYOCOM-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  LNK-RYOCOM-PTID                   PIC  9(10).
      *算定年月
           03  LNK-RYOCOM-SANTEIYM               PIC  X(06).
      *返却項目
           03  LNK-RYOCOM-HENKYAKU-AREA.
      *コメント返却テーブル
             04  LNK-RYOCOM-HENKYAKU-TABLE       OCCURS  31.
      *療養入院区分（空白：療養入院以外　１：療養入院）
               05  LNK-RYOCOM-NYUINKBN           PIC  X(01).
      *明細数
               05  LNK-RYOCOM-MEISAISU           PIC  9(03).
      *診療コード計（ＡＤＬコメント含む）
               05  LNK-RYOCOM-SRYCDTOTAL         PIC  9(14).
      *診療コード計２（診療コード計（ＡＤＬコメント含む）＋ＡＤＬ点数）
               05  LNK-RYOCOM-SRYCDTOTAL2        PIC  9(14).
      *コメント記録エリア
               05  LNK-RYOCOM-RECORD             OCCURS 30.
      *医療区分コメントコード
                   07  LNK-RYOCOM-IRYO-SRYCD     PIC  X(09).
      *医療区分コメント剤番号
                   07  LNK-RYOCOM-IRYO-ZAINUM    PIC  9(08).
      *医療区分コメント先頭２文字（並び替え用）
                   07  LNK-RYOCOM-IRYO-SORT-COM  PIC  X(04).
      *
      *ＡＤＬコメントコード（平成２２年３月までの返却項目）
               05  LNK-RYOCOM-ADL-SRYCD          PIC  X(09).
      *ＡＤＬ点数（数字）（平成２２年３月までの返却項目）
               05  LNK-RYOCOM-ADL-TEN            PIC  9(02).
      *ＡＤＬ点数（全角）（平成２２年３月までの返却項目）
               05  LNK-RYOCOM-ADL-TEN-MOJI       PIC  X(04).
      *
      *ＡＤＬ返却領域（平成２２年４月以降）
               05  LNK-RYOCOM-ADL-H22            OCCURS   4.
      *ＡＤＬコメントコード
                 07  LNK-RYOCOM-ADL-SRYCD-H22    PIC  X(09).
      *ＡＤＬ点数（数字）
                 07  LNK-RYOCOM-ADL-TEN-H22      PIC  9(02).
      *ＡＤＬ点数（全角）
                 07  LNK-RYOCOM-ADL-TEN-MOJI-H22 PIC  X(04).
      *
      *ＡＤＬ返却領域（平成２８年４月以降はこちらにも新設コードを返却）
               05  LNK-RYOCOM-ADL-H28            OCCURS   4.
      *ＡＤＬコメントコード
                 07  LNK-RYOCOM-ADL-SRYCD-H28    PIC  X(09).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　　４：療養入院なし
      *            ５：療養コメント入力なし）
           03  LNK-RYOCOM-RC                     PIC S9(9) BINARY.
      *

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
      *    食事負担据え置き対象取得パラメタ　COPY (CPORCSSKJKEEP.INC)
      *                            使用プログラム (ORORCSSKJKEEP)
      *    設定項目
      *      医療機関番号
      *      患者ＩＤ
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSSKJKEEP-AREA.
      *
      *医療機関番号
           03  LNK-SKJKEEP-HOSPNUM             PIC 9(2).
      *患者ＩＤ
           03  LNK-SKJKEEP-PTID                PIC  9(10).
      *算定年月
           03  LNK-SKJKEEP-SANTEIYM            PIC  X(06).
      *返却項目
           03  LNK-SKJKEEP-HENKYAKU-AREA.
      *食事負担据え置き区分
             05  LNK-SKJKEEP-TBL.
      *  （空白：対象外　１：難病等据え置き　２：精神病床据え置き）
               07  LNK-SKJKEEP-KBN             PIC  X(01)  OCCURS  31.
      *難病システム予約入力区分
      *  （空白：対象外　１：難病据え置き　２：急性増悪による転棟）
               07  LNK-SKJKEEP-NANBYOKBN       PIC  X(01)  OCCURS  31.
      *精神病床入院年月日
             05  LNK-SKJKEEP-SEI-NYUINYMD      PIC  X(08).
      *精神病床入院年月日コメント手入力区分
      *  （空白：未入力　１：手入力）
             05  LNK-SKJKEEP-SEI-COMMENT-KBN   PIC  X(01).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　４：難病等、精神入院の据え置き対象外
      *            ５：精神病床据え置き入力２０回超過）
           03  LNK-SKJKEEP-RC                  PIC S9(9) BINARY.
      *

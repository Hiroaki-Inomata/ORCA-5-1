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
      *    食事保険取得パラメタ　COPY (CPORCSSKJHKN.INC)
      *                        使用プログラム (ORCSSKJHKN)
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
       01  ORCSSKJHKN-AREA.
      *
      *医療機関番号
           03  LNK-SKJHKN-HOSPNUM             PIC 9(2).
      *患者ＩＤ
           03  LNK-SKJHKN-PTID                PIC  9(10).
      *算定年月
           03  LNK-SKJHKN-SANTEIYM            PIC  X(06).
      *返却項目
           03  LNK-SKJHKN-HENKYAKU-AREA.
      *入院区分
             05  LNK-SKJHKN-TBL.
               07  LNK-SKJHKN-DAY             OCCURS  31.
      *          保険組合せ番号
      *        （添字１：入院分　添字２：同日再入院分　空白：入力無し）
                 09  LNK-SKJHKN-HKNCOMBINUM   PIC X(04)  OCCURS  2.
      *          保険種類
      *        （１：社保　２：国保　３：後期高齢　４：労災　５：自賠
      *          ６：公害　７：自費　８：治験　空白：入力無し）
                 09  LNK-SKJHKN-HKNSYU        PIC X(01)  OCCURS  2.
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院なし　４：システム予約入力なし
      *            ５：システム予約重複入力エラー）
           03  LNK-SKJHKN-RC                PIC S9(9) BINARY.
      *

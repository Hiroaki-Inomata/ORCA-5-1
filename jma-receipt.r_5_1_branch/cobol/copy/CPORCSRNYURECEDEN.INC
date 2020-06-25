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
      *    労災レセ電用入院会計取得パラメタ　　　COPY (CPORCSRNYURECEDEN.INC)
      *                                使用プログラム (ORCSRNYURECEDEN)
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
       01  ORCSRNYURECEDEN-AREA.
      *
      *医療機関ＩＤ
           03  LNK-RNYURECEDEN-HOSPNUM               PIC  9(02).
      *患者ＩＤ
           03  LNK-RNYURECEDEN-PTID                  PIC  9(10).
      *算定年月
           03  LNK-RNYURECEDEN-SANTEIYM              PIC  X(06).
      *保険組合せ番号
           03  LNK-RNYURECEDEN-HKNCOMBI              PIC  9(04).
      *保険区分（４をセット）
           03  LNK-RNYURECEDEN-HKNKBN                PIC  X(01).
      *返却項目
           03  LNK-RNYURECEDEN-HENKYAKU-AREA.
      *入院返却テーブル（添字１：入院料　添字２：特定入院料　３：食事）
             04  LNK-RNYURECEDEN-HENKYAKU-TABLE      OCCURS  3.
      *入院料返却テーブル
               05  LNK-RNYURECEDEN-HENKYAKU-TBL      OCCURS  31.
      *明細数
                   07  LNK-RNYURECEDEN-MEISAISU      PIC  9(03).
      *日別情報
                   07  LNK-RNYURECEDEN-DAY-TBL.
                       09  LNK-RNYURECEDEN-DAY       PIC  9(01)
                                                          OCCURS 31.
      *レセ電記録エリア
                   07  LNK-RNYURECEDEN-NYUIN         OCCURS 300.
      *レコード識別
                       09  LNK-RNYURECEDEN-RECSKB    PIC  X(02).
      *診療識別
                       09  LNK-RNYURECEDEN-SRYSKB    PIC  X(02).
      *診療行為コード
                       09  LNK-RNYURECEDEN-SRYCD     PIC  X(09).
      *数量
                       09  LNK-RNYURECEDEN-SURYO     PIC  9(01).
      *点数（食事の場合は金額を返却）
                       09  LNK-RNYURECEDEN-TEN       PIC  9(07).
      *回数
                       09  LNK-RNYURECEDEN-KAISU     PIC  9(02).
      *コメント記録データ
                       09  LNK-RNYURECEDEN-COMMENT   PIC  X(76).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー　
      *            ３：保険組合せ取得エラー　４：入院会計なし）
           03  LNK-RNYURECEDEN-RC                     PIC S9(9) BINARY.
      *

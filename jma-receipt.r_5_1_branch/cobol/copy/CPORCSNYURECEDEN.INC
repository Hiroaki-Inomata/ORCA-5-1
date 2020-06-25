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
      *    レセ電用入院会計取得パラメタ　　　COPY (CPORCSNYURECEDEN.INC)
      *                                使用プログラム (ORCSNYURECEDEN)
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
       01  ORCSNYURECEDEN-AREA.
      *
      *医療機関ＩＤ
           03  LNK-NYURECEDEN-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  LNK-NYURECEDEN-PTID                   PIC  9(10).
      *算定年月
           03  LNK-NYURECEDEN-SANTEIYM               PIC  X(06).
      *保険区分（１：社保　２：国保　３：広域連合）
           03  LNK-NYURECEDEN-HKNKBN                 PIC  X(01).
      *対象保険組合せ番号
           03  LNK-NYURECEDEN-SET-HKNCOMBI           PIC  9(04)
                                                          OCCURS 10.
      *返却項目
           03  LNK-NYURECEDEN-HENKYAKU-AREA.
      *入院返却テーブル（添字１：入院料　添字２：特定入院料　３：食事
      *                  添字４：生活療養（食事療養）
      *                  添字５：生活療養（環境療養））
             04  LNK-NYURECEDEN-HENKYAKU-TABLE       OCCURS  5.
      *入院料返却テーブル
               05  LNK-NYURECEDEN-HENKYAKU-TBL       OCCURS  31.
      *保険組合せ
                   07  LNK-NYURECEDEN-HKNCOMBI       PIC  9(04).
      *明細数
                   07  LNK-NYURECEDEN-MEISAISU       PIC  9(03).
      *日別情報
                   07  LNK-NYURECEDEN-DAY-TBL.
                       09  LNK-NYURECEDEN-DAY        PIC  9(01)
                                                          OCCURS 31.
      *レセ電記録エリア
                   07  LNK-NYURECEDEN-NYUIN          OCCURS 300.
      *レコード識別
                       09  LNK-NYURECEDEN-RECSKB     PIC  X(02).
      *診療識別
                       09  LNK-NYURECEDEN-SRYSKB     PIC  X(02).
      *診療行為コード
                       09  LNK-NYURECEDEN-SRYCD      PIC  X(09).
      *数量
                       09  LNK-NYURECEDEN-SURYO      PIC  9(01).
      *点数
                       09  LNK-NYURECEDEN-TEN        PIC  9(07).
      *回数
                       09  LNK-NYURECEDEN-KAISU      PIC  9(02).
      *コメント記録データ
                       09  LNK-NYURECEDEN-COMMENT    PIC  X(76).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー　３：保険区分エラー
      *            ４：入院会計なし　　５：保険組合せ取得エラー）
           03  LNK-NYURECEDEN-RC                     PIC S9(9) BINARY.
      *

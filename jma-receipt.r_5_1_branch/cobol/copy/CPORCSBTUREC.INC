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
      *    レセ電用病床機能報告データ取得パラメタ　　　COPY (CPORCSBTUREC.INC)
      *                                使用プログラム (ORCSBTUREC)
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
       01  ORCSBTUREC-AREA.
      *
      *医療機関ＩＤ
           03  LNK-BTUREC-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  LNK-BTUREC-PTID                   PIC  9(10).
      *算定年月
           03  LNK-BTUREC-SANTEIYM               PIC  X(06).
      *返却項目
           03  LNK-BTUREC-HENKYAKU-AREA.
      *病棟情報返却テーブル
             04    LNK-BTUREC-HENKYAKU-TBL       OCCURS  31.
      *入院会計保険組合せ
                05  LNK-BTUREC-HKNCOMBI          PIC  9(04).
      *診療識別
                05  LNK-BTUREC-SRYSKB            PIC  X(02).
      *病棟コード（診療行為コード）
                05  LNK-BTUREC-BTUCD             PIC  X(09).
      *患者保険組合せ返却テーブル明細数
             04    LNK-BTUREC-SYU-HKNCOMBI-MEISAI
                                                 PIC  9(02).
      *患者保険組合せ返却テーブル
             04    LNK-BTUREC-SYU-HKNCOMBI-TBL   OCCURS  31.
      *患者保険組合せ
                05  LNK-BTUREC-SYU-HKNCOMBI      PIC  9(04)  OCCURS 31.
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　　４：保険組合せ取得エラー
      *            ５：医療機関種別エラー）
           03  LNK-BTUREC-RC                     PIC S9(9) BINARY.
      *

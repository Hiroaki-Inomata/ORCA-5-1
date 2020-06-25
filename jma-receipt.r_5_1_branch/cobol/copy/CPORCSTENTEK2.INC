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
      *    入院点滴抽出パラメタ　　COPY (CPORCSCTENTEK2.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTENTEK2AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCSTEN2-HOSPNUM            PIC  9(02).
      *患者ＩＤ
           03  LNK-ORCSTEN2-PTID               PIC  9(10).
      *診療年月
           03  LNK-ORCSTEN2-SRYYM              PIC  X(06).
      *保険組合
           03  LNK-ORCSTEN2-HKNCOMBI           PIC 9(04).
      *返却項目
           03  LNK-ORCSTEN2-OUT-AREA.
      *        剤番号
               05  LNK-ORCSTEN2-ZAINUM         PIC 9(08)  OCCURS 40.
      *        点滴注射
               05  LNK-ORCSTEN2-TENTEK-AREA.
                   07  LNK-ORCSTEN2-TENTEK-DAY            OCCURS   31.
      *              診療科
                     09  LNK-ORCSTEN2-TENTEK-SRYKA        PIC X(02).
      *              明細数
                     09  LNK-ORCSTEN2-TENTEK-MEISAISU     PIC 9(03).
      *              剤点数
                     09  LNK-ORCSTEN2-TENTEK-ZAITEN       PIC 9(07).
      *              診療コード計
                     09  LNK-ORCSTEN2-TENTEK-SRYCDTOTAL   PIC 9(13).
      *              明細返却
                     09  LNK-ORCSTEN2-TENTEK-MEISAI       OCCURS  5.
      *                診療行為コード
                       11  LNK-ORCSTEN2-TENTEK-SRYCD      PIC X(09).
      *                コメント明細数
                       11   LNK-ORCSTEN2-TENTEK-COM-MEISAISU
                                                          PIC 9(03).
      *                コメント返却領域
                       11  LNK-ORCSTEN2-TENTEK-COM-AREA   OCCURS   5.
      *                 コメントフラグ（空白：通常コメント　１：補足コメント）
                        13  LNK-ORCSTEN2-TENTEK-COM-FLG   PIC X(1).
      *                 コメントコード
                        13  LNK-ORCSTEN2-TENTEK-COM-SRYCD PIC X(09).
      *                 コメント文字
                        13  LNK-ORCSTEN2-TENTEK-COM-MOJI  PIC X(100).
      *        エラーコード（０：正常終了　１：対象なし）
                  07  LNK-ORCSTEN2-TENTEK-ERRCD           PIC 9(01).
      *
      *        中心静脈
               05  LNK-ORCSTEN2-CHUJOU-AREA.
                   07  LNK-ORCSTEN2-CHUJOU-DAY            OCCURS   31.
      *              診療科
                     09  LNK-ORCSTEN2-CHUJOU-SRYKA        PIC X(02).
      *              明細数
                     09  LNK-ORCSTEN2-CHUJOU-MEISAISU     PIC 9(03).
      *              剤点数
                     09  LNK-ORCSTEN2-CHUJOU-ZAITEN       PIC 9(07).
      *              診療コード計
                     09  LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL   PIC 9(13).
      *              明細返却
                     09  LNK-ORCSTEN2-CHUJOU-MEISAI       OCCURS  5.
      *                診療行為コード
                       11  LNK-ORCSTEN2-CHUJOU-SRYCD      PIC X(09).
      *                コメント明細数
                       11   LNK-ORCSTEN2-CHUJOU-COM-MEISAISU
                                                          PIC 9(03).
      *                コメント返却領域
                       11  LNK-ORCSTEN2-CHUJOU-COM-AREA   OCCURS   5.
      *                 コメントフラグ（空白：通常コメント　１：補足コメント）
                        13  LNK-ORCSTEN2-CHUJOU-COM-FLG   PIC X(1).
      *                 コメントコード
                        13  LNK-ORCSTEN2-CHUJOU-COM-SRYCD PIC X(09).
      *                 コメント文字
                        13  LNK-ORCSTEN2-CHUJOU-COM-MOJI  PIC X(100).
      *        エラーコード（０：正常終了　１：対象なし）
                  07  LNK-ORCSTEN2-CHUJOU-ERRCD           PIC 9(01).
      *      復帰コード（１：診療年月エラー　２：患者検索エラー
      *                  ３：保険組合せエラー　４：入院会計なし
      *                  ５：手技＋加算組合せエラー）
             03  LNK-ORCSTEN2-RC              PIC S9(9)  BINARY.
      *

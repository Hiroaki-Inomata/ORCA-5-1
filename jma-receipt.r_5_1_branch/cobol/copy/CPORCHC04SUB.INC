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
      *----------------------------------------------------------*
      *    外来診療費明細書出力パラメタ       COPY (CPORCHC04.INC)
      *                                          (ORCHC04 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  14/10/XX  コメントコード対象等追加
      ************************************************************
      *
       01  ORCHC04SUBAREA.
      *
      *    伝票番号（必須）
           03  ORCHC04SUB-DENPNUM      PIC 9(07).
      *    合計単位（プログラムオプション）
           03  ORCHC04SUB-GOKEITANI    PIC X(01).
      *    小計（１：あり）（請求書）
           03  ORCHC04SUB-PARAKBN      PIC X(01).
      *    コメントコード対象（１：対象）
           03  ORCHC04SUB-PARACOMKBN   PIC X(01).
      *    保険外対象（１：対象）
           03  ORCHC04SUB-PARAJIHIKBN  PIC X(01).
      *    API返却用
           03  ORCHC04SUB-APIKBN       PIC X(01).
      *出力
           03  ORCHC04SUB-INFO-AREA.
               05  ORCHC04SUB-SRYYMD           PIC X(08).
               05  ORCHC04SUB-SRYYMDMEI        PIC X(22).
               05  ORCHC04SUB-SRYKA            PIC X(02).
               05  ORCHC04SUB-SRYKAMEI         PIC X(30).
               05  ORCHC04SUB-HKNCOMBI         PIC X(04).
               05  ORCHC04SUB-HKNCOMBIMEI      PIC X(40).
      *    印刷内容
           03  ORCHC04SUB-LIST-AREA.
               05  ORCHC04SUB-HEN-G    OCCURS   800.
      *            編集済み内容
                   07  ORCHC04SUB-H-SRYKBN        PIC X(10).
                   07  ORCHC04SUB-NAME            PIC X(140).
                   07  ORCHC04SUB-TANINAME        PIC X(10).
                   07  ORCHC04SUB-H-SURYO         PIC X(11).
                   07  ORCHC04SUB-H-TENSU.
                       09  ORCHC04SUB-H-TENSU1    PIC X(02).
                       09  ORCHC04SUB-H-TENSU2    PIC X(07).
                   07  ORCHC04SUB-H-KINGAKU.
                       09  ORCHC04SUB-H-KINGAKU1  PIC X(02).
                       09  ORCHC04SUB-H-KINGAKU2  PIC X(09).
                   07  ORCHC04SUB-H-KAISU         PIC X(03).
      *            剤開始
                   07  ORCHC04SUB-H-ZAIHEN        PIC X(02).
      *            数量＋単位（全角）
                   07  ORCHC04SUB-H-ZEN-SURYO     PIC X(40).
                   07  ORCHC04SUB-H-ZENMAX        PIC 9(02).
      *            １：円、０：点
                   07  ORCHC04SUB-H-ENKBN         PIC X(01).
      *
                   07  ORCHC04SUB-SRYKBN          PIC X(02).
      *            数量
                   07  ORCHC04SUB-SURYO           PIC 9(05)V9(05).
      *            回数
                   07  ORCHC04SUB-KAISU           PIC 9(03).
      *            点数（コードの点数）（対象分のみ）
                   07  ORCHC04SUB-TENSU           PIC 9(07).
      *            剤点数
                   07  ORCHC04SUB-ZAITENSU        PIC S9(08).
      *            診療コード
                   07  ORCHC04SUB-SRYCD           PIC X(09).
      *            剤終了
                   07  ORCHC04SUB-ZAIEND          PIC X(01).
      *
               05  ORCHC04SUB-TBL-MAX         PIC  9(04).
      *

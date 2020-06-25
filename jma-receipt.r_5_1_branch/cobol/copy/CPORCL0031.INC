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
      *    ＡＰＩ中途データ作成パラメタ　　COPY (CPORCL0031.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  LNK-ORCL0031AREA.
      *    処理区分（1:API）
           03  LNK-KBN-CODE            PIC X(01).
      *    処理区分２（1:病名のみ）
           03  LNK-KBN-CODE2           PIC X(01).
      *    APIバージョン（1:ver.2)
           03  LNK-KBN-V2              PIC X(01).
      *
      *    保険組合せ番号指定（ver.2のみ）
           03  LNK-IN-HKNCOMBI         PIC X(04).
      *    外来中途データ追加（ver.2のみ）
           03  LNK-ADDKBN              PIC X(01).
           03  LNK-MAX-ZAINUM          PIC 9(08).
      *
      *    保険組合せ番号（作成）
           03  LNK-OUT-HKNCOMBI        PIC X(04).
      *    中途データ作成件数
           03  LNK-OUT-CNT1            PIC 9(04).
      *    病名登録件数
           03  LNK-OUT-CNT2            PIC 9(04).
      *
      *    中途データ処理　返却
           03  LNK-OUT01-AREA.
      *        エラーコード
               05  LNK-ERR1-CODE            PIC X(02).
               05  LNK-ERR1-MSG             PIC X(100).
               05  LNK-MEDI-TBL-G           OCCURS   50.
      *        警告
                   07  LNK-MEDI-CODE            PIC X(03).
                   07  LNK-MEDI-MSG             PIC X(100).
                   07  LNK-MEDI-POSITION        PIC 9(02).
                   07  LNK-MEDI-ITEM            PIC 9(02).
                   07  LNK-MEDI-SRYCD           PIC X(09).
      *    病名処理　返却
           03  LNK-OUT02-AREA.
      *        エラーコード
               05  LNK-ERR2-CODE            PIC X(02).
               05  LNK-ERR2-MSG             PIC X(100).
               05  LNK-DIAG-TBL-G           OCCURS   50.
      *            警告コード
                   07  LNK-DIAG-WARNING         PIC X(03).
      *            メッセージ
                   07  LNK-DIAG-MSG             PIC X(100).
      *            行番号
                   07  LNK-DIAG-POSITION        PIC 9(02).
      *            病名
                   07  LNK-DIAG-NAME            PIC X(80).
      *            病名コード
                   07  LNK-DIAG-CODE            PIC X(50).
      *            移行(廃止)
                   07  LNK-DIAG-CHANGE          PIC X(02).
      *
      *

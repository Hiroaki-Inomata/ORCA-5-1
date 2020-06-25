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
      *    入院調剤料一括更新パラメタ　　COPY (CPORCSC100.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSC100AREA.
      *
      *処理区分（１：入力、２：登録）
           03  ORCSC100-KBN                 PIC  X(01).
      *    １：チェック処理
           03  ORCSC100-TOROKU              PIC  X(01).
      *    １：初期区分
           03  ORCSC100-INIT                PIC  9(01).
      *    １：同一検査削除
           03  ORCSC100-KENSADEL            PIC  9(01).
      *    １：リハビリ確認あり
           03  ORCSC100-RIHTEI              PIC  9(01).
      *出力
           03  ORCSC100-OUT-AREA.
      *        １：入院検査まとめエラー
               05  ORCSC100-KENSAERR        PIC 9(01).
      *

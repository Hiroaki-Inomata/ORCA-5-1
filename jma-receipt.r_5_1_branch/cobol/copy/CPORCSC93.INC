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
      *    剤セット、チェック　   COPY (CPORCSC93.INC)
      *                              (ORCSC93 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  06/07/20  入院検査まとめ追加
      ************************************************************
      *
       01  ORCSC93AREA.
      *
      *    処理画面
           03  LNK-SC93-PG             PIC X(04).
      *    処理フラグ（１：同一検査削除）
           03  LNK-SC93-DELSYORIFLG    PIC  9(01).
      *
      *    セット入力（Ｋ０５）のセットコード
           03  LNK-SC93-SETCODE        PIC X(06).
      *    小児科外来診療科算定有り
           03  LNK-SC93-SYONIKA-ARI2   PIC  9(01).
      *    寝たきり在宅料有り
           03  LNK-SC93-NETAKIRI-ARI   PIC  9(01).
      *    入院検査まとめ有り
           03  LNK-SC93-KENSAARIFLG    PIC  9(01).
      *    残量廃棄区分
           03  LNK-SC93-HAIKIKBN-FLG   PIC  9(01).
      *
      *    入院検査まとめエラー
           03  LNK-SC93-ERRCD          PIC  9(01).
      *

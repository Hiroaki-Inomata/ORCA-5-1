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
      *--------------------------------------------------------*
      *    ファイルエンコーディングパラメタ COPY (CPORCSFILEENCODE.INC)  *
      *    EUC-JP から Shift-JIS
      *--------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSFILEENCODEAREA.
      *      入力ファイル名
           03  FLEN-INFILENAME         PIC X(256).
      *      出力ファイル名
           03  FLEN-OUTFILENAME        PIC X(256).
      *      処方せんQR例外オプション　(1:処方せん,3:その他)
           03  FLEN-OPTION             PIC X(01).
      *      リターンコード
           03  FLEN-RET                PIC X(01).

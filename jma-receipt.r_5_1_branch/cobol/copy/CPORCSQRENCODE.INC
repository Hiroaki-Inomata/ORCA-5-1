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
      *    ファイル削除パラメタ  COPY (CPORCSQRENCODE.INC)     *
      *--------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-多々納  14/07/30  一時ディレクトリ対応
      ************************************************************
      *
       01  ORCSQRENCODEAREA.
      *      入力ファイル名
      **** 03  QREN-FILENAME           PIC X(256).
           03  QREN-FILENAME           PIC X(1024).
      *      ＱＲ画像ファイル名
      **** 03  QREN-QRFILENAME         PIC X(256).
           03  QREN-QRFILENAME         PIC X(1024).
      *      QRENCODE -l オプション(L)
           03  QREN-SL-OPTION          PIC X(01).
      *      QRENCODE -s オプション(1)
           03  QREN-SS-OPTION          PIC X(01).
      *      QRENCODE -m オプション(2)
           03  QREN-SM-OPTION          PIC X(01).
      *      QRENCODE -v オプション(12)
           03  QREN-SV-OPTION          PIC X(02).
      *      QRENCODE -k オプション(0:指定なし,1:-k)
           03  QREN-SK-OPTION          PIC X(01).
      *      QRENCODE -C オプション(0:指定なし,1:-C)
           03  QREN-LC-OPTION          PIC X(01).
      *      リターンコード(non -C:ＱＲ画像バージョン)
           03  QREN-RET-VERSION        PIC X(02).
      *      リターンコード(ＱＲ画像ファイル数)
           03  QREN-RET-COUNT          PIC X(02).
      *      リターンコード
           03  QREN-RET                PIC X(01).

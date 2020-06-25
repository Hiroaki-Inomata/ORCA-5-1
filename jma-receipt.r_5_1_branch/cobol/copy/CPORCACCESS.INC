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
      *    ファイル存在チェックサブ(C関数との連絡領域)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CACCESS-AREA.
      *    1024固定
           03  CACCESS-STR-LEN                 PIC S9(9) BINARY.
      *    0:F_OK/1:X_OK/2:W_OK/4:R_OK
           03  CACCESS-MODE                    PIC S9(9) BINARY.
      *    環境変数(COB_LIBRARY_PATH等)
           03  CACCESS-ENVNAME                 PIC X(1024).
      *    ファイル名（入力）
      *      通常はパス付きで設定する。
      *      MODE=X_OKの時にファイル名のみで指定されている場合、
      *      環境変数を参照してファイルを探索する。
           03  CACCESS-IN-FILENAME             PIC X(1024).
      *----以下返却項目
      *    フルパス付きファイル名
           03  CACCESS-FULLPATH                PIC X(1024).
      *    ファイルの存在するディレクトリ名
           03  CACCESS-OT-DIRNAME              PIC X(1024).
      *    パスなしファイル名
           03  CACCESS-OT-FILENAME             PIC X(1024).
      *    書き込み可能か
      *    (1:有/0:無）
           03  CACCESS-WRITABLE                PIC S9(9) BINARY.
      *    ファイルが存在するか
      *    (1:有/0:無）
           03  CACCESS-FILE-EXISTS             PIC S9(9) BINARY.
      *    実行結果 (0:探索成功 / 0以外: 探索失敗)
           03  CACCESS-RC                      PIC S9(9) BINARY.

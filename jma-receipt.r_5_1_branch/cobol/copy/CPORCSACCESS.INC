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
      *    ファイル存在チェックサブ(COBOL)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SACCESS-AREA.
      *    R:ファイル存在チェック
      *    W:書き込み可否チェック
      *    X:実行ファイル存在チェック(COB_LIBRARY_PATHを参照して探索)
           03  SACCESS-MODE                    PIC X(01).
           03  SACCESS-IN-FILENAME             PIC X(1024).
           03  SACCESS-OT-FILENAME             PIC X(1024).
           03  SACCESS-FULLPATH                PIC X(1024).
           03  SACCESS-DIRNAME                 PIC X(1024).
           03  SACCESS-EXISTS                  PIC S9(9).
           03  SACCESS-RC                      PIC S9(9).

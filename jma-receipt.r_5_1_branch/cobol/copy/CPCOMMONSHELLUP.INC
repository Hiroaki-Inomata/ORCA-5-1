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
      *****************************************************************
      *
      *    バックアップ用シェル領域    CPCOMMONSHELLUP.INC
      *
      *      $1 TABLE-NAME
      *      $2 FILE-NAME
      *      $3 OUT-NAME-WK
      *      $4 OUT-NAME
      *      $5 KBEN
      * 
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    ひさなが     02/09/13  ディレクトリをサブプロへ
      *
      *****************************************************************
       01  SHELL-AREA.
           03  SHELL-NAME.
      *         05  FILLER             PIC  X(32) VALUE
      *                   "/usr/local/orca/scripts/allways/".
      *         05  SHELL-SHELL        PIC  X(11).
      *         05  FILLER             PIC  X(57) VALUE SPACE.
               05  SHELL-SHELL        PIC  X(43).
               05  FILLER             PIC  X(57) VALUE SPACE.
           03  SHELL-ARG1.
               05  SHELL-T-NAME       PIC  X(20).
               05  FILLER             PIC  X(80) VALUE SPACE.
           03  SHELL-ARG2.
               05  SHELL-C-NAME       PIC  X(20).
               05  FILLER             PIC  X(80) VALUE SPACE.
           03  SHELL-ARG3.
               05  SHELL-W-NAME       PIC  X(15).
               05  FILLER             PIC  X(85) VALUE SPACE.
           03  SHELL-O-NAME.
               05  SHELL-ARG4         PIC  X(100).
               05  SHELL-ARG5         PIC  X(30).
               05  FILLER             PIC  X(70) VALUE SPACE.
           03  SHELL-ARG6.
               05  SHELL-KBEN         PIC  X(05).
               05  FILLER             PIC  X(95) VALUE SPACE.

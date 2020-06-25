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
      *-------------------------------------------------*
      *    日付画面入力チェックパラメタ                 *
      *                         COPY  (CPORCSGDAY.INC)  *
      *
      *     SPTID-RC    : ZERO （正常終了）
      *                   10    (日付エラー）
      *                   99    (パラメタエラー）
      *-------------------------------------------------*
       01  ORCSGDAYAREA.
      *入力日付
           03  SGDAY-INDAY              PIC X(10).
      *入力タイプ（１：年月、空白：年月日、２：タイプなし）
           03  SGDAY-INTYPE             PIC X(01).
      *システム日付
           03  SGDAY-SYSYMD.
               05  SGDAY-SYSYY          PIC X(04).
               05  SGDAY-SYSMM          PIC X(02).
               05  SGDAY-SYSDD          PIC X(02).
      *省略区分（１：システム日付１月の１２月入力は前年
      *          ２：システム日付の過去日）
           03  SGDAY-SYSKBN             PIC X(01).
      *出力
           03  SGDAY-OT-AREA.
      *        画面用日付（HZ9.Z9.Z9)
               05  SGDAY-OTDAY          PIC X(10).
      *        西暦日付
               05  SGDAY-SDAY           PIC X(08).
      *        和暦日付
               05  SGDAY-WDAY           PIC X(07).
      *
      *復帰コード
           03  SGDAY-RC                 PIC S9(9)   BINARY.
      *

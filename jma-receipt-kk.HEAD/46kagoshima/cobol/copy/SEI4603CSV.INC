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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      01  CSV-REC.
      *レコード区分
           03  CSV-RECKBN         PIC  9(01).
           03  FILLER             PIC  X(02)  VALUE  ",""".
      *医療機関番号
           03  CSV-HOSPNUM        PIC  X(10).
           03  FILLER             PIC  X(02)  VALUE  """,".
      *診療科コード
           03  CSV-SRYKA          PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *診療年月
           03  CSV-SRYYM          PIC  9(05).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *市町村番号
           03  CSV-CITYNUM        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *事業番号
           03  CSV-JIGYONUM       PIC  9(01).
           03  FILLER             PIC  X(02)  VALUE  ",""".
      *受給者番号
           03  CSV-JKYSNUM        PIC  X(09).
           03  FILLER             PIC  X(03)  VALUE  """,""".
      *氏名
           03  CSV-NAME           PIC  X(30).
           03  FILLER             PIC  X(02)  VALUE  """,".
      *生年月日
           03  CSV-BIRTHDAY       PIC  9(07).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *保険区分
           03  CSV-HKNKBN         PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *保険者番号
           03  CSV-HKNJANUM       PIC  9(08).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *入外区分
           03  CSV-NYUGAIKBN      PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *割合
           03  CSV-RATE           PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *実日数
           03  CSV-JNISSU         PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *合計点数
           03  CSV-TENSU          PIC  9(10).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *自己負担支払額
           03  CSV-ITBFTN         PIC  9(10).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *実診療年月
           03  CSV-JSRYYM         PIC  9(05).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *公費番号１
           03  CSV-KOHNUM1        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *公費番号２
           03  CSV-KOHNUM2        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  X"0D".

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
      *    マスタチェックパラメタファイル                *
      *    COPY  (CPBUMCPPARA.INC)                              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
           03  BUMCP-JOBID             PIC 9(07).
           03  BUMCP-SHELLID           PIC X(08).
           03  BUMCP-HOSPNUM           PIC 9(02).
           03  BUMCP-ERRFNAME          PIC X(1024).
           03  BUMCP-MSTKBN            PIC X(1).
           03  BUMCP-CSVKBN            PIC X(1).
           03  BUMCP-SHELLMSG          PIC X(100).
           03  BUMCP-PAGE              PIC 9(05).
           03  BUMCP-RC                PIC 9(02).
           03  BUMCP-MSTYMD            PIC X(8).

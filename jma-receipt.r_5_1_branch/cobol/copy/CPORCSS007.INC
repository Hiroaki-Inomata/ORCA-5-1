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
      *    入院基本料点数チェック　　COPY (CPORCSS007.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SS007-AREA.
           03  SS007-IN.
      *診療年月
               05  SS007-SRYYM             PIC X(06).
           03  SS007-OT.
      *リターンコード (0:正常 / 1:不整合あり)
               05  SS007-RC                PIC 9(01).
      *生活療養入院算定リターンコード (0:正常 / 1:生活療養への異動対象者)
               05  SS007-RC2               PIC 9(01).
      *食堂加算算定可能病棟の入院対象日 (0:対象外 / 1:対象)
               05  SS007-RC3-TBL.
                 07    SS007-RC3           PIC X(01)    OCCURS  31.

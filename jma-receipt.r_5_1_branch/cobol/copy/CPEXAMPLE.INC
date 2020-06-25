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
      *  雛形マスター       (CPEXAMPLE.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  EXAMPLE-REC.
           03  EXAMPLE-KEY.
      *医療機関ＩＤ
               05  EXAMPLE-HOSPNUM        PIC 9(2).
      *区分（１：文例雛形）
               05  EXAMPLE-KUBUN         PIC X(01).
      *連番
               05  EXAMPLE-RENNUM        PIC 9(03).
      *雛形
           03  EXAMPLE-COMMENT           PIC X(800).
      *端末ＩＤ
           03  EXAMPLE-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  EXAMPLE-OPID              PIC X(16).
      *作成年月日
           03  EXAMPLE-CREYMD            PIC X(08).
      *更新年月日
           03  EXAMPLE-UPYMD             PIC X(08).
      *更新時間
           03  EXAMPLE-UPHMS             PIC X(06).

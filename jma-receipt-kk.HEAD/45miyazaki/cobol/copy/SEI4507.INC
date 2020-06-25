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
      *01  MF100-REC.
           03  MF100-KEY.
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *請求データエリア
           03  MF100-SEIKYU-AREA.
               05  MF100-SEIKYU-TBL OCCURS 6.
      *    件数
                   07  MF100-KENSU   PIC 9(05).
      *    点数
                   07  MF100-TENSU   PIC 9(10).
      *    一部負担金
                   07  MF100-ITBFTN  PIC 9(10).

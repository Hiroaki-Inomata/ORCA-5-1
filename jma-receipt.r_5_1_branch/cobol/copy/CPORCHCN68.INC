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
      * 113-8681, Japan.
      ********************************************************************
      *-------------------------------------------------------------*
      *    医療区分・ＡＤＬ区分評価票パラメタ  COPY (CPORCHCN68.INC)
      *-------------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHCN68AREA.
      *    印刷区分（0:頭紙/1:内容印字）
           03  ORCHCN68-PRTKBN         PIC X(01).
      *    診療年月
           03  ORCHCN68-SRYYM          PIC X(06).
      *    患者ＩＤ
           03  ORCHCN68-PTID           PIC 9(10).
      *    患者番号
           03  ORCHCN68-PTNUM          PIC X(20).
      *    連番
           03  ORCHCN68-CNT            PIC 9(06).
      *    バッチ印刷情報(BATCH="1")
           03  ORCHCN68-BATCH          PIC X(1).
           03  ORCHCN68-PRTAREA        PIC X(25000).


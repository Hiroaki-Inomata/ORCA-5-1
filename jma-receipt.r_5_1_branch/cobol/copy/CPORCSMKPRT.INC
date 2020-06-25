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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    オンライン帳票印刷サブ                                      *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ORCSMKPRTAREA.
      *    ＳＨＥＬＬ名
           03  MKPRT-ID                 PIC  X(20).
      *    帳票名
           03  MKPRT-DIA                PIC  X(50).
      *    ＤＥＦ名
           03  MKPRT-DEF                PIC  X(34).
      *    データの種類
           03  MKPRT-TYPE               PIC  X(16).
      *    データ内容
           03  MKPRT-DAT                PIC  X(1024).
      *    印刷位置
           03  MKPRT-PRTNM              PIC  X(16).
      *    印刷内容
           03  MKPRT-INFO               PIC  X(100).
      *    オフセット
      **     03  MKPRT-OFFSET-X           PIC  S9(2)V9(1).
      **     03  MKPRT-OFFSET-Y           PIC  S9(2)V9(1).
      *
      *    復帰コード
           03  MKPRT-RC                 PIC  9(04).
      *

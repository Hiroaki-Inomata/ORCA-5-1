      ******************************************************************
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
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 労災自賠責公務災害集計表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.01    門間    12/11/30   新規作成
      *   01.00.02    門間    16/07/13   該当患者が集計されない不具合を修正
      *******************************************************************
        01  TA07TMP.
            03  TA07TMP-KEY.
              05  TA07TMP-HKNKBN-NUM  PIC X(1).
              05  TA07TMP-NYUGAI      PIC 9(1).
              05  TA07TMP-PTNUM       PIC X(20).
              05  TA07TMP-HKNCOMBI    PIC 9(4).
            03  TA07TMP-HKNNUM      PIC 9(3).
            03  TA07TMP-PTNUM10     PIC X(10).
            03  TA07TMP-PTNUM20     PIC X(20).
            03  TA07TMP-NAME24      PIC X(24).
            03  TA07TMP-NAME48      PIC X(48).
            03  TA07TMP-HKNKBN      PIC X(12).
            03  TA07TMP-RHNUM       PIC 9(15).
            03  TA07TMP-SGIKBN      PIC X(16).
            03  TA07TMP-SKSBETU     PIC X(8).
            03  TA07TMP-SKYKBN      PIC X(24).
            03  TA07TMP-TENKI       PIC X(4).
            03  TA07TMP-SYMD        PIC X(9).
            03  TA07TMP-RYMDST      PIC X(9).
            03  TA07TMP-RYMDED      PIC X(9).
            03  TA07TMP-SKYTENSU    PIC 9(9).
            03  TA07TMP-SKYMONEY    PIC 9(11).

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
      *  コンポーネント名  : 請求収納チェック表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.00    門間    11/08/31   新規作成
      ******************************************************************
        01  TA06-REC.
            02  TA06-SRYYM          PIC X(26).
            02  TA06-CREATEYMD      PIC X(22).
            02  TA06-PAGE           PIC X(2).
            02  TA06-MEISAI-TABLE   OCCURS  24 TIMES.
              03  TA06-RENNUM         PIC X(6).
              03  TA06-PTNUM          PIC X(10).
              03  TA06-PTNUM2-1       PIC X(10).
              03  TA06-PTNUM2-2       PIC X(10).
              03  TA06-KANA-NAME      PIC X(20).
              03  TA06-NAME           PIC X(20).
              03  TA06-HKN            PIC X(4).
              03  TA06-NYUGAI         PIC X(4).
              03  TA06-SKY-TEN        PIC X(10).
              03  TA06-SYU-TEN        PIC X(10).
              03  TA06-BIKO           PIC X(36).

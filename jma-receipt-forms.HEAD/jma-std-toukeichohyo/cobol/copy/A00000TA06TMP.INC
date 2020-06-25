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
        01  TA06TMP-REC.
            02  TA06TMP-KEY.
              03  TA06TMP-NYUGAIKBN   PIC X(1).
              03  TA06TMP-HKN         PIC X(1).
              03  TA06TMP-PTNUM       PIC X(20).
              03  TA06TMP-KANA-NAME   PIC X(20).
              03  TA06TMP-NAME        PIC X(20).
            02  TA06TMP-SKY-TEN       PIC 9(10).
            02  TA06TMP-SYU-TEN       PIC 9(10).
            02  TA06TMP-BIKO          PIC X(36).
            02  TA06TMP-HOSPNUM       PIC 9(2).
            02  TA06TMP-PTID          PIC 9(10).
            02  TA06TMP-SRYYM         PIC X(6).
            02  TA06TMP-SKYYM         PIC X(6).

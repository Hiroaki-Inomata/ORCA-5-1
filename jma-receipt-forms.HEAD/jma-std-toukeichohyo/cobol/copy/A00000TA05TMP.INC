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
      *******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 指定病名・診療行為患者一覧
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/03/07  新規作成
      *****************************************************************
       01  TA05TMP-REC.
           03  TA05TMP-KEY.
      *    患者ID
             05  TA05TMP-PTID              PIC X(10).
      *    診療・病名内容TBL
           03  TA05TMP-SRY-TBL             OCCURS 6.
      *    診療行為FLG１〜６(1〜6は入力パラに対応)
             05  TA05TMP-SRYFLG            PIC 9(01).
      *    病名・診療行為名１〜６(1〜6は入力パラに対応)
             05  TA05TMP-SRYNAME           PIC X(44).
      *    病名開始日
             05  TA05TMP-BYOSTYMD          PIC X(08).
      *    転帰日
             05  TA05TMP-TENKIYMD          PIC X(08).
      *    転帰
             05  TA05TMP-TENKI             PIC X(04).
      *    最終来院日
             05  TA05TMP-LASTYMD           PIC 9(08).

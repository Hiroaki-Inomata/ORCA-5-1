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
      *  コンポーネント名  : 未収金一覧表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.00    門間    11/03/29   新規作成
      ******************************************************************
       01  TA03TMP-REC.
           03  TA03TMP-KEY.
      *      患者番号
             05  TA03TMP-PTNUM       PIC X(20).
      *      伝票番号
             05  TA03TMP-DENPNUM     PIC X(7).
      *    氏名
           03  TA03TMP-NAME          PIC X(20).
      *    保険
           03  TA03TMP-HKN           PIC X(4).
      *    請求月日
           03  TA03TMP-SEIKYUMD      PIC X(9).
      *    病室／診療科
           03  TA03TMP-SRYKA-BR      PIC X(8).
      *    保険計
           03  TA03TMP-HKN-TOTAL     PIC 9(9).
      *    患者負担
           03  TA03TMP-KANJAFTN      PIC 9(9).
      *    自費
           03  TA03TMP-JIHI          PIC 9(9).
      *    未収額
           03  TA03TMP-MISYU         PIC S9(9).
      *    入金額
           03  TA03TMP-NYUKIN        PIC 9(9).
      *    入金月日
           03  TA03TMP-NYUKINMD      PIC X(9).
      *    最終受診日
           03  TA03TMP-JYUSHINYMD    PIC X(9).
      *    電話番号
           03  TA03TMP-TEL           PIC X(15).

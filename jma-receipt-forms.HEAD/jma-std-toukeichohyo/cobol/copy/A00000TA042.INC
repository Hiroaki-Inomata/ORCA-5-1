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
      *  コンポーネント名  : 来院・在院状況分析表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *  01.00.00     田中    2011/03/23  新規作成
      ******************************************************************
       01  TA042.
      *    診療年月
           03  TA042-SRYYM             PIC X(16).
      *    グループタイトル
           03  TA042-GRPTITLE          PIC X(20).
      *    作成年月日
           03  TA042-CREATEYMD         PIC X(22).
      *    頁数
           03  TA042-PAGE              PIC X(02).
      *    合計頁数
           03  TA042-GPAGE             PIC X(02).
      *    入外区分
           03  TA042-NYUGAI            PIC X(04).
      *    明細テーブル
           03  TA042-MEISAI-TBL        OCCURS 7 TIMES.
      *    科名／医師名
             05  TA042-KANAME          PIC X(20).
      *    初診患者数
             05  TA042-SYOSIN          PIC X(05).
      *    [内]新患者数
             05  TA042-SINKAN          PIC X(05).
      *    [内]診療時間外
             05  TA042-SYOJKNGAI       PIC X(05).
      *    再診患者数
             05  TA042-SAISIN          PIC X(05).
      *    [内]診療時間外
             05  TA042-SAIJKNGAI       PIC X(05).
      *    患者延べ数
             05  TA042-NNINZU          PIC X(05).
      *    実人数
             05  TA042-JNINZU          PIC X(05).
      *    １日平均患者数
             05  TA042-KANJA-AVG       PIC X(05).
      *    [内]初診平均数
             05  TA042-SYOSIN-AVG      PIC X(05).
      *    平均通院回数
             05  TA042-TUUIN-AVG       PIC X(05).
      *    合計テーブル
           03  TA042-GOKEI-TBL.
      *    合計―初診患者数
             05  TA042-GSYOSIN         PIC X(06).
      *    合計―[内]新患者数
             05  TA042-GSINKAN         PIC X(06).
      *    合計―[内]診療時間外
             05  TA042-GSYOJKNGAI      PIC X(06).
      *    合計―再診患者数
             05  TA042-GSAISIN         PIC X(06).
      *    合計―[内]診療時間外
             05  TA042-GSAIJKNGAI      PIC X(06).
      *    合計―患者延べ数
             05  TA042-GNNINZU         PIC X(06).
      *    合計―実日数
             05  TA042-GJNINZU         PIC X(06).
      *    合計―１日平均患者数
             05  TA042-GKANJA-AVG      PIC X(06).
      *    合計―[内]初診平均数
             05  TA042-GSYOSIN-AVG     PIC X(06).
      *    合計―平均通院回数
             05  TA042-GTUUIN-AVG      PIC X(06).

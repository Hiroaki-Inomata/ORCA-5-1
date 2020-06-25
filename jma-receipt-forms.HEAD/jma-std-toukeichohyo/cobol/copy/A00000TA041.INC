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
       01  TA041.
      *    診療年月
           03  TA041-SRYYM           PIC X(16).
      *    グループタイトル
           03  TA041-GRPTITLE        PIC X(20).
      *    作成年月日
           03  TA041-CREATEYMD       PIC X(22).
      *    頁数
           03  TA041-PAGE            PIC X(02).
      *    合計頁数
           03  TA041-GPAGE           PIC X(02).
      *    入外区分
           03  TA041-NYUGAI          PIC X(04).
      *    明細テーブル
           03  TA041-MEISAI-TBL      OCCURS 7 TIMES.
      *    科名／医師名
             05  TA041-KANAME        PIC X(20).
      *    繰越在院数
             05  TA041-KZAIIN        PIC X(05).
      *    入院数
             05  TA041-NYUIN         PIC X(05).
      *    退院数
             05  TA041-TAIIN         PIC X(05).
      *    [内]死亡
             05  TA041-SIBOU         PIC X(05).
      *    在院数
             05  TA041-ZAIIN         PIC X(05).
      *    在院延べ数
             05  TA041-NZAIIN        PIC X(05).
      *    [内]外泊延べ数
             05  TA041-NGAIHAKU      PIC X(05).
      *    １日平均入院数
             05  TA041-NYUIN-AVG     PIC X(05).
      *    １日平均退院数
             05  TA041-TAIIN-AVG     PIC X(05).
      *    １日平均在院数
             05  TA041-ZAIIN-AVG     PIC X(05).
      *    合計テーブル
           03  TA041-GOKEI-TBL.
      *    合計―繰越在院数
             05  TA041-GKZAIIN       PIC X(06).
      *    合計―入院数
             05  TA041-GNYUIN        PIC X(06).
      *    合計―退院数
             05  TA041-GTAIIN        PIC X(06).
      *    合計―[内]死亡
             05  TA041-GSIBOU        PIC X(06).
      *    合計―在院数
             05  TA041-GZAIIN        PIC X(06).
      *    合計―在院延べ数
             05  TA041-GNZAIIN       PIC X(06).
      *    合計―[内]外泊延べ数
             05  TA041-GNGAIHAKU     PIC X(06).
      *    合計―１日平均入院数
             05  TA041-GNYUIN-AVG    PIC X(06).
      *    合計―１日平均愛因数
             05  TA041-GTAIIN-AVG    PIC X(06).
      *    合計―１日平均在院数
             05  TA041-GZAIIN-AVG    PIC X(06).

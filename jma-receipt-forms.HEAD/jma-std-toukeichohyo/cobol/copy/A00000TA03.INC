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
       01  TA03.
      *    診療年月日月初
           02  TA03-SRYSTYMD         PIC X(22).
      *    診療年月日月末
           02  TA03-SRYEDYMD         PIC X(22).
      *    作成日
           02  TA03-CREATEYMD        PIC X(22).
      *    ﾍﾟｰｼﾞ数
           02  TA03-PAGE             PIC X(2).
      *    総ﾍﾟｰｼﾞ数
           02  TA03-GPAGE            PIC X(2).
      *    種別
           02  TA03-NYUGAI           PIC X(6).
      *    病室／診療科
           02  TA03-LABEL            PIC X(10).
           02  TA03-MEISAI-TBL       OCCURS  34 TIMES.
      *      連番
             03  TA03-RENNUM         PIC X(6).
      *      患者番号
             03  TA03-PTNUM          PIC X(10).
      *      患者番号2-1
             03  TA03-PTNUM2-1       PIC X(10).
      *      患者番号2-2
             03  TA03-PTNUM2-2       PIC X(10).
      *      氏名
             03  TA03-NAME           PIC X(20).
      *      保険
             03  TA03-HKN            PIC X(4).
      *      請求月日
             03  TA03-SEIKYUMD       PIC X(9).
      *      伝票番号
             03  TA03-DENPNUM        PIC X(7).
      *      病室／診療科
             03  TA03-SRYKA-BR       PIC X(8).
      *      保険計
             03  TA03-HKN-TOTAL      PIC X(9).
      *      患者負担
             03  TA03-KANJAFTN       PIC X(9).
      *      自費
             03  TA03-JIHI           PIC X(9).
      *      未収額
             03  TA03-MISYU          PIC X(9).
      *      入金額
             03  TA03-NYUKIN         PIC X(9).
      *      入金月日
             03  TA03-NYUKINMD       PIC X(9).
      *      最終受診日
             03  TA03-JYUSHINYMD     PIC X(9).
      *      電話番号
             03  TA03-TEL            PIC X(15).
      *    合計
           02  TA03-GOKEI-TBL.
      *      件数
             03  TA03-GKENSU         PIC X(6).
      *      保険計
             03  TA03-GHKN-TOTAL     PIC X(10).
      *      患者負担
             03  TA03-GKANJAFTN      PIC X(10).
      *      自費
             03  TA03-GJIHI          PIC X(10).
      *      未収額
             03  TA03-GMISYU         PIC X(10).
      *      入金額
             03  TA03-GNYUKIN        PIC X(10).

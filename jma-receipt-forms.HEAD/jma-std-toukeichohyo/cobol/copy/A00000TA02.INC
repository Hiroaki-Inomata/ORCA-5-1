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
      *  コンポーネント名  : 診療行為別集計表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/02/14  新規作成
      *   01.00.01    門間    2014/05/22  薬価／点数、件数、使用量を小数点第2位まで対応
      *   01.00.02    門間    2017/06/30  外来金額、入院金額欄の桁数を変更
      *******************************************************************
       01  TA02.
      *    診療年月
           03  TA02-PRTYMD              PIC X(50).
      *    グループタイトル
           03  TA02-GRPTITLE            PIC X(10).
      *    診療行為区分名
           03  TA02-SRYKBNNAME          PIC X(16).
      *    作成年月日
           03  TA02-CREATEYMD           PIC X(22).
      *    ページ数
           03  TA02-PAGE                PIC X(02).
      *    合計ページ数
           03  TA02-GPAGE               PIC X(02).
      *    科名（医師名）
           03  TA02-KANAME              PIC X(20).
      *    明細テーブル
           03  TA02-MEISAI-TBL          OCCURS 34 TIMES.
      *    診区
               05  TA02-SRYKBN          PIC X(02).
      *    コード
               05  TA02-SRYCD           PIC X(09).
      *    診療名称
               05  TA02-SRYNAME         PIC X(40).
      *    薬価／点数
               05  TA02-YAKKA           PIC X(12).
      *    外来件数
               05  TA02-GAIRAI-KEN      PIC X(09).
      *    入院件数
               05  TA02-NYUIN-KEN       PIC X(09).
      *    合計件数
               05  TA02-TOTAL-KEN       PIC X(10).
      *    単価
               05  TA02-TANKA           PIC X(12).
      *    購入価
               05  TA02-KONYU           PIC X(09).
      *    外来使用量
               05  TA02-GAIRAI-SIYO     PIC X(09).
      *    入院使用量
               05  TA02-NYUIN-SIYO      PIC X(09).
      *    合計使用量
               05  TA02-TOTAL-SIYO      PIC X(10).
      *    外来金額
               05  TA02-GAIRAI-KIN      PIC X(11).
      *    入院金額
               05  TA02-NYUIN-KIN       PIC X(11).
      *    合計金額
               05  TA02-TOTAL-KIN       PIC X(11).
      *    購入金額
               05  TA02-KONYU-KIN       PIC X(09).
      *    差額
               05  TA02-SAGAKU          PIC X(09).
      *    全体比率
               05  TA02-ZENHI           PIC X(05).
      *    グループ比率
               05  TA02-GRPHI           PIC X(05).
      *    合計テーブル
           03  TA02-GOKEI-TBL.
      *    合計－薬価／点数
               05  TA02-GYAKKA          PIC X(10).
      *    合計－外来件数
               05  TA02-GGAIRAI-KEN     PIC X(10).
      *    合計－入院件数
               05  TA02-GNYUIN-KEN      PIC X(10).
      *    合計－合計件数
               05  TA02-GTOTAL-KEN      PIC X(11).
      *    合計－単価
               05  TA02-GTANKA          PIC X(10).
      *    合計－購入価
               05  TA02-GKONYU          PIC X(10).
      *    合計－外来使用量
               05  TA02-GGAIRAI-SIYO    PIC X(10).
      *    合計－入院使用量
               05  TA02-GNYUIN-SIYO     PIC X(10).
      *    合計－合計使用量
               05  TA02-GTOTAL-SIYO     PIC X(11).
      *    合計－外来金額
               05  TA02-GGAIRAI-KIN     PIC X(11).
      *    合計－入院金額
               05  TA02-GNYUIN-KIN      PIC X(11).
      *    合計－合計金額
               05  TA02-GTOTAL-KIN      PIC X(12).
      *    合計－購入金額
               05  TA02-GKONYU-KIN      PIC X(10).
      *    合計－差額
               05  TA02-GSAGAKU         PIC X(10).
      *    合計－全体比率
               05  TA02-GZENHI          PIC X(05).
      *    合計－グループ比率
               05  TA02-GGRPHI          PIC X(05).

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
      ********************************************************************
      ************************************************************
      *    診療行為セット展開パラメタ　　COPY (CPORCSCSETHEN.INC)
      *                           使用プログラム (ORCGK02,ORCGK02N)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCSETHENAREA.
      *
      *プログラムＩＤ
           03  ORCSCSETHEN-PG                  PIC  X(08).
      *セット名
           03  ORCSCSETHEN-SRYCD               PIC  X(09).
      *対象行
           03  ORCSCSETHEN-IDX                 PIC  9(04).
      *共通編集用
           03  ORCSCSETHEN-SC92-AREA.
      *        処理
               05  ORCSCSETHEN-SYORIFLG          PIC 9(01).
      *        年齢 
               05  ORCSCSETHEN-NENREI.
                   07  ORCSCSETHEN-NENREI-YY     PIC 9(03).
                   07  ORCSCSETHEN-NENREI-MM     PIC 9(02).
                   07  ORCSCSETHEN-NENREI-DD     PIC 9(02).
      *        １５歳未満再診科算定有り（初診・再診のみ使用）
               05  ORCSCSETHEN-15MIMAN-ARI       PIC  9(01).
      *        老人区分
               05  ORCSCSETHEN-ROUJIN            PIC 9(01).
      *        最終受診科
               05  ORCSCSETHEN-LSTSRYKA          PIC  X(02).
      *        最終受診科
               05  ORCSCSETHEN-LSTSRYKAMEI       PIC  X(20).
      *        訂正時の診療コード
               05  ORCSCSETHEN-TEISEI-AREA.
                   07  ORCSCSETHEN-TEISEI-OCC      OCCURS   100.
                       09  ORCSCSETHEN-TEI-SRYCD   PIC X(09).
                       09  ORCSCSETHEN-TEI-KAISU   PIC 9(03).
      *    エラーメッセージ
           03  ORCSCSETHEN-ERRMSG2        PIC  X(100).
      *

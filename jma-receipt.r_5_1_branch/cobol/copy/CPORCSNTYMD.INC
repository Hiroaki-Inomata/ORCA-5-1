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
      *    入退院日取得サブ用パラメタ  COPY (CPORCSNTYMD.INC)
      *                                使用プログラム (ORCSNTYMD)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNTYMDAREA.
           03  SNTYMD-IN.
      *        医療機関ＩＤ
               05  SNTYMD-HOSPNUM          PIC 9(2).
               05  SNTYMD-PTID             PIC 9(10).
      *        処理区分 1...全ての歴を取得
      *                 2...他院歴を除く
      *                 3...他院歴・自院歴を除く
               05  SNTYMD-KBN              PIC X(01).
           03  SNTYMD-OT.
      *        返却件数
               05  SNTYMD-MAX              PIC 9(05).
               05  SNTYMD-OCC              OCCURS  50.
      *            入院履歴番号
                   07  SNTYMD-RRKNUM       PIC 9(03).
      *            入院日
                   07  SNTYMD-NYUINYMD     PIC X(08).
      *            退院日
                   07  SNTYMD-TAIINYMD     PIC X(08).
      *            初回番号
                   07  SNTYMD-SHONUM       PIC 9(03).
      *            状態区分
                   07  SNTYMD-JTIKBN       PIC X(01).
      *        リターンコード
               05  SNTYMD-RC               PIC 9(02).

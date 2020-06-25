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
      *    ORCGW20サブプロ（入院基本料算定）     COPY (CPW20SUB01.INC)       *
      ************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  W20SUB01-LNK.
           03  W20SUB01-IN.
      *
      *        処理区分
               05  W20SUB01-I-KBN              PIC X(01).
      *        基準日
               05  W20SUB01-I-KJNYMD           PIC X(08).
      *
      *        検索キー１
               05  W20SUB01-I-KEY1.
      *
      *            基本診療行為コード
                   07  W20SUB01-I-KHNSRYCD     PIC X(09).
      *
           03  W20SUB01-OT.
      *
      *        リターンコード（０：正常／１：該当なし等）
               05  W20SUB01-O-RC               PIC 9(01).
      *        診療行為コード
               05  W20SUB01-O-KHNSRYCD         PIC  X(09).
      *        診療行為名称
               05  W20SUB01-O-NAME             PIC X(200).
      *        基本点数
               05  W20SUB01-O-KHNTEN           PIC  9(06).
      *        施設基準区分

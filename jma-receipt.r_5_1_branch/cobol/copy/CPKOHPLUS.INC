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
      *    公費付加情報      　 COPY (CPKOHPLUS.INC)             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  KOHPLUS-REC.
      *キー
           03 KOHPLUS-KEY.
      *医療機関ＩＤ
               05 KOHPLUS-HOSPNUM        PIC 9(2).
      *県番号
               05 KOHPLUS-PREFNUM       PIC  X(02).
      *市町村番号
               05 KOHPLUS-CITYNUM       PIC  X(03).
      *公費番号
               05 KOHPLUS-KOHNUM        PIC  X(03).
      *患者区分
               05 KOHPLUS-PTKBN         PIC  X(01).
      *有効開始年月日
               05 KOHPLUS-YUKOSTYMD     PIC  X(08).
      *有効終了年月日
           03 KOHPLUS-YUKOEDYMD         PIC  X(08).
      *負担区分
           03 KOHPLUS-FTNKBN            PIC  X(01).
      *入院−食事療養区分
           03 KOHPLUS-NYUSKJRYYKBN      PIC  X(01).
      *レセプト請求区分
           03 KOHPLUS-RECESKYKBN        PIC  X(01).
      *制度名
           03 KOHPLUS-SEIDONAME         PIC  X(100).
      *端末ＩＤ
           03 KOHPLUS-TERMID            PIC  X(16).
      *オペーレータＩＤ
           03 KOHPLUS-OPID              PIC  X(16).
      *作成年月日
           03 KOHPLUS-CREYMD            PIC  X(08).
      *更新年月日
           03 KOHPLUS-UPYMD             PIC  X(08).
      *更新時間
           03 KOHPLUS-UPHMS             PIC  X(06).
      *更新用キー
           03 KOHPLUSUP-KEY.
      *医療機関ＩＤ
               05 KOHPLUSUP-HOSPNUM      PIC 9(2).
      *県番号
               05 KOHPLUSUP-PREFNUM     PIC  X(02).
      *市町村番号
               05 KOHPLUSUP-CITYNUM     PIC  X(03).
      *公費番号
               05 KOHPLUSUP-KOHNUM      PIC  X(03).
      *患者区分
               05 KOHPLUSUP-PTKBN       PIC  X(01).
      *有効開始年月日
               05 KOHPLUSUP-YUKOSTYMD   PIC  X(08).

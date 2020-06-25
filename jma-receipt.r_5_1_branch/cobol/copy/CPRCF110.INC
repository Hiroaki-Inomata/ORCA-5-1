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
      *    公害レセプト・病名情報         COPY (CPRCF110.INC) 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  RECE110-REC.
           03  RECE110-KEY.
               05  RECE110-KEY1.
      *医療機関番号
                   07  RECE110-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE110-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE110-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE110-PTID        PIC 9(10).
      *レセプト診療科
                   07  RECE110-RECEKA      PIC X(02).
      *公費ＩＤ
                   07  RECE110-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE110-TEKSTYMD    PIC X(08). 
      *主病名フラグ
               05  RECE110-SYUBYOFLG       PIC X(01).
      *診療開始日
               05  RECE110-SRYYMD          PIC X(08).
      *転帰区分
               05  RECE110-TENKIKBN        PIC X(01).
      *転帰日
               05  RECE110-TENKIYMD        PIC X(08).
      *連番号
               05  RECE110-RENNUM          PIC 9(02).
      *病名
               05  RECE110-BYOMEI          PIC X(160).
      *病名文字数
           03  RECE110-BYOMEIMOJI        PIC 9(03).

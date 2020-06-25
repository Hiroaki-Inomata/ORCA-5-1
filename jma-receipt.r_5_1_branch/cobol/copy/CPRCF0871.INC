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
      *    労災レセプト明細書（入院）     COPY (CPRCF0871.INC)   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    05.07.28  剤番号追加
      *  03.05.01    NACL-門脇    07.06.01  グループ診療対応
      ************************************************************
      *01  RECE871-REC.
           03  RECE871-KEY.
      *医療機関ＩＤ
               05  RECE871-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE871-SRYYM           PIC  9(06).
      *入外区分
               05  RECE871-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE871-PTID            PIC  9(10).
      *レセプト科
               05  RECE871-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE871-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE871-TEKSTYMD        PIC  X(08).
      *診療区分
               05  RECE871-SRYKBN          PIC  X(02).
      *
               05  RECE871-NYUTENTTLKBN    PIC  9(03).
      *診療コード（剤の先頭の診療コード）
               05  RECE871-SRYCD1          PIC  X(09).
      *剤番号
               05  RECE871-ZAINUM          PIC  9(08).
      *連番号
               05  RECE871-LINENUM         PIC  9(05).
      *患者番号
           03  RECE871-PTNUM               PIC  X(20).
      *診療科
           03  RECE871-SRYKA               PIC  X(02).
      *適要欄情報
           03  RECE871-TEKIYO-INF.
      *剤区切（＊）
               05  RECE871-ASTER           PIC  X(02).
      *摘要
               05  RECE871-TEKIYO          PIC  X(100).

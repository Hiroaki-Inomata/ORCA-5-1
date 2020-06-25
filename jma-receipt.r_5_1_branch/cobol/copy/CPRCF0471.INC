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
      *    レセプト明細書     COPY (CPRCF047.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *  01.00.01    NACL-門脇    05.07.28  剤番号追加
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      ************************************************************
      *01  RECE471-REC.
           03  RECE471-KEY.
      *医療機関ＩＤ
               05  RECE471-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE471-SRYYM           PIC  9(06).
      *入外区分
               05  RECE471-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE471-PTID            PIC  9(10).
      *レセプト科
               05  RECE471-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE471-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE471-TEKSTYMD        PIC  X(08). 
      *診療区分
               05  RECE471-SRYKBN          PIC  X(02).
      *
               05  RECE471-GAITENTTLKBN    PIC  9(03).
      *診療コード（剤の先頭の診療コード）
               05  RECE471-SRYCD1          PIC  X(09).                    
      *剤番号
               05  RECE471-ZAINUM          PIC  9(08).
      *連番号
               05  RECE471-LINENUM         PIC  9(05).
      *患者番号
           03  RECE471-PTNUM               PIC  X(20).
      *診療科
           03  RECE471-SRYKA               PIC  X(02).
      *適要欄情報
           03  RECE471-TEKIYO-INF.
      *剤区切（＊）
               05  RECE471-ASTER           PIC  X(02).
      *摘要
               05  RECE471-TEKIYO          PIC  X(100).

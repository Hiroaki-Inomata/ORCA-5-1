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
      *    労災レセプト明細書             COPY (CPRCF040.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  03.03.01    NACL-藤原    06/09/19  月途中での氏名変更対応
      *                                     レセプト対象年月日情報追加
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *
      *  04.07.01    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      ************************************************************
      *01  RECE40-REC.
           03  RECE40-KEY.
      *医療機関ＩＤ
               05  RECE40-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE40-SRYYM           PIC  9(06).
      *入外区分
               05  RECE40-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE40-PTID            PIC  9(10).
      *レセプト診療科
               05  RECE40-RECEKA          PIC  X(02).
      *主保険ＩＤ
               05  RECE40-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE40-TEKSTYMD        PIC  X(08). 
      *主保険保険番号
           03  RECE40-HKNNUM              PIC  X(03).
      *保険組合せ番号
           03  RECE40-HKNCOMBI            PIC  9(04).
      *診療科情報
           03 RECE40-SRYKAINF.
               05  RECE40-SRYKA           PIC  X(02)  OCCURS  20.
      *第三者行為時の医保分主保険情報
           03  RECE40-HKN-X.
      *医保分主保険番号
               05  RECE40-HKNNUM-X        PIC  X(03).
      *医保分主保険ＩＤ
               05  RECE40-HKNID-X         PIC  9(10).
      *自賠責区分
           03  RECE40-JIBAIKBN            PIC  X(01).
      *未訂正区分
           03  RECE40-TEISEIKBN           PIC  X(01).
      *レセプト対象年月日情報
           03  RECE40-TEKYMD.
      *適用開始年月日
               05  RECE40-TEKSTYMD-X      PIC  X(08). 
      *適用終了年月日
               05  RECE40-TEKEDYMD-X      PIC  X(08). 
      

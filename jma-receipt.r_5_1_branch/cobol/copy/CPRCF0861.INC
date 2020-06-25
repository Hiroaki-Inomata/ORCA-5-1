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
      *    労災レセプト明細書（入院）     COPY (CPRCF0861.INC)   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    05.07.28  剤ラストフラグの追加
      *  03.05.01    NACL-門脇    07.06.01  グループ診療対応
      *  04.01.01    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      ************************************************************
      *01  RECE861-REC.
           03  RECE861-KEY.
      *医療機関ＩＤ
               05  RECE861-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE861-SRYYM           PIC  9(06).
      *入外区分
               05  RECE861-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE861-PTID            PIC  9(10).
      *レセプト科
               05  RECE861-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE861-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE861-TEKSTYMD        PIC  X(08).
      *診療区分
               05  RECE861-SRYKBN          PIC  X(02).
      *
               05  RECE861-NYUTENTTLKBN    PIC  9(03).
      *診療コード（剤の先頭の診療コード）
               05  RECE861-SRYCD1          PIC  X(09).                    
      *剤番号
               05  RECE861-ZAINUM          PIC  9(08).
      *連番号
               05  RECE861-LINENUM         PIC  9(05).
      *患者番号
           03  RECE861-PTNUM               PIC  X(20).
      *診療科
           03  RECE861-SRYKA               PIC  X(02).
      *剤回数
           03  RECE861-ZAIKAISU            PIC  9(04).
      *剤ラストフラグ
           03  RECE861-ZAILAST             PIC  9(01).
      *診療行為情報
           03  RECE861-SRYACTINF.
      *診療コード
               05  RECE861-SRYCD           PIC  X(09).
      *数量
               05  RECE861-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  RECE861-SRYKAISU        PIC  9(03).
      *単位コード
               05  RECE861-TANICD          PIC  9(03).
      *単位名称有効桁数
               05  RECE861-TANIMOJISU      PIC  9(01).
      *単位名称
               05  RECE861-TANINAME        PIC  X(24).
      *点数識別区分
               05  RECE861-TENSIKIBETU     PIC  9(01).
      *点数
               05  RECE861-TEN             PIC  9(09)V9(02).
      *明細請求フラグ
               05  RECE861-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  RECE861-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  RECE861-INPUTNUM        PIC  9(03).
      *点数欄集計先識別
               05  RECE861-TENTTLKBN       PIC  9(03).
      *名称情報
           03  RECE861-NAMEINF.
      *漢字有効桁数
               05  RECE861-YUKOKETA        PIC  9(03).
      *漢字名称
               05  RECE861-NAME            PIC  X(200).

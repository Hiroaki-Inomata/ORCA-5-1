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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者登録情報（保険・公費の種類、補助区分）(xml2)
      *                                   COPY (CPINSURANCEINF1V2RES.INC)
      ******************************************************************
       01  XML-INSURANCEINFRES.
           02  INSUINFS-CONTEXT          PIC S9(9)   BINARY.
           02  INSUINFS-OBJECT           PIC X(36).
           02  INSUINFS-MODE             PIC S9(9)   BINARY.
           02  INSUINFS-RECORDNAME       PIC X(128).
           02  INSUINFS-INSURANCEINF-RES.
      *        実施日
               03  INSUINFS-INFORMATION-DATE   PIC X(10).
      *        実施時間
               03  INSUINFS-INFORMATION-TIME   PIC X(8).
      *        返却コード
               03  INSUINFS-API-RESULT         PIC X(2).
      *        返却メッセージ
               03  INSUINFS-API-RESULT-MSG     PIC X(100).
               03  INSUINFS-RESKEY             PIC X(100).
      *
      *        基準日付
               03  INSUINFS-BASE-DATE           PIC X(10).
      *
      *        保険情報
               03  INSUINFS-INSURANCE-INFO-G.
      *            保険情報
                   05  INSUINFS-INSURANCE-TBL   OCCURS   99.
      *                保険の種類
                       07  INSUINFS-INSURANCE-CLASS    PIC X(03).
      *                保険の種類名称
                       07  INSUINFS-INSURANCE-NAME     PIC X(20).
      *                法別番号
                       07  INSUINFS-INSURANCE-HKNHBTNUM    PIC X(02).
      *                補助区分
                       07  INSUINFS-ASSISTANCE-TBL     OCCURS  30.
      *                    補助区分
                           09  INSUINFS-ASSISTANCE         PIC X(01).
      *                    補助区分
                           09  INSUINFS-ASSISTANCE-NAME    PIC X(20).
      *                    高齢者区分（１：高齢者のみ）
                           09  INSUINFS-ASSISTANCE-MODE    PIC X(01).
      *        公費情報
               03  INSUINFS-PUBLIC-INSURANCE-G.
      *            公費情報
                   05  INSUINFS-PUBLIC-INSURANCE-TBL  OCCURS  200.
      *                公費の種類
                       07  INSUINFS-PUBLIC-CLASS         PIC X(03).
      *                公費の種類名称
                       07  INSUINFS-PUBLIC-CLASS-NAME     PIC X(20).
      *                公費の法別番号
                       07  INSUINFS-PUBLIC-HKNHBTNUM      PIC X(02).
      *                公費の支払区分（老人のみ
                       07  INSUINFS-PUBLIC-PAYKBN         PIC X(02).
      *

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
      *    月代り受給者番号           　 COPY  (CPMONTHLYNUM.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01   NACL-藤原     03.01.20  入外区分の追加
      *
      *  03.04.01   NACL-藤原     07.02.06  公費ＩＤの追加
      ************************************************************
      *01  MONTHLYNUM-REC.
      *キー
           03  MONTHLYNUM-KEY.
      *医療機関ＩＤ
               05  MONTHLYNUM-HOSPNUM         PIC 9(2).
      *患者ＩＤ
               05  MONTHLYNUM-PTID             PIC  9(10).
      *保険番号（公費）
               05  MONTHLYNUM-KOHNUM           PIC  X(03).
      *公費ＩＤ
               05  MONTHLYNUM-KOHID            PIC  9(10).
      *診療年月
               05  MONTHLYNUM-SRYYM            PIC  X(06).
      *入外区分
               05  MONTHLYNUM-NYUGAIKBN        PIC  X(01).
      *受給者番号
           03  MONTHLYNUM-JKYSNUM              PIC  X(20).
      *端末ＩＤ
           03  MONTHLYNUM-TERMID               PIC  X(32).
      *オペーレータＩＤ
           03  MONTHLYNUM-OPID                 PIC  X(16).
      *作成年月日
           03  MONTHLYNUM-CREYMD               PIC  X(08).
      *更新年月日
           03  MONTHLYNUM-UPYMD                PIC  X(08).
      *更新時間
           03  MONTHLYNUM-UPHMS                PIC  X(06).

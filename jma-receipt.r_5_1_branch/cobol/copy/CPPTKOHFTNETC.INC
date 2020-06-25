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
      *    患者公費負担               COPY  (CPPTKOHFTNETC.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTKOHFTNETC-REC.
      *キー
           03  PTKOHFTNETC-KEY.
      *医療機関ＩＤ
               05  PTKOHFTNETC-HOSPNUM        PIC  9(02).
      *患者ＩＤ
               05  PTKOHFTNETC-PTID           PIC  9(10).
      *公費ＩＤ
               05  PTKOHFTNETC-KOHID          PIC  9(10).
      *年月日
               05  PTKOHFTNETC-SRYYMD         PIC  X(08).
      *連番
               05  PTKOHFTNETC-RENNUM         PIC  9(02).
      *公費保険番号
           03  PTKOHFTNETC-KOHNUM             PIC  X(03).
      *入外区分
           03  PTKOHFTNETC-NYUGAIKBN          PIC  X(01).
      *負担金額
           03  PTKOHFTNETC-FTNMONEY           PIC  9(07).
      *端末ＩＤ
           03  PTKOHFTNETC-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  PTKOHFTNETC-OPID               PIC  X(16).
      *作成年月日
           03  PTKOHFTNETC-CREYMD             PIC  X(08).
      *更新年月日
           03  PTKOHFTNETC-UPYMD              PIC  X(08).
      *更新時間
           03  PTKOHFTNETC-UPHMS              PIC  X(06).

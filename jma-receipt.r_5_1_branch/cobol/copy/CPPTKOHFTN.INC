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
      *    患者公費負担                  COPY  (CPPTKOHFTN.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTKOHFTN-REC.
      *キー
           03  PTKOHFTN-KEY.
      *医療機関ＩＤ
               05  PTKOHFTN-HOSPNUM         PIC 9(2).
      *患者ＩＤ
               05  PTKOHFTN-PTID           PIC  9(10).
      *公費ＩＤ
               05  PTKOHFTN-KOHID          PIC  9(10).
      *連番
               05  PTKOHFTN-RENNUM         PIC  9(03).
      *適用開始年月日
           03  PTKOHFTN-TEKSTYMD           PIC  X(08).
      *適用終了年月日
           03  PTKOHFTN-TEKEDYMD           PIC  X(08).
      *保険番号（公費）
           03  PTKOHFTN-KOHNUM             PIC  X(03).
      *支払区分
           03  PTKOHFTN-PAYKBN             PIC  X(02).
      *外来上限額
           03  PTKOHFTN-GAIJGNGAK          PIC  9(07).
      *入院上限額
           03  PTKOHFTN-NYUJGNGAK          PIC  9(07).
      *端末ＩＤ
           03  PTKOHFTN-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  PTKOHFTN-OPID               PIC  X(16).
      *作成年月日
           03  PTKOHFTN-CREYMD             PIC  X(08).
      *更新年月日
           03  PTKOHFTN-UPYMD              PIC  X(08).
      *更新時間
           03  PTKOHFTN-UPHMS              PIC  X(06).

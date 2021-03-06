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
      *    患者番号変換         COPY  (CPPTNUM.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTNUM-REC.
      *
           03  PTNUM-KEY.
      *医療機関ＩＤ
               05  PTNUM-HOSPNUM              PIC 9(2).
      *患者ＩＤ
               05  PTNUM-PTID                PIC  9(10).
      *患者番号
           03  PTNUM-PTNUM                   PIC  X(20).
      *保険ＩＤ
           03  PTNUM-HKNID                   PIC  9(10).
      *公費ＩＤ
           03  PTNUM-KOHID                   PIC  9(10).
      *自動組合せ番号
           03  PTNUM-AUTOCOMBINUM            PIC  9(04).
      *手動組合せ番号
           03  PTNUM-MANUCOMBINUM            PIC  9(04).
      *テスト患者番号区分
           03  PTNUM-TSTPTNUMKBN             PIC  X(01).
      *端末ＩＤ
           03  PTNUM-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTNUM-OPID                    PIC  X(16).
      *作成年月日
           03  PTNUM-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTNUM-UPYMD                   PIC  X(08).
      *更新時間
           03  PTNUM-UPHMS                   PIC  X(06).

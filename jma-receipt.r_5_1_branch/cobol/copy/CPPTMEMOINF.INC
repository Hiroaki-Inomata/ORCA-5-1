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
      ***************************************************************
      *    患者メモ登録                  COPY  (CPPTMEMOINF.INC)    *
      ***************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTMEMOINF-REC.
      *患者メモ
           03  PTMEMOINF-KEY.
      *  医療機関ＩＤ
               05  PTMEMOINF-HOSPNUM       PIC 9(2).
      *  患者ＩＤ
               05  PTMEMOINF-PTID          PIC  9(10).
      *  登録日
               05  PTMEMOINF-SYSYMD        PIC  X(08).
      *  区分（1:メモ１、2:メモ２(ＣＬＡＩＭに渡す）
               05  PTMEMOINF-MEMOKBN       PIC  9(01).
      *  連番(日の区分・診療科毎の連番。一覧表示に使用する）
               05  PTMEMOINF-RENNUM        PIC  9(02).
      *  診療科
               05  PTMEMOINF-SRYKA         PIC  X(02).
      *メモ内容
           03  PTMEMOINF-MEMO              PIC  X(1000).
      *端末ＩＤ
           03  PTMEMOINF-TERMID            PIC  X(16).
      *オペーレータＩＤ
           03  PTMEMOINF-OPID              PIC  X(16).
      *作成年月日
           03  PTMEMOINF-CREYMD            PIC  X(08).
      *更新年月日
           03  PTMEMOINF-UPYMD             PIC  X(08).
      *更新時間
           03  PTMEMOINF-UPHMS             PIC  X(06).

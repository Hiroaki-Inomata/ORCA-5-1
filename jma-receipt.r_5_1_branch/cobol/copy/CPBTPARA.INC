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
      *****************************************************************
      *  パラメタ       (CPBTPARA.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  BTPARA-REC.
           03  BTPARA-KEY.
               05  BTPARA-HOSPNUM      PIC 9(2).
      *シェルＩＤ
               05  BTPARA-SHELLID       PIC X(08).
      *オペレータＩＤ
               05  BTPARA-RUN-OPID      PIC X(16).
      *
               05  BTPARA-DSP-RENNUM    PIC 9(03).
      *連番号
               05  BTPARA-RENNUM        PIC 9(04).
      *ジョブＩＤ
               05  BTPARA-JOBID         PIC 9(03).
      *scriptID
           03  BTPARA-SCRIPTID          PIC X(20).
      *パラメタ区分
           03  BTPARA-INFO-KBN          PIC X(02).
      *パラメタ
           03  BTPARA-INFO-PARA         PIC X(500).
           03  BRPARA-INFO-PARA-X       REDEFINES    BTPARA-INFO-PARA.
               05  BTPARA-INFO-PTID     PIC 9(10).
               05  BTPARA-INFO-PTNUM    PIC X(20).
               05  BTPARA-FILLER        PIC X(470).
      *端末ＩＤ
           03  BTPARA-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  BTPARA-OPID              PIC X(16).
      *作成年月日
           03  BTPARA-CREYMD            PIC X(08).
      *更新年月日
           03  BTPARA-UPYMD             PIC X(08).
      *更新時間
           03  BTPARA-UPHMS             PIC X(06).
      *LIMIT
           03  BTPARA-SELLIMIT          PIC 9(08).
      *OFFSET
           03  BTPARA-SELOFFSET         PIC 9(08).

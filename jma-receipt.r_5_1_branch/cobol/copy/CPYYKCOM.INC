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
      *  予約コメントマスター       (CPYYKCOM.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  YYKCOM-REC.
           03  YYKCOM-KEY.
      *医療機関ＩＤ
               05  YYKCOM-HOSPNUM          PIC 9(2).
      *診療内容
               05  YYKCOM-SRYNAIYO         PIC X(02).
      *ドクターコード
               05  YYKCOM-DRCD             PIC X(05).
      *予約年月日
               05  YYKCOM-YYKYMD           PIC X(08).
      *キー予約時間
               05  YYKCOM-KEYYYKTIME       PIC 9(04).
      *予約ＩＤ
               05  YYKCOM-YYKID            PIC 9(02).
      *連番号
               05  YYKCOM-RENNUM           PIC 9(01).
      *メモ
           03  YYKCOM-COMMENT              PIC X(100).
      *端末ＩＤ
           03  YYKCOM-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  YYKCOM-OPID                 PIC  X(16).
      *作成年月日
           03  YYKCOM-CREYMD               PIC  X(08).
      *更新年月日
           03  YYKCOM-UPYMD                PIC  X(08).
      *更新時間
           03  YYKCOM-UPHMS                PIC  X(06).

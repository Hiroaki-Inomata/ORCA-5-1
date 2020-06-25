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
      *  PUSH通知情報格納用       (CPPUSH-INFO.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PUSHINFO-REC.
           03  PUSH-KEY.
      *医療機関ＩＤ
               05  PUSH-HOSPNUM        PIC 9(2).
      *UID
               05  PUSH-UID            PIC X(37).
      *
           03  PUSH-EVENT              PIC X(50).
      *
           03  PUSH-PATIENT-MODE       PIC X(10).
      *患者番号
           03  PUSH-PTNUM              PIC X(20).
      *患者ＩＤ
           03  PUSH-PTID               PIC 9(10).
      *削除日付（編集）
           03  PUSH-INFORMATION-DATE   PIC X(10).
      *削除時間（編集）
           03  PUSH-INFORMATION-TIME   PIC X(08).
      *診療日付（編集）
           03  PUSH-PERFORM-DATE       PIC X(10).
      *診療情報
           03  PUSH-PUSHINFO           PIC X(50).
      *診療日付
           03  PUSH-SRYYMD             PIC X(08).
      *オペーレータＩＤ
           03  PUSH-OPID               PIC  X(16).
      *作成年月日
           03  PUSH-CREYMD             PIC  X(08).
      *更新時間
           03  PUSH-UPHMS              PIC  X(06).
      *

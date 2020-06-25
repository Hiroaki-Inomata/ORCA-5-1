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
      *  感染症判定                                (CPINFJUDGMENT.INC)*
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  INFJUDGMENT-REC.
           03  INFJUD-KEY.
      *ＩＤ
             05  INFJUD-ID             PIC 9(05).
      *病原体区分
           03  INFJUD-DISEASE          PIC 9(02).
      *判定コード
           03  INFJUD-CODE             PIC X(09).
      *コード区分
           03  INFJUD-CODE-KBN         PIC 9(01).
      *診療区分
           03  INFJUD-SRYKBN           PIC X(02).
      *作成年月日
           03  INFJUD-CREYMD           PIC X(08).
      *更新年月日
           03  INFJUD-UPYMD            PIC X(08).
      *更新時間
           03  INFJUD-UPHMS            PIC X(06).

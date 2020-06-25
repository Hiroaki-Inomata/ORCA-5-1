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
      *    ＤＰＣ患者情報        COPY (CPDPCPTINF.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  DPCPTINF-REC.
      *キー
           03  DPCPTINF-KEY.
      *医療機関ＩＤ
               05  DPCPTINF-HOSPNUM            PIC  9(2).
      *患者ＩＤ
               05  DPCPTINF-PTID               PIC  9(10).
      *ＤＰＣ履歴番号
               05  DPCPTINF-DPCRRKNUM          PIC  9(03).
      *ＤＰＣ入院日
               05  DPCPTINF-DPCSTYMD           PIC  X(08).
      *ＤＰＣ退院日
               05  DPCPTINF-DPCEDYMD           PIC  X(08).
      *ＤＰＣ起算日
               05  DPCPTINF-BASEYMD            PIC  X(08).
      *一般病棟以外の病棟移動有無（0:なし/1:あり）
           03  DPCPTINF-IDOKBN                 PIC  X(01).
      *予定・緊急入院区分
           03  DPCPTINF-YOTEIKBN               PIC  X(01).
      *前回退院年月日
           03  DPCPTINF-ZENTAIINYMD            PIC  X(08).
      *前回同一傷病での入院の有無（0:なし/1:あり）
           03  DPCPTINF-DOBYOKBN               PIC  X(01).
      *出生時体重
           03  DPCPTINF-BIRTHWT                PIC  X(04).
      *ＪＣＳ
           03  DPCPTINF-JCS                    PIC  X(03).
      *Burn Index
           03  DPCPTINF-BI                     PIC  9(03)V9(01).
      *重症度等
           03  DPCPTINF-JUSHODO                PIC  X(100).
      *端末ＩＤ
           03  DPCPTINF-TERMID                 PIC  X(16).
      *オペーレータＩＤ
           03  DPCPTINF-OPID                   PIC  X(16).
      *作成年月日
           03  DPCPTINF-CREYMD                 PIC  X(08).
      *更新年月日
           03  DPCPTINF-UPYMD                  PIC  X(08).
      *更新時間
           03  DPCPTINF-UPHMS                  PIC  X(06).
      *以下virtual項目
      *入院履歴番号
           03  DPCPTINF-RRKNUM                 PIC  9(03).

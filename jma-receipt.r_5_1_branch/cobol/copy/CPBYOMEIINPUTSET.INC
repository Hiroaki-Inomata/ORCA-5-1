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
      *******************************************************************
      *****************************************************************
      *    病名入力セット                  (CPBYOMEIINPUTSET.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  BYOMEIINPUTSET-REC.
           03  BYOSET-KEY.
      *    医療機関番号
               05  BYOSET-HOSPNUM      PIC 9(2).
      *    病名セットコード
               05  BYOSET-BYOMEIINPUTSET
                                       PIC X(10).
      *    病名セット名
           03  BYOSET-BYOMEIINPUTSETNAME
                                       PIC X(40).
      *    病名入力コード
           03  BYOSET-BYOMEIINPUTCD-G.
               05  BYOSET-BYOMEIINPUTCD
                                       PIC X(20)    OCCURS  6.
      *    端末ＩＤ
           03  BYOSET-TERMID           PIC X(64).
      *    オペーレータＩＤ
           03  BYOSET-OPID             PIC X(16).
      *    作成年月日
           03  BYOSET-CREYMD           PIC X(08).
      *    更新年月日
           03  BYOSET-UPYMD            PIC X(08).
      *    更新時間
           03  BYOSET-UPHMS            PIC X(06).

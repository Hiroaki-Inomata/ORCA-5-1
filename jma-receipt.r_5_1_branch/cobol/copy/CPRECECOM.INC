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
      *  レセプトコメントマスター       (CPRECECOM.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03/03/05  入外区分を追加
      *  01.00.02    NACL-多々納  05/07/19  コメントを１６００へ
      *
      *  03.00.01    NACL-藤原    06/06/13  症状詳記区分を追加
      *  04.06.00    NACL-多々納  10/10/01  コメントを４８００へ
      *                                     連番追加
      *****************************************************************
      *
      *01  RECECOM-REC.
           03  RECECOM-KEY.
      *医療機関ＩＤ
               05  RECECOM-HOSPNUM        PIC 9(2).
      *患者ＩＤ
               05  RECECOM-PTID          PIC 9(10).
      *入外区分
               05  RECECOM-NYUGAIKBN     PIC X(01).
      *診療科
               05  RECECOM-SRYKA         PIC X(02).
      *診療年月
               05  RECECOM-SRYYM         PIC X(06).
      *保険組合せ番号
               05  RECECOM-HKNCOMBI      PIC 9(04).
      *診療日
               05  RECECOM-SRYDD         PIC X(02).
      *症状詳記区分
               05  RECECOM-SJKBN         PIC X(02).
      *連番
               05  RECECOM-RENNUM        PIC 9(02).
      *コメント
           03  RECECOM-COMMENT           PIC X(4800).
      *端末ＩＤ
           03  RECECOM-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  RECECOM-OPID              PIC X(16).
      *作成年月日
           03  RECECOM-CREYMD            PIC X(08).
      *更新年月日
           03  RECECOM-UPYMD             PIC X(08).
      *更新時間
           03  RECECOM-UPHMS             PIC X(06).
      *
      *更新用キー
           03  RECECOM-UPD-KEY.
      *医療機関ＩＤ
               05  RECECOM-UPD-HOSPNUM       PIC 9(2).
      *患者ＩＤ
               05  RECECOM-UPD-PTID          PIC 9(10).
      *入外区分
               05  RECECOM-UPD-NYUGAIKBN     PIC X(01).
      *診療科
               05  RECECOM-UPD-SRYKA         PIC X(02).
      *診療年月
               05  RECECOM-UPD-SRYYM         PIC X(06).
      *保険組合せ番号
               05  RECECOM-UPD-HKNCOMBI      PIC 9(04).
      *診療日
               05  RECECOM-UPD-SRYDD         PIC X(02).
      *症状詳記区分
               05  RECECOM-UPD-SJKBN         PIC X(02).
      *連番
               05  RECECOM-UPD-RENNUM        PIC 9(02).

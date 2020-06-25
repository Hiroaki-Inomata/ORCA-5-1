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
      *----------------------------------------------------------*
      *    診療行為リハビリ当月算定カウントパラメタ 
      *                                  COPY (CPORCSC80CNT.NC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSC80CNTAREA.
      *    リハビリ回数集計
           03  ORCSC80CNT-RIGAKU-AREA.
      *        １:理学、２：作業、３：言語
               05  ORCSC80CNT-DCNT-TBL        OCCURS  3.
      *        集団
                   07  ORCSC80CNT-DCNT-SN-G.
                       09  ORCSC80CNT-DCNT-SN      PIC 9(03)
                                                   OCCURS  31.
      *            当日
                   07  ORCSC80CNT-TCNT-S           PIC 9(03).
      *            月計
                   07  ORCSC80CNT-TCNT-SN          PIC 9(03).
      *        個別
                   07  ORCSC80CNT-DCNT-KN-G.
                       09  ORCSC80CNT-DCNT-KN      PIC 9(03)
                                                   OCCURS  31.
                   07  ORCSC80CNT-TCNT-K           PIC 9(03).
                   07  ORCSC80CNT-TCNT-KN          PIC 9(03).
      *        早期リハ等加算（逓減対象外）
               05  ORCSC80CNT-GAICNT-G.
                   07  ORCSC80CNT-GCNT             PIC 9(01)
                                                   OCCURS  31.
      *    訂正フラク
           03  ORCSC80CNT-SYORIFLG                 PIC X(01).
      *

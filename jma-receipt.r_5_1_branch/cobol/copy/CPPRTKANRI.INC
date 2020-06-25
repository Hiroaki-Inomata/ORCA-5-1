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
      *    印刷管理                       (CPPRTKANRI.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PRTKANRI-REC.
      *
           03  PRTKANRI-KEY.
               05  PRTKANRI-HOSPNUM   PIC 9(2).
      *ＫＥＹ
               05  PRTKANRI-TBL-KEY    PIC X(08).
      *連番
               05  PRTKANRI-RENNUM     PIC 9(04).
      *グループ
               05  PRTKANRI-TBL-GROUP  PIC X(14).
      *UUID
               05  PRTKANRI-TBL-UUID   PIC X(36).
      *処理内連番
               05  PRTKANRI-SHORI-RENNUM
                                       PIC 9(04).
      *業務コード
               05  PRTKANRI-GYOUMU-CD  PIC 9(03).                        
      *シェルＩＤ
           03  PRTKANRI-SHELLID        PIC X(20).
      *診療年月
           03  PRTKANRI-SRYYM          PIC X(06).
      *請求年月日
           03  PRTKANRI-SKYYMD         PIC X(08).
      *印刷順連番
           03  PRTKANRI-PRIORITY       PIC 9(04).
      *頁
           03  PRTKANRI-PAGE           PIC 9(05).
      *印刷フラグ
           03  PRTKANRI-PRTFLG         PIC X(01).
      *帳票タイトル
           03  PRTKANRI-TITLE          PIC X(100).
      *プリンタ名
           03  PRTKANRI-PRTNM          PIC X(16).
      *ＡＰＩ−ＩＤ
           03  PRTKANRI-API-ID         PIC X(20).
      *端末ＩＤ
           03  PRTKANRI-TERMID         PIC X(32).
      *オペレータＩＤ
           03  PRTKANRI-OPID           PIC X(16).
      *作成日
           03  PRTKANRI-CREYMD         PIC X(8).
      *LIMIT
           03  PRTKANRI-LIMIT          PIC 9(8).
      *OFFSET
           03  PRTKANRI-OFFSET         PIC 9(8).

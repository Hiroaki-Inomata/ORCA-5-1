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
000100************************************************************
000200* 印刷ＤＢ制御サブパラメタ    CPORCSPRT.INC                *
000300************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.00.02    NACL-藤原    03/03/03  印刷済フラグ追加
      *  01.00.03    NACL-藤原    03/03/10  端末ＩＤを６４に変更
      ************************************************************
       01  ORCSPRTAREA.
      *    処理区分                              
           03  SPRT-MODE           PIC X(03).
      *    復帰コード     
           03  SPRT-RETURN         PIC S9(03).
      *    連番
           03  SPRT-RENNUM         PIC 9(04).
      *    ＫＥＹ
           03  SPRT-TBL-KEY        PIC X(08).
      *    グループ
           03  SPRT-TBL-GROUP      PIC X(14).
      *    UUID
           03  SPRT-TBL-UUID       PIC X(36).
      *    業務コード
           03  SPRT-GYOUMU-CD      PIC 9(03).
      *    診療年月
           03  SPRT-SRYYM          PIC X(06).
      *    請求年月日
           03  SPRT-SKYYMD         PIC X(08).
      *    シェルＩＤ
           03  SPRT-SHELLID        PIC X(20).
      *    患者ＩＤ
           03  SPRT-PTID           PIC 9(10).
      *    処理内連番
           03  SPRT-SHORI-RENNUM   PIC 9(04).
      *    印刷順連番
           03  SPRT-PRIORITY       PIC 9(04).
      *    帳票ＩＤ
           03  SPRT-PRTID          PIC X(20).
      *    帳票タイトル
           03  SPRT-TITLE          PIC X(100).
      *    帳票データ
           03  SPRT-PRTDATA        PIC X(20000).
      *    端末ＩＤ
           03  SPRT-TERMID         PIC X(64).
      *    オペレータＩＤ
           03  SPRT-OPID           PIC X(16).
      *    プリンタ名
           03  SPRT-PRTNM          PIC X(16).
      *    サイト区分
           03  SPRT-SITEKBN        PIC X(01).
      *    印刷済フラグ	
           03  SPRT-PRTFLG         PIC X(01).
      *    データの種類
           03  SPRT-DATA-TYPE      PIC X(16).
      *    PDF(UUID)
           03  SPRT-PDF            PIC X(36).
      *    API-ID
           03  SPRT-API-ID         PIC X(20).

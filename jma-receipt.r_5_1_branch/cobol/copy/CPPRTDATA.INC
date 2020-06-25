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
      *    印刷用データ     COPY (CPPRTDATA.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.00.02    NACL-藤原    03/03/03  プリンタ名追加
      ************************************************************
      *01  PRTDATA-REC.
      *
           03  PRTDATA-KEY.
               05  PRTDATA-HOSPNUM   PIC 9(2).
      *ＫＥＹ
               05  PRTDATA-TBL-KEY    PIC X(08).
      *連番
               05  PRTDATA-RENNUM     PIC 9(04).
      *グループ
               05  PRTDATA-TBL-GROUP  PIC X(14).
      *UUID
               05  PRTDATA-TBL-UUID   PIC X(36).
      *処理内連番
               05  PRTDATA-SHORI-RENNUM
                                      PIC 9(04).
      *業務コード
               05  PRTDATA-GYOUMU-CD  PIC 9(03).
      *シェルＩＤ
           03  PRTDATA-SHELLID        PIC X(20).
      *帳票ＩＤ
           03  PRTDATA-PRTID          PIC X(20).
      *診療年月
           03  PRTDATA-SRYYM          PIC X(06).
      *請求年月日
           03  PRTDATA-SKYYMD         PIC X(08).
      *患者ＩＤ
           03  PRTDATA-PTID           PIC 9(10).
      *印刷順連番
           03  PRTDATA-PRIORITY       PIC 9(04).
      *頁
           03  PRTDATA-PAGE           PIC 9(05).
      *印刷フラグ
           03  PRTDATA-PRTFLG         PIC X(01).
      *サイト区分
           03  PRTDATA-SITEKBN        PIC X(01).
      *プリンタ名
           03  PRTDATA-PRTNM          PIC X(16).
      *データの種類
           03  PRTDATA-DATA-TYPE      PIC X(16).
      *ＡＰＩ−ＩＤ
           03  PRTDATA-API-ID         PIC X(20).
      *帳票データ
           03  PRTDATA-PRTDATA        PIC X(20000).
      *作成日
           03  PRTDATA-CREYMD         PIC X(8).
      *
      *検索用項目
           03  PRTDATAXX-STPAGE       PIC 9(05).
           03  PRTDATAXX-EDPAGE       PIC 9(05).    

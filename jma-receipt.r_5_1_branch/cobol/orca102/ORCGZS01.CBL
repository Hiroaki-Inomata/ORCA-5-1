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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZS01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ用入力コード更新（ＺＳ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/05/23  表示キー追加
      *  01.00.02    NACL-多々納  05/12/09  MONFUNC対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
      *
           03  FLG-INPUTCD                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDXX                              PIC 9(02).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
      *
           03  IDX                               PIC 9(04).
           03  IDX-K                             PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 10.
      *
           03  WRK-CDSYU               PIC X(01).
           03  WRK-KETA                PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    
      *    入力コード削除
      *****MOVE  SPA-HOSPID            TO  ICD-HOSPID
           MOVE  SPA-Z01-SRYCD         TO  ICD-SRYCD
           MOVE  SPA-HOSPNUM           TO  ICD-HOSPNUM
           MOVE  INPUTCD-REC           TO  MCPDATA-REC
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "del22"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
      *************UNTIL       IDX     >       23
                   UNTIL       IDX     >       10
                   OR          SPA-ERRCD   NOT =   SPACE
               IF     SPA-Z99-INPUTCD(IDX) NOT =   SPACE
      *            コード種別判定
      *************EVALUATE    TRUE
      *                 WHEN   SPA-Z99-INPUTCD(IDX)(4:)  =   SPACE
      *                     MOVE    "6"            TO  WRK-CDSYU
      *                 WHEN   SPA-Z99-INPUTCD(IDX)(5:)  =   SPACE
      *                     MOVE    "4"            TO  WRK-CDSYU
      *                 WHEN   SPA-Z99-INPUTCD(IDX)(6:1)  =   SPACE
      *                     MOVE    "5"            TO  WRK-CDSYU
      *                 WHEN   SPA-Z99-INPUTCD(IDX)(6:1)  NOT =   SPACE
      *                     MOVE    "6"            TO  WRK-CDSYU
      *                 WHEN   OTHER
      *                     MOVE    SPACE          TO  WRK-CDSYU
      *************END-EVALUATE
                   MOVE    SPA-Z99-CDSYU (IDX)     TO  WRK-CDSYU
      *
                   IF      WRK-CDSYU       NOT =   SPACE
      *        登録
                       PERFORM 410022-INPUTCD-INS-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
      *
           EXIT    PROGRAM
           .
       000-PROC-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード追加処理
      *****************************************************************
       410022-INPUTCD-INS-SEC          SECTION.
      *
           INITIALIZE                      INPUTCD-REC
      *****MOVE    SPA-HOSPID          TO  ICD-HOSPID
           MOVE    WRK-CDSYU           TO  ICD-CDSYU
           MOVE    SPA-Z99-INPUTCD(IDX)  TO  ICD-INPUTCD
           MOVE    SPA-Z99-DSPNAME(IDX)  TO  ICD-DSPNAME
           MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
      *---- (01.00.01) LINE ADD START --------------------------------
           MOVE    IDX                 TO  ICD-DSPSEQ
      *---- (01.00.01) LINE ADD END   --------------------------------
           MOVE    SPA-OPID            TO  ICD-OPID
           MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
           MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
           MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
           .
       410022-INPUTCD-INS-EXT.
           EXIT.
      *

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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSMKPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : オンライン帳票印刷サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/12    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-伊藤    06/03/03  オフセット対応
      *  03.05.00    NACL-太田    07/05/22  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  STS-AREA.
           03  STS-IN              PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *
       01  FLG-AREA.
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DIA             PIC X(10).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
           03  WRK-MKPRT-ST        PIC 9(05).
           03  WRK-CLIENT-TEMP-FLG PIC 9(01).
      *
      *    デイレクトリ位置
       01  SHELL-NAME.
           03  FILLER              PIC X(30)   VALUE
               "scripts/daily/".
      *   Ｄｉａ
       01  SHELL-DIA.
           03  FILLER              PIC X(40)   VALUE
               "scripts/daily/print-data.rb".
      *   ＭＯＮＰＥ
       01  SHELL-MONPE.
           03  FILLER              PIC X(10)   VALUE
               "red2ps".
      *    ＦＯＲＭ位置
       01  SHELL-FORM.
           03  FILLER              PIC X(10)   VALUE
               "form/".
      *    レコード位置
       01  SHELL-RECORD.
           03  FILLER              PIC X(10)   VALUE
               "record/".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *    オフセット用
           COPY   "CPSK1044.INC".
      *    ディレクトリ位置編集サブ
           COPY   "CPMKPASS.INC".
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSMKPRT.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPRTAREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           IF      SPA-CLIENT-PRT-FLG  =   1
               MOVE  SPA-CLIENT-TEMP-FLG
                                       TO  WRK-CLIENT-TEMP-FLG
               MOVE  2                 TO  SPA-CLIENT-TEMP-FLG
               CALL   "ORCSMKPRT1"     USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
               MOVE  WRK-CLIENT-TEMP-FLG
                                       TO  SPA-CLIENT-TEMP-FLG
               EXIT    PROGRAM
           END-IF
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
           MOVE    SPA-ENCODING    TO  WRK-ENCODING9
      *
           PERFORM 200-MON-PRINT-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    編集処理
      *****************************************************************
       200-MON-PRINT-SEC            SECTION.
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    MKPRT-DAT       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-ST     TO  WRK-MKPRT-ST
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
      *    ＳＨＥＬＬ名
           STRING  SHELL-NAME          DELIMITED  BY  SPACE
                   MKPRT-ID            DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-01
           END-STRING
      *    ＦＯＲＭ
           STRING  SHELL-FORM          DELIMITED  BY  SPACE
                   MKPRT-DIA           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-03
           END-STRING
      *    DEF名
           STRING  SHELL-RECORD        DELIMITED  BY  SPACE
                   MKPRT-DEF           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-04
           END-STRING
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           PERFORM 201-OFFSET-GET-SEC
      *
      *    SHELL
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    MKPRT-ID            TO  SHELLTBL-NAME
      *    MONP
           MOVE    SHELL-MONPE         TO  SHELLTBL-ARG1
      *    FROM
           MOVE    MKPASS-OT-03        TO  SHELLTBL-ARG2
      *    イメージ削除フラグ（T:削除）
           MOVE    "T"                 TO  SHELLTBL-ARG3
      *    データ内容
           MOVE    MKPRT-DAT (WRK-MKPRT-ST:)
                                       TO  SHELLTBL-ARG4
      *    印刷位置
           MOVE    MKPRT-PRTNM         TO  SHELLTBL-ARG5
      *    オフセット
           MOVE    SYS-1044-OFFSET-X   TO  SHELLTBL-ARG6
           MOVE    SYS-1044-OFFSET-Y   TO  SHELLTBL-ARG7
      *    エンコーディング
           MOVE    WRK-ENCODING        TO  SHELLTBL-ARG8
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       200-MON-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       201-OFFSET-GET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    MKPRT-DIA           TO  SYS-1044-FORMID
           STRING  MKPRT-PRTNM         DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   MKPRT-DIA  )  AND
                        (SYS-1044-PRTNM   =   MKPRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       201-OFFSET-GET-EXT.
           EXIT.

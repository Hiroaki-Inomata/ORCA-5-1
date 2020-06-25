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
       PROGRAM-ID.             ORCHC67.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約票　印刷（ＨＣＭ６７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/01/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  04.08.00    NACL-多々納  16/02/03  プログラムオプションメモ対応
      *  04.08.00    NACL-多々納  16/10/19  API(GINBEE)対応
      *  05.00.00    NACL-太田    17/02/26  API(GINBEE)対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(2000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    予約票データ
           COPY    "HCM67.INC".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-YYKCOM          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-HEN             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *API
           03  FLG-APISYORI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY             PIC X(03).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-HOSPADRS            PIC X(80).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(40).
               05  WRK-HOSPADRS2       PIC X(40).
           03  WRK-HOSPNAME            PIC X(80).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(40).
               05  WRK-HOSPNAME2       PIC X(40).
           03  WRK-HOSPTEL             PIC X(15).
      *
           03  WRK-API-HOSPNAME        PIC X(120)  OCCURS 3.
           03  WRK-API-HOSPADRS        PIC X(200)  OCCURS 3.
           03  WRK-API-POST            PIC X(07).
      *
           03  WRK-YYKYMD              PIC X(08).
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(10).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(50).
           03  WRK-DRCD-KANANAME       PIC X(50).
      *
           03  CNT-LINE                PIC 9(03).
      *
           03  WRK-PAGE-CNT            PIC 9(03).
           03  WRK-PAGE                PIC 9(03).
           03  WRK-Z9                  PIC Z9.
      *
      *H28.10
      *    API対応用
       01  WRK-API-AREA.
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
      *    予約テーブル
       01  TBL-YY-AREA.
           03  TBL-YYK-REC             OCCURS   50.
               07  TBL-SRYNAIYO            PIC X(02).
               07  TBL-DRCD                PIC X(05).
               07  TBL-YYKYMD              PIC X(08).
               07  TBL-SRYKA               PIC X(02).
               07  TBL-YYKTIME             PIC X(04).
               07  TBL-YYKCOMMENT-G. 
                   09  TBL-COMMENT         PIC X(80)   OCCURS  6.
           03  TBL-MAX                     PIC 9(04).
      *
      *予約票明細行
       01  LINE-MAX                PIC 9(02)   VALUE   12.
      *コメント最大桁
       01  COM-MAX                 PIC 9(02)   VALUE   48.
      *
       01  WRK-TIME-HEN.
           03  WRK-THH             PIC 9(02).
           03  WRK-F1              PIC X(01)   VALUE    ":".
           03  WRK-TSS             PIC 9(02).
      *H28.2
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-YUKOSTYMD          PIC X(08).
           03  PARA-YUKOEDYMD          PIC X(08).
           03  PARA-MEMO               PIC X(100).
      *H28.10  API
           03  PARA-API-DATA           PIC X(01).
           03  PARA-API-CUSTOM-ID      PIC X(30).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(100).
           03  WRK-PARA-MAX            PIC 9(04).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *    予約メモマスター
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *H28.2
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    予約票パラメタ
           COPY    "CPORCHC67.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *H28.10
      * API対応
      *    予約票情報構造体
           COPY    "CPFMYYKHYO.INC".
      *
           COPY    "CPORCRPMAIN.INC".
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPONPRTDATA.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ONPRTDATA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期編集処理
           PERFORM 100-INIT-SYORI-SEC
      *    予約票
           PERFORM 200-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SYORI-SEC                SECTION.
      *
           MOVE    ONPRTDATA-DATA      TO  ORCHC67AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    有効開始日
           MOVE    "YUKOSTYMD"         TO  WRK-PARA-NAME
           MOVE    8                   TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-YUKOSTYMD
      *    有効終了日
           MOVE    "YUKOEDYMD"         TO  WRK-PARA-NAME
           MOVE    8                   TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-YUKOEDYMD
      *    メモ
           MOVE    "MEMO"              TO  WRK-PARA-NAME
           MOVE    100                 TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-MEMO
      *    有効期間初期設定
           IF      PARA-YUKOSTYMD      =   SPACE
               MOVE    "00000000"          TO  PARA-YUKOSTYMD
           END-IF
           IF      PARA-YUKOEDYMD      =   SPACE
               MOVE    "99999999"          TO  PARA-YUKOEDYMD
           END-IF
      *H28.10
      *API対応
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"          TO  WRK-PARA-NAME
           MOVE    1                   TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-API-DATA
           IF     (PARA-API-DATA       =   "1"       )
               CONTINUE
           ELSE
      *        API送信なし
               MOVE    "0"             TO  PARA-API-DATA
           END-IF
      *    HAORI対応
           IF     ( SPA-API-FLG    NOT =   ZERO )
               IF    ( SPA-PRTCONF     =   "2" OR "3" )
                   MOVE    ZERO        TO  PARA-API-DATA
               ELSE
                   MOVE    "1"         TO  PARA-API-DATA
               END-IF
           END-IF
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           MOVE    30                  TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-API-CUSTOM-ID
      *
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC67"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
               IF      WRK-PARA-MAX        >   ZERO
                    MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                                   TO  WRK-PARA
               END-IF
           ELSE
      *        全角判定
               IF      WRK-PARA-MAX        >   ZERO
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                               TO  KANACHK-MAE-INPUT
                   MOVE    WRK-PARA-MAX        TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
               END-IF
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
      *
      *    医療機関編集情報
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
                                           WRK-API-HOSPNAME (1)
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
                                           WRK-API-HOSPADRS (1)
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
           MOVE    SYS-1002-POST       TO  WRK-API-POST
      *    医療機関編集
           PERFORM 1010-HOSPHENSYU-SEC
      *
      *    患者情報
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-NAME          TO  SPA-NAME
           .
       100-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC67"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *
           ADD     1                   TO  WRK-PAGE-CNT
           MOVE    WRK-PAGE-CNT        TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *    印刷用データ出力
           MOVE    HCM67               TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM67.red"         TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
      **** MOVE   HC01PARA            TO  MKPRT-DAT
           MOVE   ASGNPARA            TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(17)  TO  MKPRT-PRTNM
           MOVE   "予約票"            TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"        USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *   予約票 処理
      *****************************************************************
       200-MAIN-SYORI-SEC                SECTION.
      *
           INITIALIZE                      TBL-YY-AREA
      *
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-PTID            TO  YYK-PTID
      *    システム日付以降の予約を対象とする
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       FLG-YYK     =   1
      *        来院済みは対象外
               IF      YYK-RAIINZUMI       =   "1"
                   CONTINUE
               ELSE
                   PERFORM 2001-YYK-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           END-PERFORM
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.10
           IF      PARA-API-DATA        =   "1"
      *        ＡＰＩ送信内容編集処理
               IF      TBL-MAX             >   ZERO
                   PERFORM 2009-API-HENSYU-SEC
               END-IF
               GO  TO  200-MAIN-SYORI-EXT
           END-IF
      *
           IF      TBL-MAX             >   ZERO
      *        予約票編集印刷処理
               PERFORM 2002-YYKHYO-HENSYU-SEC
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約編集処理
      *****************************************************************
       2001-YYK-HENSYU-SEC             SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    YYK-SRYNAIYO        TO  TBL-SRYNAIYO (IDX)
           MOVE    YYK-DRCD            TO  TBL-DRCD     (IDX)
           MOVE    YYK-YYKYMD          TO  TBL-YYKYMD   (IDX)
           MOVE    YYK-SRYKA           TO  TBL-SRYKA    (IDX)
           MOVE    YYK-YYKTIME         TO  TBL-YYKTIME  (IDX)
           PERFORM 20011-YYKCOM-HEN-SEC
           MOVE    IDX                 TO  TBL-MAX
           .
       2001-YYK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コメント編集処理
      *****************************************************************
       20011-YYKCOM-HEN-SEC             SECTION.
      *
      *    予約メモ
           INITIALIZE                  YYKCOM-REC
           MOVE    YYK-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    YYK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
           MOVE    YYK-DRCD            TO  YYKCOM-DRCD
           MOVE    YYK-YYKYMD          TO  YYKCOM-YYKYMD
           MOVE    YYK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
           MOVE    YYK-YYKID           TO  YYKCOM-YYKID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYKCOM-READ-SEC
           ELSE
               MOVE    1                  TO  FLG-YYKCOM
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL   (FLG-YYKCOM     =   1    )
                       OR  (IDY            >=  6    )
               ADD     1                   TO  IDY
               MOVE    YYKCOM-COMMENT      TO  TBL-COMMENT (IDX IDY)
      *
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYKCOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20011-YYKCOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *   予約票編集印刷処理
      *****************************************************************
       2002-YYKHYO-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-PAGE
           MOVE    ZERO                TO  CNT-LINE
      *    帳票初期編集
           PERFORM 230-HCM67-INIT-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-MAX
      *        予約明細編集
               PERFORM 20021-YYKHYO-MEI-SEC
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   6  )    OR
                                  (TBL-COMMENT (IDX IDY)   =   SPACE)
      *            予約コメント編集
                   PERFORM 20022-YYKCOM-MEI-SEC
               END-PERFORM
      *        予約毎に１行改行
               IF      IDX             NOT =   TBL-MAX
                   ADD     1                   TO  CNT-LINE
                   IF      CNT-LINE            >   LINE-MAX
                       PERFORM 400-PRINT-SYORI-SEC
                       ADD     1                   TO  CNT-LINE
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           .
       20011-YYKCOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *   予約票初期編集処理
      *****************************************************************
       20021-YYKHYO-MEI-SEC          SECTION.
      *
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE            >   LINE-MAX
               PERFORM 400-PRINT-SYORI-SEC
               ADD     1                   TO  CNT-LINE
           END-IF
      *    予約日
           MOVE    TBL-YYKYMD (IDX)    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM67-YYKYMD (CNT-LINE)
      *    予約時間
           MOVE    TBL-YYKTIME(IDX)(1:2)   TO  WRK-THH
           MOVE    TBL-YYKTIME(IDX)(3:2)   TO  WRK-TSS
           MOVE    WRK-TIME-HEN        TO  HCM67-YYKTIME (CNT-LINE)
      *    診療科
           MOVE    TBL-YYKYMD (IDX)    TO  WRK-SYSYMD
           MOVE    TBL-SRYKA  (IDX)    TO  WRK-SRYKA
           PERFORM 2008-SRYKAMEI-HEN-SEC
           MOVE    WRK-SRYKA-MEI       TO  HCM67-SRYKA (CNT-LINE)
      *    ドクター
           MOVE    TBL-DRCD   (IDX)    TO  WRK-DRCD
           PERFORM 2009-DRNAME-HEN-SEC
           MOVE    WRK-DRCD-NAME       TO  HCM67-DRNAME  (CNT-LINE)
      *
      *H28.2
      *    プログラムオプション編集
           IF     (PARA-YUKOSTYMD      <=  SPA-SYSYMD )
              AND (PARA-YUKOEDYMD      >=  SPA-SYSYMD )
               MOVE    PARA-MEMO           TO  HCM67-MSG
           END-IF
           .
       20021-YYKHYO-MEI-EXT.
           EXIT.
      *****************************************************************
      *   予約コメント編集処理
      *****************************************************************
       20022-YYKCOM-MEI-SEC          SECTION.
      *
           IF      IDY                 >   1
               ADD     1                   TO  CNT-LINE
               IF      CNT-LINE            >   LINE-MAX
                   PERFORM 400-PRINT-SYORI-SEC
                   ADD     1                   TO  CNT-LINE
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-HEN
           MOVE    1                   TO  IDH
           PERFORM
                   UNTIL       FLG-HEN     =   1
               MOVE    TBL-COMMENT (IDX IDY)(IDH:)
                                       TO  HCM67-BIKOH (CNT-LINE)
               COMPUTE IDH             =   IDH     +  COM-MAX
               IF     (IDH             >   80    ) 
               OR     (TBL-COMMENT (IDX IDY)(IDH:)
                                           =   SPACE )
                   MOVE    1               TO  FLG-HEN
               ELSE
                   ADD     1                   TO  CNT-LINE
                   IF      CNT-LINE            >   LINE-MAX
                       PERFORM 400-PRINT-SYORI-SEC
                       ADD     1                   TO  CNT-LINE
                   END-IF
               END-IF
           END-PERFORM
           .
       20022-YYKCOM-MEI-EXT.
           EXIT.
      *****************************************************************
      *   予約票初期編集処理
      *****************************************************************
       230-HCM67-INIT-SEC          SECTION.
      *
           INITIALIZE                  HCM67
      *
           ADD     1                   TO  WRK-PAGE
      *
           MOVE    WRK-PAGE            TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM67-PAGE
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM67-PTNUM
      *    患者名称
           MOVE    PTINF-NAME          TO  HCM67-NAME
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM67-PRTYMD
      *
      *    医療機関
           MOVE    WRK-HOSPADRS        TO  HCM67-HOSPADRS
           INSPECT HCM67-HOSPADRS      REPLACING ALL "  " BY "　"
      *
           MOVE    WRK-HOSPNAME        TO  HCM67-HOSPNAME
           INSPECT HCM67-HOSPNAME      REPLACING ALL "  " BY "　"
      *
           MOVE    WRK-HOSPTEL         TO  HCM67-HOSPTEL
      *
           MOVE    ZERO                TO  CNT-LINE
           .
       230-HCM67-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       2008-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1
                                           TO  WRK-SRYKA-MEI
           ELSE
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           END-IF
      *
           .
       2008-SRYKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       2009-DRNAME-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
               MOVE    SYS-1010-KANANAME   TO  WRK-DRCD-KANANAME
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-NAME
               MOVE    SPACE               TO  WRK-DRCD-KANANAME
           END-IF
      *
           .
       2009-DRNAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
      *    帳票初期編集
           PERFORM 230-HCM67-INIT-SEC
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *H28.7
      *****************************************************************
      *    ＡＰＩ送信内容編集処理
      *****************************************************************
       2009-API-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  FMYYKHYO-REC
      *
           MOVE    "yoyakuhyo"         TO  FMYH-FORM-ID
           MOVE    SYS-1031-PRTNM(17)  TO  FMYH-PRINTER
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMYH-KOFYMD
      *    患者情報
           MOVE    SPA-PTNUM           TO  FMYH-PTNUM
           MOVE    PTINF-KANANAME      TO  FMYH-KANANAME
           MOVE    PTINF-NAME          TO  FMYH-NAME
           MOVE    PTINF-SEX           TO  FMYH-SEX
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMYH-BIRTHDAY
      *    医療機関名称
           MOVE    WRK-API-HOSPNAME(1) TO  FMYH-HOSP-NAME (1)
           MOVE    WRK-API-HOSPNAME(2) TO  FMYH-HOSP-NAME (2)
           MOVE    WRK-API-HOSPNAME(3) TO  FMYH-HOSP-NAME (3)
      *    医療機関住所
           MOVE    WRK-API-HOSPADRS(1) TO  FMYH-HOSP-ADRS (1)
           MOVE    WRK-API-HOSPADRS(2) TO  FMYH-HOSP-ADRS (2)
           MOVE    WRK-API-HOSPADRS(3) TO  FMYH-HOSP-ADRS (3)
      *
           MOVE    WRK-API-POST        TO  FMYH-HOSP-POST
           MOVE    WRK-HOSPTEL         TO  FMYH-HOSP-TEL
      *    プログラムオプション編集
           IF     (PARA-YUKOSTYMD      <=  SPA-SYSYMD )
              AND (PARA-YUKOEDYMD      >=  SPA-SYSYMD )
               MOVE    PARA-MEMO           TO  FMYH-MEMO
           END-IF
      *
      *    予約明細編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-MAX
      *        予約日
               MOVE    TBL-YYKYMD (IDX)    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  FMYH-TYYKYMD(IDX)
      *        予約時間
               MOVE    TBL-YYKTIME(IDX)(1:2)   TO  WRK-THH
               MOVE    TBL-YYKTIME(IDX)(3:2)   TO  WRK-TSS
               MOVE    WRK-TIME-HEN        TO  FMYH-TYYKTIME(IDX)
      *        診療科
               MOVE    TBL-YYKYMD (IDX)    TO  WRK-SYSYMD
               MOVE    TBL-SRYKA  (IDX)    TO  WRK-SRYKA
               PERFORM 2008-SRYKAMEI-HEN-SEC
               MOVE    TBL-SRYKA  (IDX)    TO  FMYH-TSRYKA    (IDX)
               MOVE    WRK-SRYKA-MEI       TO  FMYH-TSRYKANAME(IDX)
      *        ドクター
               MOVE    TBL-DRCD   (IDX)    TO  WRK-DRCD
               PERFORM 2009-DRNAME-HEN-SEC
               MOVE    TBL-DRCD   (IDX)    TO  FMYH-TDR-CODE (IDX)
               MOVE    WRK-DRCD-NAME       TO  FMYH-TDR-NAME (IDX)
               MOVE    WRK-DRCD-KANANAME   TO  FMYH-TDR-KANANAME(IDX)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   6  )    OR
                                  (TBL-COMMENT (IDX IDY)   =   SPACE)
      *            予約コメント編集
                   MOVE    TBL-COMMENT (IDX IDY)
                                           TO  FMYH-TCOMMENT (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
      *    ＡＰＩ送信
           PERFORM 20031-API-SYORI-SEC
      *
           .
       2009-API-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       20031-API-SYORI-SEC         SECTION.
      *
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMYH-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "yoyakuhyo"         TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "予約票"            TO  RPMAIN-TITLE
           MOVE    "fm_appointment_form"
                                       TO  MCP-TABLE
           MOVE    FMYH-DATA           TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       20031-API-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理（API編集）
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集処理
      *****************************************************************
       1010-HOSPHENSYU-SEC         SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE               TO  SYS-1900-REC
           INITIALIZE                      SYS-1900-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1900-HOSPNUM
           MOVE    "1900"              TO  SYS-1900-KANRICD
           MOVE    "*"                 TO  SYS-1900-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1900-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(22) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-SYS1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-SYS1902-READ-SEC
               END-IF
           END-IF
           .
       1010-HOSPHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-SYS1901-READ-SEC            SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(22)
                                       TO  SYS-1901-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1901-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1901-EDYUKYMD
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
      *
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-API-HOSPNAME(1)
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-API-HOSPNAME(2)
               MOVE    SYS-1901-HOSPNAME3  TO  WRK-API-HOSPNAME(3)
      *
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
       900-SYS1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-SYS1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(22)
                                       TO  SYS-1902-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1902-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1902-EDYUKYMD
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1      TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2      TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL        TO  WRK-HOSPTEL
      *
               MOVE    SYS-1902-ADRS1      TO  WRK-API-HOSPADRS(1)
               MOVE    SYS-1902-ADRS2      TO  WRK-API-HOSPADRS(2)
               MOVE    SYS-1902-ADRS3      TO  WRK-API-HOSPADRS(3)
               MOVE    SYS-1902-POST       TO  WRK-API-POST
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
       900-SYS1902-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込
      *****************************************************************
       900-YYK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYK-REC
               MOVE    ZERO            TO  FLG-YYK
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           .
       900-YYK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       910-YYKCOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYKCOM-REC
               MOVE    ZERO            TO  FLG-YYKCOM
           ELSE
               INITIALIZE                  YYKCOM-REC
               MOVE    1               TO  FLG-YYKCOM
           END-IF
      *
           .
       910-YYKCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
          CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *

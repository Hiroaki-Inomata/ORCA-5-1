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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI000R2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 画像データ取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/11/14    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
           SELECT  META-FILE       ASSIGN  METAPARA
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-META.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  META-FILE.
           COPY    "MCPDATA2.INC"      REPLACING   //MCPDATA2//
                                       BY          //META//.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //META//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //ZIP//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  STS-AREA.
           03  STS-MTYPE               PIC X(02).
           03  STS-META                PIC X(02).
           03  STS-SYSTEM              PIC S9(09).
      *
       01  FLG-AREA.
           03  FLG-MEDIMAGE            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXR                    PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-IMAGEID             PIC X(80).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPIMGGETV2REQ.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSACCESS.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPMEDIMAGE.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
           MOVE     SPACE          TO  METAPARA
           MOVE     SPACE          TO  ZIPPARA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           MOVE    ZERO            TO  MCP-RC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE   "0089"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( IMGGETREQ-DATAID(3:)    =   SPACE )
                   MOVE    "0003"          TO  WRK-ERRCD
           ELSE
               EVALUATE    IMGGETREQ-DATAID(1:1)
               WHEN    "1"
                   PERFORM 2000-MEDIMAGE-GET-SEC
               WHEN    "2"
                   PERFORM 2001-MEDIMAGE-GET-SEC
               WHEN    OTHER
                   MOVE    "0003"          TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像データ取得処理
      *****************************************************************
       2000-MEDIMAGE-GET-SEC           SECTION.
      *
           MOVE    IMGGETREQ-DATAID(3:)
                                   TO  WRK-IMAGEID
      *
           MOVE    SPACE           TO  ZIPPARA
           STRING  WRK-IMAGEID         DELIMITED BY SPACE
                   ".zip"              DELIMITED BY SIZE
           INTO    ZIPPARA
           END-STRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    ZIPPARA         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ZIPPARA
      *
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  SHELLTBL
      *
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
           MOVE    "export_blob.sh"
                                   TO  SHELLTBL-NAME
           MOVE    "API000R2"      TO  SHELLTBL-SHELLID
           MOVE    1               TO  SHELLTBL-JOBID
           MOVE    SPA-TERMID      TO  SHELLTBL-TERMID
      *
           MOVE    WRK-IMAGEID     TO  SHELLTBL-ARG1
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "EXCOMMAND"     TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "daily"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           INITIALIZE                  SACCESS-AREA
           MOVE    ZIPPARA         TO  SACCESS-IN-FILENAME
           CALL    "ORCSACCESS"        USING
                                       SACCESS-AREA
           DISPLAY "SACCESS-RC=" SACCESS-RC
           IF    ( SACCESS-RC      NOT =   ZERO )
               MOVE    "0004"      TO  WRK-ERRCD
           END-IF
      *
           .
       2000-MEDIMAGE-GET-EXT.
           EXIT.
      *****************************************************************
      *    画像データ取得処理
      *****************************************************************
       2001-MEDIMAGE-GET-SEC           SECTION.
      *
           MOVE    IMGGETREQ-DATAID(3:9)
                                       TO  WRK-SRYCD
      *
           PERFORM 900-MEDIMAGE-KEY-SEL-SEC
      *
           IF    ( MEDIMG-IMAGEDATA    NOT =   SPACE )
      *
               MOVE    WRK-SRYCD       TO  METAPARA
               MOVE    SPACE           TO  ZIPPARA
               STRING  METAPARA            DELIMITED BY SPACE
                       ".zip"              DELIMITED BY SIZE
               INTO    ZIPPARA
               END-STRING
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    METAPARA        TO  SGETTEMP-BASENAMES (1)
               MOVE    ZIPPARA         TO  SGETTEMP-BASENAMES (2)
               CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  METAPARA
               MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ZIPPARA
      *
               OPEN    OUTPUT  META-FILE
               MOVE    MEDIMG-IMAGEDATA    TO  META-REC
               WRITE   META-REC
               CLOSE   META-FILE
      *
               MOVE    SPACE           TO  SHELLTBL
               INITIALIZE                  SHELLTBL
      *
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
               MOVE    "convert_image.sh"
                                       TO  SHELLTBL-NAME
               MOVE    "API000R2"      TO  SHELLTBL-SHELLID
               MOVE    2               TO  SHELLTBL-JOBID
               MOVE    SPA-TERMID      TO  SHELLTBL-TERMID
      *
               MOVE    WRK-SRYCD       TO  SHELLTBL-ARG1
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "EXCOMMAND"     TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "allways"       TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               INITIALIZE                  SACCESS-AREA
               MOVE    ZIPPARA         TO  SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"        USING
                                           SACCESS-AREA
               IF    ( SACCESS-RC      NOT =   ZERO )
                   MOVE    "0004"      TO  WRK-ERRCD
               END-IF
           ELSE
                   MOVE    "0005"      TO  WRK-ERRCD
           END-IF
      *
           .
       2001-MEDIMAGE-GET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 890-ERRCD-MSG-SEC
      *
           IF    ( WRK-ERRCD       =   SPACE )
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           PERFORM 990-TERMINATE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           DISPLAY "ZIPPARA=" ZIPPARA
           DISPLAY "WRK-ERRCD=" WRK-ERRCD
           DISPLAY "IMGGETREQ-DATAID=" IMGGETREQ-DATAID
           IF    ( WRK-ERRCD   NOT =   SPACE )
               MOVE    404         TO  APILST30-HTTPSTATUS
           END-IF
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST30-BODY    NOT =   LOW-VALUE )
               DISPLAY "imagegetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_imagegetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST30-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "imagegetv2req"     TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST30-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  IMGGETREQ-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC           SECTION.
      *
           IF    ( APILST30-BODY   NOT =   LOW-VALUE )
               DISPLAY "imagegetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           MOVE     ZIPPARA            TO  ORCA-BLOB-FILE
      *
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           ORCA-BLOB
      *
           IF    ( MCP-RC              =   ZERO )
               MOVE    ORCA-BLOB-OBJECT    TO  APILST30-BODY
               MOVE    "application/zip"   TO  APILST30-CONTENT-TYPE
      *
           ELSE
               DISPLAY "BLOBIMPORT failure = " MCP-RC
               DISPLAY "imageget blob export failure "
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理検索処理
      *****************************************************************
       900-MEDIMAGE-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-MEDIMAGE
      *
           INITIALIZE                  MEDIMAGE-REC
           MOVE    SPA-HOSPNUM     TO  MEDIMG-HOSPNUM
           MOVE    WRK-SRYCD       TO  MEDIMG-SRYCD
           MOVE    MEDIMAGE-REC    TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_med_image" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA2-REC
                                       TO  MEDIMAGE-REC
               ELSE
                   MOVE    1           TO  FLG-MEDIMAGE
               END-IF
           ELSE
               MOVE    1               TO  FLG-MEDIMAGE
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
      *
           .
       900-MEDIMAGE-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           PERFORM 890-ERRCD-MSG-SEC
      *
           PERFORM 990-TERMINATE-SEC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    終了前実行処理
      *****************************************************************
       990-TERMINATE-SEC               SECTION.
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       990-TERMINATE-EXT.
           EXIT.

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
       PROGRAM-ID.             ORCSAPIFMGEN.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 共通
      *  コンポーネント名  : API用帳票データレスポンス情報作成
      *  作成日付    作業者        記述
      *  19/09/20    NMED-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    FORMデータ
           SELECT  FORM-FILE       ASSIGN  FORMPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL 
                                   FILE    STATUS  IS  STS-FORM.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  FORM-FILE.
       01  FORM-REC                    PIC X(20000).
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //FORM//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //UTF//.
      *
       01  STS-AREA.
           03  STS-FORM                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXR                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
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
           03  WRK-TEXT                PIC X(1024).
           03  WRK-ELEMENT             PIC X(128).
           03  WRK-JSON                PIC X(20000).
           03  WRK-JSON-NEW            PIC X(20000).
           03  WRK-LENGTH              PIC 9(08).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    印刷データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  LNKPRTKANRI-AREA.
           COPY    "CPPRTKANRI.INC"    REPLACING    //PRTKANRI-//
                                       BY           //LNKPRTKANRI-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           LNKPRTKANRI-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE           TO  SPA-WORK
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           INITIALIZE                  UIDIO-AREA.
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           MOVE    SPACE           TO  WRK-TEXT
           STRING  SMCNDATE-YMD        DELIMITED BY SPACE
                   SMCNDATE-TIME       DELIMITED BY SPACE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".dat"              DELIMITED BY SIZE
           INTO    WRK-TEXT
           END-STRING
      *
           MOVE    SPACE           TO  FORMPARA
           STRING  "RPF"           DELIMITED BY SIZE
                   WRK-TEXT        DELIMITED BY SPACE
           INTO    FORMPARA
           END-STRING
      *
           MOVE    SPACE           TO  UTFPARA
           STRING  "RPU"           DELIMITED BY SIZE
                   WRK-TEXT        DELIMITED BY SPACE
           INTO    UTFPARA
           END-STRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    FORMPARA        TO  SGETTEMP-BASENAMES (1)
           MOVE    UTFPARA         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  FORMPARA
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  UTFPARA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              FORM-FILE
      *
           MOVE    "{"             TO  FORM-REC
           WRITE   FORM-REC
      *
           PERFORM 2001-HEADER-SEC
           MOVE    WRK-JSON        TO  FORM-REC
           WRITE   FORM-REC
      *
           IF    ( SPA-ERRCD               =   SPACE )
            AND  ( LNKPRTKANRI-TBL-KEY NOT =   SPACE )
               PERFORM 2001-BODY-SEC
           END-IF
      *
           MOVE    "}"             TO  FORM-REC
           WRITE   FORM-REC
      *
           CLOSE   FORM-FILE
      *
           CALL    "SYSTEM"    USING
                               FUNCTION CONCATENATE
                               ("iconv -f EUC-JISX0213 "
                               "-t UTF-8 "
                               FORMPARA
                               " > "
                               UTFPARA
                               )
      *
           MOVE    UTFPARA         TO  SPA-WORK
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       FORMPARA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダー編集処理
      *****************************************************************
       2001-HEADER-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-JSON
      *
           INITIALIZE                  PTINF-REC
           INITIALIZE                  PTNUM-REC
      *
           PERFORM 890-ERRCD-MSG-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
           END-IF
      *
           MOVE    "Information_Date"  TO  WRK-ELEMENT
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Information_Time"  TO  WRK-ELEMENT
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Api_Result"        TO  WRK-ELEMENT
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "0000"          TO  WRK-TEXT
           ELSE
               MOVE    SPA-ERRCD       TO  WRK-TEXT
           END-IF
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Api_Result_Message"
                                       TO  WRK-ELEMENT
           MOVE    SPA-ERRMSG          TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           IF    ( LNKPRTKANRI-API-ID
                                   NOT =   SPACE )
               MOVE    "Form_ID"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-API-ID
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    ( LNKPRTKANRI-TITLE
                                   NOT =   SPACE )
               MOVE    "Form_Name"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TITLE
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    (LNKPRTKANRI-TBL-GROUP (1:8)
                                   NOT =   SPACE )
               MOVE    "Print_Date"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-GROUP (1:8)
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    (LNKPRTKANRI-TBL-GROUP (9:6)
                                   NOT =   SPACE )
               MOVE    "Print_Time"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-GROUP (9:6)
                                       TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-TIME        TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    ( PTINF-PTID      NOT =   ZERO )
      *
               PERFORM 9001-JSON-WRITE-SEC
      *
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '"Patient":{'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
      *
               IF    ( PTNUM-PTNUM
                                   NOT =   SPACE )
                   MOVE    "ID"
                                       TO  WRK-ELEMENT
                   MOVE    PTNUM-PTNUM
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-NAME
                                   NOT =   SPACE )
                   MOVE    "Name"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-NAME
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-KANANAME
                                   NOT =   SPACE )
                   MOVE    "KanaName"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-KANANAME
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-BIRTHDAY
                                   NOT =   SPACE )
                   MOVE    "BirthDate"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-BIRTHDAY
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-SEX
                                   NOT =   SPACE )
                   MOVE    "Sex"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-SEX
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               PERFORM 9003-JSON-WRITE-SEC
      *
           END-IF
      *
           IF    ( LNKPRTKANRI-TBL-UUID
                                   NOT =   SPACE )
            AND  ( SPA-PRTCONF         =   "2" OR "3" OR "A" )
      *
               PERFORM 9001-JSON-WRITE-SEC
      *
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '"Data_Id_Information":{'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
      *
               MOVE    "Data_Id"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-UUID
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
      *
               PERFORM 9003-JSON-WRITE-SEC
      *
           END-IF
      *
           .
       2001-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ編集処理
      *****************************************************************
       2001-BODY-SEC                 SECTION.
      *
           MOVE    ZERO                TO  IDXR
      *
           PERFORM 900-PRTDATA-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PRTDATA NOT =   ZERO )
      *
               COMPUTE IDXR    =   IDXR    +   1
      *
               IF    ( IDXR    =   1 )
      *
                   MOVE    ',"Forms":['
                                       TO  FORM-REC
                   WRITE   FORM-REC
               END-IF
      *
               MOVE    PRTDATA-PRTDATA TO  FORM-REC
               WRITE   FORM-REC
      *
               PERFORM 900-PRTDATA-KEY3-FET-SEC
      *
           END-PERFORM
      *
           IF    ( IDXR        >   ZERO )
               MOVE    "]"             TO  FORM-REC
               WRITE   FORM-REC
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-BODY-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
           WHEN    SPACE
      *
               MOVE    "処理終了"      TO  SPA-ERRMSG
      *
           WHEN    "0001"
               MOVE    "帳票データが存在しませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "リクエスト番号の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "患者番号が未入力です。" 
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "患者番号の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "伝票番号が未入力です。" 
                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "伝票番号に該当する受診履歴が存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "院内・院外区分の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE    "印刷指示の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "診療科の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE    "保険組合せの入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "入院日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0111"
               MOVE    "退院日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE    "入院日＞退院日です。"
                                       TO  SPA-ERRMSG
           WHEN    "0113"
               MOVE    "担当医の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0114"
               MOVE    "診療年月が未入力です。" 
                                       TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "診療年月の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0116"
               MOVE    "印刷開始日の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0117"
               MOVE    "最終印刷頁の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0118"
               MOVE    "最終印刷行の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0119"
               MOVE    "最終印刷行は４０行未満にして下さい。" 
                                       TO  SPA-ERRMSG
           WHEN    "0120"
               STRING  "保険組合せでの印刷となります。"
                                               DELIMITED  BY  SIZE
                       "保険組合せを確認して下さい。"
                                               DELIMITED  BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0121"
               MOVE    "カルテ３号紙の印刷対象がありませんでした。" 
                                       TO  SPA-ERRMSG
           WHEN    "0122"
               MOVE    "入外区分の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0123"
               MOVE    "請求情報が存在しません。" 
                                       TO  SPA-ERRMSG
           WHEN    "0124"
               MOVE    "入外区分が未入力です。" 
                                       TO  SPA-ERRMSG
           WHEN    "0125"
               MOVE    "明細情報が存在しません。" 
                                       TO  SPA-ERRMSG
           WHEN    "0126"
               MOVE    "伝票番号の入力に誤りがあります。" 
                                       TO  SPA-ERRMSG
           WHEN    "0127"
               MOVE    "印刷モードの入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0128"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "印刷モードを有効にする場合、"
                                           DELIMITED BY SIZE
                       "ミドルウェアを更新してください。"
                                           DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
      *共通エラー
           WHEN    "0097"
               MOVE   "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE   "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               STRING  "グループ医療機関が不整合です。"
                                           DELIMITED  BY  SIZE
                       "処理を終了して下さい。"
                                           DELIMITED  BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    JSON情報を書き出す
      *****************************************************************
       910-JSON-WRITE-SEC             SECTION.
      *
           PERFORM 9001-JSON-WRITE-SEC
           PERFORM 9002-JSON-WRITE-SEC
      *
           .
       910-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON情報を書き出す
      *****************************************************************
       9001-JSON-WRITE-SEC             SECTION.
      *
           IF    ( WRK-JSON        =   SPACE )
               CONTINUE
           ELSE
               MOVE    FUNCTION LENGTH(FUNCTION TRIM(WRK-JSON TRAILING))
                                       TO  WRK-LENGTH
               IF    ( WRK-JSON (WRK-LENGTH : 1)   =   "{" OR "[" )
                   MOVE    WRK-JSON    TO  WRK-JSON-NEW
               ELSE
                   MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       ',' )           TO  WRK-JSON-NEW
               END-IF
      *
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
           END-IF
      *
           .
       9001-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON情報を書き出す
      *****************************************************************
       9002-JSON-WRITE-SEC             SECTION.
      *
           IF    ( WRK-TEXT            =   SPACE )
               CONTINUE
           ELSE
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       ' "'
                       FUNCTION TRIM (WRK-ELEMENT TRAILING)
                       '": "'
                       FUNCTION TRIM (WRK-TEXT  TRAILING)
                       '"'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
           END-IF
      *
      *
           .
       9002-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON情報を書き出す
      *****************************************************************
       9003-JSON-WRITE-SEC             SECTION.
      *
           MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '}' )           TO  WRK-JSON-NEW
      *
           MOVE    WRK-JSON-NEW        TO  WRK-JSON
      *
           .
       9003-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
               MOVE    ZERO        TO  FLG-PTINF
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
               MOVE    ZERO        TO  FLG-PTNUM
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ検索処理
      *****************************************************************
       900-PRTDATA-KEY3-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    LNKPRTKANRI-TBL-KEY TO  PRTDATA-TBL-KEY
           MOVE    LNKPRTKANRI-RENNUM  TO  PRTDATA-RENNUM
           MOVE    LNKPRTKANRI-TBL-GROUP
                                       TO  PRTDATA-TBL-GROUP
           MOVE    LNKPRTKANRI-TBL-UUID
                                       TO  PRTDATA-TBL-UUID
           MOVE    LNKPRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票データFETCH処理
      *****************************************************************
       900-PRTDATA-KEY3-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-FET-EXT.
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

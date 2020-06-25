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
       PROGRAM-ID.         ORAPI021S2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 中途データ取得、削除  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/07/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
      *
           03  FLG-HAORI-UP            PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      ***  03  FLG-ERR                 PIC 9(01).
           03  FLG-UPDOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-DOUJITSUKBN         PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE.
               05  WRK-PERFORM-DATE1       PIC X(07).
               05  WRK-PERFORM-DATE2       PIC X(03).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-IN-HKNCOMBI             PIC X(04).
      *
           03  WRK-ORCA-UID                PIC X(36).
      *
      *    HAORI共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
           03  WRK-KARTE-UID           PIC X(36).
      *    患者番号
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-APIV3DATA-REC.
      *        レスポンス番号
               05  WRK-RESPONSE-NUMBER     PIC X(02).
      *        処理ＰＧ
               05  WRK-PARA-SYORIPG        PIC X(02).
               05  WRK-SELECT-CODE         PIC X(04).
      *        処理区分
               05  WRK-PARA-MODE           PIC X(03).
      *        伝票番号
               05  WRK-PARA-DENPNUM        PIC 9(07).
      *
      *    ワーク診療行為マスタ−
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *    API用剤削除連番
           03  LNK-WKSRYAPI-RENNUM         PIC 9(02).
      *    剤削除区分
           03  LNK-WKSRYAPI-DELKBN         PIC 9(01).
      *
      *    スパ領域（HAORI用）
           COPY    "COMMON-SPA"        REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      **   COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPTMDCMODV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPTMDCMODV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    保険組合せセット領域
      ***  COPY    "CPORCSAPIHKN.INC".
      *
      *    診療内容編集サブ領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "CPAPILSTV2.INC".
      *V3  一体化
           COPY    "API21V3SCRAREA.INC".
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
           DISPLAY   "********************"
           DISPLAY   "* tmedicalmod start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST10-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* tmedicalmod end *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-TMEDICALMODRES
           INITIALIZE                      XML-TMEDICALMODREQ
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  TMDCRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TMDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  TMDCRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    TMDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        内容取得
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        中途データ削除
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        患者情報内容編集
               PERFORM 20021-TMDCRES-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    中途データ情報
           EVALUATE    TMDCREQ-REQUEST-NUMBER
           WHEN    "01"
               PERFORM 2002-SYORI-01-SEC
           WHEN    "02"
               PERFORM 2003-SYORI-02-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST10-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_tmedicalmodv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST10-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "tmedicalmodreq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_tmedicalmodv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST10-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "tmedicalmodreq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-TMEDICALMODREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST10-BODY
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
               MOVE    "W"                 TO  WRK-ERRKBN
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  TMDCRES-API-RESULT
               EVALUATE    FLG-SYORI
               WHEN    1
                   IF      FLG-HAORI-UP    =   ZERO
                       MOVE    "処理終了"
                                           TO  TMDCRES-API-RESULT-MSG
                   ELSE
                       MOVE    "展開処理終了"
                                           TO  TMDCRES-API-RESULT-MSG
                   END-IF
               WHEN    2
                   IF      FLG-HAORI-UP    =   ZERO
                       MOVE    "削除処理終了"
                                           TO  TMDCRES-API-RESULT-MSG
                   ELSE
                       MOVE    "解除処理終了"
                                           TO  TMDCRES-API-RESULT-MSG
                   END-IF
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  TMDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  TMDCRES-API-RESULT-MSG
           END-IF
      *
           PERFORM 3001-TMDCXMLMAKE-SEC
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-TMDCXMLMAKE-SEC        SECTION.
      *
           IF      APILST10-BODY   NOT =   LOW-VALUE
               DISPLAY "TMDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_tmedicalmodv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  TMDCRES-MODE
           MOVE    "tmedicalmodres"    TO  TMDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-TMEDICALMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_tmedicalmodv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  TMDCRES-MODE
           MOVE    "tmedicalmodres"    TO  TMDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-TMEDICALMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    TMDCRES-OBJECT      TO  APILST10-BODY 
               MOVE    "application/xml"   TO  APILST10-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-TMDCXMLMAKE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      TMDCREQ-PATIENTID   =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-PATIENTID
           END-IF
           IF     TMDCREQ-PERFORM-DATE =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-PERFORM-DATE
           END-IF
           IF      TMDCREQ-DEP-CODE    =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-DEP-CODE
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF     (WRK-ERRCD           =   SPACE )
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    保険組合せチェック
           IF     (WRK-ERRCD           =   SPACE  )
             AND  (WRK-IN-HKNCOMBI    NOT =   SPACE  )
               PERFORM 20011-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    診療日付
           MOVE    TMDCREQ-PERFORM-DATE    TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    TMDCRES-INFORMATION-DATE
                                           TO  TMDCREQ-PERFORM-DATE
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    診療日付
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *
      *    入外区分
           IF      TMDCREQ-OUTPATIENT-CLASS    =   "I"
      *        入院
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
      *        外来
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *    患者番号
           MOVE    TMDCREQ-PATIENTID   TO  WRK-PTNUM
      *    診療科
           MOVE    TMDCREQ-DEP-CODE    TO  WRK-SRYKA
      *    保険組合せ
           MOVE    TMDCREQ-COMBINATION-NUMBER  TO  WRK-IN-HKNCOMBI
      *
      *    同日再入院（１：同日再入院）
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    TMDCREQ-DOUJITSUKBN TO  WRK-DOUJITSUKBN
           ELSE
               MOVE    SPACE               TO  WRK-DOUJITSUKBN
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PERFORM-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-PERFORM-DATE
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
      *    HAORI展開中解除は未設定あり
           IF     (WRK-SRYKA           =   SPACE )
             AND  (TMDCREQ-REQUEST-NUMBER  =   "02")
             AND  (TMDCREQ-LOCK-MODE   NOT =   SPACE)
               MOVE    SPACE               TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ番号チェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC           SECTION.
      *
           IF      WRK-IN-HKNCOMBI     =   SPACE
               MOVE    ZERO                TO  WRK-HKNCOMBI
           ELSE
      *        保険組合せチェック
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-IN-HKNCOMBI     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
      *            数値エラー
                   MOVE    "13"                TO  WRK-ERRCD
               ELSE
                  MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
               END-IF
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    取得情報編集処理
      *****************************************************************
       20021-TMDCRES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PERFORM-DATE    TO  TMDCRES-PERFORM-DATE
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "I"             TO  TMDCRES-OUTPATIENT-CLASS
           ELSE
               MOVE    "O"             TO  TMDCRES-OUTPATIENT-CLASS
           END-IF
      *
           MOVE    WRK-PTNUM           TO  TMDCRES-PATIENTID
      *
           MOVE    WRK-SRYKA           TO  TMDCRES-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  TMDCRES-DEP-NAME
           IF      WRK-IN-HKNCOMBI     =   SPACE
               CONTINUE
           ELSE
               MOVE    WRK-HKNCOMBI    TO  TMDCRES-COMBINATION-NUMBER
               IF      WRK-HKNCOMBI        =   ZERO
                                           OR  9999
                   CONTINUE
               ELSE
                   PERFORM 200214-HKNCOMBI-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-PTID        NOT =   ZERO
               PERFORM 200211-PTINF-HEN-SEC
           END-IF
      *
      *    同日再入院（１：同日再入院）
           MOVE    WRK-DOUJITSUKBN     TO  TMDCRES-DOUJITSUKBN
      *    電子カルテキー（中途データのＵＩＤ）
           MOVE    TMDCREQ-MEDICAL-UID TO  TMDCRES-MEDICAL-UID
      *
      *    UID（電子カルテ）
           MOVE    TMDCREQ-KARTE-UID   TO  TMDCRES-KARTE-UID
      *    UID（オルカ）
           MOVE    TMDCREQ-ORCA-UID    TO  TMDCRES-ORCA-UID
           .
       20021-TMDCRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       200211-PTINF-HEN-SEC     SECTION.
      *
      *    患者情報
           MOVE    WRK-PTNUM           TO  TMDCRES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  TMDCRES-NAME
           MOVE    PTINF-KANANAME      TO  TMDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TMDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  TMDCRES-SEX
           .
       200211-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途データ返却処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-TMDCRES-HEN-SEC
      *
      *    中途データ検索
           IF      TMDCREQ-MEDICAL-UID NOT =   SPACE
      *        対象のキーを検索する
               PERFORM 20023-WKSRYACT-KEY-SEC
               IF      WRK-ERRCD           NOT =   SPACE
                   GO      TO      2002-SYORI-01-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-HAORI-UP
      *    HAORIからの処理判定
           IF     (TMDCREQ-LOCK-MODE    NOT =   SPACE )
              OR  (TMDCREQ-KARTE-UID    NOT =   SPACE )
              OR  (TMDCREQ-ORCA-UID     NOT =   SPACE )
               PERFORM 20022-HAORI-CHEK-SEC
               IF      WRK-ERRCD           NOT =   SPACE
                   GO      TO      2002-SYORI-01-EXT
               END-IF
           END-IF
      *
      *    中途データ検索
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-DOUJITSUKBN     TO  WKSRY-DOUJITSUKBN
      *
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  IDX
           INITIALIZE                  LNK-WKSRYACT-AREA
           PERFORM
               UNTIL   (FLG-WKSRYACT       =   1 )
                    OR (WRK-ERRCD      NOT =   SPACE)
               IF      IDX             =   ZERO
      *           ドクター等編集
                  PERFORM 20021-WKSRYACT-HEAD-SEC
               END-IF
               ADD     1                   TO  IDX
               MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC (IDX)
      *        HAORI 展開中設定
               IF      FLG-HAORI-UP        =   1
                   IF     (TMDCRES-MODE-FLG    =   "0"  )
      *                １件目が展開中でなければすべて更新
                       MOVE    5               TO  WKSRY-MOD-FLG
                       MOVE    TMDCREQ-ORCA-UID TO  WKSRY-KARTE-KEY
                       PERFORM 20029-WKSRYACT-UPDATE-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
      *        対象なし
               MOVE    "15"                TO  WRK-ERRCD
           ELSE
               PERFORM 20021-WKSRYACT-HENSYU-SEC 
           END-IF
      *
           IF     (FLG-HAORI-UP        =   1    )
              AND (TMDCRES-MODE-FLG    =   "1"  )
      *        展開中は更新なし
               MOVE    "K2"            TO  WRK-KEICD
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ情報編集処理
      *****************************************************************
       20023-WKSRYACT-KEY-SEC     SECTION.
      *
      *    中途データ検索
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
      *
           MOVE    TMDCREQ-MEDICAL-UID TO  WKSRY-KARTE-KEY
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-WKSRYACT        =   ZERO
               MOVE    WKSRY-HKNCOMBI      TO  WRK-HKNCOMBI
               MOVE    WKSRY-DOUJITSUKBN   TO  WRK-DOUJITSUKBN
           ELSE
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       20023-WKSRYACT-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ情報編集処理
      *****************************************************************
       20021-WKSRYACT-HEAD-SEC     SECTION.
      *
      *    ドクター
           IF      WKSRY-DRCD      NOT =   ZERO
               MOVE    WKSRY-DRCD          TO   WRK-DRCD
               PERFORM 2002131-DRCD-CHK-SEC
               MOVE    WRK-DRCD            TO  TMDCRES-DR-CODE
               MOVE    WRK-DRCD-NAME       TO  TMDCRES-DR-NAME
           END-IF
      *    保険組合せ
           INITIALIZE                  TMDCRES-INSURANCE-INFO
           MOVE    WKSRY-HKNCOMBI      TO  TMDCRES-COMBINATION-NUMBER
           IF      WKSRY-HKNCOMBI      =   ZERO
                                       OR  9999
               CONTINUE
           ELSE
               MOVE    WKSRY-HKNCOMBI      TO  WRK-HKNCOMBI
               PERFORM 200214-HKNCOMBI-HEN-SEC
           END-IF
      *    入外区分
           IF      WKSRY-NYUGAIKBN     =   "1"
               MOVE    "I"             TO  TMDCRES-OUTPATIENT-CLASS
           ELSE
               MOVE    "O"             TO  TMDCRES-OUTPATIENT-CLASS
           END-IF
      *    同日再入院
           MOVE    WKSRY-DOUJITSUKBN   TO  TMDCRES-DOUJITSUKBN
      *
      *    更新中フラグ
           EVALUATE    WKSRY-MOD-FLG
               WHEN    1
               WHEN    9
      *            オルカ展開中
                   MOVE    "1"              TO  TMDCRES-MODE-FLG
               WHEN    5
      *            HAORI展開中
                   MOVE    "5"              TO  TMDCRES-MODE-FLG
               WHEN    ZERO
                   MOVE    "0"              TO  TMDCRES-MODE-FLG
           END-EVALUATE
      *    電子カルテキー（ＵＩＤ）
      **** MOVE    WKSRY-KARTE-KEY      TO  TMDCRES-MEDICAL-UID
           .
       20021-WKSRYACT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ情報編集処理
      *****************************************************************
       20021-WKSRYACT-HENSYU-SEC     SECTION.
      *
      *    診療内容編集サブ
           INITIALIZE                  ORAPI021SUB2V3AREA
           MOVE    "2"                 TO  ORAPISUB2-SYORIKBN
           MOVE    IDX                 TO  ORAPISUB2-WKSRYACT-MAX
           CALL    "ORAPI021SUB2V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB2V3AREA
                                       LNK-WKSRYACT-AREA
      *
           INITIALIZE                  TMDCRES-MEDICAL-INF-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   120   )
                       OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )
               MOVE    ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           TO  TMDCRES-MDC-CLASS-KBN
                                                                 (IDX)
               MOVE    ORAPISUB2-MDC-CLASS (IDX)
                                           TO  TMDCRES-MDC-CLASS (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NAME (IDX)
                                           TO  TMDCRES-MDC-CLASS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NUMBER (IDX)
                                           TO  TMDCRES-MDC-CLASS-NUMBER
                                                               (IDX)
      *        入院算定日
               MOVE    ORAPISUB02-MDC-DAY-G (IDX)
                                           TO  TMDCRES-MDC-DAY-G
                                                               (IDX)
      *        剤内容
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   (IDY            >   50    )
                           OR  (ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                               =   SPACE )
                   MOVE    ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-CODE
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAME (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-NAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NUMBER (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-NUMBER
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-MONEY (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-MONEY
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-ATAI-G
                                                            (IDX IDY)
      *            単位コード
                   MOVE    ORAPISUB2-PRSCRPT-TANICD (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-TANICD
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-TANICD-NAME (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-TANICD-NAME
                                                            (IDX IDY)
      *            フィルム分画数
                   MOVE    ORAPISUB2-PRSCRPT-FILMNUM (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-FILMNUM
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-CONTKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  TMDCRES-PRSCRPT-NAIFKBN
                                                            (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           .
       20021-WKSRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       2002131-DRCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-NAME
           END-IF
      *
           .
       2002131-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200214-HKNCOMBI-HEN-SEC     SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
           PERFORM 920-HKNCOMBI-READ-SEC
      *
      *    主保険−保険番号
           MOVE    COMB-HKNNUM         TO  TMDCRES-MAIN-INSURANCE-CLASS
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  TMDCRES-MAIN-INSURANCE-NAME
           IF      COMB-HKNID      NOT =   ZERO
      *        保険情報編集
               PERFORM 930-PTHKNINF-READ-SEC
      *        保険者番号
               MOVE    PTHKN-HKNJANUM  TO
                                       TMDCRES-MAIN-INSURANCE-NUMBER
      *        記号・番号
               MOVE    PTHKN-KIGO      TO  TMDCRES-MAIN-MARK 
               MOVE    PTHKN-NUM       TO  TMDCRES-MAIN-NUMBER
           END-IF
      *    公費情報
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   4
               IF      COMB-KOHID (IDK)    NOT =   ZERO
      *            公費の種類
                   MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  TMDCRES-INSURANCE-CLASS
                                                            (IDK)
                   MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  TMDCRES-INSURANCE-NAME
                                                            (IDK)
      *            公費情報編集
                   PERFORM 940-PTKOHINF-READ-SEC
      *            負担者番号
                   MOVE    PTKOH-FTNJANUM  TO  TMDCRES-PROVIDER-NUMBER
                                                            (IDK)
                   MOVE    PTKOH-JKYSNUM   TO  TMDCRES-RECIPIENT-NUMBER
                                                            (IDK)
               END-IF
           END-PERFORM
      *    削除区分
           IF      COMB-DELKBN         =   "1"
               MOVE    "1"             TO  TMDCRES-COMBINATION-ERR
           ELSE
      *        期間外
               IF     (COMB-TEKSTYMD       <=  SPA-SRYYMD )
                  AND (COMB-TEKEDYMD       >=  SPA-SRYYMD )
                   CONTINUE
               ELSE
                   MOVE    "2"             TO  TMDCRES-COMBINATION-ERR
               END-IF
           END-IF
           .
       200214-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    HAORI処理中チェック処理
      *****************************************************************
       20022-HAORI-CHEK-SEC        SECTION.
      *
      *    UIDコードをTERMID
           MOVE    TMDCREQ-ORCA-UID    TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
      *    レスポンス番号取得
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA         =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "16"                TO  WRK-ERRCD
           END-IF
      *
      *    TBL-SPATMP
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  WRK-SPA-AREA
           ELSE
               MOVE    "16"                TO  WRK-ERRCD
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
      *        診療行為登録の処理の続きであること
               IF     (WRK-PARA-SYORIPG    =   "01" )
                 AND  (WRK-KARTE-UID       =   TMDCREQ-KARTE-UID)
                 AND  (WRK-PARA-PTNUM      =   WRK-PTNUM       )
                 AND  (SPA-SRYYMD          =   WRK-SPA-SRYYMD  )
                   CONTINUE
               ELSE
                   MOVE    "17"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        展開中
               EVALUATE    TMDCREQ-REQUEST-NUMBER
               WHEN    "01"
                   IF      TMDCREQ-LOCK-MODE   =   "1"
                       IF      (WRK-RESPONSE-NUMBER =   "02"
                                                    OR  "03"   )
      *                    展開中更新
                           MOVE    1               TO  FLG-HAORI-UP
                       ELSE
      *                    展開エラー
                           MOVE    "17"            TO  WRK-ERRCD
                       END-IF
                   END-IF
               WHEN    "02"
      *            展開中解除
                   IF      TMDCREQ-LOCK-MODE   =   "1"
                       MOVE    2               TO  FLG-HAORI-UP
                   END-IF
               END-EVALUATE
           END-IF
           .
       20022-HAORI-CHEK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    中途データ更新処理
      *****************************************************************
       20029-WKSRYACT-UPDATE-SEC       SECTION.
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "18"                TO  WRK-ERRCD
           END-IF
           .
       20029-WKSRYACT-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途データ削除処理
      *****************************************************************
       2003-SYORI-02-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-TMDCRES-HEN-SEC
      *
      *    中途データ検索
           IF      TMDCREQ-MEDICAL-UID NOT =   SPACE
      *        対象のキーを検索する
               PERFORM 20023-WKSRYACT-KEY-SEC
               IF      WRK-ERRCD           NOT =   SPACE
                   GO      TO      2003-SYORI-02-EXT
               END-IF
           END-IF
      *    保険組合せ
           IF      TMDCREQ-MEDICAL-UID NOT =   SPACE
               INITIALIZE                  TMDCRES-INSURANCE-INFO
               MOVE    WRK-HKNCOMBI    TO  TMDCRES-COMBINATION-NUMBER
               IF      WRK-HKNCOMBI    =   ZERO
                                       OR  9999
                   CONTINUE
               ELSE
                  PERFORM 200214-HKNCOMBI-HEN-SEC
               END-IF
           END-IF
      *
      *    HAORI展開中解除処理
           IF      TMDCREQ-LOCK-MODE   NOT =   SPACE
               PERFORM 20031-HAORI-SYORI-SEC
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *
      *    中途データ検索
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-DOUJITSUKBN     TO  WKSRY-DOUJITSUKBN
      *
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-WKSRYACT        =   ZERO
      *        1件目で判定
               IF      WKSRY-MOD-FLG       =   0
      *            削除
                   PERFORM 20039-WKSRYACT-DELETE-SEC
               ELSE
      *            展開中は削除なし
                   MOVE    "20"            TO  WRK-ERRCD
               END-IF
           ELSE
      *        対象なし
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途データ削除処理
      *****************************************************************
       20039-WKSRYACT-DELETE-SEC       SECTION.
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "21"                TO  WRK-ERRCD
           END-IF
           .
       20039-WKSRYACT-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    HAORI展開中解除処理
      *****************************************************************
       20031-HAORI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HAORI-UP
      *
      *    HAORIからの処理判定
           IF     (TMDCREQ-KARTE-UID    NOT =   SPACE )
              OR  (TMDCREQ-ORCA-UID     NOT =   SPACE )
               PERFORM 20022-HAORI-CHEK-SEC
           ELSE
               IF      TMDCREQ-LOCK-MODE   =   "1"
                   MOVE    2               TO  FLG-HAORI-UP
      *
                   MOVE    SPACE           TO  SPA-TERMID
      *            排他中のチェックをする
                   PERFORM 990-LOCK-IN-SEC
                   IF      SLOCK-RETURN    NOT =   ZERO
      *                排他中は警告
                       MOVE    "K3"            TO  WRK-KEICD
                   END-IF
                   PERFORM 990-LOCK-OUT-SEC
               END-IF
           END-IF
      *
      *    展開中解除処理
           IF      FLG-HAORI-UP        =   2
               PERFORM 200311-WKSRYACT-KAIJO-SEC
           END-IF
           IF     (WRK-ERRCD           =   SPACE)
              AND (FLG-UPDOK           =   ZERO )
              AND (WRK-KEICD           =   SPACE)
      *        解除対象なし
               MOVE    "19"            TO  WRK-ERRCD
           END-IF
           .
       20031-HAORI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中途データ展開中解除処理
      *****************************************************************
       200311-WKSRYACT-KAIJO-SEC       SECTION.
      *
      *    中途データ検索
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
           MOVE    WRK-DOUJITSUKBN     TO  WKSRY-DOUJITSUKBN
      *
           IF     (WRK-SRYKA           =   SPACE )
             AND  (WRK-IN-HKNCOMBI     =   SPACE )
      *        診療日付、入外区分で検索
               MOVE    "key15"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  FLG-UPDOK
           PERFORM
               UNTIL   (FLG-WKSRYACT       =   1 )
      *        HAORI 展開中
               IF      WKSRY-MOD-FLG       =   5
                   MOVE    ZERO            TO  WKSRY-MOD-FLG
                   PERFORM 20029-WKSRYACT-UPDATE-SEC
                   MOVE    1                   TO  FLG-UPDOK
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200311-WKSRYACT-KAIJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
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
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "保険組合せ番号エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "処理対象がありません"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               STRING  "処理中ではありません。"
                       "展開中の設定はできません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "17"
               STRING  "中途データが展開できる処理ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "18"
               MOVE    "中途データ更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               MOVE    "展開解除対象がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "展開中です。削除できません。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "中途データ削除エラー"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
                                               TO  WRK-ERRMSG
           WHEN    "K2"
               STRING  "中途データは他で展開中です。"
                       "展開中の更新はできません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "K3"
               STRING  "患者番号は他端末使用中です。"
                       "展開中を解除しました。"
                                               INTO  WRK-ERRMSG
               END-STRING
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ読込
      *****************************************************************
       910-WKSRYACT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       910-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC              SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       930-PTHKNINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID 
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       940-PTKOHINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    COMB-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    COMB-PTID           TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDK)    TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA2-REC    TO  SPATMP-REC
                   MOVE    ZERO            TO  FLG-SPATMP
               ELSE
                   INITIALIZE                  SPATMP-REC
                   MOVE    1               TO  FLG-SPATMP
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *

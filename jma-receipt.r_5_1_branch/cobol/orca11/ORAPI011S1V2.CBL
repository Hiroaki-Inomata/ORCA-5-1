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
       PROGRAM-ID.         ORAPI011S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール(サンプル)
      *  コンポーネント名  : 受付登録  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/12    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/09/26  CLAIM診療区分名称編集
      *  04.07.00    NACL-多々納  14/06/XX  ワーニングメッセージ対応他
      *  04.08.00    NACL-多々納  15/12/01  保険組合せ番号送信追加
      *  04.08.00    NACL-多々納  17/04/XX  保険組合せ非表示区分追加
      *  05.00.00    NACL-多々納  17/10/XX  患者未登録受付対応追加
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
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
      *
           03  FLG-YYK-OK              PIC 9(01).
           03  FLG-YYK-CHK             PIC 9(01).
           03  FLG-YYKHIT              PIC 9(01).
           03  FLG-UKETUKE-CHK         PIC 9(01).
           03  FLG-CLAIM-MULTIHOST     PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
           03  FLG-UKETIME             PIC 9(01).
           03  FLG-APIHKN              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-GYOUMU-ARI          PIC 9(01).
           03  FLG-USERPG              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDY1                    PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  TBL-MAX                 PIC 9(04).
           03  IDK                     PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-UKEDELETE       PIC 9(06).
      *
      *    一時領域
      *
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
      *    受付ＩＤ
           03  WRK-UKEID               PIC 9(05).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC 9(04).
           03  WRK-IN-HKNCOMBI         PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD-G.
               05  WRK-KEICD               PIC X(02)
                                           OCCURS   5.
      *    ユーザプログラム
           03  WRK-PTID                PIC X(10).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-SYORI-FLG           PIC 9(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-TIME                PIC X(06).
           03  WRK-CK-TIME1.
             05  WRK-CK-HH1            PIC 9(02).
             05  WRK-CK-MM1            PIC 9(02).
             05  WRK-CK-SS1            PIC 9(02).
           03  WRK-CK-TIME2.
             05  WRK-CK-HH2            PIC 9(02).
             05  WRK-CK-MM2            PIC 9(02).
             05  WRK-CK-SS2            PIC 9(02).
           03  WRK-CK-TIME3.
             05  WRK-CK-HH3            PIC 9(02).
             05  WRK-CK-MM3            PIC 9(02).
             05  WRK-CK-SS3            PIC 9(02).
      *
       01  WRK-XML-AREA.
           03  WRK-REGIST-YMD10        PIC X(10).
           03  WRK-REGIST-TIME8        PIC X(08).
      *
           03  WRK-REGIST-YMD          PIC X(08).
           03  WRK-REGIST-TIME.
               05  WRK-REGIST-THH      PIC X(02).
               05  WRK-REGIST-TMM      PIC X(02).
               05  WRK-REGIST-TSS      PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-SRYNAIYO            PIC X(02).
           03  WRK-SRYNAIYO-NAME       PIC X(24).
      *
           03  WRK-NAME                PIC X(100).
      *
           03  WRK-REGIST-ID           PIC X(05).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *v4.8
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ（拡張)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      **** COPY    "CPORCXKANACONV.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    ユーザプログラム起動チェックサブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *    API XML読み込み用定義体
           COPY    "CPACCPTXMLV2REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPACCPTXMLV2RES.INC".
      *    保険組合せ決定関数サブ
           COPY    "CPORCSAPIHKN.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
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
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    受付
       01  WRK-UKETUKE-REC.
           COPY    "CPUKETUKE.INC"   REPLACING
                                     //UKE-// BY //WRK-UKE-//.
      *
      *    予約
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "U02.INC".
           COPY    "U02Q.INC".
           COPY    "U02X.INC".
           COPY    "U2ERR.INC".
           COPY    "U2ID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
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
               MOVE   404                  TO  APIMOD-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-ACCEPTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  ACCPTRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACCPTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  ACCPTRES-INFORMATION-TIME
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
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  ACCPTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
           MOVE    ZERO                TO  IDK
      *
           MOVE    ZERO                TO  FLG-SYORI
      *H29.10
      *    リクエスト番号設定
           IF      ACCPTREQ-REQUEST-NUMBER =   SPACE
               IF      APIMOD-CLASS        =   "01"
                                           OR  "02"
                   MOVE    APIMOD-CLASS    TO  ACCPTREQ-REQUEST-NUMBER
               END-IF
           END-IF
           EVALUATE    ACCPTREQ-REQUEST-NUMBER
           WHEN    "01"
      *        受付登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        受付削除
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        受付更新（患者番号設定）
               MOVE    3                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *TEST!!
               PERFORM 890-ERRCD-MSG-SEC
               DISPLAY "必須項目チェックエラー "    WRK-ERRCD
                       " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
      **** EVALUATE    APIMOD-CLASS
           EVALUATE    ACCPTREQ-REQUEST-NUMBER
           WHEN    "01"
      *        受付登録処理
               PERFORM 2002-UKE-ADD-SEC
           WHEN    "02"
      *        受付取消処理
               PERFORM 2003-UKE-DELETE-SEC
           WHEN    "03"
      *        受付更新処理
               PERFORM 2004-UKE-UPDATE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ACCEPTREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acceptv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "acceptreq"         TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
           END-IF
      *
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_acceptv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "acceptreq"         TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ACCEPTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_acceptv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
      *    IF      WRK-ERRCD           =   SPACE
      *        IF      FLG-SYORI           =   1
      *            MOVE    WRK-KEICD           TO  WRK-ERRCD
      *        END-IF
      **** END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "受付登録終了"
                                           TO  ACCPTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "受付削除終了"
                                           TO  ACCPTRES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "受付更新終了"
                                           TO  ACCPTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  ACCPTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  ACCPTRES-API-RESULT-MSG
           END-IF
      *    警告メッセージ
           IF     (WRK-ERRCD           =   SPACE )
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL      (IDK     >   5   )
                              OR  (WRK-KEICD (IDK) =   SPACE )
                   MOVE    WRK-KEICD (IDK)     TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   IF      IDK             =   1
                       MOVE    WRK-ERRCD       TO  ACCPTRES-API-RESULT
                   END-IF
                   MOVE    WRK-ERRMSG      TO  ACCPTRES-API-WARNING-MSG
                                                                 (IDK)
               END-PERFORM
           END-IF
      *
           IF      APIMOD-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acceptv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  ACCPTRES-MODE
           MOVE    "acceptres"         TO  ACCPTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ACCEPTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_acceptv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "acceptres"         TO  ACCPTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ACCEPTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_acceptv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "acceptres"         TO  ACCPTRES-RECORDNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-ACCEPTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE  ACCPTRES-OBJECT   TO  APIMOD-BODY
               MOVE  "application/xml" TO  APIMOD-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "accept xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF  ACCPTREQ-PATIENTID      =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-PATIENTID
           END-IF
           IF  ACCPTREQ-REGIST-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-DATE
           END-IF
           IF  ACCPTREQ-REGIST-TIME    =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-TIME
           END-IF
           IF  ACCPTREQ-REGIST-ID      =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-ID
           END-IF
           IF  ACCPTREQ-REGIST-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-DEP-CODE
           END-IF
           IF  ACCPTREQ-REGIST-DR-CODE =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-DR-CODE
           END-IF
           IF  ACCPTREQ-REGIST-DEP-INFO =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-REGIST-DEP-INFO
           END-IF
           IF  ACCPTREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACCPTREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  ACCPTREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACCPTREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  ACCPTREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACCPTREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  ACCPTREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-MARK
           END-IF
           IF  ACCPTREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-NUMBER
           END-IF
           IF  ACCPTREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-CONTINUATION
           END-IF
           IF  ACCPTREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-ASSISTANCE
           END-IF
           IF  ACCPTREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-FAMILY
           END-IF
           IF  ACCPTREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-POLICYHOLDER
           END-IF
           IF  ACCPTREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-START-DATE
           END-IF
           IF  ACCPTREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  ACCPTREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      ACCPTREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  ACCPTREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      ACCPTREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACCPTREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      ACCPTREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACCPTREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      ACCPTREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACCPTREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      ACCPTREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   ACCPTREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      ACCPTREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  ACCPTREQ-INSURANCE-END-DATE
                                                                (IDX)
               END-IF
           END-PERFORM 
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
      *    受付日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-REGYMD-CHK-SEC
           END-IF
      *    受付時間チェック
           IF      WRK-ERRCD           =   SPACE
      *H29.10  削除は空白あり
               IF     (WRK-REGIST-TIME     =   SPACE  )
                 AND  (FLG-SYORI           =   2      )
                   CONTINUE
               ELSE
                   PERFORM 20011-REGTIME-CHK-SEC
               END-IF
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-PTNUM           =   SPACE )
      *            患者氏名チェック
                   PERFORM 20011-KNJANAME-CHK-SEC
               ELSE
      *            患者番号チェック
                   PERFORM 20011-PTNUM-CHK-SEC
               END-IF
           END-IF
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    ドクターコードチェック
           IF     (WRK-DRCD        NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DRCD-CHK-SEC
           END-IF
      *    診療内容チェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYNAIYO-CHK-SEC
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
      *    患者番号
           MOVE    ACCPTREQ-PATIENTID      TO  ACCPTRES-PATIENTID
           MOVE    ACCPTREQ-PATIENTID      TO  WRK-PTNUM
      *    診療科
           MOVE    ACCPTREQ-REGIST-DEP-CODE
                                           TO  ACCPTRES-REGIST-DEP-CODE
           MOVE    ACCPTREQ-REGIST-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    ACCPTREQ-REGIST-DR-CODE
                                           TO  ACCPTRES-REGIST-DR-CODE
           MOVE    ACCPTREQ-REGIST-DR-CODE
                                           TO  WRK-DRCD
      *    受付日・時間
           MOVE    ACCPTREQ-REGIST-DATE    TO  ACCPTRES-REGIST-DATE
           MOVE    ACCPTREQ-REGIST-TIME    TO  ACCPTRES-REGIST-TIME
      *    数字のみに編集
           MOVE    ACCPTREQ-REGIST-DATE    TO  WRK-HEN-DATE
           MOVE    ACCPTREQ-REGIST-TIME    TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-REGIST-YMD
           MOVE    WRK-THMS                TO  WRK-REGIST-TIME
      *    未設定時はシステム情報設定
           IF      WRK-REGIST-YMD      =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-REGIST-YMD
               ADD     1                   TO  IDK
               MOVE    "K1"                TO  WRK-KEICD (IDK)
           END-IF
           IF     (WRK-REGIST-TIME     =   SPACE  )
      *H29.10   更新は必須
             AND  (FLG-SYORI           =   1      )
               MOVE    SMCNDATE-HMS        TO  WRK-REGIST-TIME
               ADD     1                   TO  IDK
               MOVE    "K2"                TO  WRK-KEICD (IDK)
           END-IF
      *    受付日とシステム日付が違う時、受付日をＳＰＡへ
           IF      SPA-SYSYMD      NOT =   WRK-REGIST-YMD
               MOVE    WRK-REGIST-YMD      TO  SPA-SYSYMD
           END-IF
      *
      *    受付ＩＤ
           MOVE    ACCPTREQ-REGIST-ID      TO  ACCPTRES-REGIST-ID
           MOVE    ACCPTREQ-REGIST-ID      TO  WRK-REGIST-ID
      *    診療内容区分
           MOVE    ACCPTREQ-REGIST-DEP-INFO
                                           TO  WRK-SRYNAIYO
      *H29.10
      *    患者氏名
           MOVE    ACCPTREQ-NAME       TO  ACCPTRES-NAME
           MOVE    ACCPTREQ-NAME       TO  WRK-NAME
      *
      *    必須入力チェック
      *    患者番号、氏名
           IF      WRK-PTNUM           =   SPACE
             AND  (WRK-NAME            =   SPACE )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (FLG-SYORI           =   1
                                           OR  3      )
                 AND  (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    ドクター
           IF      WRK-DRCD            =   SPACE
               IF     (FLG-SYORI           =   1
                                           OR  3      )
                 AND  (WRK-ERRCD           =   SPACE  )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    診療内容
           IF      WRK-SRYNAIYO            =   SPACE
               IF     (FLG-SYORI           =   3      )
                 AND  (WRK-ERRCD           =   SPACE  )
                   MOVE    "15"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付日チェック
      *****************************************************************
       20011-REGYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-REGIST-YMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-REGIST-YMD
           END-IF
      *
           .
       20011-REGYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付時間チェック
      *****************************************************************
       20011-REGTIME-CHK-SEC    SECTION.
      *
           IF      WRK-REGIST-TIME NOT NUMERIC
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-REGIST-THH      >=  "24"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           IF      WRK-REGIST-TMM      >=  "60"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           IF      WRK-REGIST-TSS      >=  "60"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-REGTIME-CHK-EXT.
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
           MOVE    SPTID-PTNUM         TO  ACCPTRES-PATIENTID
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
      *    患者氏名チェック
      *****************************************************************
       20011-KNJANAME-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-PTID
           INITIALIZE                      PTINF-REC
      *    全角チェック
           MOVE    WRK-NAME            TO  WRK-MAE-INPUT
           MOVE    50                  TO  WRK-LEN
           PERFORM 891-HALF-ZEN2-SEC
           MOVE    WRK-ATO-INPUT       TO  WRK-NAME
                                           ACCPTRES-NAME
           IF      FLG-ERR             =   1
               MOVE    "25"                TO  WRK-ERRCD
           END-IF
           .
       20011-KNJANAME-CHK-EXT.
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
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクタコードチェック
      *****************************************************************
       20011-DRCD-CHK-SEC          SECTION.
      *
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-DRCD (1:1)  NOT =   "1"
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容チェック
      *****************************************************************
       20011-SRYNAIYO-CHK-SEC      SECTION.
      *
      *    診療内容情報の存在チェック
           MOVE    SPACE               TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    WRK-SRYNAIYO        TO  SYS-1012-KBNCD
           IF      WRK-SRYNAIYO        =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               IF     (WRK-SRYNAIYO        =   SPACE )
      *            設定がなければ１件目を対象とする
                   ADD     1                   TO  IDK
                   MOVE    "K3"                TO  WRK-KEICD (IDK)
               END-IF
               MOVE    SYS-1012-KBNCD(1:2)     TO  WRK-SRYNAIYO
               MOVE    SYS-1012-SRYNAIYODSP    TO  WRK-SRYNAIYO-NAME
           ELSE
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-SRYNAIYO        =   SPACE
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付前チェックおよび受付登録
      *****************************************************************
       2002-UKE-ADD-SEC        SECTION.
      *
      *    患者番号なしの受付
           IF      WRK-PTNUM           =   SPACE
               PERFORM 20020-UKE-NAME-ADD-SEC
               GO      TO              2002-UKE-ADD-EXT
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20021-ACCPTRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2002-UKE-ADD-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
      *    出力領域保険情報編集
           PERFORM 20023-HKNHENSYU-SEC
      *
      *    受付登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-UKEADD-SYORI-SEC
           END-IF
           .
       2002-UKE-ADD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-ACCPTRES-HEN-SEC     SECTION.
      *
      *    予約日・予約時間編集
           MOVE    WRK-REGIST-YMD      TO  WRK-SYMD
           MOVE    WRK-REGIST-TIME     TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACCPTRES-REGIST-DATE
           MOVE    WRK-HEN-TIME        TO  ACCPTRES-REGIST-TIME
      *
           MOVE    WRK-SRYKA-NAME      TO  ACCPTRES-REGIST-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  ACCPTRES-REGIST-DR-NAME
      *
           IF      SPA-PTID            =   ZERO
               GO      TO      20021-ACCPTRES-HEN-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  ACCPTRES-NAME
      *    患者氏名
           MOVE    PTINF-NAME          TO  WRK-NAME
      *
           MOVE    PTINF-KANANAME      TO  ACCPTRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACCPTRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  ACCPTRES-SEX
           MOVE    PTINF-HOME-POST     TO  ACCPTRES-HOME-ZIP-CODE
           STRING  PTINF-HOME-ADRS
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    ACCPTRES-HOME-ADDRESSES
           END-STRING
      *
           .
       20021-ACCPTRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC     SECTION.
      *
           IF      ACCPTREQ-COMBINATION-NUMBER   =   SPACE
               PERFORM 200231-HKNCOMBI-HEN-SEC
           ELSE
      *        保険組合せ番号あり
               PERFORM 200232-COMBINATION-HEN-SEC
           END-IF
      *
           .
       20023-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200231-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
           IF      FLG-SYORI           =   1
                                       OR  3
      *        追加処理
               MOVE    "1"                 TO  ORCSAPIHKN-KBN
           ELSE
      *        削除処理
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
           END-IF
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険種類
           MOVE    ACCPTREQ-MAIN-INSURANCE-CLASS 
                                       TO  ORCSAPIHKN-IN-HKNNUM
      *    保険者番号
           MOVE    ACCPTREQ-MAIN-INSURANCE-NUMBER
                                       TO  ORCSAPIHKN-IN-HKNJANUM
      *    保険種類
           MOVE    ACCPTREQ-MAIN-INSURANCE-NAME 
                                       TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *    記号
           MOVE    ACCPTREQ-MAIN-MARK  TO  ORCSAPIHKN-IN-MARK
      *    番号
           MOVE    ACCPTREQ-MAIN-NUMBER
                                       TO  ORCSAPIHKN-IN-NUMBER
      *    有効期間
           MOVE    ACCPTREQ-MAIN-START-DATE
                                       TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKSTYMD
           MOVE    ACCPTREQ-MAIN-END-DATE
                                       TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKEDYMD
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    ACCPTREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *        公費名称
               MOVE    ACCPTREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *        負担者番号
               MOVE    ACCPTREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *        受給者番号
               MOVE    ACCPTREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
           END-PERFORM
      *
           MOVE    "1"                 TO  ORCSAPIHKN-KBN3
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               MOVE    ZERO                    TO  WRK-HKNCOMBI
               MOVE    ZERO                    TO  WRK-IN-HKNCOMBI
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-IN-HKNCOMBI
           END-IF
           EVALUATE    ORCSAPIHKN-ERRCD
               WHEN    1
                   MOVE    "21"                TO  WRK-ERRCD
               WHEN    2
                   MOVE    "22"                TO  WRK-ERRCD
               WHEN    3
                   MOVE    "23"                TO  WRK-ERRCD
           END-EVALUATE
           .
       200231-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ番号から保険組合せ編集処理
      *****************************************************************
       200232-COMBINATION-HEN-SEC          SECTION.
      *
      *    保険組合せチェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    ACCPTREQ-COMBINATION-NUMBER TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
      *        数値エラーはゼロ（送信なし）
               MOVE    ACCPTREQ-COMBINATION-NUMBER
                                           TO  WRK-HKNCOMBI
               MOVE    "24"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
      *    保険組合せ編集処理
           IF      WRK-ERRCD           =   SPACE
               INITIALIZE                      ORCSAPIHKNAREA
               IF      FLG-SYORI           =   1
                                           OR  3
      *            追加処理
                   MOVE    "1"                 TO  ORCSAPIHKN-KBN
               ELSE
      *            削除処理
                   MOVE    "2"                 TO  ORCSAPIHKN-KBN
               END-IF
               MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
               MOVE    "1"                 TO  ORCSAPIHKN-KBN3
               CALL    "ORCSAPIHKN"        USING
                                           SPA-AREA
                                           ORCSAPIHKNAREA
               IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
                   MOVE    ZERO                    TO  WRK-IN-HKNCOMBI
               ELSE
                   MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
                   MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-IN-HKNCOMBI
               END-IF
      *        保険番号対象なし
               IF      ORCSAPIHKN-ERRCD        =   4
                   MOVE    "24"                TO  WRK-ERRCD
                   MOVE    ZERO                    TO  WRK-HKNCOMBI
                   MOVE    ZERO                    TO  WRK-IN-HKNCOMBI
               END-IF
           END-IF
           .
       200232-COMBINATION-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       20023-HKNHENSYU-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDY
           IF      WRK-HKNCOMBI    NOT =   ZERO
      *        対象の保険組合せを１件目
               PERFORM VARYING     IDX     FROM    1   BY  1
      *!!              UNTIL      (IDX     >   20  )
                       UNTIL      (IDX     >   200 )
                             OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
                   IF      ORCSAPIHKN-THKNCOMBI (IDX)  =   WRK-HKNCOMBI
                       PERFORM 200231-HKNHEN-TBL-SEC
                       MOVE    200                 TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *    残りを編集
           PERFORM VARYING     IDX     FROM    1   BY  1
      *H29.4       UNTIL      (IDX     >   20   )
                   UNTIL      (IDX     >   200  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                       =   SPACE  )
                         OR   (IDY     >=  30     )
               IF     (WRK-HKNCOMBI        NOT =   ZERO  )  AND
                      (ORCSAPIHKN-THKNCOMBI (IDX)  =   WRK-HKNCOMBI )
                   CONTINUE
               ELSE
                   PERFORM 200231-HKNHEN-TBL-SEC
               END-IF
           END-PERFORM
           .
       20023-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       200231-HKNHEN-TBL-SEC              SECTION.
      *
           ADD     1                   TO  IDY
      *H27.12
      *    保険組合せ番号
           MOVE    ORCSAPIHKN-THKNCOMBI (IDX)
                                       TO  ACCPTRES-COMBINATION-NUMBER
                                                                 (IDY)
      *H29.4
      *    保険組合せ非表示
           EVALUATE    ORCSAPIHKN-HYOJIKBN (IDX)
               WHEN    "2"
      *            入院非表示
                   MOVE    "I"         TO  ACCPTRES-COMBINATION-HYOJI
                                                            (IDY)
               WHEN    "3"
      *            外来非表示
                   MOVE    "O"         TO  ACCPTRES-COMBINATION-HYOJI
                                                            (IDY)
               WHEN    OTHER
      *            非表示なし
                   MOVE    "N"         TO  ACCPTRES-COMBINATION-HYOJI
                                                            (IDY)
           END-EVALUATE
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  ACCPTRES-MAIN-INSURANCE-CLASS
                                                                 (IDY)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  ACCPTRES-MAIN-INSURANCE-NUMBER
                                                                 (IDY)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  ACCPTRES-MAIN-INSURANCE-NAME
                                                                 (IDY)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  ACCPTRES-MAIN-MARK       (IDY)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  ACCPTRES-MAIN-NUMBER     (IDY)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  ACCPTRES-MAIN-CONTINUATION(IDY)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  ACCPTRES-MAIN-ASSISTANCE (IDY)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  ACCPTRES-MAIN-FAMILY     (IDY)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  ACCPTRES-MAIN-POLICYHOLDER(IDY)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  ACCPTRES-MAIN-START-DATE  (IDY)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  ACCPTRES-MAIN-END-DATE  (IDY)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  ACCPTRES-INSURANCE-CLASS
                                                             (IDY IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  ACCPTRES-INSURANCE-NAME
                                                             (IDY IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  ACCPTRES-PROVIDER-NUMBER
                                                             (IDY IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  ACCPTRES-RECIPIENT-NUMBER
                                                             (IDY IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  ACCPTRES-INSURANCE-START-DATE
                                                             (IDY IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  ACCPTRES-INSURANCE-END-DATE
                                                             (IDY IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  ACCPTRES-RATE-ADMISSION
                                                             (IDY IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  ACCPTRES-MONEY-ADMISSION
                                                             (IDY IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  ACCPTRES-RATE-OUTPATIENT
                                                             (IDY IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  ACCPTRES-MONEY-OUTPATIENT
                                                             (IDY IDY2)
           END-PERFORM
      *
           .
       20023-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付追加処理
      *****************************************************************
       20022-UKEADD-SYORI-SEC              SECTION.
      *
      *    受付２重登録疑いチェック
           PERFORM 200221-UKECHEK-SEC
           IF      FLG-UKETUKE-CHK     =   1
               MOVE    "16"                TO  WRK-ERRCD
               GO  TO  20022-UKEADD-SYORI-EXT
           END-IF
      *
      *    予約マスタの検索
           PERFORM 200222-YYKCHK-SEC
      *
      *    受付登録処理
           PERFORM 200224-UKETUKE-INS-SEC
      *
      *    予約来院済み
           IF      FLG-YYKHIT          =   1
               PERFORM 20023-YYK-UPDATE-SEC
           END-IF
      *!!
           DISPLAY "受付 登録 OK"
      *!!
      *    CLAIM送信チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    1                   TO  FLG-USERPG
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           .
       20022-UKEADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付登録処理
      *****************************************************************
       200224-UKETUKE-INS-SEC            SECTION.
      *
      *    受付ＩＤ＝００検索(制御レコード)
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    ZERO                TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               ADD     1                   TO  UKE-PTID
               IF      UKE-PTID            >   99999
                   MOVE    "50"                TO  WRK-ERRCD
                   GO  TO  200224-UKETUKE-INS-EXT
               END-IF
               MOVE    UKE-PTID            TO  WRK-UKEID
      *
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
               MOVE    WRK-OPID            TO  UKE-OPID
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    ZERO                TO  UKE-UKEID
               MOVE    1                   TO  UKE-PTID
               MOVE    1                   TO  WRK-UKEID
      *        作成年月日
               MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    WRK-OPID            TO  UKE-OPID
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "51"                TO  WRK-ERRCD
               DISPLAY "UKETUKE KEY INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
               GO  TO  200224-UKETUKE-INS-EXT
           END-IF
      *
      *    受付ＩＤを出力項目へ
           MOVE    WRK-UKEID           TO  ACCPTRES-REGIST-ID
      *    診療内容
           MOVE    WRK-SRYNAIYO        TO  ACCPTRES-REGIST-DEP-INFO
      *
      *    受付ＩＤ登録チェック
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    WRK-UKEID           TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               MOVE    "52"                TO  WRK-ERRCD
               DISPLAY "UKETUKE INS ERR KEY:"  UKE-KEY
               GO  TO  200224-UKETUKE-INS-EXT
           END-IF
      *
      *    明細レコード追加
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    WRK-UKEID           TO  UKE-UKEID
           MOVE    WRK-REGIST-TIME     TO  UKE-UKETIME
           MOVE    SPA-PTID            TO  UKE-PTID
      **** MOVE    PTINF-NAME          TO  UKE-NAME
           MOVE    WRK-NAME            TO  UKE-NAME
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
           MOVE    WRK-DRCD            TO  UKE-DRCD
           MOVE    WRK-SRYNAIYO        TO  UKE-SRYNAIYO
           MOVE    WRK-HKNCOMBI        TO  UKE-HKNCOMBI
      *    予約情報
           IF      FLG-YYKHIT          =   1
               MOVE    WRK-YYK-YYKTIME     TO  UKE-YYKTIME
               MOVE    WRK-YYK-KEYYYKTIME  TO  UKE-KEYYYKTIME
               MOVE    WRK-YYK-YYKID       TO  UKE-YYKID
           END-IF
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
           MOVE    WRK-OPID            TO  UKE-OPID
      *
      *    受付登録
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "52"                TO  WRK-ERRCD
               DISPLAY "U02 UKETUKE INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
               GO  TO  200224-UKETUKE-INS-EXT
           END-IF
           .
       200224-UKETUKE-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付二重登録チェック処理
      *****************************************************************
       200221-UKECHEK-SEC            SECTION.
      * 
      *    受付マスタ読み込み (key5)
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-UKETUKE
               INITIALIZE                  UKETUKE-REC
           END-IF
           MOVE    ZERO                TO  FLG-UKETUKE-CHK
           PERFORM
                   UNTIL     ( FLG-UKETUKE     =   1  )
                          OR ( FLG-UKETUKE-CHK =   1  )
      *        受付中
               IF     (UKE-UKEID           >   ZERO    )   AND
                      (UKE-KAIKEITIME      =   ZERO    )
                   IF      (WRK-HKNCOMBI       =   UKE-HKNCOMBI)
                       MOVE    1               TO  FLG-UKETUKE-CHK
                   END-IF
               END-IF
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200221-UKECHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタ存在チェック処理
      *****************************************************************
       200222-YYKCHK-SEC            SECTION.
      * 
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           MOVE    SPA-PTID            TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1                  TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  FLG-YYK-OK
           MOVE    ZERO                TO  FLG-YYK-CHK
           MOVE    SPACE               TO  WRK-YYK-REC
           PERFORM UNTIL      (FLG-YYK     =   1    )
                          OR  (FLG-YYK-OK  NOT =   ZERO )
               IF     (YYK-YYKID       NOT =   ZERO  )
                  AND (YYK-RAIINZUMI       =   SPACE )
                   IF      YYK-SRYKA           =   WRK-SRYKA
                       PERFORM 2002221-YYK-CK-SEC
                   END-IF
               END-IF
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約あり
           IF     (FLG-YYK-OK          =   1    )
              OR  (FLG-YYK-CHK     NOT =   ZERO )
               MOVE    1               TO  FLG-YYKHIT
           END-IF
      *    診療内容区分が設定になく予約がある時、予約の診療区分とする
           IF     (ACCPTREQ-REGIST-DEP-INFO    =   SPACE )
             AND  (WRK-SRYNAIYO        NOT =   WRK-YYK-SRYNAIYO)
             AND  (FLG-YYKHIT              =   1         )
               MOVE    WRK-YYK-SRYNAIYO    TO  WRK-SRYNAIYO
           END-IF
           .
       200222-YYKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約更新処理
      *****************************************************************
       20023-YYK-UPDATE-SEC           SECTION.
      *
      *    予約来院済み更新
           MOVE    WRK-YYK-REC     TO  YYK-REC
           MOVE    "1"             TO  YYK-RAIINZUMI
           MOVE    YYK-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "53"            TO  WRK-ERRCD
               DISPLAY "YYK UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
           END-IF
           .
       20023-YYK-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約チェック処理
      *****************************************************************
       2002221-YYK-CK-SEC            SECTION.
      *
      *    ドクター・診療内容が一致すれば対象とする
           IF     (WRK-DRCD            =   YYK-DRCD )
             AND  (WRK-SRYNAIYO        =   YYK-SRYNAIYO)
               MOVE    1                   TO  FLG-YYK-OK
               MOVE    YYK-REC             TO  WRK-YYK-REC
           END-IF
      *    ドクターのみ一致
           IF     (WRK-DRCD            =   YYK-DRCD )
             AND  (WRK-SRYNAIYO   NOT  =   YYK-SRYNAIYO)
               MOVE    1                   TO  FLG-YYK-CHK
               MOVE    YYK-REC             TO  WRK-YYK-REC
           END-IF
      *    診療内容のみ一致
           IF     (WRK-DRCD       NOT  =   YYK-DRCD )
             AND  (WRK-SRYNAIYO        =   YYK-SRYNAIYO)
               MOVE    2                   TO  FLG-YYK-CHK
               MOVE    YYK-REC             TO  WRK-YYK-REC
           END-IF
      *
           .
       2002221-YYK-CK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号なしの受付処理
      *****************************************************************
       20020-UKE-NAME-ADD-SEC     SECTION.
      *
      *    予約日・予約時間編集
           MOVE    WRK-REGIST-YMD      TO  WRK-SYMD
           MOVE    WRK-REGIST-TIME     TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACCPTRES-REGIST-DATE
           MOVE    WRK-HEN-TIME        TO  ACCPTRES-REGIST-TIME
      *
           MOVE    WRK-SRYKA-NAME      TO  ACCPTRES-REGIST-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  ACCPTRES-REGIST-DR-NAME
      *    患者氏名
           MOVE    WRK-NAME            TO  ACCPTRES-NAME
      *
      *    受付登録処理
           PERFORM 200224-UKETUKE-INS-SEC
           .
       20020-UKE-NAME-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付削除
      *****************************************************************
       2003-UKE-DELETE-SEC     SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-ACCPTRES-HEN-SEC
      *
      *    受付ＩＤ編集
           IF      WRK-REGIST-ID   NOT =   SPACE
               PERFORM 20031-UKEID-CHK-SEC
      *        患者番号送信時、一致すること
               IF     (WRK-PTNUM       NOT =   SPACE )
                 AND  (UKE-PTID        NOT =   PTINF-PTID )
                 AND  (WRK-ERRCD           =   SPACE  )
      *            患者ＩＤ不一致
                   MOVE    "20"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO              2003-UKE-DELETE-EXT
           END-IF
      *    患者番号なし
           IF     (WRK-REGIST-ID   NOT =   SPACE )
              AND (UKE-PTID            =   ZERO  )
               PERFORM 20030-UKE-NAME-DEL-SEC
               GO      TO              2003-UKE-DELETE-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2003-UKE-DELETE-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *    受付削除処理
               IF      WRK-REGIST-ID       =   SPACE OR  ZERO
      *        受付ＩＤなし
                   PERFORM 20032-SAKUJO-NASI-SEC
               ELSE
      *        受付ＩＤあり
                   PERFORM 20031-SAKUJO-ARI-SEC
               END-IF
           END-IF
           .
       2003-UKE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付ＩＤチェック処理
      *****************************************************************
       20031-UKEID-CHK-SEC             SECTION.
      *
      *    数値変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-REGIST-ID       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   5     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "19"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(11:5)   TO  WRK-REGIST-ID
           END-IF
           IF     (WRK-ERRCD       NOT =   SPACE )
               OR (WRK-REGIST-ID       =   ZERO )
               GO  TO  20031-UKEID-CHK-EXT
           END-IF
      *
      *    受付マスタ検索
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    WRK-REGIST-ID       TO  UKE-UKEID
      *
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE     NOT =   ZERO
      *        受付なし
               MOVE    "17"                TO  WRK-ERRCD
           END-IF
           .
       20031-UKEID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               GO      TO      4905-CLAIM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  FLG-CLAIM-MULTIHOST
               END-IF
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
                   IF      SYS-9000-UKEBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
      *    送信時ポップアップなしの時のみ送信
           IF      FLG-CLAIM           =   1
      *        ＣＬＡＩＭ接続情報取得処理
               PERFORM 4906-CLAIM-SOUSIN-SEC
           END-IF
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      FLG-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
                   IF      WRK-GMN-SELNUM        =   1
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       49061-CLAIM-SOUSINSYOSI-SEC               SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
           MOVE    SYS-9000-CODE       TO  SHELLTBL-ARG1
           MOVE    PTINF-PTID          TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    UKE-SRYKA           TO  SHELLTBL-ARG3
           MOVE    UKE-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
           MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
           MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *    ドクター
           IF      UKE-DRCD            =   SPACE
               MOVE    "@@@@@"             TO  SHELLTBL-ARG8
           ELSE
               MOVE    UKE-DRCD            TO  SHELLTBL-ARG8
           END-IF
      *    保険組合せ
           IF      UKE-HKNCOMBI        =   ZERO
               MOVE    "@@@@"              TO  SHELLTBL-ARG9
           ELSE
               MOVE    UKE-HKNCOMBI    TO  SHELLTBL-ARG9
           END-IF
      *    診療内容（名称編集）
           MOVE    UKE-SRYNAIYO        TO  WRK-SRYNAIYO
           PERFORM 20011-SRYNAIYO-CHK-SEC
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-SRYNAIYO-NAME   TO  SHELLTBL-ARG10
           ELSE
               MOVE    UKE-SRYNAIYO        TO  SHELLTBL-ARG10
           END-IF
      *    連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *    システム日付
           MOVE    SPA-SYSYMD          TO   SHELLTBL-ARG12
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           DISPLAY "SHELLTBL-ARG1 = " SHELLTBL-ARG1
           DISPLAY "SHELLTBL-ARG6 = " SHELLTBL-ARG6
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC                 SECTION.
      *
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        GO      TO      4907-USERPG-EXT
      **   END-IF
      *
           EVALUATE    FLG-USERPG
               WHEN    1
      *        追加
                   MOVE    1               TO  WRK-SYORI-FLG
               WHEN    2
      *        更新
                   MOVE    2               TO  WRK-SYORI-FLG
               WHEN    3
      *        削除
                   MOVE    3               TO  WRK-SYORI-FLG
           END-EVALUATE
      *
      *    ユーザプログラム起動処理
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "U02"               TO  ORCSUSERCHK-GMNPG
           MOVE    "2"                 TO  ORCSUSERCHK-SYORI
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SRYYMD
           MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
           MOVE    WRK-PTNUM           TO  ORCSUSERCHK-PTNUM
           MOVE    UKE-SRYKA           TO  ORCSUSERCHK-SRYKA
           MOVE    UKE-HKNCOMBI        TO  ORCSUSERCHK-HKNCOMBI
           MOVE    UKE-DRCD            TO  ORCSUSERCHK-DRCD
           MOVE    WRK-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付取消処理
      *****************************************************************
       20032-SAKUJO-NASI-SEC             SECTION.
      *
           DISPLAY "受付削除(ID なし )  " WRK-REGIST-ID
      *
      *    診療科は必須
           IF      WRK-SRYKA           =   SPACE
               MOVE    "02"                TO  WRK-ERRCD
               GO      TO      20032-SAKUJO-NASI-EXT
           END-IF
      *    受付マスタ削除(key5 で対象を削除)
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-UKETUKE
               INITIALIZE                  UKETUKE-REC
           END-IF
           MOVE    ZERO                TO  CNT-UKEDELETE
           INITIALIZE                  WRK-UKETUKE-REC
           PERFORM
                   UNTIL     ( FLG-UKETUKE     =   1  )
                          OR ( FLG-UKETUKE-CHK =   1  )
      *        受付中
               IF     (UKE-UKEID           >   ZERO    )   AND
                      (UKE-KAIKEITIME      =   ZERO    )
                   IF      (WRK-IN-HKNCOMBI    NOT =   ZERO        )
                     AND   (WRK-IN-HKNCOMBI        =   UKE-HKNCOMBI)
      *                受付削除（保険組合せ一致分のみ）
                       PERFORM 200321-UKETUKE-DEL-SEC
                   END-IF
                   IF      (WRK-IN-HKNCOMBI        =   ZERO      )
      *                受付削除（診療科一致）
                       PERFORM 200321-UKETUKE-DEL-SEC
                   END-IF
               END-IF
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      CNT-UKEDELETE       =   0
      *        受付削除なし
               MOVE    "17"                TO  WRK-ERRCD
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           IF      WRK-ERRCD           =   SPACE
               MOVE    3                   TO  FLG-USERPG
               PERFORM 4907-USERPG-SEC
           END-IF
           .
       20032-SAKUJO-NASI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付取消処理
      *****************************************************************
       200321-UKETUKE-DEL-SEC             SECTION.
      *
           ADD     1                   TO  CNT-UKEDELETE
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "54"                TO  WRK-ERRCD
               DISPLAY "UKETUKE DELETE ERR:" MCP-RC
                      ",KEY:"  UKE-KEY
           END-IF
      *
      *    削除内容編集
           IF      CNT-UKEDELETE       =   1
               PERFORM 2003211-DELETE-HEN-SEC
           END-IF
      *
      *    予約レコードの更新
           IF     (UKE-YYKTIME     NOT =   ZERO   )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 200321-DEL-YYKUPD-SEC
           END-IF
           .
       200321-UKETUKE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付取消内容編集処理
      *****************************************************************
       2003211-DELETE-HEN-SEC             SECTION.
      *
      *    受付ＩＤ編集
           MOVE    UKE-UKEID           TO  ACCPTRES-REGIST-ID
      *    受付時間
           MOVE    UKE-UKEYMD          TO  WRK-SYMD
           MOVE    UKE-UKETIME         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-TIME        TO  ACCPTRES-REGIST-TIME
      *    診療科
           MOVE    UKE-SRYKA           TO  WRK-SRYKA
           PERFORM 20011-SRYKA-CHK-SEC
           MOVE    WRK-SRYKA           TO  ACCPTRES-REGIST-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  ACCPTRES-REGIST-DEP-NAME
      *    ドクター
           MOVE    UKE-DRCD            TO  WRK-DRCD
           PERFORM 20011-DRCD-CHK-SEC
           MOVE    WRK-DRCD            TO  ACCPTRES-REGIST-DR-CODE
           MOVE    WRK-DRCD-NAME       TO  ACCPTRES-REGIST-DR-NAME
      *    診療内容
           MOVE    UKE-SRYNAIYO        TO  ACCPTRES-REGIST-DEP-INFO
      *    保険組合せ編集
           IF      UKE-PTID            =   ZERO
               CONTINUE
           ELSE
               MOVE    UKE-HKNCOMBI       TO  WRK-HKNCOMBI
               PERFORM 20023-HKNHENSYU-SEC
           END-IF
      *    患者氏名
           IF      UKE-PTID            =   ZERO
               INITIALIZE                  ACCPTRES-PATIENT-INFOMATION
               MOVE    UKE-NAME            TO  ACCPTRES-NAME
           END-IF
           .
       2003211-DELETE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付取消処理（受付ＩＤあり）
      *****************************************************************
       20031-SAKUJO-ARI-SEC             SECTION.
      *
           DISPLAY "受付削除(ID あり )  " WRK-REGIST-ID
      *
      *    INITIALIZE                      UKETUKE-REC
      *    MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
      *    MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
      *    MOVE    WRK-REGIST-ID       TO  UKE-UKEID
      *
      *    PERFORM 900-UKETUKE-READ-SEC
      *    IF      FLG-UKETUKE         =   ZERO
      *        IF      UKE-PTID            =   PTINF-PTID
      *            受付削除
      *            PERFORM 200321-UKETUKE-DEL-SEC
      *        ELSE
      *            患者ＩＤ不一致
      *            MOVE    "20"                TO  WRK-ERRCD
      *        END-IF
      *    ELSE
      ***      MOVE    "17"                TO  WRK-ERRCD
      **** END-IF
           IF      UKE-PTID            =   PTINF-PTID
      *        受付削除
               PERFORM 200321-UKETUKE-DEL-SEC
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           IF      WRK-ERRCD           =   SPACE
               MOVE    3                   TO  FLG-USERPG
               PERFORM 4907-USERPG-SEC
           END-IF
           .
       20031-SAKUJO-ARI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号なし受付削除処理
      *****************************************************************
       20030-UKE-NAME-DEL-SEC            SECTION.
      *
      *    受付ＩＤ必須
           IF      WRK-REGIST-ID       =   SPACE
                                       OR  ZERO
               MOVE    "26"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        受付の患者番号がないこと
               IF     (FLG-UKETUKE         =   ZERO )
                 AND  (UKE-PTID            =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "27"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    受付時間が一致すること
           IF      WRK-ERRCD           =   SPACE
               IF      UKE-UKETIME     NOT =   WRK-REGIST-TIME
                   MOVE    "29"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSAPIHKNAREA
      *
           IF      WRK-ERRCD           =   SPACE
      *        受付削除
               PERFORM 200321-UKETUKE-DEL-SEC
           END-IF
      *
           .
       20030-UKE-NAME-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタの更新(受付取消時)
      *****************************************************************
       200321-DEL-YYKUPD-SEC            SECTION.
      *
      *    予約マスタ読み込み 
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    UKE-UKEYMD          TO  YYK-YYKYMD
           MOVE    UKE-PTID            TO  YYK-PTID
           MOVE    UKE-DRCD            TO  YYK-DRCD
           MOVE    UKE-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    UKE-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    UKE-YYKID           TO  YYK-YYKID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1                  TO  FLG-YYK
           END-IF
           IF      FLG-YYK             =   ZERO
      *        来院済み解除
               MOVE    SPACE               TO  YYK-RAIINZUMI
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "53"                TO  WRK-ERRCD
                   DISPLAY "YYK UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               END-IF
           END-IF
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200321-DEL-YYKUPD-EXT.
           EXIT.
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       2004-UKE-UPDATE-SEC                SECTION.
      *
      *    患者番号なし
           IF     (WRK-PTNUM           =   SPACE )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           IF     (WRK-REGIST-ID       =   SPACE
                                       OR  ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "26"                TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2004-UKE-UPDATE-EXT
           END-IF
      *
      *    受付データチェック
           PERFORM 20031-UKEID-CHK-SEC
           IF      FLG-UKETUKE         =   ZERO
      *        受付の患者番号がないこと
               IF      UKE-PTID        NOT =   ZERO
                   MOVE    "27"            TO  WRK-ERRCD
               END-IF
      *        受付時間が一致すること
               IF      WRK-ERRCD           =   SPACE
                   IF      UKE-UKETIME     NOT =   WRK-REGIST-TIME
                       MOVE    "29"            TO  WRK-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    "28"                TO  WRK-ERRCD
           END-IF
      *    患者情報内容編集
           PERFORM 20021-ACCPTRES-HEN-SEC
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2004-UKE-UPDATE-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2004-UKE-UPDATE-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
      *    出力領域保険情報編集
           PERFORM 20023-HKNHENSYU-SEC
      *
      *    受付２重登録疑いチェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200221-UKECHEK-SEC
               IF      FLG-UKETUKE-CHK     =   1
                   MOVE    "16"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    受付登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20042-UKEUPD-SYORI-SEC
           END-IF
           .
       2004-UKE-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       20042-UKEUPD-SYORI-SEC             SECTION.
      *
      *    受付検索
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    WRK-REGIST-ID       TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE     NOT =   ZERO
               MOVE    "28"                TO  WRK-ERRCD
               GO  TO  20042-UKEUPD-SYORI-EXT
           END-IF
      *    受付の患者番号がないこと
           IF      UKE-PTID        NOT =   ZERO
               MOVE    "27"            TO  WRK-ERRCD
               GO  TO  20042-UKEUPD-SYORI-EXT
           END-IF
      *
      *    明細レコード更新
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    PTINF-NAME          TO  UKE-NAME
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
           MOVE    WRK-DRCD            TO  UKE-DRCD
           MOVE    WRK-SRYNAIYO        TO  UKE-SRYNAIYO
           MOVE    WRK-HKNCOMBI        TO  UKE-HKNCOMBI
      *
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
           MOVE    WRK-OPID            TO  UKE-OPID
      *
      *    受付登録
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "51"                TO  WRK-ERRCD
               DISPLAY "UKETUKE UPDATE ERR:" MCP-RC
                      ",KEY:"  UKE-KEY
           END-IF
      *
           .
       20042-UKEUPD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       891-HALF-ZEN2-SEC         SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               PERFORM 8910-HALF-ZENC2-SEC
           ELSE
      *        拡張漢字不可
               PERFORM 890-HALF-ZEN-SEC
           END-IF
      *
           .
       891-HALF-ZEN2-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       8910-HALF-ZENC2-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-MAE-INPUT
           IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU    =   1     )
      *ver.5.1 から　改行エラー
               OR (KANACHK2-RC5    NOT =   ZERO  ) 
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-ATO-INPUT
           END-IF
      *
           .
       8910-HALF-ZENC2-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      ***  MOVE    ZERO                TO  FLG-ERR2
      *    拡張漢字有無
      *    INITIALIZE                  ORCSSTRINGAREA
      *    MOVE    "check"             TO  ORCSSTR-COMMAND
      *    MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
      *    CALL    "orcsstring"        USING
      *                                ORCSSTRINGAREA
      *    IF      ORCSSTR-NUM2    NOT =   ZERO
      *        MOVE    1                   TO  FLG-ERR2
      **** END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *ver.5.1 から　改行エラー
               OR (KANACHK-RC4     NOT =   ZERO  ) 
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
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
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-DATE
           END-IF
           IF      WRK-THMS            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-TIME
           END-IF
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
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "ドクターが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "受付日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "受付時間設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "診療内容情報が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               STRING  "診療科・保険組合せで受付登録済みです。"
                       "二重登録疑い"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               MOVE    "削除対象の受付レコードが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               MOVE    "受付ＩＤ設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "受付ＩＤの受付患者番号と患者番号が一致しません"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "保険の一致する患者保険情報がありません"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "公費の一致する患者公費情報がありません"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "保険情報と一致する保険組合せがありません"
                                               TO  WRK-ERRMSG
      *!!
           WHEN    "24"
               MOVE    "保険組合せ番号がありません"
                                               TO  WRK-ERRMSG
      *H29.10
           WHEN    "25"
               MOVE    "患者氏名にエラーがあります"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "受付ＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "27"
               MOVE    "該当の受付は患者番号の設定があります。"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "該当の受付と受付時間が一致しません。"
                                               TO  WRK-ERRMSG
           WHEN    "28"
               MOVE    "更新対象の受付レコードが存在しません"
                                               TO  WRK-ERRMSG
      *
           WHEN    "50"
               STRING  "受付登録件数が上限以上となります。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "51"
               MOVE    "受付更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "受付登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "53"
               MOVE    "予約更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "54"
               MOVE    "受付削除エラー"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "受付日を自動設定しました"
                                               TO  WRK-ERRMSG
           WHEN    "K2"
               MOVE    "受付時間を自動設定しました"
                                               TO  WRK-ERRMSG
           WHEN    "K3"
               MOVE    "診療内容情報を自動設定しました"
                                               TO  WRK-ERRMSG
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
               PERFORM 920-DBFETCH-SEC
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
      *
      *****************************************************************
      *    受付マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
                MOVE    1              TO  FLG-UKETUKE
           END-IF
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-UKETUKE-READ-EXT.
           EXIT.
      *****************************************************************
      *    受付マスター読込処理
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       910-YYK-READ-SEC            SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       910-YYK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           DISPLAY "mcp-window = "    MCP-WINDOW 
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "U02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "排他エラー = " PTINF-PTID
               DISPLAY "TERMID     = " SPA-TERMID
               DISPLAY "RC         = " SLOCK-RETURN
           END-IF
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
      *    MOVE    "U02"               TO  SPA-MOTOPG
      *                                    MCP-WINDOW
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
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
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
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    No Commit処理
      *****************************************************************
       900-NOCOMMIT-SEC            SECTION.
      *
      * For Debug
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      * For Debug
      *
           .
       900-NOCOMMIT-EXT.
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

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
       PROGRAM-ID.         ORAPI021S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　登録　（xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/29    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/08/XX  病名補足コメント追加対応
      *  04.07.00    NACL-多々納  14/10/XX  詳細メッセージ追加等対応
      *  04.07.00    NACL-多々納  14/11/XX  保険組合せ設定対応
      *  04.07.00    NACL-多々納  14/11/XX  CLASS=4 外来追加対応他
      *  04.08.00    NACL-多々納  15/12/01  保険組合せ番号送信追加
      *  04.08.00    NACL-多々納  17/04/XX  内服１種類・継続コメント対応
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
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHKWK               PIC 9(01).
           03  FLG-DOUJITU             PIC 9(01).
      *
           03  FLG-WKSRYACT-INS        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDK                 PIC 9(04).
           03  IDM1                PIC 9(04).
           03  IDM2                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-BYOCNT              PIC 9(04).
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
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-IN-HKNCOMBI         PIC X(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WERRCD              PIC X(03).
           03  WRK-KEICD-G.
               05  WRK-KEICD           PIC X(03)   OCCURS   10.
      *
           03  WRK-ERRCD-1             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-2            PIC X(100).
           03  WRK-BYO-IDX             PIC 9(02).
      *
      *    ユーザプログラム
           03  WRK-PTID                PIC X(10).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-SYORI-FLG           PIC 9(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-MAX-ZAINUM          PIC 9(08).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
           03  WRK-START-DATE          PIC X(10).
           03  WRK-END-DATE            PIC X(10).
      *    入院日
           03  WRK-NYUIN-DATE          PIC X(10).
           03  WRK-NYUIN-YMD           PIC X(08).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    claim連携リターンエリア
      *01  WRK-ERROR-AREA.
      *    03  WRK-KBN-CODE            PIC X(01).
      *    03  WRK-KBN-CODE2           PIC X(01).
      *    03  WRK-KBN-V2              PIC X(01).
      *    03  WRK-ERR-CODE            PIC 9(02).
      *    03  WRK-ERR-MSG             PIC X(100).
      *    03  WRK-OUT-HKNCOMBI        PIC X(04).
      *    03  WRK-OUT-CNT1            PIC 9(04).
      *    03  WRK-OUT-CNT2            PIC 9(04).
      *!
      *H26.8
      *    claim連携サブリターンエリア
           COPY    "CPORCL0031.INC".
      *!
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *
      ***  COPY    "CPORCXKANACONV.INC".
      *
      ***  COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    ユーザプログラム起動チェックサブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV2RES.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
       01  LF04-REC.
           COPY    "CPRLF004.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *　　（ＣＬＡＩＭ接続）
      *    COPY    "CPSK9000.INC".
      *    COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "CPAPILSTV2.INC".
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
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
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
           INITIALIZE                      XML-MEDICALRES
      *
           INITIALIZE                  LNK-ORCL0031AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  MDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MDCRES-INFORMATION-TIME
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
      *
      *    MOVE    SPACE               TO  SPA-MOTOPG
      *    ＳＰＡ共通設定
      *    INITIALIZE                  SYS-1010-REC
      *    INITIALIZE                  ORCSCOMMONAREA
      *    MOVE    "M00"               TO  ORCSCOMMON-PG
      *    CALL    "ORCSCOMMON"        USING
      *                                MCPAREA
      *                                SPA-AREA
      *                                ORCSCOMMONAREA
      *                                SYS-1010-REC
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        MOVE   "89"             TO  WRK-ERRCD
      *    END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ共通設定処理
      *****************************************************************
       1001-ORCSCOMMON-SEC           SECTION.
      *
      *    診療日チェック
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
           END-IF
      *    診療日チェック
           PERFORM 20011-PERFORM-CHK-SEC
      *    診療日をシステム日付へ
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
           ELSE
      *        エラーは他項目と同時
               MOVE    SPACE               TO  WRK-ERRCD
           END-IF
      *
           MOVE    "API"               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  SPA-MOTOPG
           .
       1001-ORCSCOMMON-EXT.
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
      *    ＳＰＡ共通設定
           PERFORM 1001-ORCSCOMMON-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-WERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST-CLASS
           WHEN    "01"
      *        登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        削除
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        置換
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
      *        外来の追加
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    SPACE               TO  WRK-KEICD-G
      *TEST!!
      *        PERFORM 890-ERRCD-MSG-SEC
      *        DISPLAY "必須項目チェックエラー "    WRK-ERRCD
      *                " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APILST-CLASS
           WHEN    "01"
      *        登録処理
               PERFORM 2002-WKSRYACT-ADD-SEC
           WHEN    "02"
      *        削除処理
               PERFORM 2003-WKSRYACT-DEL-SEC
           WHEN    "03"
      *        置換処理（入院）
               PERFORM 2004-WKSRYACT-UPD-SEC
           WHEN    "04"
      *        外来追加処理
               PERFORM 2005-WKSRYACT-GAI-SEC
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
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalreq"        TO  APIREQ-RECORDNAME
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
           MOVE    SPACE               TO  APIREQ-PATIENTINFOREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalreq"        TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST-BODY
      *
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
      *    IF      WRK-ERRCD           =   SPACE
      *        IF      FLG-SYORI           =   1
      **           MOVE    WRK-KEICD           TO  WRK-ERRCD
      *        END-IF
      **** END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                       IF     (LNK-OUT-CNT1        =   ZERO )
                         AND  (LNK-OUT-CNT2        >   ZERO )
                           MOVE    "病名登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                       END-IF
                   WHEN    2
                       MOVE    "削除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "置換処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    4
                       MOVE    "登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
           END-IF
      *!!!!
      *    各エラー、ワーニング編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *!!!! 
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalres"        TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalres"        TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST-BODY 
               MOVE  "application/xml" TO  APILST-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    各エラー、ワーニング編集
      *****************************************************************
       3001-WAINGMSG-HEN-SEC        SECTION.
      *
      *    中途データ
           IF      WRK-ERRCD-1      NOT =   SPACE
               MOVE    WRK-ERRCD-1         TO  MDCRES-MEDI-RESULT
               MOVE    WRK-ERRMSG-1        TO  MDCRES-MEDI-RESULT-MSG
           END-IF
           MOVE    ZERO                TO  IDM1
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   10   )   OR
                              (WRK-KEICD (IDK) =   SPACE)
               MOVE    WRK-KEICD (IDK)     TO  WRK-WERRCD
      *        エラー・警告メッセージ
               PERFORM 891-WARNING-MSG-SEC
               ADD     1                   TO  IDM1
               MOVE    WRK-WERRCD          TO  MDCRES-MEDI-WARNING
                                                               (IDM1)
               MOVE    WRK-ERRMSG          TO  MDCRES-MEDI-WARNING-MSG
                                                               (IDM1)
           END-PERFORM
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   50  )   OR
                              (LNK-MEDI-CODE(IDK) =   SPACE)
                          OR  (IDM1    >   50  )
               MOVE    LNK-MEDI-CODE(IDK)  TO  WRK-WERRCD
               MOVE    LNK-MEDI-MSG (IDK)  TO  WRK-ERRMSG
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 891-WARNING-MSG-SEC
               END-IF
               ADD     1                   TO  IDM1
               MOVE    WRK-WERRCD          TO  MDCRES-MEDI-WARNING
                                                               (IDM1)
               MOVE    WRK-ERRMSG          TO  MDCRES-MEDI-WARNING-MSG
                                                               (IDM1)
               IF      LNK-MEDI-POSITION (IDK) NOT =   ZERO
                   MOVE    LNK-MEDI-POSITION (IDK)
                                           TO  MDCRES-MEDI-POSITION
                                                               (IDM1)
               END-IF
               IF      LNK-MEDI-ITEM  (IDK)    NOT =   ZERO
                   MOVE    LNK-MEDI-ITEM (IDK)
                                           TO  MDCRES-MEDI-ITEM-POSITION
                                                               (IDM1)
               END-IF
               MOVE    LNK-MEDI-SRYCD (IDK)
                                           TO  MDCRES-MEDI-CODE
                                                               (IDM1)
           END-PERFORM
      *
      *    病名
           IF      WRK-ERRCD-2      NOT =   SPACE
               MOVE    WRK-ERRCD-2         TO  MDCRES-DIAG-RESULT
               MOVE    WRK-ERRMSG-2        TO  MDCRES-DIAG-RESULT-MSG
           END-IF
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   50  )
                          OR  (LNK-DIAG-WARNING(IDK) =   SPACE)
                          OR  (IDM2    >   50  )
               MOVE    LNK-DIAG-WARNING (IDK)  TO  WRK-WERRCD
               MOVE    LNK-DIAG-MSG     (IDK)  TO  WRK-ERRMSG
               IF      WRK-ERRMSG          =   SPACE
      *        警告メッセージ
                   PERFORM 891-WARNING-MSG-SEC
               END-IF
               ADD     1                   TO  IDM2
               MOVE    WRK-WERRCD          TO  MDCRES-DIAG-WARNING
                                                               (IDM2)
               MOVE    WRK-ERRMSG          TO  MDCRES-DIAG-WARNING-MSG
                                                               (IDM2)
               IF      LNK-DIAG-POSITION  (IDK)    NOT =   ZERO
                   MOVE    LNK-DIAG-POSITION  (IDK)
                                           TO  MDCRES-DIAG-ITEM-POSITION
                                                               (IDM2)
               END-IF
               MOVE    LNK-DIAG-NAME (IDK)
                                           TO  MDCRES-DIAG-NAME (IDM2)
               MOVE    LNK-DIAG-CODE (IDK)
                                           TO  MDCRES-DIAG-CODE (IDM2)
               MOVE    LNK-DIAG-CHANGE (IDK)
                                           TO  MDCRES-DIAG-CHANGE
                                                                (IDM2)
           END-PERFORM
          .
       3001-WAINGMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      MDCREQ-OUTPATIENT-CLASS    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-OUTPATIENT-CLASS
           END-IF
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF     MDCREQ-PERFORM-TIME  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-TIME
           END-IF
           IF      MDCREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DEP-CODE
           END-IF
           IF      MDCREQ-APPOINT-DR-CODE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DR-CODE
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  MDCREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-MARK
           END-IF
           IF  MDCREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-NUMBER
           END-IF
           IF  MDCREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-CONTINUATION
           END-IF
           IF  MDCREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-ASSISTANCE
           END-IF
           IF  MDCREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-FAMILY
           END-IF
           IF  MDCREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-POLICYHOLDER
           END-IF
           IF  MDCREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-START-DATE
           END-IF
           IF  MDCREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      MDCREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  MDCREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      MDCREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      MDCREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      MDCREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   MDCREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  MDCREQ-INSURANCE-END-DATE
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
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
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
      *    入院日付チェック
           IF     (WRK-NYUIN-DATE  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-NYUINYMD-CHK-SEC
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
           MOVE    MDCREQ-PATIENTID        TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID        TO  WRK-PTNUM
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  MDCRES-APPOINT-DR-CODE
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
      *    診療日・時間
           MOVE    MDCREQ-PERFORM-DATE     TO  MDCRES-PERFORM-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  MDCRES-PERFORM-TIME
      *    数字のみに編集
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-PERFORM-DATE
               ADD     1                   TO  IDK
               MOVE    "W01"               TO  WRK-KEICD(IDK)
           END-IF
      *    診療日をシステム日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *    UID
           MOVE    MDCREQ-MEDICAL-UID  TO  MDCRES-MEDICAL-UID
      *    入外区分
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *    入院日（同日再入院判定）
           IF      MDCREQ-ADMISSION-DATE   =   SPACE
               MOVE    SPACE               TO  WRK-NYUIN-DATE
               MOVE    SPACE               TO  WRK-NYUIN-YMD
           ELSE
               MOVE    MDCREQ-ADMISSION-DATE   TO  WRK-HEN-DATE
               MOVE    SPACE                   TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-NYUIN-YMD
               MOVE    MDCREQ-ADMISSION-DATE   TO  WRK-NYUIN-DATE
               MOVE    MDCREQ-ADMISSION-DATE   TO  MDCRES-ADMISSION-DATE
           END-IF
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    ドクター
           IF      WRK-DRCD            =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-SYORI           =   1      )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    UID
           IF      MDCREQ-MEDICAL-UID  =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-SYORI           =   2   OR  3 )
                   MOVE    "04"                TO  WRK-ERRCD
               END-IF
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
               DISPLAY "Accept Ymd =  " WRK-PERFORM-YMD
           END-IF
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
           MOVE    SPTID-PTNUM         TO  MDCRES-PATIENTID
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
      *****************************************************************
      *    入院日付チェック
      *****************************************************************
       20011-NYUINYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-NYUIN-YMD       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "23"                TO  WRK-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  FLG-DOUJITU
           IF     (WRK-ERRCD           =   SPACE )
              AND (SPA-NYUGAIKBN       =   "1"   )
      *        入院履歴検索
               PERFORM 200111-NYUINRRK-HEN-SEC
               IF      FLG-PTNYUINRRK      =   1
                   MOVE    "24"                TO  WRK-ERRCD
               END-IF
      *        同日再入院区分
               IF     (PTNYUINRRK-NYUINYMD     =   WRK-PERFORM-YMD ) AND
                      (PTNYUINRRK-DOUJITSUKBN  =   "1"             )
                   MOVE    1                   TO  FLG-DOUJITU
               END-IF
           END-IF
           .
       20011-NYUINYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       200111-NYUINRRK-HEN-SEC        SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    WRK-NYUIN-YMD       TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
               ELSE
                   INITIALIZE                  PTNYUINRRK-REC
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200111-NYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ登録処理
      *****************************************************************
       2002-WKSRYACT-ADD-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      2002-WKSRYACT-ADD-EXT
           END-IF
      *
      *    診療行為登録処理
           PERFORM 20021-CLAIM-CALL-SEC
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2002-WKSRYACT-ADD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *    MOVE    PTINF-HOME-POST     TO  MDCRES-HOME-ZIP-CODE
      *    STRING  PTINF-HOME-ADRS
      *            PTINF-HOME-BANTI    DELIMITED  BY  SPACE
      *                                INTO    MDCRES-HOME-ADDRESSES
      *    END-STRING
      *
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-CLAIM-CALL-SEC        SECTION.
      *
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
      *
      *H26.11
           IF      FLG-WKSRYACT-INS    =   1
      *        追加は決定済み
               CONTINUE
           ELSE
      *        保険組合せの決定
               PERFORM 200215-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE    SPACE               TO  LF04-REC
           INITIALIZE                  LF04-REC
      *    入外区分
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               MOVE    "true"              TO  LF04-NYUGAIKBN
           END-IF
      *    医療機関ＩＤ
           MOVE    SPA-HOSPID          TO  LF04-CRE001-FACILITYID
      *    診療日付
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  LF04-PERFORM-TIME
           MOVE    WRK-HEN-DATE        TO  WRK-PERFORM-DATE
      *    UID
           MOVE    MDCRES-MEDICAL-UID  TO  LF04-MEDICAL-UID
      *    患者番号
           MOVE    WRK-PTNUM           TO  LF04-PTID1
           MOVE    WRK-SRYKA           TO  LF04-CRE001-DPERTID
           MOVE    WRK-DRCD            TO  LF04-DOCCD
      *
      *    同日再入院区分
           IF      FLG-DOUJITU         =   1
               MOVE    "1"                 TO  LF04-DOUJITSUKBN
           END-IF
      **
      *H26.11   保険情報使用なし
      *    保険情報
           MOVE    MDCREQ-MAIN-INSURANCE-CLASS
                                       TO  LF04-HKN-HKNSBTCODE (01)
           PERFORM 2001-HKNNUM-SET-SEC
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   4)
      *
      *        名称なしの時、公費種類から名称設定
               IF     (MDCREQ-INSURANCE-NAME (IDY)     =   SPACE )
                  AND (MDCREQ-INSURANCE-CLASS(IDY) NOT =   SPACE )
                   PERFORM 2002-KOHNUM-SET-SEC
               END-IF
               MOVE    MDCREQ-INSURANCE-NAME (IDY)
                                       TO  LF04-HKNKOH-PROVIDER(1 IDY)
               MOVE    MDCREQ-PROVIDER-NUMBER (IDY)
                                       TO  LF04-HKNKOH-FTNJANUM(1 IDY)
               MOVE    MDCREQ-RECIPIENT-NUMBER (IDY)
                                       TO  LF04-HKNKOH-JKYSNUM(1 IDY)
           END-PERFORM
      **
      *    診療情報設定
           PERFORM VARYING   IDY   FROM    1   BY  1
                   UNTIL    (IDY           >   40)
               MOVE    MDCREQ-MDC-CLASS (IDY)
                                           TO  LF04-SRYCD(IDY)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDY)
                                           TO  LF04-KAISU(IDY)
      *        剤内容
               PERFORM VARYING   IDZ   FROM    1   BY  1
                       UNTIL    (IDZ           >   40)
                   MOVE    MDCREQ-PRSCRPT-CODE (IDY IDZ)
                                           TO  LF04-IJICD(IDY IDZ)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDY IDZ)
                                           TO  LF04-ALSNAME(IDY IDZ)
                   MOVE    MDCREQ-PRSCRPT-NUMBER(IDY IDZ)
                                           TO  LF04-SURYO(IDY IDZ)
      *H24.4
      *            数量コードに一般名記載区分編集
                   EVALUATE    MDCREQ-GENERIC-FLG (IDY IDZ)
                      WHEN     "yes"
      *                   一般名記載
                          MOVE    "90"         TO  LF04-SURYOCD(IDY IDZ)
                      WHEN     "no"
      *                   銘柄名記載
                          MOVE    "91"         TO  LF04-SURYOCD(IDY IDZ)
                   END-EVALUATE
      *H29.4
      *            内服１種類区分
                   IF      MDCREQ-NAIFKBN  (IDY IDZ) =   "1"
                       MOVE    "1"             TO  LF04-NAIFKBN
                                                            (IDY IDZ)
                   END-IF
      *            継続コメント指示区分
                   IF      MDCREQ-CONTKBN  (IDY IDZ) =   "1"
                       MOVE    "1"             TO  LF04-CONTKBN
                                                            (IDY IDZ)
                   END-IF
      *
                   IF      MDCREQ-PRSCRPT-CODE (IDY IDZ)   NOT =   SPACE
                       ADD     1               TO  CNT-SRYCNT
                   END-IF
               END-PERFORM
           END-PERFORM
      *    診断履歴情報設定（病名）
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   50)
      *                     置換えは病名なし
                            OR  (FLG-SYORI     =   3 )
      *                     エラー終了
                            OR  (WRK-ERRCD NOT =   SPACE)
               MOVE    MDCREQ-DIAGNOSIS-NUMBER(IDY)
                                           TO  LF04-SND-SPICOD1(IDY)
               MOVE    MDCREQ-DIAGNOSIS-NAME (IDY)
                                           TO  LF04-SND-SPINAME1(IDY)
      *
               IF     (MDCREQ-DIAGNOSIS-NUMBER(IDY)
                                               NOT =   SPACE )
                 OR   (MDCREQ-DIAGNOSIS-NAME (IDY)
                                               NOT =   SPACE )
                   ADD     1                   TO  CNT-BYOCNT
               END-IF
      *ver.4.7
               EVALUATE    MDCREQ-DIAGNOSIS-INOUT (IDY)
               WHEN    "I"
      *            入院
                   MOVE    "inpatient"     TO  LF04-SND-NYUGAIKBN (IDY)
               WHEN    "O"
      *            外来
                   MOVE    "outpatient"    TO  LF04-SND-NYUGAIKBN (IDY)
               WHEN    OTHER
                   MOVE    SPACE           TO  LF04-SND-NYUGAIKBN (IDY)
               END-EVALUATE
      *
               PERFORM VARYING   IDZ   FROM    1   BY  1
                       UNTIL    (IDZ           >   6)
                   MOVE    MDCREQ-DIAGNOSIS-SINGLE-NUMBER(IDY IDZ)
                                           TO  LF04-SNDSPITBL-SPICOD2
                                                          (IDY IDZ)
                   MOVE    MDCREQ-DIAGNOSIS-SINGLE-NAME (IDY IDZ)
                                           TO  LF04-SNDSPITBL-SPINAME2
                                                           (IDY IDZ)
      *
                   IF     (MDCREQ-DIAGNOSIS-SINGLE-NUMBER(IDY IDZ)
                                               NOT =   SPACE  )
                     OR   (MDCREQ-DIAGNOSIS-SINGLE-NAME (IDY IDZ)
                                               NOT =   SPACE  )
                       ADD     1               TO  CNT-BYOCNT
                   END-IF
               END-PERFORM
      *ver.4.7
      *        補足コメント
               MOVE    MDCREQ-DIAGNOSIS-SCODE1(IDY)
                                           TO  LF04-HOSOKUCD(IDY 1)
               MOVE    MDCREQ-DIAGNOSIS-SCODE2(IDY)
                                           TO  LF04-HOSOKUCD(IDY 2)
               MOVE    MDCREQ-DIAGNOSIS-SCODE3(IDY)
                                           TO  LF04-HOSOKUCD(IDY 3)
               MOVE    MDCREQ-DIAGNOSIS-SNAME (IDY)
                                           TO  LF04-HOSOKU-COMMENT(IDY)
      *
               IF      MDCREQ-DIAGNOSIS-CATEGORY (IDY)
                                           = "PD"
                   MOVE    "MML0012"       TO  LF04-SNDBNRTBL-BNRTBL1
                                                             (IDY 1)
                   MOVE    "mainDiagnosis"
                                           TO  LF04-SNDBNRTBL-BNRNAME1
                                                              (IDY 1)
               END-IF
               IF      MDCREQ-DIAGNOSIS-SUSPECTEDFLAG (IDY)
                                           = "S"
                   MOVE    "MML0015"       TO  LF04-SNDBNRTBL-BNRTBL1
                                                             (IDY 2)
                   MOVE    "suspectedDiagnosis"
                                           TO  LF04-SNDBNRTBL-BNRNAME1
                                                             (IDY 2)
               END-IF
      *        病名日付チェック
               MOVE    MDCREQ-DIAGNOSIS-START-DATE(IDY)
                                           TO  WRK-START-DATE
               MOVE    MDCREQ-DIAGNOSIS-END-DATE(IDY)
                                           TO  WRK-END-DATE
               PERFORM 200211-BYOMEI-YMD-SEC
      *!!!
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    IDY             TO  WRK-BYO-IDX
               END-IF
      *!!!
               MOVE    WRK-START-DATE      TO  LF04-SND-SKNSTARTDATE
                                                             (IDY)
               MOVE    WRK-END-DATE        TO  LF04-SND-SKNENDDATE(IDY)
               EVALUATE    MDCREQ-DIAGNOSIS-OUTCOME(IDY)
                   WHEN    "D"
                     MOVE  "died"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "F"
                     MOVE  "fullyRecovered"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "W"
                     MOVE  "worsening"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "N"
                     MOVE  "unchanged"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "R"
                     MOVE  "recovering"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "U"
                     MOVE  "pause"
                                       TO  LF04-SND-TENKI(IDY)
                   WHEN    "S"
                     MOVE  "continued"
                                       TO  LF04-SND-TENKI(IDY)
      *
                   WHEN    "O"
                     MOVE  "delete"
                                       TO  LF04-SND-TENKI(IDY)
               END-EVALUATE
           END-PERFORM
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      20021-CLAIM-CALL-EXT
           END-IF
      *
           INITIALIZE                  LNK-ORCL0031AREA
           MOVE    "1"                 TO  LNK-KBN-CODE
      *    診療行為なし
           IF     (CNT-SRYCNT          =   ZERO )
               MOVE    "1"                 TO  LNK-KBN-CODE2
           END-IF
      *    V2区分
           MOVE    "1"                 TO  LNK-KBN-V2
      *H26.11
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  LNK-IN-HKNCOMBI
           IF      FLG-WKSRYACT-INS    =   1
      *        追加（外来）
               MOVE    "1"                 TO  LNK-ADDKBN
      *        追加開始の剤番号
               MOVE    WRK-MAX-ZAINUM      TO  LNK-MAX-ZAINUM
           END-IF
      *
           CALL    "ORCL0031"          USING LF04-REC
                                             LNK-ORCL0031AREA
      *
           IF      LNK-OUT-HKNCOMBI        =   ZERO
                                           OR  "9999"
               CONTINUE
           ELSE
      *        対象保険組合せ情報編集
               PERFORM 200210-HKNCOMBI-HEN-SEC
           END-IF
      *H26.11
      *    保険組合せ番号
           MOVE    LNK-OUT-HKNCOMBI    TO  MDCRES-COMBINATION-NUMBER
      *
      *!!!
      *    中途データエラー
           IF      LNK-ERR1-CODE       =   "02"
      *        「内容を置き換えました」は警告へ
               ADD     1                   TO  IDK
               MOVE    "W03"               TO  WRK-KEICD(IDK)
               MOVE    SPACE               TO  LNK-ERR1-CODE
               MOVE    SPACE               TO  LNK-ERR1-MSG
           END-IF
           IF      LNK-ERR1-CODE       =   "K1"
      *        剤番号最大値の警告
               ADD     1                   TO  IDK
               MOVE    "W10"               TO  WRK-KEICD(IDK)
               MOVE    SPACE               TO  LNK-ERR1-CODE
               MOVE    SPACE               TO  LNK-ERR1-MSG
           END-IF
           IF      LNK-ERR1-CODE   NOT =   SPACE
               IF      CNT-BYOCNT          =   ZERO
                   MOVE    LNK-ERR1-CODE       TO  WRK-ERRCD
                   MOVE    "80"                TO  WRK-ERRCD
                   MOVE    LNK-ERR1-MSG        TO  WRK-ERRMSG
               ELSE
                   MOVE    LNK-ERR1-CODE       TO  WRK-ERRCD-1
                   MOVE    LNK-ERR1-MSG        TO  WRK-ERRMSG-1
               END-IF
           END-IF
           IF      LNK-ERR2-CODE   NOT =   SPACE
               IF      (CNT-SRYCNT         =   ZERO)
                 AND   (CNT-BYOCNT         >   ZERO)
                 AND   (LNK-OUT-CNT2       =   ZERO)
      *            病名登録エラー
                   MOVE    "80"                TO  WRK-ERRCD
                   MOVE    "病名の登録ができません"
                                               TO  WRK-ERRMSG
               END-IF
               MOVE    LNK-ERR2-CODE       TO  WRK-ERRCD-2
               MOVE    LNK-ERR2-MSG        TO  WRK-ERRMSG-2
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        対象なし
               IF     (CNT-SRYCNT          =   ZERO )  AND
                      (CNT-BYOCNT          =   ZERO )
                   MOVE    "22"                TO  WRK-ERRCD
               END-IF
      *        中途データ・病名
               IF      (CNT-SRYCNT         >   ZERO )
                 AND   (CNT-BYOCNT         >   ZERO )
                   IF      (LNK-OUT-CNT1       >   ZERO)
                       AND (LNK-OUT-CNT2       =   ZERO)
      *                中途データ登録
                       MOVE    "21"            TO  WRK-ERRCD
                       STRING  "中途データ登録処理終了。"
                               "病名の登録ができませんでした。"
                                               INTO  WRK-ERRMSG
                       END-STRING
                   END-IF
                   IF      (LNK-OUT-CNT1       =   ZERO)
                       AND (LNK-OUT-CNT2       >   ZERO)
      *                中途データ登録
                       STRING  "病名登録処理終了。"
                               "中途データが登録できませんでした。"
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    "20"            TO  WRK-ERRCD
                   END-IF
               END-IF
               IF     (LNK-OUT-HKNCOMBI    =   ZERO
                                           OR  SPACE )
               AND    (LNK-OUT-CNT1        >   ZERO )
                   ADD     1                   TO  IDK
                   MOVE    "W02"               TO  WRK-KEICD(IDK)
               END-IF
           END-IF
      *
      *    診療行為ありの時、入外区分判定
           IF     (CNT-SRYCNT          >   ZERO )
               PERFORM 200219-NYUGAIKBN-CHK-SEC
           END-IF
      *
      *!!!!!!!!!!!!!!!!!!!!!
      *    IF      WRK-ERR-CODE    NOT =   ZERO
      ******** MOVE    WRK-ERR-CODE        TO  WRK-ERRCD
      *        MOVE    "80"                TO  WRK-ERRCD
      *        MOVE    WRK-ERR-MSG         TO  WRK-ERRMSG
      *    ELSE
      *        IF     (WRK-OUT-HKNCOMBI    =   ZERO
      *                                    OR  SPACE )
      *        AND    (CNT-SRYCNT          >   ZERO )
      *            MOVE    "K2"                TO  WRK-KEICD
      *        END-IF
      *        IF     (CNT-SRYCNT          >   ZERO )  AND
      *               (WRK-OUT-CNT1        =   ZERO )
      *            MOVE    "20"                TO  WRK-ERRCD
      *        END-IF
      *        IF     (CNT-BYOCNT          >   ZERO )  AND
      *               (WRK-OUT-CNT2        =   ZERO )
      *            MOVE    "21"                TO  WRK-ERRCD
      *        END-IF
      *        IF     (CNT-SRYCNT          =   ZERO )  AND
      *               (CNT-BYOCNT          =   ZERO )
      *            MOVE    "22"                TO  WRK-ERRCD
      *        END-IF
      *    END-IF
      *!!!!!!!!!!!!!!!!!!!!!
      *
           IF      LNK-OUT-CNT1        =   ZERO
      *        中途終了データ登録なしは、ＵＩＤのクリア
               MOVE    SPACE           TO  MDCRES-MEDICAL-UID
           END-IF
      *
           .
       20021-CLAIM-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       200211-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           MOVE    C-UID               TO  MDCRES-MEDICAL-UID
      *
           .
       200211-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       2001-HKNNUM-SET-SEC     SECTION.
      *
           EVALUATE    MDCREQ-MAIN-INSURANCE-CLASS
               WHEN    "060"
                   MOVE    "00"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "971"
                   MOVE    "R1"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "973"
                   MOVE    "R3"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    "975"
                   MOVE    "K5"            TO  LF04-HKN-HKNSBTCODE(01)
               WHEN    OTHER
                   EVALUATE    MDCREQ-MAIN-INSURANCE-CLASS (1:2)
                   WHEN    "98" 
                       MOVE  "Z"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    "90" 
                       MOVE  "A"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    "91" 
                       MOVE  "B"           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(1:1)
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (3:1)
                                           TO  LF04-HKN-HKNSBTCODE
                                                           (01)(2:1)
                   WHEN    OTHER
                       MOVE    MDCREQ-MAIN-INSURANCE-CLASS (2:2)
                                           TO  LF04-HKN-HKNSBTCODE
                                                          (01)(1:2)
                   END-EVALUATE
           END-EVALUATE
      *
           .
       2001-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費種類名称変換処理
      *****************************************************************
       2002-KOHNUM-SET-SEC     SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    MDCREQ-INSURANCE-CLASS(IDY)
                                       TO  HKN-HKNNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 950-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    HKN-TANSEIDONAME    TO
                                           MDCREQ-INSURANCE-NAME (IDY)
           END-IF
      *
           .
       2002-KOHNUM-SET-EXT.
           EXIT.
      *
      *H26.11
      *****************************************************************
      *    保険組合せの決定処理
      *****************************************************************
       200215-HKNCOMBI-SET-SEC     SECTION.
      *
      *    包括分入力
           IF      MDCREQ-MAIN-INSURANCE-CLASS =   "999"
               MOVE    "9999"          TO  WRK-HKNCOMBI
           ELSE
               PERFORM 2002151-HKNCOMBI-CALL-SEC
           END-IF
      *    保険組合せ番号
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
           .
       200215-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       2002151-HKNCOMBI-CALL-SEC     SECTION.
      *
           IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    保険組合せ決定
           MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険種類
           MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                       TO  ORCSAPIHKN-IN-HKNNUM
      *    保険者番号
           MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                       TO  ORCSAPIHKN-IN-HKNJANUM
      *    保険種類
           MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                       TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *    記号
           MOVE    MDCREQ-MAIN-MARK    TO  ORCSAPIHKN-IN-MARK
      *    番号
           MOVE    MDCREQ-MAIN-NUMBER
                                       TO  ORCSAPIHKN-IN-NUMBER
      *    有効期間
           MOVE    MDCREQ-MAIN-START-DATE
                                       TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKSTYMD
           MOVE    MDCREQ-MAIN-END-DATE
                                       TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKEDYMD
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *        公費名称
               MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *        負担者番号
               MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *        受給者番号
               MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
           END-PERFORM
      *
           ELSE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           END-IF
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF     (ORCSAPIHKN-OT-HKNCOMBI  =   SPACE )
      *        保険番号対象なし
              OR  (ORCSAPIHKN-ERRCD        =   4     )
               MOVE    ZERO                    TO  WRK-HKNCOMBI
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
      *
               MOVE    1                   TO  IDX
      *        出力領域保険情報編集
               PERFORM 2002101-HKNHEN-TBL-SEC
           END-IF
           .
       2002151-HKNCOMBI-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
      *        数値エラーはゼロ（送信なし）
               MOVE    ZERO                TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
           .
       200311-HKNCOMBI-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       200210-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    追加処理
      **** MOVE    "1"                 TO  ORCSAPIHKN-KBN
      *    指定保険組合せのみ編集
           MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険組合せ
           MOVE    LNK-OUT-HKNCOMBI    TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
      *    出力領域保険情報編集
           MOVE    1                   TO  IDX
           PERFORM 2002101-HKNHEN-TBL-SEC
      *
      *    出力領域保険情報編集
      *     PERFORM VARYING     IDX     FROM    1   BY  1
      *             UNTIL      (IDX     >   20  )
      *                   OR   (ORCSAPIHKN-THKNCOMBI (IDX)
      *                                     =   SPACE  )
      *         IF      ORCSAPIHKN-THKNCOMBI (IDX)  =   LNK-OUT-HKNCOMBI
      *             PERFORM 2002101-HKNHEN-TBL-SEC
      *             MOVE    20                  TO  IDX
      *         END-IF
      *     END-PERFORM
           .
       200210-HKNCOMBI-HEN-EXT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  MDCRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  MDCRES-MAIN-ASSISTANCE
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  MDCRES-MAIN-FAMILY
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  MDCRES-MAIN-START-DATE
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  MDCRES-MAIN-END-DATE
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  MDCRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  MDCRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  MDCRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  MDCRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-START-DATE
                                                             (IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-END-DATE
                                                             (IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  MDCRES-RATE-ADMISSION
                                                             (IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  MDCRES-MONEY-ADMISSION
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDY2)
           END-PERFORM
      *
      *    労災区分
           MOVE    ORCSAPIHKN-RSI-HKNKBN (IDX) TO  WRK-RSI-HKNKBN
           MOVE    ORCSAPIHKN-HKNNUM (IDX)     TO  WRK-HKNNUM
           .
       2002101-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名日付チェック編集処理
      *****************************************************************
       200211-BYOMEI-YMD-SEC                SECTION.
      *
      *    開始日
           MOVE    WRK-START-DATE      TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           IF      WRK-SYMD            =   SPACE
      *        空白は診療日付
               MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
               MOVE    WRK-PERFORM-DATE    TO  WRK-START-DATE
           END-IF
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "17"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-SYMD
               GO      TO   200211-BYOMEI-YMD-EXT
           END-IF
      *    終了日
           IF      WRK-END-DATE    NOT =   SPACE
               MOVE    WRK-END-DATE        TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               IF      WRK-SYMD            =   SPACE
                   MOVE    SPACE           TO  WRK-END-DATE
               ELSE
      *            日付チェックサブ
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "18"                TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF     (WRK-END-DATE    NOT =   SPACE )
              AND (WRK-END-DATE        <   WRK-START-DATE )
               MOVE    "19"                TO  WRK-ERRCD
           END-IF
           .
       200211-BYOMEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分判定処理
      *****************************************************************
       200219-NYUGAIKBN-CHK-SEC        SECTION.
      *
      *    入院履歴検索
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
               ELSE
                   INITIALIZE                  PTNYUINRRK-REC
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
      *        入院中
               IF     (SPA-NYUGAIKBN       =   "2" )
                 AND  (SPA-SYSYMD      NOT =   PTNYUINRRK-NYUINYMD)
                 AND  (SPA-SYSYMD      NOT =   PTNYUINRRK-TAIINYMD)
      *            外来は、自費・アフターケア以外を警告
                   IF     (WRK-HKNNUM (1:2)    =   "98" )
                    OR   ((WRK-HKNNUM          =   "971")  AND
                          (WRK-RSI-HKNKBN      =   "3"  ))
                       CONTINUE
                   ELSE
                       ADD     1                   TO  IDK
                       MOVE    "W04"               TO  WRK-KEICD(IDK)
                   END-IF
               END-IF
           ELSE
      *        入院中以外
               IF      SPA-NYUGAIKBN       =   "1"
                   ADD     1                   TO  IDK
                   MOVE    "W05"               TO  WRK-KEICD(IDK)
               END-IF
           END-IF
           .
       200219-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ削除処理
      *****************************************************************
       2003-WKSRYACT-DEL-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      2003-WKSRYACT-DEL-EXT
           END-IF
      *
      *    ワーク診療行為削除処理
           PERFORM 20031-WKSRYACT-DEL-SEC
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2003-WKSRYACT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ置換処理
      *****************************************************************
       20031-WKSRYACT-DEL-SEC        SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
           MOVE    MDCREQ-MEDICAL-UID  TO  WKSRY-KARTE-KEY
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    削除対象チェック
           IF      FLG-WKSRYACT        =   ZERO
               MOVE    WKSRY-HKNCOMBI      TO  WRK-HKNCOMBI
           ELSE
               PERFORM 200311-WKSRYACT-CHK-SEC
               IF      APILST-CLASS        =   "02"
                   IF      FLG-CHKWK           =   ZERO
                       MOVE    "30"                TO  WRK-ERRCD
                   ELSE
                       MOVE    "31"                TO  WRK-ERRCD
                   END-IF
               ELSE
                   IF      FLG-CHKWK           =   ZERO
                       MOVE    "32"                TO  WRK-ERRCD
                   ELSE
                       MOVE    "33"                TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
           IF     (FLG-WKSRYACT        =   ZERO  )  AND
                  (WRK-ERRCD           =   SPACE )
      *        削除
               INITIALIZE                  WKSRYACT-REC
               MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
               MOVE    SPA-PTID            TO  WKSRY-PTID
               MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
               MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
               MOVE    MDCREQ-MEDICAL-UID  TO  WKSRY-KARTE-KEY
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "del13"             TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "34"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20031-WKSRYACT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ削除判定処理
      *****************************************************************
       200311-WKSRYACT-CHK-SEC        SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  FLG-CHKWK
           PERFORM UNTIL  (FLG-WKSRYACT    =   1   )
                       OR (FLG-CHKWK   NOT =   ZERO)
               IF     (WKSRY-KARTE-KEY     =   SPACE )
                 AND  (WKSRY-KARTE-FLG     =   1     )
                   MOVE    1               TO  FLG-CHKWK
               END-IF
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20031-WKSRYACT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ置換処理
      *****************************************************************
       2004-WKSRYACT-UPD-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *H26.11 廃止
      **** 入院であること
      *    IF      MDCREQ-OUTPATIENT-CLASS =  "I"
      *        CONTINUE
      *    ELSE
      *        MOVE    "05"            TO  WRK-ERRCD
      *        MOVE    SPACE           TO  WRK-KEICD-G
      *        GO      TO      2004-WKSRYACT-UPD-EXT
      **** END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      2004-WKSRYACT-UPD-EXT
           END-IF
      *
      *    ワーク診療行為削除処理
           PERFORM 20031-WKSRYACT-DEL-SEC
      *H26.11 
      *    外来は、追加処理を行う
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               CONTINUE
           ELSE
      *        ワーク診療行為追加チェック処理
               PERFORM 20051-WKSRYACT-CHK-SEC
           END-IF
      *
      *    診療行為登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-CLAIM-CALL-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2004-WKSRYACT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来中途終了データ追加処理
      *****************************************************************
       2005-WKSRYACT-GAI-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    外来であること
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               MOVE    "40"            TO  WRK-ERRCD
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      2005-WKSRYACT-GAI-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               MOVE    SPACE           TO  WRK-KEICD-G
               GO      TO      2005-WKSRYACT-GAI-EXT
           END-IF
      *
      *    ワーク診療行為チェック処理
           PERFORM 20051-WKSRYACT-CHK-SEC
      *    診療行為登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-CLAIM-CALL-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2005-WKSRYACT-GAI-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ追加チェック処理
      *****************************************************************
       20051-WKSRYACT-CHK-SEC        SECTION.
      *
      *    外来追加処理
           MOVE    1                   TO  FLG-WKSRYACT-INS
      *    保険組合せの決定
           PERFORM 200215-HKNCOMBI-SET-SEC
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *    同日再入院区分
           IF      FLG-DOUJITU         =   1
               MOVE    "1"                 TO  WKSRY-DOUJITSUKBN
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-MAX-ZAINUM
      *    対象チェック
           IF      FLG-WKSRYACT        =   ZERO
      *        ドクターが一致すること
               IF      WKSRY-DRCD      NOT =   WRK-DRCD
                   MOVE    "41"                TO  WRK-ERRCD
               END-IF
      *        中途終了での作成分
               IF      WKSRY-KARTE-FLG     =   1
                   IF      WRK-ERRCD           =   SPACE
                       MOVE    "42"                TO  WRK-ERRCD
                   END-IF
               END-IF
      *        最大剤番号検索
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 200511-WKSRYACT-ZAINUM-SEC
               END-IF
           END-IF
      *
           .
       20051-WKSRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ追加剤番号処理
      *****************************************************************
       200511-WKSRYACT-ZAINUM-SEC                SECTION.
      *
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *    同日再入院区分
           IF      FLG-DOUJITU         =   1
               MOVE    "1"                 TO  WKSRY-DOUJITSUKBN
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key16"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    対象チェック
           IF      FLG-WKSRYACT        =   ZERO
               MOVE    WKSRY-ZAINUM        TO  WRK-MAX-ZAINUM
               IF      WKSRY-ZAINUM        =   99999999
                   MOVE    "43"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       200511-WKSRYACT-ZAINUM-EXT.
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
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "ドクターが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "ＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "置換処理は、入院のみ可能です"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "17"
      **       MOVE    "病名開始日付が暦日エラーです。"
      **                                       TO  WRK-ERRMSG
               IF      WRK-BYO-IDX             >   ZERO
                   STRING  "病名開始日付が暦日エラーです。"
                           "　（"
                           WRK-BYO-IDX
                           "列）"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               ELSE
                   MOVE    "病名開始日付が暦日エラーです。"
                                               TO  WRK-ERRMSG
               END-IF
           WHEN    "18"
      **       MOVE    "病名転帰日付が暦日エラーです。"
      **                                       TO  WRK-ERRMSG
               IF      WRK-BYO-IDX             >   ZERO
                   STRING  "病名転帰日付が暦日エラーです。"
                           "　（"
                           WRK-BYO-IDX
                           "行目）"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               ELSE
                   MOVE    "病名転帰日付が暦日エラーです。"
                                               TO  WRK-ERRMSG
               END-IF
           WHEN    "19"
      **       MOVE    "病名開始日付＞転帰日付です。"
      **                                       TO  WRK-ERRMSG
               IF      WRK-BYO-IDX             >   ZERO
                   STRING  "病名開始日付＞転帰日付です。"
                           "　（"
                           WRK-BYO-IDX
                           "行目）"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               ELSE
                   MOVE    "病名開始日付＞転帰日付です。"
                                               TO  WRK-ERRMSG
               END-IF
      **   WHEN    "20"
      *        MOVE    "中途終了データの登録ができません"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "21"
      *        MOVE    "病名の登録ができません"
      **                                       TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "登録対象のデータがありません"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "入院日付が暦日エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "入院日付が入院日ではありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "30"
               MOVE    "削除対象の中途終了データがありません"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               STRING  "削除対象の中途終了データがありません。"
                       "内容が更新されている可能性があります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "32"
               MOVE    "置換対象の中途終了データがありません。"
                                               TO  WRK-ERRMSG
           WHEN    "33"
               STRING  "置換対象の中途終了データがありません。"
                       "内容が更新されている可能性があります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "34"
               MOVE    "中途終了データ削除エラー"
                                               TO  WRK-ERRMSG
      *H26.11
           WHEN    "40"
               MOVE    "追加処理は、外来のみ可能です"
                                               TO  WRK-ERRMSG
           WHEN    "41"
               STRING  "追加対象の中途終了データとドクターコードが"
                       "違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "42"
               STRING  "追加対象の中途終了データの登録方法が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "43"
               STRING  "中途終了データの剤番号が最大です。"
                       "追加できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       891-WARNING-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-WERRCD
      *警告
           WHEN    "W01"
               MOVE    "診療日を設定しました。"
                                               TO  WRK-ERRMSG
           WHEN    "W02"
               MOVE    "保険組合せをゼロで登録しました。"
                                               TO  WRK-ERRMSG
           WHEN    "W03"
               STRING  "内容を置き換えました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W04"
               STRING  "入院期間中です。"
                       "外来で展開できない保険組合せです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W05"
               STRING  "入院中ではありません。"
                       "入院で展開できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W10"
               STRING  "中途終了データの剤番号が最大になりました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           END-EVALUATE
           .
       891-WARNING-MSG-EXT.
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
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
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
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       950-HKNNUM-READ-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       950-HKNNUM-READ-EXT.
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

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
       PROGRAM-ID.         ORAPI021R2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 来院患者一覧  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/08/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-多々納  17/04/04  最大件数を1000へ
      * 05.00.00     NACL-多々納  19/12/07  患者情報更新日編集
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
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDD                 PIC 9(02).
      *
           03  TBL-MAX             PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
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
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).
      *
      *    患者番号
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-RENNUM              PIC 9(01).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
       01  WRK-XML-AREA.
           03  WRK-VISIT-DATE.
               05  WRK-VISIT-DATE1         PIC X(07).
               05  WRK-VISIT-DATE2         PIC X(03).
           03  WRK-VISIT-YMD.
               05  WRK-VISIT-YM            PIC X(06).
               05  WRK-VISIT-DD            PIC X(02).
           03  WRK-VIS-SRYKA               PIC X(02).
           03  WRK-VIS-SRYKA-NAME          PIC X(80).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
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
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPVISITPTLSTV2REQ.INC".
      *    API XML書き込み用定義体（受診日付指定）
           COPY    "CPVISITPTLST01V2RES.INC".
      *    API XML書き込み用定義体（受診年月指定）
           COPY    "CPVISITPTLST02V2RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
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
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA3.INC".
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
           DISPLAY   "*******************"
           DISPLAY   "* visitptlst start*"
           DISPLAY   "*******************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *****INITIALIZE                      CNT-AREA
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
           DISPLAY   "* visitptlst end  *"
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
           INITIALIZE                      XML-VISITPTLST01RES
           INITIALIZE                      XML-VISITPTLST02RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  VIS01RES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  VIS01RES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  VIS01RES-INFORMATION-TIME
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
           EVALUATE    VISREQ-REQUEST-NUMBER
           WHEN    "01"
      *        受診年月日
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        受診年月
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    VISREQ-REQUEST-NUMBER
           WHEN    "01"
      *        受診年月日
               PERFORM 2002-SYORI-01-SEC
           WHEN    "02"
      *        受診年月情報
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
           MOVE    "xml_visitptlstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST10-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "visitptlstreq"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_visitptlstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST10-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "visitptlstreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-VISITPTLSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
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
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  VIS01RES-API-RESULT
               MOVE    "処理終了"          TO  VIS01RES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  VIS01RES-API-RESULT
               MOVE    WRK-ERRMSG          TO  VIS01RES-API-RESULT-MSG
           END-IF
      *
           EVALUATE    FLG-SYORI
               WHEN    0
               WHEN    1
                   PERFORM 3001-MDC01XMLMAKE-SEC
               WHEN    2
                   PERFORM 3002-MDC02XMLMAKE-SEC
           END-EVALUATE
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（受診履歴内容）
      *****************************************************************
       3001-MDC01XMLMAKE-SEC        SECTION.
      *
           IF      APILST10-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_visitptlst01v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  VIS01RES-MODE
           MOVE    "visitptlst01res"   TO  VIS01RES-RECORDNAME
      *****CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-VISITPTLST01RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_visitptlst01v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  VIS01RES-MODE
           MOVE    "visitptlst01res"   TO  VIS01RES-RECORDNAME
      **** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-VISITPTLST01RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    VIS01RES-OBJECT     TO  APILST10-BODY 
               MOVE    "application/xml"   TO  APILST10-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-MDC01XMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（診療行為内容）
      *****************************************************************
       3002-MDC02XMLMAKE-SEC        SECTION.
      *
      *    共通編集
           MOVE   "Medical Info"           TO  VIS02RES-RESKEY
      *
           MOVE    VIS01RES-INFORMATION-DATE
                                           TO  VIS02RES-INFORMATION-DATE
           MOVE    VIS01RES-INFORMATION-TIME
                                           TO  VIS02RES-INFORMATION-TIME
           MOVE    VIS01RES-API-RESULT     TO  VIS02RES-API-RESULT
           MOVE    VIS01RES-API-RESULT-MSG TO  VIS02RES-API-RESULT-MSG
      *
           IF      APILST10-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_visitptlst02v2res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  VIS02RES-MODE
           MOVE    "visitptlst02res"   TO  VIS02RES-RECORDNAME
     ****  CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-VISITPTLST02RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_visitptlst02v2res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "visitptlst02res"   TO  VIS02RES-RECORDNAME
      **** CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-VISITPTLST02RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_visitptlst02v2res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *****CALL    "ORCDBMAIN"         USING
           CALL    "ORCDBAPIV3"        USING
                                       MCPAREA
                                       XML-VISITPTLST02RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    VIS02RES-OBJECT     TO  APILST10-BODY 
               MOVE    "application/xml"   TO  APILST10-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "visitptlst xml write failure "
           END-IF
      *
          .
       3002-MDC02XMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF     VISREQ-VISIT-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  VISREQ-VISIT-DATE
           END-IF
           IF      VISREQ-DEP-CODE     =   LOW-VALUE
               MOVE  SPACE             TO  VISREQ-DEP-CODE
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    来院一覧情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-VISIT-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-VIS-SRYKA   NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               MOVE    WRK-VIS-SRYKA   TO  WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
               MOVE    WRK-SRYKA-NAME  TO  WRK-VIS-SRYKA-NAME
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               IF      FLG-SYORI       =   2
                   MOVE    VISREQ-VISIT-DATE   TO  VIS02RES-VISIT-DATE
                   MOVE    WRK-VIS-SRYKA       TO  VIS02RES-DEP-CODE
                   MOVE    WRK-VIS-SRYKA-NAME  TO  VIS02RES-DEP-NAME
               ELSE
                   MOVE    VISREQ-VISIT-DATE   TO  VIS01RES-VISIT-DATE
                   MOVE    WRK-VIS-SRYKA       TO  VIS01RES-DEP-CODE (1)
                   MOVE    WRK-VIS-SRYKA-NAME  TO  VIS01RES-DEP-NAME (1)
               END-IF
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    診療科
           MOVE    VISREQ-DEP-CODE         TO  WRK-VIS-SRYKA
      *    診療日付
           MOVE    VISREQ-VISIT-DATE       TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-VISIT-YMD
      *    未設定時はシステム情報設定
           IF      WRK-VISIT-YMD       =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-VISIT-YMD
               MOVE    VIS01RES-INFORMATION-DATE
                                           TO  VISREQ-VISIT-DATE
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
           IF      FLG-SYORI           =   2
      *        年月指定は、日を０１とする
               MOVE    "01"            TO  WRK-VISIT-DD
           END-IF
      *    システム日付とする
           MOVE    WRK-VISIT-YMD       TO  SPA-SYSYMD
      *
      *    必須入力チェック
      *    診療科
           IF      WRK-VIS-SRYKA       =   SPACE
               IF     (FLG-SYORI           =   2      )
               AND    (WRK-ERRCD           =   SPACE  )
                   MOVE    "01"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-VISIT-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-VISIT-YMD       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-VISIT-YMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-VISIT-DATE
      *
           .
       20011-VISIT-CHK-EXT.
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
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    情報内容編集
           PERFORM 20021-VISITRES-HEN-SEC
      *
      *    外来
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    受診歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-VISIT-YMD       TO  JYURRK-SRYYMD
      *
           MOVE    "key55"             TO  WRK-MCP-PATHNAME
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL   (FLG-JYURRK         =   1    )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *
               MOVE    ZERO            TO  FLG-CHK
      *        診療科一致チェック
               IF     (WRK-VIS-SRYKA   NOT =   SPACE )
                 AND  (WRK-VIS-SRYKA   NOT =   JYURRK-SRYKA )
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF      JYURRK-HKNCOMBINUM  =   "9999"
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  TBL-MAX
      *H29.4       IF      TBL-MAX         >   500
                   IF      TBL-MAX         >   1000
                       MOVE    "12"            TO  WRK-ERRCD
                   ELSE
      *                明細項目編集
                       PERFORM 20021-JYURRK-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-MAX             =   ZERO
      *        対象なし
               MOVE    "13"                TO  WRK-ERRCD
               IF      WRK-VIS-SRYKA   NOT =   SPACE
                   MOVE    WRK-VIS-SRYKA       TO  VIS01RES-DEP-CODE (1)
                   MOVE    WRK-VIS-SRYKA-NAME  TO  VIS01RES-DEP-NAME (1)
               END-IF
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴内容編集処理
      *****************************************************************
       20021-VISITRES-HEN-SEC        SECTION.
      *
           MOVE    WRK-VISIT-DATE    TO  VIS01RES-VISIT-DATE
      *
           .
       20021-VISITRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴内容編集処理
      *****************************************************************
       20021-JYURRK-HENSYU-SEC        SECTION.
      *
      *    患者情報検索
           PERFORM 200211-PTNUM-HEN-SEC
      *
           ADD    1                    TO  IDX
      *    患者情報
           MOVE    WRK-PTNUM           TO  VIS01RES-PATIENTID (IDX)
      *
           MOVE    PTINF-NAME          TO  VIS01RES-NAME     (IDX)
           MOVE    PTINF-KANANAME      TO  VIS01RES-KANANAME (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  VIS01RES-BIRTHDAY  (IDX)
           MOVE    PTINF-SEX           TO  VIS01RES-SEX  (IDX)
      *    連番
           MOVE    JYURRK-RENNUM       TO  VIS01RES-NUMBER (IDX)
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  VIS01RES-DENPNUM (IDX)
      *    診療科
           PERFORM 200212-SRYKAMEI-HEN-SEC
           MOVE    JYURRK-SRYKA        TO  VIS01RES-DEP-CODE (IDX)
           MOVE    WRK-SRYKA-NAME      TO  VIS01RES-DEP-NAME (IDX)
      *    ドクター
           IF      JYURRK-DRCD     NOT =   ZERO
               PERFORM 200213-DRCD-HEN-SEC
               MOVE    WRK-DRCD            TO  VIS01RES-DR-CODE (IDX)
               MOVE    WRK-DRCD-NAME       TO  VIS01RES-DR-NAME (IDX)
           END-IF
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  VIS01RES-COMBINATION-NUMBER
                                                               (IDX)
           PERFORM 200214-HKNCOMBI-HEN-SEC
      *H29.4
      *    更新日
           MOVE    JYURRK-UPYMD        TO  WRK-SYMD
           MOVE    JYURRK-UPHMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  VIS01RES-UPDATE-DATE(IDX)
           MOVE    WRK-HEN-TIME        TO  VIS01RES-UPDATE-TIME(IDX)
      *H29.12
      *    患者登録更新日
           MOVE    PTNUM-UPYMD         TO  WRK-SYMD
           MOVE    PTNUM-UPHMS         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  VIS01RES-PTNUM-UPDATE(IDX)
           MOVE    WRK-HEN-TIME        TO  VIS01RES-PTNUM-UPTIME(IDX)
           .
       20021-JYURRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       200211-PTNUM-HEN-SEC     SECTION.
      *
      *    患者番号取得
      *    INITIALIZE                  ORCSPTIDAREA
      *    MOVE    JYURRK-HOSPNUM      TO  SPTID-HOSPNUM
      *    MOVE    JYURRK-PTID         TO  SPTID-PTID
      *    CALL    "ORCSPTID"          USING   ORCSPTIDAREA
      *                                        SPA-AREA
      *    MOVE    SPTID-PTNUM         TO  WRK-PTNUM
      *    IF      SPTID-RC        NOT =   ZERO
      *        患者番号エラー
      *        MOVE    ALL "*"             TO  WRK-PTNUM
      *        MOVE    1                   TO  FLG-ERR
      *****END-IF
      *H29.12
      *    患者番号検索
           INITIALIZE                  PTNUM-REC
           MOVE    JYURRK-HOSPNUM      TO  PTNUM-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTNUM-PTID
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM           =   ZERO
               MOVE    PTNUM-PTNUM         TO  WRK-PTNUM
           ELSE
      *        患者番号エラー
               MOVE    ALL "*"             TO  WRK-PTNUM
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    JYURRK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
           .
       200211-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       200212-SRYKAMEI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (JYURRK-SRYKA    =  VIS01RES-DEP-CODE (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        診療科検索
               MOVE    JYURRK-SRYKA        TO   WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
           ELSE
               MOVE    VIS01RES-DEP-NAME (IDX-H)
                                           TO  WRK-SRYKA-NAME
           END-IF
           .
       200212-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター名称編集処理
      *****************************************************************
       200213-DRCD-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (JYURRK-DRCD     =  VIS01RES-DR-CODE (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        ドクター検索
               MOVE    JYURRK-DRCD        TO   WRK-DRCD
               PERFORM 2002131-DRCD-CHK-SEC
           ELSE
               MOVE    JYURRK-DRCD        TO   WRK-DRCD
               MOVE    VIS01RES-DR-NAME (IDX-H)
                                           TO  WRK-DRCD-NAME
           END-IF
           .
       200213-DRCD-HEN-EXT.
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
           MOVE    JYURRK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    JYURRK-PTID         TO  COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
           PERFORM 920-HKNCOMBI-READ-SEC
      *
      *    主保険−保険番号
           MOVE    COMB-HKNNUM         TO  VIS01RES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  VIS01RES-MAIN-INSURANCE-NAME
                                                            (IDX)
           IF      COMB-HKNID      NOT =   ZERO
      *        保険情報編集
               PERFORM 930-PTHKNINF-READ-SEC
      *        保険者番号
               MOVE    PTHKN-HKNJANUM  TO
                                       VIS01RES-MAIN-INSURANCE-NUMBER
                                                            (IDX)
      *        記号・番号
               MOVE    PTHKN-KIGO      TO  VIS01RES-MAIN-MARK  (IDX)
               MOVE    PTHKN-NUM       TO  VIS01RES-MAIN-NUMBER(IDX)
           END-IF
      *    公費情報
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   4
               IF      COMB-KOHID (IDK)    NOT =   ZERO
      *            公費の種類
                   MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  VIS01RES-INSURANCE-CLASS
                                                            (IDX IDK)
                   MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  VIS01RES-INSURANCE-NAME
                                                            (IDX IDK)
      *            公費情報編集
                   PERFORM 940-PTKOHINF-READ-SEC
      *            負担者番号
                   MOVE    PTKOH-FTNJANUM  TO  VIS01RES-PROVIDER-NUMBER
                                                            (IDX IDK)
                   MOVE    PTKOH-JKYSNUM   TO  VIS01RES-RECIPIENT-NUMBER
                                                            (IDX IDK)
               END-IF
           END-PERFORM
           .
       200214-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴診療行為内容編集処理
      *****************************************************************
       2003-SYORI-02-SEC          SECTION.
      *
      *    情報内容編集
           PERFORM 20031-VISITRES02-HEN-SEC
      *
      *    外来
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    受診歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-VIS-SRYKA       TO  JYURRK-SRYKA
           MOVE    WRK-VISIT-YM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "%"                 TO  JYURRK-SRYYMD(7:1)
      *
           MOVE    "key56"             TO  WRK-MCP-PATHNAME
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PTID
           PERFORM
               UNTIL   (FLG-JYURRK         =   1    )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *
               IF     (WRK-PTID        NOT =   ZERO )
                  AND (JYURRK-PTID         =   WRK-PTID)
      *            受診日設定
                   PERFORM 20031-JYURRK-DAYHEN-SEC
               ELSE
      *            患者情報設定
                   ADD     1               TO  TBL-MAX
                   IF      TBL-MAX         >   2000
                       MOVE    "14"            TO  WRK-ERRCD
                   ELSE
      *                明細項目編集
                       PERFORM 20031-JYURRK-HENSYU02-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-MAX             =   ZERO
      *        対象なし
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴内容編集処理
      *****************************************************************
       20031-VISITRES02-HEN-SEC        SECTION.
      *
           MOVE    WRK-VISIT-DATE1     TO  VIS02RES-VISIT-DATE
           MOVE    WRK-VIS-SRYKA       TO  VIS02RES-DEP-CODE
           MOVE    WRK-VIS-SRYKA-NAME  TO  VIS02RES-DEP-NAME
      *
           .
       20031-VISITRES02-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20031-JYURRK-HENSYU02-SEC     SECTION.
      *
      *    患者情報検索
           PERFORM 200211-PTNUM-HEN-SEC
      *
           MOVE    JYURRK-PTID         TO  WRK-PTID
      *
           ADD    1                    TO  IDX
      *    患者情報
           MOVE    WRK-PTNUM           TO  VIS02RES-PATIENTID (IDX)
      *
           MOVE    PTINF-NAME          TO  VIS02RES-NAME     (IDX)
           MOVE    PTINF-KANANAME      TO  VIS02RES-KANANAME (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  VIS02RES-BIRTHDAY  (IDX)
           MOVE    PTINF-SEX           TO  VIS02RES-SEX  (IDX)
      *
           MOVE    ZERO                TO  VIS02RES-VISIT-CALENDAR (IDX)
      *    受診日設定
           PERFORM 20031-JYURRK-DAYHEN-SEC
      *
           .
       20031-JYURRK-HENSYU02-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日設定処理
      *****************************************************************
       20031-JYURRK-DAYHEN-SEC                SECTION.
      *
           MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
           MOVE    WRK-SRYDD           TO  IDD
           MOVE    "1"                 TO  VIS02RES-VISIT-DAY (IDX IDD)
      *
           .
       20031-JYURRK-DAYHEN-EXT.
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
               MOVE    "診療科未設定"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "診療日設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療科コード誤り"
                                               TO  WRK-ERRMSG
           WHEN    "12"
      *        MOVE    "対象が５００件以上存在します。"
               MOVE    "対象が１０００件以上存在します。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "対象がありません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "対象が２０００件以上存在します。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
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
      *    患者基本情報取得
      *****************************************************************
       900-PTNUM-READ-SEC              SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
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
           MOVE    JYURRK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTHKN-PTID
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
           MOVE    JYURRK-HOSPNUM      TO  PTKOH-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTKOH-PTID
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

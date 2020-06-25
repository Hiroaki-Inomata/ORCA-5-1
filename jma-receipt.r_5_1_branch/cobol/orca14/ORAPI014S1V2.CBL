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
       PROGRAM-ID.         ORAPI014S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 予約登録 (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/07/17    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  14/06/XX  ワーニングメッセージ対応他
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
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-YYKOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDK                     PIC 9(04).
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
      *    予約ＩＤ
           03  WRK-YYKID               PIC 9(05).
      *    予約件数
           03  WRK-YYKCNT              PIC 9(05).
           03  WRK-YYKMAXCNT           PIC 9(02).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC 9(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD-G.
               05  WRK-KEICD               PIC X(02)
                                           OCCURS   5.
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
       01  WRK-XML-AREA.
           03  WRK-APPOINT-YMD10       PIC X(10).
           03  WRK-APPOINT-TIME8       PIC X(08).
      *
           03  WRK-APPOINT-YMD         PIC X(08).
           03  WRK-APPOINT-TIME.
               05  WRK-APPOINT-HHMM.
                   07  WRK-APPOINT-THH     PIC X(02).
                   07  WRK-APPOINT-TMM     PIC X(02).
               05  WRK-APPOINT-TSS     PIC X(02).
           03  WRK-KEYYYKTIME.
               05  WRK-KEYYYKTIME-HH       PIC X(02).
               05  WRK-KEYYYKTIME-MM       PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-SRYNAIYO            PIC X(02).
           03  WRK-SRYNAIYO-NAME       PIC X(24).
           03  WRK-YYKNAIYO            PIC X(02).
           03  WRK-YYKMEMO             PIC X(100).
           03  WRK-NAME                PIC X(100).
           03  WRK-KANANAME            PIC X(100).
           03  WRK-YYKNAME             PIC X(100).
           03  WRK-KAKREN              PIC X(02).
      *
           03  WRK-APPOINT-ID          PIC X(05).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
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
           COPY    "CPAPPTXMLV2REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPAPPTXMLV2RES.INC".
      *
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
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      **** COPY    "CPSYSUSER.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *   （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    予約内容
           COPY  "CPSK1028.INC".
      *    予約連絡
           COPY  "CPSK0051.INC".
      *    予約情報
           COPY  "CPSK1045.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約メモマスター
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *    予約
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *
      *　　（ＣＬＡＩＭ接続）
      *    COPY    "CPSK9000.INC".
      *    COPY    "CPSK9001.INC".
      *
      *    COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
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
           DISPLAY   "* appoint start*"
           DISPLAY   "***************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APIMOD-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "***************"
           DISPLAY   "* appoint end  *"
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
           INITIALIZE                      XML-APPOINTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  APNTRES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  APNTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  APNTRES-INFORMATION-TIME
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
           MOVE    ZERO                TO  APNTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
           MOVE    ZERO                TO  IDK
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        予約登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        予約取消
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
      *TEST!!
               PERFORM 890-ERRCD-MSG-SEC
               DISPLAY "必須項目チェックエラー "    WRK-ERRCD
                       " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *TEST
               DISPLAY "予約登録"    
      *TEST
      *        予約登録処理
               PERFORM 2002-YYK-ADD-SEC
           WHEN    "02"
      *TEST
               DISPLAY "予約取消"    
      *TEST
      *        予約取消処理
               PERFORM 2003-YYK-DELETE-SEC
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
           IF      APIMOD-BODY         NOT =   LOW-VALUE
               DISPLAY "APNTRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-APPOINTREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_appointv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "appointreq"        TO  APIREQ-RECORDNAME
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
           MOVE    "xml_appointv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "appointreq"        TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-APPOINTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_appointv2req"  TO  MCP-TABLE
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
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      ***  エラーメッセージ編集
      *    IF      WRK-ERRCD           =   SPACE
      *        IF      FLG-SYORI           =   1
      *            MOVE    WRK-KEICD           TO  WRK-ERRCD
      *        END-IF
      ***  END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "予約登録終了"
                                           TO  APNTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "予約取消終了"
                                           TO  APNTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  APNTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  APNTRES-API-RESULT-MSG
           END-IF
      *H26.6
      *    警告メッセージ
           IF     (WRK-ERRCD           =   SPACE )  AND
                  (WRK-KEICD-G     NOT =   SPACE )
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL      (IDK     >   5   )
                              OR  (WRK-KEICD (IDK) =   SPACE )
                   MOVE    WRK-KEICD (IDK)     TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   IF      IDK             =   1
                       MOVE    WRK-ERRCD       TO  APNTRES-API-RESULT
                   END-IF
                   MOVE    WRK-ERRMSG      TO  APNTRES-API-WARNING-MSG
                                                                 (IDK)
               END-PERFORM
           END-IF
      *
      *
           IF      APIMOD-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_appointv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  APNTRES-MODE
           MOVE    "appointres"        TO  APNTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_appointv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "appointres"        TO  APNTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_appointv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLCLOSE OK     = " MCP-RC
               MOVE    APNTRES-OBJECT      TO  APIMOD-BODY 
               MOVE    "application/xml"   TO  APIMOD-CONTENT-TYPE 
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF  APNTREQ-PATIENTID       =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-PATIENTID
           END-IF
           IF  APNTREQ-APPOINT-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-DATE
           END-IF
           IF  APNTREQ-APPOINT-TIME    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-TIME
           END-IF
           IF  APNTREQ-APPOINT-ID      =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-ID
           END-IF
           IF  APNTREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-DEP-CODE
           END-IF
           IF  APNTREQ-APPOINT-DR-CODE =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-DR-CODE
           END-IF
           IF  APNTREQ-APPOINT-DEP-INFO =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-DEP-INFO
           END-IF
           IF  APNTREQ-APPOINT-INFO    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-INFO
           END-IF
           IF  APNTREQ-APPOINT-MEMO    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-MEMO
           END-IF
           IF  APNTREQ-APPOINT-NAME    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-NAME
           END-IF
           IF  APNTREQ-APPOINT-KANANAME    =   LOW-VALUE
               MOVE  SPACE             TO  APNTREQ-APPOINT-KANANAME
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
      *    予約日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-YYKYMD-CHK-SEC
           END-IF
      *    予約時間チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-YYKTIME-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-PTNUM       NOT =   SPACE
                   PERFORM 20011-PTNUM-CHK-SEC
               END-IF
           END-IF
      *    予約氏名チェック
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-NAME        NOT =   SPACE )
               OR     (WRK-KANANAME    NOT =   SPACE )
                   PERFORM 20011-YYKNAME-CHK-SEC
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
      *    予約内容チェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-YYKNAIYO-CHK-SEC
           END-IF
      *    予約メモチェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-YYKMEMO-CHK-SEC
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
           MOVE    APNTREQ-PATIENTID      TO  APNTRES-PATIENTID
           MOVE    APNTREQ-PATIENTID      TO  WRK-PTNUM
      *    診療科
           MOVE    APNTREQ-APPOINT-DEP-CODE
                                           TO  APNTRES-APPOINT-DEP-CODE
           MOVE    APNTREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    APNTREQ-APPOINT-DR-CODE
                                           TO  APNTRES-APPOINT-DR-CODE
           MOVE    APNTREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
      *    受付日・時間
           MOVE    APNTREQ-APPOINT-DATE    TO  APNTRES-APPOINT-DATE
           MOVE    APNTREQ-APPOINT-TIME    TO  APNTRES-APPOINT-TIME
      *    数字のみに編集
           MOVE    APNTREQ-APPOINT-DATE    TO  WRK-HEN-DATE
           MOVE    APNTREQ-APPOINT-TIME    TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-APPOINT-YMD
           MOVE    WRK-THMS                TO  WRK-APPOINT-TIME
      *    予約ＩＤ
           MOVE    APNTREQ-APPOINT-ID      TO  APNTRES-APPOINT-ID
           MOVE    APNTREQ-APPOINT-ID      TO  WRK-APPOINT-ID
      *    診療内容区分
           MOVE    APNTREQ-APPOINT-DEP-INFO
                                           TO  WRK-SRYNAIYO
      *    予約区分
           MOVE    APNTREQ-APPOINT-INFO    TO  WRK-YYKNAIYO
      *    予約メモ
           MOVE    APNTREQ-APPOINT-MEMO    TO  WRK-YYKMEMO
      *    予約氏名
           MOVE    APNTREQ-APPOINT-NAME    TO  WRK-NAME
      *    予約カナ氏名
           MOVE    APNTREQ-APPOINT-KANANAME    TO  WRK-KANANAME
      *
      *    必須入力チェック
      *    患者番号・予約氏名・予約カナ氏名
           IF     (WRK-PTNUM           =   SPACE  )
              AND (WRK-NAME            =   SPACE  )
              AND (WRK-KANANAME        =   SPACE  )
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "01"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    予約日
           IF      WRK-APPOINT-YMD     =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    予約時間
           IF      WRK-APPOINT-TIME    =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
      *        新規は必須
               IF     (FLG-SYORI           =   1      )
                 AND  (WRK-ERRCD           =   SPACE  )
                   MOVE    "04"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    ドクター
           IF      WRK-DRCD            =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "05"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約日チェック
      *****************************************************************
       20011-YYKYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-APPOINT-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-APPOINT-YMD
           END-IF
      *
           .
       20011-YYKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約時間チェック
      *****************************************************************
       20011-YYKTIME-CHK-SEC    SECTION.
      *
           IF      WRK-APPOINT-TIME    NOT NUMERIC
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-APPOINT-THH     >=  "24"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           IF      WRK-APPOINT-TMM     >=  "60"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           MOVE    "00"                    TO  WRK-APPOINT-TSS
      *    予約時間帯編集
           MOVE    SPACE               TO  WRK-KEYYYKTIME
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-APPOINT-THH     TO  WRK-KEYYYKTIME-HH
               IF      WRK-APPOINT-THH     >   "21"
                   MOVE    "21"            TO  WRK-KEYYYKTIME-HH
               END-IF
               IF      WRK-APPOINT-THH     <   "08"
                   MOVE    "08"            TO  WRK-KEYYYKTIME-HH
               END-IF
               MOVE    "00"                TO  WRK-KEYYYKTIME-MM
           END-IF
      *
           .
       20011-YYKTIME-CHK-EXT.
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
           MOVE    SPTID-PTNUM         TO  APNTRES-PATIENTID
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
      *    予約氏名チェック処理
      *****************************************************************
       20011-YYKNAME-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-YYKNAME
      *    予約氏名
           IF      WRK-NAME        NOT =   SPACE
      *        全角変換
               MOVE    WRK-NAME            TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               IF      SPA-ENCODING        =   2
      *            拡張漢字対応
                   PERFORM 890-HALF-ZEN02-SEC
               ELSE
                   PERFORM 890-HALF-ZEN-SEC
               END-IF
               MOVE    WRK-ATO-INPUT       TO  WRK-NAME
               IF      FLG-ERR             =   1
                   MOVE    "18"                TO  WRK-ERRCD
               ELSE
                   MOVE    WRK-NAME            TO  WRK-YYKNAME
               END-IF
           END-IF
      *    予約カナ氏名
           IF     (WRK-NAME            =   SPACE )
           AND    (WRK-KANANAME    NOT =   SPACE )
      *        全角変換
               MOVE    WRK-KANANAME        TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               IF      SPA-ENCODING        =   2
      *            拡張漢字対応
                   PERFORM 890-HALF-ZEN02-SEC
               ELSE
                   PERFORM 890-HALF-ZEN-SEC
               END-IF
               MOVE    WRK-ATO-INPUT       TO  WRK-KANANAME
               IF      FLG-ERR             =   1
                   MOVE    "19"                TO  WRK-ERRCD
               ELSE
                   MOVE    WRK-KANANAME        TO  WRK-YYKNAME
               END-IF
           END-IF
      *
           .
       20011-YYKNAME-CHK-EXT.
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
               IF      WRK-SRYNAIYO        =   SPACE
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
           .
       20011-SRYNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約内容チェック
      *****************************************************************
       20011-YYKNAIYO-CHK-SEC      SECTION.
      *
      *    予約内容＝００はシステム管理になくても登録可能
           IF     (WRK-YYKNAIYO        =   "00" )
             OR   (WRK-YYKNAIYO        =   SPACE )
               MOVE    "00"            TO  WRK-YYKNAIYO
               GO      TO      20011-YYKNAIYO-CHK-EXT
           END-IF
      *    診療内容情報の存在チェック
           MOVE    SPACE               TO  SYS-1028-REC
           INITIALIZE                  SYS-1028-REC
           MOVE    "1028"              TO  SYS-1028-KANRICD
           MOVE    WRK-YYKNAIYO        TO  SYS-1028-KBNCD
           IF      WRK-YYKNAIYO        =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    SPA-SYSYMD          TO  SYS-1028-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1028-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1028-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1028-REC
               MOVE    SYS-1028-KBNCD(1:2) TO  WRK-YYKNAIYO
           ELSE
               MOVE    "16"                TO  WRK-ERRCD
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YYKNAIYO        =   SPACE
               MOVE    "16"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-YYKNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約メモ内容チェック処理
      *****************************************************************
       20011-YYKMEMO-CHK-SEC        SECTION.
      *
           IF      WRK-YYKMEMO     NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    WRK-YYKMEMO         TO  WRK-MAE-INPUT
               MOVE    80                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-YYKMEMO
               IF      FLG-ERR             =   1
                   MOVE    "17"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20011-YYKMEMO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約前チェックおよび予約登録
      *****************************************************************
       2002-YYK-ADD-SEC       SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-APNTRES-HEN-SEC
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2002-YYK-ADD-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    予約登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-YYKADD-SYORI-SEC
           END-IF
           .
       2002-YYK-ADD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       20021-APNTRES-HEN-SEC        SECTION.
      *
           IF      WRK-PTNUM           =   SPACE
      *        新規患者
               MOVE    WRK-YYKNAME         TO  APNTRES-NAME
           ELSE
               MOVE    PTINF-NAME          TO  APNTRES-NAME
               MOVE    PTINF-KANANAME      TO  APNTRES-KANANAME
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  APNTRES-BIRTHDAY
               MOVE    PTINF-SEX           TO  APNTRES-SEX
               MOVE    PTINF-HOME-POST     TO  APNTRES-HOME-ZIP-CODE
               STRING  PTINF-HOME-ADRS
                       PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    APNTRES-HOME-ADDRESSES
               END-STRING
           END-IF
      *
           IF      WRK-PTNUM       NOT =   SPACE
      *        保険組合せ編集処理
               PERFORM 20023-HKNCOMBI-HEN-SEC
      *        出力領域保険情報編集
               PERFORM 20023-HKNHENSYU-SEC
           END-IF
      *
           MOVE    WRK-SRYNAIYO        TO  APNTRES-APPOINT-DEP-INFO
           MOVE    WRK-SRYKA-NAME      TO  APNTRES-APPOINT-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  APNTRES-APPOINT-DR-NAME
      *    予約メモ
           MOVE    WRK-YYKMEMO         TO  APNTRES-APPOINT-MEMO
      *
      *    予約確認連絡コード
           PERFORM 200211-KAKREN-HEN-SEC
      *
           .
       20021-APNTRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約確認連絡コード編集処理
      *****************************************************************
       200211-KAKREN-HEN-SEC     SECTION.
      *
      *    予約確認連絡コードの１件目とする
           MOVE    SPACE               TO  SYS-0051-REC
           INITIALIZE                  SYS-0051-REC
           MOVE    SPA-HOSPNUM         TO  SYS-0051-HOSPNUM
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-0051-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-0051-EDYUKYMD
      *
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-0051-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-0051-REC
               MOVE    SYS-0051-KBNCD (1:2)    TO  WRK-KAKREN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最大予約件数
           MOVE    SPACE               TO  SYS-1045-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1045-HOSPNUM
           MOVE    "1045"              TO  SYS-1045-KANRICD
           MOVE    "*"                 TO  SYS-1045-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1045-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1045-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1045-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1045-REC
               MOVE    SYS-1045-YYKMAXCNT  TO  WRK-YYKMAXCNT
           END-IF
           IF      WRK-YYKMAXCNT       =   ZERO
               MOVE    12                  TO  WRK-YYKMAXCNT
           END-IF
           .
       200211-KAKREN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
           IF      FLG-SYORI           =   1
      *        追加処理
               MOVE    "1"                 TO  ORCSAPIHKN-KBN
           ELSE
      *        削除処理
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
           END-IF
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               MOVE    ZERO                    TO  WRK-HKNCOMBI
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
           END-IF
           .
       20023-HKNCOMBI-HEN-EXT.
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
                       UNTIL      (IDX     >   20  )
                             OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
                   IF      ORCSAPIHKN-THKNCOMBI (IDX)  =   WRK-HKNCOMBI
                       PERFORM 200231-HKNHEN-TBL-SEC
                       MOVE    20                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *    残りを編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                       =   SPACE  )
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
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  APNTRES-MAIN-INSURANCE-CLASS
                                                                 (IDY)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  APNTRES-MAIN-INSURANCE-NUMBER
                                                                 (IDY)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  APNTRES-MAIN-INSURANCE-NAME
                                                                 (IDY)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  APNTRES-MAIN-MARK       (IDY)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  APNTRES-MAIN-NUMBER     (IDY)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  APNTRES-MAIN-CONTINUATION(IDY)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  APNTRES-MAIN-ASSISTANCE (IDY)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  APNTRES-MAIN-FAMILY     (IDY)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  APNTRES-MAIN-POLICYHOLDER(IDY)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  APNTRES-MAIN-START-DATE  (IDY)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  APNTRES-MAIN-END-DATE  (IDY)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  APNTRES-INSURANCE-CLASS
                                                             (IDY IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  APNTRES-INSURANCE-NAME
                                                             (IDY IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  APNTRES-PROVIDER-NUMBER
                                                             (IDY IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  APNTRES-RECIPIENT-NUMBER
                                                             (IDY IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  APNTRES-INSURANCE-START-DATE
                                                             (IDY IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  APNTRES-INSURANCE-END-DATE
                                                             (IDY IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  APNTRES-RATE-ADMISSION
                                                             (IDY IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  APNTRES-MONEY-ADMISSION
                                                             (IDY IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  APNTRES-RATE-OUTPATIENT
                                                             (IDY IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  APNTRES-MONEY-OUTPATIENT
                                                             (IDY IDY2)
           END-PERFORM
      *
           .
       20023-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       20022-YYKADD-SYORI-SEC              SECTION.
      *
           IF      WRK-PTNUM           =   SPACE
      *        患者番号なし
               PERFORM 200222-YYKCHEK-NAME-SEC
           ELSE
      *        患者番号あり
               PERFORM 200221-YYKCHEK-SEC
           END-IF
      *    予約追加処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-YYKADD-SEC
           END-IF
      *
           .
       20022-YYKADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約二重登録チェック処理
      *****************************************************************
       200221-YYKCHEK-SEC            SECTION.
      * 
      *    予約マスタ読み込み (key4)
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    SPA-PTID            TO  YYK-PTID
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-YYK
           END-IF
           PERFORM UNTIL   (FLG-YYK        =   1   )
                       OR  (WRK-ERRCD  NOT =   SPACE )
      *        来院済みは対象としない
               IF     (YYK-RAIINZUMI       =   "1"  )
               OR     (YYK-YYKID           =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 2002211-YYK-CK-SEC
               END-IF
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200221-YYKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約の時間チェック
      *****************************************************************
       2002211-YYK-CK-SEC            SECTION.
      *
      *    診療内容・ドクター・時間帯が一致は二重登録
           IF     (YYK-SRYNAIYO        =   WRK-SRYNAIYO )
             AND  (YYK-DRCD            =   WRK-DRCD     )
             AND  (YYK-KEYYYKTIME      =   WRK-KEYYYKTIME)
               MOVE    "20"                TO  WRK-ERRCD
           END-IF
      *
           .
       2002211-YYK-CK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約二重登録チェック処理
      *****************************************************************
       200222-YYKCHEK-NAME-SEC            SECTION.
      * 
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-YYK
           END-IF
           PERFORM UNTIL   (FLG-YYK        =   1   )
                       OR  (WRK-ERRCD  NOT =   SPACE )
      *        来院済みは対象としない
               IF     (YYK-RAIINZUMI       =   "1"  )
               OR     (YYK-YYKID           =   ZERO )
                   CONTINUE
               ELSE
                   IF      YYK-NAME            =   WRK-YYKNAME
                       MOVE    "20"                TO  WRK-ERRCD
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200222-YYKCHEK-NAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       20023-YYKADD-SEC              SECTION.
      *
      *    明細レコード最大件数
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-YYKID
           IF      FLG-YYK             =   ZERO
               MOVE    YYK-YYKID           TO  WRK-YYKID
               IF      WRK-YYKID           =   99
                   MOVE    "50"            TO  WRK-ERRCD
                   GO      TO      20023-YYKADD-EXT
               END-IF
               ADD     1                   TO  WRK-YYKID
           END-IF
      *
      *    予約ＩＤ＝００検索
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    ZERO                TO  YYK-PTID
           PERFORM 910-YYK-SELECT-SEC
           IF      FLG-YYK             =   ZERO
               ADD     1                   TO  YYK-YYKCNT
               MOVE    YYK-YYKCNT          TO  WRK-YYKCNT
      *        予約最大件数
               IF      WRK-YYKCNT          >   WRK-YYKMAXCNT
                   ADD     1                   TO  IDK
                   MOVE    "K4"            TO  WRK-KEICD (IDK)
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
               MOVE    WRK-OPID            TO  YYK-OPID
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "51"             TO  WRK-ERRCD
                   DISPLAY "YYK-KEY UPD ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO      TO      20023-YYKADD-EXT
               END-IF
           ELSE
               MOVE    SPACE               TO  YYK-REC
               INITIALIZE                      YYK-REC
               MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
               MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
               MOVE    WRK-DRCD            TO  YYK-DRCD
               MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
               MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
               MOVE    ZERO                TO  YYK-YYKID
               MOVE    WRK-YYKMAXCNT       TO  YYK-YYKMAXCNT
               MOVE    1                   TO  YYK-YYKCNT
               MOVE    1                   TO  WRK-YYKCNT
               MOVE    ZERO                TO  YYK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
               MOVE    WRK-OPID            TO  YYK-OPID
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "51"             TO  WRK-ERRCD
                   DISPLAY "YYK-KEY INS ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO      TO      20023-YYKADD-EXT
               END-IF
           END-IF
      *
      *    明細レコード追加
           IF      WRK-YYKID           =   ZERO
               MOVE    WRK-YYKCNT          TO  WRK-YYKID
           END-IF
      *    登録内容編集
           MOVE    WRK-YYKID           TO  APNTRES-APPOINT-ID
      *    予約区分
           MOVE    WRK-YYKNAIYO        TO  APNTRES-APPOINT-INFO
      *    予約日＜システム日付
           IF      WRK-APPOINT-YMD     <   SPA-SYSYMD
               ADD     1                   TO  IDK
               MOVE    "K5"                TO  WRK-KEICD (IDK)
           END-IF
      *
      *    明細レコード追加
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    WRK-YYKID           TO  YYK-YYKID
      *
           MOVE    WRK-APPOINT-HHMM    TO  YYK-YYKTIME
           MOVE    SPA-PTID            TO  YYK-PTID
           IF      WRK-PTNUM           =   SPACE
               MOVE    WRK-YYKNAME         TO  YYK-NAME
           ELSE
               MOVE    PTINF-NAME          TO  YYK-NAME
           END-IF
           MOVE    WRK-SRYKA           TO  YYK-SRYKA
           MOVE    WRK-YYKNAIYO        TO  YYK-YYKKBN
           MOVE    WRK-KAKREN          TO  YYK-KAKREN
      *
           MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
           MOVE    WRK-OPID            TO  YYK-OPID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "51"             TO  WRK-ERRCD
               DISPLAY "Y01 YYK MEI INS ERR:" MCP-RC "."
           END-IF
      *
      *    予約メモ登録
           IF     (WRK-ERRCD           =   SPACE )
            AND   (WRK-YYKMEMO     NOT =   SPACE )
               PERFORM 200231-YYKCOM-ADD-SEC
           END-IF
           .
       20023-YYKADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約メモ登録処理
      *****************************************************************
       200231-YYKCOM-ADD-SEC           SECTION.
      *
      *    レコード追加
           MOVE    SPACE               TO  YYKCOM-REC
           INITIALIZE                      YYKCOM-REC
           MOVE    SPA-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    WRK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYKCOM-DRCD
           MOVE    WRK-APPOINT-YMD     TO  YYKCOM-YYKYMD
           MOVE    WRK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
           MOVE    WRK-YYKID           TO  YYKCOM-YYKID
           MOVE    1                   TO  YYKCOM-RENNUM
           MOVE    WRK-YYKMEMO         TO  YYKCOM-COMMENT
      *
           MOVE    SMCNDATE-YMD        TO  YYKCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYKCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYKCOM-UPHMS
           MOVE    WRK-OPID            TO  YYKCOM-OPID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "52"             TO  WRK-ERRCD
               DISPLAY "Y01 YYKCOM INS ERR :"  YYKCOM-KEY
                                  ",MCP-RC:" MCP-RC "@"
           END-IF
           .
       200231-YYKCOM-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約削除
      *****************************************************************
       2003-YYK-DELETE-SEC     SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-APNTRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2003-YYK-DELETE-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    予約ＩＤ編集
           IF      WRK-APPOINT-ID   NOT =   SPACE
      *        数値変換
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-APPOINT-ID      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   5     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "26"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(11:5)   TO  WRK-APPOINT-ID
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *    予約削除処理
               IF      WRK-APPOINT-ID      =   SPACE OR  ZERO
      *        予約ＩＤなし
                   PERFORM 20032-SAKUJO-NASI-SEC
               ELSE
      *        予約ＩＤあり
                   PERFORM 20031-SAKUJO-ARI-SEC
               END-IF
           END-IF
           .
       2003-YYK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約取消処理
      *****************************************************************
       20032-SAKUJO-NASI-SEC             SECTION.
      *
           DISPLAY "予約削除(ID なし )  " WRK-APPOINT-ID
      *
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  FLG-YYKOK
           MOVE    ZERO                TO  WRK-YYKCNT
           INITIALIZE                      WRK-YYK-REC
           PERFORM UNTIL   (FLG-YYK        =   1   )
               IF     (YYK-YYKID       NOT =   ZERO )
      *            来院済・予約時間不一致は対象としない
                   IF     (YYK-RAIINZUMI       =   "1"  )
                   OR     (YYK-YYKTIME     NOT =   WRK-APPOINT-HHMM )
                       CONTINUE
                   ELSE
                       IF      WRK-PTNUM           =   SPACE
                           IF      YYK-NAME        =   WRK-YYKNAME
                                MOVE    YYK-REC         TO  WRK-YYK-REC
                                MOVE    1               TO  FLG-YYKOK
                           END-IF
                       ELSE
                           IF      YYK-PTID        =   SPA-PTID
                               MOVE    YYK-REC         TO  WRK-YYK-REC
                               MOVE    1               TO  FLG-YYKOK
                           END-IF
                       END-IF
                   END-IF
      *            最大件数
                   ADD     1                   TO  WRK-YYKCNT
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-YYKOK           =   1
      *        予約削除処理
               MOVE    WRK-YYK-REC         TO  YYK-REC
               PERFORM 200321-YYKDEL-SYORI-SEC
           ELSE
      *        削除なし
               MOVE    "25"                TO  WRK-ERRCD
           END-IF
           .
       20032-SAKUJO-NASI-EXT.
           EXIT.
      *****************************************************************
      *    予約削除処理
      *****************************************************************
       200321-YYKDEL-SYORI-SEC       SECTION.
      *
      *    予約ＩＤ≠００削除
           PERFORM 200321-YYK-DELETE-SEC
      *    予約メモ削除
           PERFORM 200321-YYKCOM-DEL-SEC
      *
      *    削除内容編集
           PERFORM 2003211-DELETE-HEN-SEC
      *
      *    明細レコード最大件数
      *    前予約ＩＤ＝００修正
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YYK-SELECT-SEC
           IF      FLG-YYK             =   ZERO
               COMPUTE YYK-YYKCNT          =   YYK-YYKCNT  -   1
               PERFORM 2003212-YYK-UPDATE-SEC
           END-IF
           .
       200321-YYKDEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約取消内容編集処理
      *****************************************************************
       2003211-DELETE-HEN-SEC             SECTION.
      *
      *    予約ＩＤ編集
           MOVE    YYK-YYKID           TO  APNTRES-APPOINT-ID
           MOVE    YYK-SRYNAIYO        TO  APNTRES-APPOINT-DEP-INFO
      *    予約区分
           MOVE    YYK-YYKKBN          TO  APNTRES-APPOINT-INFO
      *    診療科
           MOVE    YYK-SRYKA           TO  WRK-SRYKA
           PERFORM 20011-SRYKA-CHK-SEC
           MOVE    WRK-SRYKA           TO  APNTRES-APPOINT-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  APNTRES-APPOINT-DEP-NAME
      *
           .
       2003211-DELETE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約削除処理
      *****************************************************************
       200321-YYK-DELETE-SEC             SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =  ZERO
               MOVE    "53"            TO  WRK-ERRCD
               DISPLAY "YYK DEL ERR:" MCP-RC "."
                       ",KEY:" YYK-KEY
           END-IF
           .
       200321-YYK-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    予約更新処理
      *****************************************************************
       2003212-YYK-UPDATE-SEC          SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
           MOVE    WRK-OPID            TO  YYK-OPID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "52"             TO  WRK-ERRCD
               DISPLAY "YYK UPD ERR:" MCP-RC 
                       ",KEY:" YYK-KEY
           END-IF
           .
       2003212-YYK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    予約メモ削除処理
      *****************************************************************
       200321-YYKCOM-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  YYKCOM-REC
           INITIALIZE                      YYKCOM-REC
           MOVE    YYK-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    YYK-SRYNAIYO        TO  YYKCOM-SRYNAIYO
           MOVE    YYK-DRCD            TO  YYKCOM-DRCD
           MOVE    YYK-YYKYMD          TO  YYKCOM-YYKYMD
           MOVE    YYK-KEYYYKTIME      TO  YYKCOM-KEYYYKTIME
           MOVE    YYK-YYKID           TO  YYKCOM-YYKID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       49031-YYKCOM-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約取消処理
      *****************************************************************
       20031-SAKUJO-ARI-SEC             SECTION.
      *
           DISPLAY "予約削除(ID あり )  " WRK-APPOINT-ID
      *    予約マスタ削除(key4 で対象を削除)
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           MOVE    WRK-KEYYYKTIME      TO  YYK-KEYYYKTIME
           MOVE    WRK-APPOINT-ID      TO  YYK-YYKID
      *
           PERFORM 910-YYK-SELECT-SEC
      *
           MOVE    ZERO                TO  FLG-YYKOK
           IF      FLG-YYK             =   ZERO
               IF     (SPA-PTID        NOT =   ZERO     )
               AND    (SPA-PTID            =   YYK-PTID )
                   MOVE    1               TO  FLG-YYKOK
               END-IF
               IF     (WRK-PTNUM           =   SPACE    )
               AND    (WRK-YYKNAME         =   YYK-NAME )
                   MOVE    1               TO  FLG-YYKOK
               END-IF
           END-IF
      *
           IF      FLG-YYKOK           =   1
      *        予約削除処理
               PERFORM 200321-YYKDEL-SYORI-SEC
           ELSE
      *        削除なし
               MOVE    "27"                TO  WRK-ERRCD
           END-IF
           .
       20031-SAKUJO-ARI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       890-HALF-ZEN-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       890-HALF-ZEN02-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "2"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-MAE-INPUT
           IF     (KANACHK2-RC     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN02-EXT.
           EXIT.
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
               STRING  "患者番号・予約氏名・予約カナ氏名のいずれかを"
                       "設定して下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "02"
               MOVE    "予約日が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "予約時間が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "診療科が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "ドクターが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "予約日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "予約時間設定誤り"
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
               MOVE    "予約内容が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "予約メモに登録できない文字があります"
                                               TO  WRK-ERRMSG
           WHEN    "18"
               MOVE    "予約氏名に登録できない文字があります"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               MOVE    "予約カナ氏名に登録できない文字があります"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               STRING  "診療内容・ドクター・予約時間帯で予約登録済み"
                       "です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "25"
               MOVE    "削除対象の予約レコードが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "予約ＩＤ設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "27"
      *****    MOVE    "予約ＩＤと患者番号が一致しません"
               MOVE    "予約ＩＤの予約情報と患者情報が一致しません"
                                               TO  WRK-ERRMSG
           WHEN    "50"
               STRING  "予約ＩＤが９９まで登録済みです。"
                       "これ以上予約できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "51"
               MOVE    "予約登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "予約メモ登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "53"
               MOVE    "予約更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "54"
               MOVE    "予約削除エラー"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K3"
               MOVE    "診療内容情報を自動設定しました"
                                               TO  WRK-ERRMSG
           WHEN    "K4"
               STRING  "予約枠の最大件数以上の登録です。"
                       "予約件数がオーバーしています"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "K5"
               STRING  "予約日＜システム日付です。"
                       "過去日の予約です"
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
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YYK-SELECT-SEC         SECTION.
      *
           MOVE    YYK-REC         TO  MCPDATA-REC
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-YYK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       910-YYK-READ-SEC            SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *     DISPLAY "mcp-window = "    MCP-WINDOW 
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "Y01"               TO  SPA-MOTOPG
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
      *    MOVE    "Y01"               TO  SPA-MOTOPG
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

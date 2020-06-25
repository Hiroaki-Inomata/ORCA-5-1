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
       PROGRAM-ID.         ORAPI011R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 受付一覧
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/27    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-多々納  13/08/01  受付ＩＤ追加対応
      *  04.08.01    NACL-多々納  16/10/XX  保険組合せ番号追加対応
      *  04.08.01    NACL-多々納  17/04/XX  予約ＩＤ追加対応
      *  05.00.00    NACL-多々納  18/11/XX  メモ登録有追加対応
      *  05.00.00    NACL-多々納  19/06/XX  受付順変更対応
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
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-PTMEMOINF           PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-NO                  PIC 9(01).
      *
           03  FLG-SORT-OK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDD                     PIC 9(04).
           03  IDK                     PIC 9(04).
           03  IDH                     PIC 9(04).
      *
           03  IDX-KEY                 PIC 9(04).
           03  IDX-MAX                 PIC 9(04).
      *
           03  TBL-MAX                 PIC 9(04).
           03  SORT-MAX                PIC 9(04).
           03  IDZ                     PIC 9(04).
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
           03  WRK-HKNCOMBI            PIC X(04).
      *H30.11
      *    患者メモ登録
           03  WRK-MEMO-AREA.
               05  WRK-MEMO1-00            PIC X(01).
               05  WRK-MEMO2-00            PIC X(01).
               05  WRK-MEMO1-KA            PIC X(01).
               05  WRK-MEMO2-KA            PIC X(01).
      *    SORT用
           03  WRK-SORT-KEY.
               05  WRK-SORT-HYOJI                PIC X(04).
               05  WRK-SORT-TIME                 PIC X(04).
      *
       01  WRK-XML-AREA.
           03  WRK-ACCEPT-YMD          PIC X(08).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-SRYNAIYO            PIC X(02).
           03  WRK-SRYNAIYO-NAME       PIC X(24).
      *
      *    システム管理テーブル
      *    診療科テーブル
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-G             OCCURS   99.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-SRYKA-NAME      PIC X(80).
      *    ドクターテーブル
       01  TBL-DRCD-AREA.
           03  TBL-DRCD-G             OCCURS   99.
               05  TBL-DRCD            PIC X(05).
               05  TBL-DRCD-NAME       PIC X(80).
      *
      *    受付テーブル
       01  TBL-UKETUKE-AREA.
           02  TBL-UKETUKE-REC          OCCURS   500.
           COPY    "CPUKETUKE.INC"     REPLACING
                                       //UKE-// BY //TBL-UKE-//.
      *      SORT用
             03  TBL-SORT-NARABE.
               05  TBL-SORT-HYOJI                PIC X(04).
               05  TBL-SORT-TIME                 PIC X(04).
      *
      *    受付テーブル
       01  WRK-UKETUKE-AREA.
           02  WRK-UKETUKE-REC          OCCURS   500.
           COPY    "CPUKETUKE.INC"     REPLACING
                                       //UKE-// BY //WRK-UKE-//.
      *      SORT用
             03  WRK-SORT-NARABE.
               05  WRK-SORT-HYOJI                PIC X(04).
               05  WRK-SORT-TIME                 PIC X(04).
      *
      *   最大件数
       01  MAX-LINE                    PIC 9(03)   VALUE  500.
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPACCPTLSTV2REQ.INC".
      *    API XML送信用定義体
           COPY    "CPACCPTLSTV2RES.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *   （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    患者メモタ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
           COPY    "MCPDATA.INC".
      *
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
           DISPLAY   "******************"
           DISPLAY   "* acceptlst start*"
           DISPLAY   "******************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTLSTMAKE-SEC
           END-IF
      *
           DISPLAY   "******************"
           DISPLAY   "* acceptlst end  *"
           DISPLAY   "******************"
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
           INITIALIZE                      XML-ACCEPTLSTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  ACPLSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACPLSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  ACPLSTRES-INFORMATION-TIME
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
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  ACPLSTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST-CLASS
           WHEN    "01"
      *        受付中
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        会計済
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        受付
               MOVE    3                   TO  FLG-SYORI
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
      *    受付一覧処理
           PERFORM 2002-UKETUKE-LIST-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-ACCEPTLSTREQ
           INITIALIZE                      XML-APIREQ
      ***  INITIALIZE                      LNK-ACCEPTLSTREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acceptlstv2req"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "acceptlstreq"      TO  APIREQ-RECORDNAME
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
           MOVE    "xml_acceptlstv2req"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "acceptlstreq"      TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-ACCEPTLSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTLSTMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  ACPLSTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  ACPLSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  ACPLSTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acceptlstv2res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  ACPLSTRES-MODE
           MOVE    "acceptlstres"      TO  ACPLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ACCEPTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_acceptlstv2res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  ACPLSTRES-MODE
           MOVE    "acceptlstres"      TO  ACPLSTRES-RECORDNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-ACCEPTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
               MOVE    ACPLSTRES-OBJECT    TO  APILST-BODY 
               MOVE    "application/xml"   TO  APILST-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTLSTMAKE-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF  ACPLSTREQ-ACCEPT-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  ACPLSTREQ-ACCEPT-DATE
           END-IF
           IF  ACPLSTREQ-ACCEPT-DEP-CODE   =   LOW-VALUE
               MOVE  SPACE             TO  ACPLSTREQ-ACCEPT-DEP-CODE
           END-IF
           IF  ACPLSTREQ-ACCEPT-DR-CODE    =   LOW-VALUE
               MOVE  SPACE             TO  ACPLSTREQ-ACCEPT-DR-CODE
           END-IF
           IF  ACPLSTREQ-ACCEPT-DEP-INFO   =   LOW-VALUE
               MOVE  SPACE             TO  ACPLSTREQ-ACCEPT-DEP-INFO
           END-IF
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
      *    受付チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-UKEYMD-CHK-SEC
           END-IF
      *
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
           IF     (WRK-SRYNAIYO    NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYNAIYO-CHK-SEC
           END-IF
      *
      *R01.6
           IF     (FLG-SYORI           =   1      )
           AND    (WRK-ERRCD           =   SPACE  )
      *        受付順SORT区分
               PERFORM 20011-SORT-CHK-SEC
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
      *    診療科
           MOVE    ACPLSTREQ-ACCEPT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    ACPLSTREQ-ACCEPT-DR-CODE
                                           TO  WRK-DRCD
      *    診療内容区分
           MOVE    ACPLSTREQ-ACCEPT-DEP-INFO
                                           TO  WRK-SRYNAIYO
      *    受付日
           MOVE    ACPLSTREQ-ACCEPT-DATE   TO  ACPLSTRES-ACCEPT-DATE
      *    数字のみに編集
           MOVE    ACPLSTREQ-ACCEPT-DATE   TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-ACCEPT-YMD
      *    受付日未設定はシステム日付
           IF      WRK-ACCEPT-YMD     =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-ACCEPT-YMD
               MOVE    WRK-ACCEPT-YMD      TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  ACPLSTRES-ACCEPT-DATE
           END-IF
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付日チェック
      *****************************************************************
       20011-UKEYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-ACCEPT-YMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-ACCEPT-YMD
           END-IF
      *
           .
       20011-UKEYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科チェック
      *****************************************************************
       20011-SRYKA-CHK-SEC          SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1005-STYUKYMD
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "12"                TO  WRK-ERRCD
               MOVE    WRK-SRYKA           TO  ACPLSTRES-ACCEPT-DEP-CODE
                                                                  (1)
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
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1010-STYUKYMD
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    WRK-DRCD            TO  ACPLSTRES-ACCEPT-DR-CODE
                                                                  (1)
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
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1012-STYUKYMD
           MOVE    WRK-ACCEPT-YMD      TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1012-REC
               MOVE    SYS-1012-SRYNAIYODSP    TO  WRK-SRYNAIYO-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
               MOVE    WRK-SRYNAIYO        TO  ACPLSTRES-ACCEPT-DEP-INFO
                                                                  (1)
           END-IF
           .
       20011-SRYNAIYO-CHK-EXT.
           EXIT.
      *
      *R01.6
      *****************************************************************
      *    受付順SORT区分チェック処理
      *****************************************************************
       20011-SORT-CHK-SEC       SECTION.
      *
      *R01.6
      *    受付SORT区分
           MOVE    ZERO                TO  FLG-SORT-OK
      *    受付中のみ
           EVALUATE    ACPLSTREQ-DISPLAY-SORT
               WHEN    "True"
                   MOVE    1               TO  FLG-SORT-OK
           END-EVALUATE
           .
       20011-SORT-CHK-EXT.
           EXIT.
      *R01.6
      *
      *****************************************************************
      *    受付一覧編集処理
      *****************************************************************
       2002-UKETUKE-LIST-SEC       SECTION.
      *
      *    システム管理テーブル編集
           PERFORM 2009-SYSKANRITBL-SEC
      *
      *    受一覧編集
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    WRK-ACCEPT-YMD      TO  UKE-UKEYMD
           MOVE    WRK-SRYKA           TO  UKE-SRYKA
           MOVE    WRK-DRCD            TO  UKE-DRCD
           MOVE    WRK-SRYNAIYO        TO  UKE-SRYNAIYO
      *
           EVALUATE    FLG-SYORI
           WHEN    1
      *        受付中
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           WHEN    2
      *        受診済
               MOVE    "key9"              TO  WRK-MCP-PATHNAME
           WHEN    3
      *        受付・受診
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL      (FLG-UKETUKE     =   1   )
                         OR   (IDX             >   500 )
               MOVE    ZERO                TO  FLG-NO
               IF     (UKE-UKEID           =   ZERO     )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        診療科
               IF     (WRK-SRYKA       NOT =   SPACE     )
                  AND (WRK-SRYKA       NOT =   UKE-SRYKA )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        ドクター
               IF     (WRK-DRCD        NOT =   SPACE  )
                  AND (WRK-DRCD        NOT =   UKE-DRCD  )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        診療内容
               IF     (WRK-SRYNAIYO    NOT =   SPACE  )
                  AND (WRK-SRYNAIYO    NOT =   UKE-SRYNAIYO )
                   MOVE    1                   TO  FLG-NO
               END-IF
               IF      FLG-NO              =   ZERO
                   ADD     1               TO  IDX
                   IF      IDX             >   500
                       MOVE    "20"            TO  WRK-ERRCD
                   ELSE
      *R01.6
      *              受付順SORTあり
                     IF     (FLG-SYORI       =   1  )
                       AND  (FLG-SORT-OK     =   1  )
                       PERFORM 20023-UKETUKETBL-HEN-SEC
                     ELSE
                       PERFORM 20021-UKETUKELST-HEN-SEC
                     END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "21"            TO  WRK-ERRCD
      *        １件目に入力値を編集する
               IF      WRK-SRYKA       NOT =   SPACE
                   MOVE    WRK-SRYKA       TO  ACPLSTRES-ACCEPT-DEP-CODE
                                                                  (1)
                   MOVE    WRK-SRYKA-NAME  TO  ACPLSTRES-ACCEPT-DEP-NAME
                                                                  (1)
               END-IF
               IF      WRK-DRCD        NOT =   SPACE
                   MOVE    WRK-DRCD        TO  ACPLSTRES-ACCEPT-DR-CODE
                                                                  (1)
                   MOVE    WRK-DRCD-NAME   TO  ACPLSTRES-ACCEPT-DR-NAME
                                                                  (1)
               END-IF
               IF      WRK-SRYNAIYO    NOT =   SPACE
                   MOVE    WRK-SRYNAIYO    TO  ACPLSTRES-ACCEPT-DEP-INFO
                                                                  (1)
               END-IF
      *R01.06
           ELSE
      *        受付順SORTあり
               IF     (FLG-SYORI       =   1  )
                 AND  (FLG-SORT-OK     =   1  )
                   PERFORM 20024-UKETUKESORT-HEN-SEC
               END-IF
      *R01.06
           END-IF
           .
       2002-YYK-LIST-EXT.
           EXIT.
      *****************************************************************
      *    受付情報編集処理
      *****************************************************************
       20021-UKETUKELST-HEN-SEC        SECTION.
      *
      *    受付時間
           MOVE    UKE-UKEYMD          TO  WRK-SYMD
           MOVE    UKE-UKETIME         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-TIME        TO  ACPLSTRES-ACCEPT-TIME (IDX)
      *ver.4.7
      *    受付ＩＤ
           MOVE    UKE-UKEID           TO  ACPLSTRES-ACCEPT-ID  (IDX)
      *    診療内容
           MOVE    UKE-SRYNAIYO        TO  ACPLSTRES-ACCEPT-DEP-INFO
                                                                 (IDX)
      *    ドクター
           MOVE    UKE-DRCD            TO  ACPLSTRES-ACCEPT-DR-CODE
                                                                 (IDX)
           PERFORM VARYING    IDD      FROM    1   BY  1
                   UNTIL     (IDD      >   99  )
                         OR  (TBL-DRCD (IDD)   =   SPACE )
               IF      TBL-DRCD (IDD)      =   UKE-DRCD
                   MOVE    TBL-DRCD-NAME (IDD)
                                       TO  ACPLSTRES-ACCEPT-DR-NAME
                                                                 (IDX)
                   MOVE    99          TO  IDD
               END-IF
           END-PERFORM
      *    診療科
           MOVE    UKE-SRYKA           TO  ACPLSTRES-ACCEPT-DEP-CODE
                                                                 (IDX)
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL     (IDK      >   99  )
                         OR  (TBL-SRYKA (IDK)   =   SPACE )
               IF      TBL-SRYKA (IDK)     =   UKE-SRYKA
                   MOVE    TBL-SRYKA-NAME (IDK)
                                       TO  ACPLSTRES-ACCEPT-DEP-NAME
                                                                 (IDX)
                   MOVE    99          TO  IDK
               END-IF
           END-PERFORM
      *    会計時間
           IF      UKE-KAIKEITIME      =   SPACE
                                       OR  ZERO
               CONTINUE
           ELSE
               MOVE    UKE-KAIKEITIME      TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-TIME        TO  ACPLSTRES-ACCOUNT-TIME
                                                                (IDX)
           END-IF
      *    送受信フラグ
           MOVE    UKE-SRFLG           TO  ACPLSTRES-CLAIM-INFO (IDX)
      *H29.4
      *    予約情報
           IF      UKE-YYKTIME     NOT =   ZERO
      *        予約時間
               MOVE    UKE-YYKTIME         TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-TIME        TO  ACPLSTRES-APPOINT-TIME
                                                                (IDX)
      *        予約ＩＤ
               MOVE    UKE-YYKID           TO  ACPLSTRES-APPOINT-ID
                                                                (IDX)
           END-IF
      *
      *
           IF      UKE-PTID            =   ZERO
      *        新規患者
               MOVE    UKE-NAME            TO  ACPLSTRES-NAME  (IDX)
           ELSE
      *        患者情報検索
               PERFORM 200212-PTINF-HEN-SEC
      *
      *H30.11
      *        メモ登録検索
               PERFORM 200214-PTMEMOINF-SEC
           END-IF
      *
      *R01.6
      *    表示順編集
           IF      FLG-SYORI           =   1
      *        受付順
               IF      UKE-HYOJIJUN        =   ZERO
                   MOVE    ALL "9"         TO  ACPLSTRES-SORT-NUMBER
                                                               (IDX)
               ELSE
                   MOVE    UKE-HYOJIJUN    TO  ACPLSTRES-SORT-NUMBER
                                                               (IDX)
               END-IF
      *        受付順変更区分
               IF      UKE-KAKREN      =   "1"
                   MOVE    "True"          TO  ACPLSTRES-SORT-MARK(IDX)
               END-IF
           END-IF
           .
       20021-UKETUKELST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       200212-PTINF-HEN-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    UKE-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    UKE-NAME            TO  PTINF-NAME
           END-IF
      *
           MOVE    PTINF-NAME          TO  ACPLSTRES-NAME     (IDX)
           MOVE    PTINF-KANANAME      TO  ACPLSTRES-KANANAME (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACPLSTRES-BIRTHDAY (IDX)
           MOVE    PTINF-SEX           TO  ACPLSTRES-SEX      (IDX)
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    UKE-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  ACPLSTRES-PATIENTID (IDX)
           END-IF
      *    保険組合せ
           IF      UKE-HKNCOMBI    NOT =   ZERO
               MOVE    UKE-HKNCOMBI    TO  WRK-HKNCOMBI
               PERFORM 202122-HKNCOMBI-HEN-SEC
           END-IF
           .
       200212-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       202122-HKNCOMBI-HEN-SEC     SECTION.
      *
      *
           MOVE    UKE-PTID            TO  SPA-PTID
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
      *    出力領域保険情報編集
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   20  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDH)
                                           =   SPACE  )
               IF      ORCSAPIHKN-THKNCOMBI (IDH)  =   WRK-HKNCOMBI
                   PERFORM 2002101-HKNHEN-TBL-SEC
                   MOVE    20                  TO  IDH
               END-IF
           END-PERFORM
           .
       202122-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
      *H28.10
      *    保険組合せ番号
           MOVE    ORCSAPIHKN-THKNCOMBI (IDH)
                                   TO  ACPLSTRES-COMBINATION-NUMBER
                                                                (IDX)
      *H29.4
      *    保険組合せ非表示
           EVALUATE    ORCSAPIHKN-HYOJIKBN (IDH)
               WHEN    "2"
      *            入院非表示
                   MOVE    "I"         TO  ACPLSTRES-COMBINATION-HYOJI
                                                            (IDX)
               WHEN    "3"
      *            外来非表示
                   MOVE    "O"         TO  ACPLSTRES-COMBINATION-HYOJI
                                                            (IDX)
               WHEN    OTHER
      *            非表示なし
                   MOVE    "N"         TO  ACPLSTRES-COMBINATION-HYOJI
                                                            (IDX)
           END-EVALUATE
      *
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDH)
                                   TO  ACPLSTRES-MAIN-INSURANCE-CLASS
                                                                (IDX)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDH)
                                   TO  ACPLSTRES-MAIN-INSURANCE-NUMBER
                                                                (IDX)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDH)
                                   TO  ACPLSTRES-MAIN-INSURANCE-NAME
                                                                (IDX)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDH)
                                   TO  ACPLSTRES-MAIN-MARK    (IDX)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDH)
                                   TO  ACPLSTRES-MAIN-NUMBER  (IDX)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDH)
                                   TO  ACPLSTRES-MAIN-CONTINUATION
                                                                (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDH)
                                   TO  ACPLSTRES-MAIN-ASSISTANCE
                                                                (IDX)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDH)
                                   TO  ACPLSTRES-MAIN-FAMILY   (IDX)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDH)
                                   TO  ACPLSTRES-MAIN-POLICYHOLDER
                                                                (IDX)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDH)
                                   TO  ACPLSTRES-MAIN-START-DATE
                                                                (IDX)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDH)
                                   TO  ACPLSTRES-MAIN-END-DATE (IDX)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDH IDY2)
                                       TO  ACPLSTRES-INSURANCE-CLASS
                                                         (IDX IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDH IDY2)
                                       TO  ACPLSTRES-INSURANCE-NAME
                                                         (IDX IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDH IDY2)
                                       TO  ACPLSTRES-PROVIDER-NUMBER
                                                         (IDX IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDH IDY2)
                                       TO  ACPLSTRES-RECIPIENT-NUMBER
                                                         (IDX IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDH IDY2)
                                   TO  ACPLSTRES-INSURANCE-START-DATE
                                                         (IDX IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDH IDY2)
                                       TO  ACPLSTRES-INSURANCE-END-DATE
                                                         (IDX IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDH IDY2)
                                       TO  ACPLSTRES-RATE-ADMISSION
                                                         (IDX IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDH IDY2)
                                       TO  ACPLSTRES-MONEY-ADMISSION
                                                         (IDX IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDH IDY2)
                                       TO  ACPLSTRES-RATE-OUTPATIENT
                                                         (IDX IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDH IDY2)
                                       TO  ACPLSTRES-MONEY-OUTPATIENT
                                                         (IDX IDY2)
           END-PERFORM
      *
           .
       2002101-HKNHENSYU-EXT.
           EXIT.
      *
      *H30.11
      *****************************************************************
      *    メモ登録検索編集処理
      *****************************************************************
       200214-PTMEMOINF-SEC     SECTION.
      *
           INITIALIZE                  WRK-MEMO-AREA
      *
      *    患者メモ検索
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    UKE-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    UKE-PTID            TO  PTMEMOINF-PTID
           MOVE    UKE-UKEYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           PERFORM UNTIL       (FLG-PTMEMOINF  =   1 )
      *
      *        診療科=00
               IF      PTMEMOINF-SRYKA         =   "00"
                   IF      PTMEMOINF-MEMOKBN   =   "1"
      *                メモ１
                       MOVE    "1"             TO  WRK-MEMO1-00
                   ELSE
                       MOVE    "1"             TO  WRK-MEMO2-00
                   END-IF
               END-IF
      *        受付診療科
               IF     (PTMEMOINF-SRYKA     =   UKE-SRYKA )
                 AND  (PTMEMOINF-SRYKA NOT =   "00"  )
                   IF      PTMEMOINF-MEMOKBN   =   "1"
      *                メモ１
                       MOVE    "1"             TO  WRK-MEMO1-KA
                   ELSE
                       MOVE    "1"             TO  WRK-MEMO2-KA
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科＝００
           IF     (WRK-MEMO1-00        =   "1"  )
              AND (WRK-MEMO2-00        =   "1"  )
               MOVE    "3"             TO  ACPLSTRES-MEMO-00 (IDX)
           END-IF
           IF     (WRK-MEMO1-00        =   "1"  )
              AND (WRK-MEMO2-00        =   SPACE)
               MOVE    "1"             TO  ACPLSTRES-MEMO-00 (IDX)
           END-IF
           IF     (WRK-MEMO1-00        =   SPACE)
              AND (WRK-MEMO2-00        =   "1"  )
               MOVE    "2"             TO  ACPLSTRES-MEMO-00 (IDX)
           END-IF
      *
      *    診療科≠００
           IF     (WRK-MEMO1-KA        =   "1"  )
              AND (WRK-MEMO2-KA        =   "1"  )
               MOVE    "3"             TO  ACPLSTRES-MEMO-KA (IDX)
           END-IF
           IF     (WRK-MEMO1-KA        =   "1"  )
              AND (WRK-MEMO2-KA        =   SPACE)
               MOVE    "1"             TO  ACPLSTRES-MEMO-KA (IDX)
           END-IF
           IF     (WRK-MEMO1-KA        =   SPACE)
              AND (WRK-MEMO2-KA        =   "1"  )
               MOVE    "2"             TO  ACPLSTRES-MEMO-KA (IDX)
           END-IF
           .
       200214-PTMEMOINF-EXT.
           EXIT.
      *H30.11
      *
      *R01.06
      *****************************************************************
      *    受付順SORTあり編集処理
      *****************************************************************
       20023-UKETUKETBL-HEN-SEC     SECTION.
      *
           MOVE    IDX                 TO  TBL-MAX
      *
           MOVE    UKETUKE-REC         TO  TBL-UKETUKE-REC (IDX)
      *    受付順
           MOVE    UKE-UKETIME         TO  TBL-SORT-TIME   (IDX)
           IF      UKE-HYOJIJUN        =   ZERO
               MOVE    ALL "9"         TO  TBL-SORT-HYOJI (IDX)
           ELSE
               MOVE    UKE-HYOJIJUN    TO  TBL-SORT-HYOJI (IDX)
           END-IF
           .
       20023-UKETUKETBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付順SORT処理
      *****************************************************************
       20024-UKETUKESORT-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-UKETUKE-AREA
           MOVE    TBL-UKETUKE-AREA    TO  WRK-UKETUKE-AREA
           MOVE    SPACE           TO  TBL-UKETUKE-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >=   MAX-LINE )
                       OR (IDX     >    TBL-MAX  )
               MOVE    HIGH-VALUE          TO  WRK-SORT-KEY
               MOVE    ZERO                TO  IDX-KEY
               PERFORM VARYING IDZ     FROM    1       BY  1
                       UNTIL  (IDZ     >=   MAX-LINE )
                           OR (IDX     >    TBL-MAX  )
                   IF      WRK-SORT-NARABE (IDZ)   NOT =   SPACE
                       IF      WRK-SORT-NARABE (IDZ)
                                           <   WRK-SORT-KEY
                           MOVE    WRK-SORT-NARABE (IDZ)
                                               TO  WRK-SORT-KEY
                           MOVE    IDZ             TO  IDX-KEY
                        END-IF
                    END-IF
               END-PERFORM
               IF      IDX-KEY             =   ZERO
                   MOVE    MAX-LINE        TO  IDX
               ELSE
                   MOVE    WRK-UKETUKE-REC (IDX-KEY)
                                           TO  TBL-UKETUKE-REC
                                                        (IDX)
                   MOVE    SPACE           TO  WRK-SORT-NARABE
                                                        (IDX-KEY)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >=   MAX-LINE )
                       OR (IDX     >    TBL-MAX  )
               MOVE    TBL-UKETUKE-REC (IDX)
                                           TO  UKETUKE-REC
               PERFORM 20021-UKETUKELST-HEN-SEC
           END-PERFORM
           .
       20024-UKETUKESORT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       2009-SYSKANRITBL-SEC     SECTION.
      *
           INITIALIZE                  TBL-DRCD-AREA
           INITIALIZE                  TBL-SRYKA-AREA
      *
      *    ドクターテーブル
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    WRK-ACCEPT-YMD     TO  SYS-1010-STYUKYMD
           MOVE    WRK-ACCEPT-YMD     TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDD
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDD            >=  99  )
               ADD     1                   TO  IDD
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-KBNCD      TO  TBL-DRCD      (IDD)
               MOVE    SYS-1010-NAME       TO  TBL-DRCD-NAME (IDD)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科テーブル
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    WRK-ACCEPT-YMD     TO  SYS-1005-STYUKYMD
           MOVE    WRK-ACCEPT-YMD     TO  SYS-1005-EDYUKYMD
      *
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDK
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDK           >=  99  )
               ADD     1                   TO  IDK
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  TBL-SRYKA     (IDK)
               MOVE    SYS-1005-SRYKANAME  TO  TBL-SRYKA-NAME(IDK)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2009-SYSKANRITBL-EXT.
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
           IF      WRK-TSS             =   SPACE
               MOVE    ZERO                TO  WRK-TSS
           END-IF
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
           WHEN    "11"
               MOVE    "受付日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "診療内容情報が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "受付対象件数が５００件以上あります。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "対象の受付はありませんでした。"
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
      *    受付マスター読込処理
      *****************************************************************
       900-UKETUKE-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
               INITIALIZE                      UKETUKE-REC
           END-IF
      *
           .
       900-UKETUKE-READ-EXT.
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
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
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

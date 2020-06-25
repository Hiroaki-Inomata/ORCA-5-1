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
       PROGRAM-ID.             ORCGW06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 職員情報（Ｗ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-藤原     01/09/11  削除処理の修正 
      * 01.01.01     MCC-藤原     01/09/21  JOINの修正      
      * 01.01.02     MCC-森脇     02/05/14  麻薬ライセンスの追加
      *                                     画面の変更      
      * 01.01.03     MCC-多々納   02/06/13  オルカマスターの
      *                                     削除を非活性にする
      * 01.01.04     MCC-太田     02/06/20  人名辞書マスタ追加
      * 01.01.05     NACL-森脇    02/07/30  薬剤情報マスタ追加
      * 01.01.06     NACL-藤原    02/09/01  看護婦の名称変更
      * 01.01.07     NACL-森脇    02/11/08  入院対応
      * 01.01.08     NACL-多々納  02/11/13  一括入力 
      * 02.00.01     NACL-森脇    04/02/06  プログラム管理の追加
      * 02.00.02     NACL-森脇    04/08/04  住所マスタの追加
      * 02.00.03     NACL-森脇    05/04/25  ヘルプマスタの追加
      * 03.00.01     NACL-多々納  05/08/29  画面変更
      *                                     診療行為初期表示追加
      * 03.00.02     NACL-多々納  05/12/01  請求書発行フラグ等追加
      *                                     MONFUNC 対応
      * 03.00.03     NACL-多々納  06/04/11  診療費明細書発行追加
      * 03.04.00     NACL-多々納  06/11/13  検査正式名称表示区分の追加
      * 03.05.00     NACL-多々納  07/05/02  グループ診療対応
      * 03.05.01     NACL-藤原    07/09/14  クライアント保存可否フラグ追加
      * 04.00.00     NACL-多々納  07/10/05  SYSUSER登録対応
      * 04.04.00     NACL-太田    08/01/27  他院設定追加
      * 04.04.01     NACL-森脇    09/03/13  外部まとめ追加
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月改正対応
      * 04.07.00     NACL-太田    12/05/27  タブラベル対応
      * 04.07.01     NACL-森脇    13/07/05  業務処理権限変更不可
      *                           13/07/17  スーパーバイザー処理変更
      * 04.07.01     NACL-太田    13/07/22  ＩＣカードリーダ／ライタ設定追加
      * 04.08.00     NACL-森脇    13/04/03  パスワード追加
      * 04.07.00     NACL-多々納  14/02/10  入力スクロール行数対応他
      * 04.08.01     NACL-森脇    14/09/02  GINBEE対応
      * 04.08.02     NACL-森脇    14/11/13  OPID変更時対応
      * 04.08.03     NACL-森脇    14/11/19  パスワード削除処理対応
      * 04.08.04     NACL-森脇    14/12/10  職員情報削除時出力先設定削除
      * 04.08.05     NACL-森脇    15/08/31  管理者有効期間チェック
      * 04.08.00     NACL-多々納  15/10/22  期間変更追加処理対応
      * 04.08.00     NACL-多々納  15/11/XX  職員一覧処理対応
      * 04.08.06     NACL-森脇    16/06/14  一般ユーザー処理制限
      * 05.00.00     NACL-森脇    19/06/04  職員情報設定ログイン中ユーザ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W06SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
           03  FLG-PASSWD          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-USERID-UPD      PIC 9(01).
           03  FLG-USERMATCH       PIC 9(01).
      *----(04.08.05)--ADD-START---
           03  FLG-KIKAN1          PIC 9(01).
           03  FLG-KIKAN2          PIC 9(01).
           03  FLG-KIKAN3          PIC 9(01).
           03  FLG-KIKAN4          PIC 9(01).
      *----(04.08.05)--ADD-END-----
           03  FLG-SAME-USERID     PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-GRP             PIC 9(04).
      *
           03  IDX0                PIC 9(04).
           03  IDX2                PIC 9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    職員区分
           03  WRK-MAE-SYOKUKBN-G.
               05  WRK-MAE-SYOKUKBN       PIC X(01).
               05  WRK-MAE-SYOKUKBNF      PIC X(01).
               05  WRK-MAE-SYOKUKBNMEI    PIC X(18).
      *
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-CD              PIC X(02).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-PAGENO          PIC 9(03).
      *
           03  WRK-MAE-USERID      PIC X(16).
           03  WRK-GRPUSERID-WIDGET.
               05  WRK-GRPUSERIDFL         PIC X(09).
               05  WRK-GRPUSERIDNUM        PIC 9(02).
           03  WRK-ERRCUR          PIC 9(03).
      *
           03  WRK-PASSWORD        PIC X(128).
      *----(04.08.05)--ADD-START---
           03  WRK-KEY-SYOKUCOD    PIC X(04).
           03  WRK-KEY-FLG         PIC 9(01).
           03  WRK-USERID          PIC X(16).
      *----(04.08.05)--ADD-END-----
           03  WRK-1010-USERID     PIC X(16).
           03  WRK-LENGTH          PIC 9(03).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-GRPHOSP-MAX   PIC 9(02)   VALUE   9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    職員情報２
           COPY  "CPSK1050.INC".
      *    基本情報
           COPY  "CPSK1001.INC".
      *    基本情報
           COPY  "CPSK1038.INC".
      *----(04.08.04)--ADD-START---
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *    帳票プログラム選択情報
           COPY  "CPSK1032.INC".
      *    出力先プリンタ名割り当て情報（入院）
           COPY  "CPSK1034.INC".
      *    帳票プログラム選択情報 （入院）
           COPY  "CPSK1035.INC".
      *----(04.08.04)--ADD-END-----
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *    パスワード
           COPY    "CPPASSWD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    カナチェック
           COPY  "CPORCSKANACHK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    パスワード変換
           COPY    "CPORCSAUTH.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
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
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W06
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W06         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W06"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
      *!!!
      *H27.11
               WHEN    "W061"
                   IF     SPA-W061-SEL-SYOKUCD     =   SPACE
                       IF     (SPA-GMN-SYOKUCOD    NOT =   SPACE)
                         AND  (SPA-GMN-SELNUM          >   ZERO )
      *                    最新の内容に置換える
                           PERFORM 2002-SYOKUCOD-SYORI-SEC
                           MOVE    SPA-GMN-SELNUM  TO  W06-SELNUM
                           PERFORM 2003-SELNUM-CHK-SEC
                       END-IF
                   ELSE
      *                職員選択
                       PERFORM 340-W061-SET-SEC
                   END-IF
                   INITIALIZE                  SPA-W061-AREA
      *!!!
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *!!!
      *H27.11
      *****************************************************************
      *    職員一覧選択後処理
      *****************************************************************
       340-W061-SET-SEC              SECTION.
      *
           MOVE    SPA-W061-SEL-SYOKUCD    TO  W06-SYOKUCOD
           MOVE    ZERO                TO  FLG-OK
           PERFORM 2002-SYOKUCOD-SYORI-SEC
      *
           IF      SPA-GMN-TBL-MAX     >   1
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL      IDX      >  SPA-GMN-TBL-MAX
                   IF      SPA-NAI-TSTYUKYMD (IDX)
                                           =   SPA-W061-SEL-STYUKYMD
                       MOVE    IDX             TO  SPA-GMN-SELNUM
                       MOVE    SPA-GMN-TBL-MAX TO  IDX
                   END-IF
               END-PERFORM
               IF      SPA-GMN-SELNUM      >   ZERO
                   MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
                   PERFORM 2003-SELNUM-CHK-SEC
               END-IF
           END-IF
           .
       340-W061-SET-EXT.
           EXIT.
      *H27.11
      *!!!
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
      *----(04.07.01)--UPD-START---
                       MOVE    6           TO  SPA-GMN-CUR
      *----(04.07.01)--UPD-END-----
                   END-IF
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
      *----(04.07.01)--ADD-START---
               WHEN    "1004"
                   IF      SPA-WID1-FLG    =   "OK"
      *----(04.08.05)--UPD-START---
                       PERFORM 210-BACK-USER-CHK-SEC
      *----(04.08.05)--UPD-END-----
                   ELSE
                       MOVE    6           TO  SPA-GMN-CUR
                   END-IF
      *----(04.07.01)--ADD-END-----
      *H27.10
      *        追加登録
               WHEN    "1005"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
      *                更新へ
                       MOVE    2               TO  SPA-GMN-SYORI
                       MOVE    "1002"          TO  SPA-WIDCD
      *                確認画面表示
                       MOVE    SPACE           TO  SPA-WID1-FLG
                       PERFORM 520-WIDSET-SEC
                       GO  TO  330-WID1-SET-EXT
                   END-IF
      *
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W06
      *
      *    職員区分
           MOVE    "1 医師"            TO  SPA-GMN-SYOKUKBN-LIST (1)
           MOVE    "2 看護師（婦）"    TO  SPA-GMN-SYOKUKBN-LIST (2)
           MOVE    "3 技師"            TO  SPA-GMN-SYOKUKBN-LIST (3)
           MOVE    "4 事務職"          TO  SPA-GMN-SYOKUKBN-LIST (4)
           MOVE    "5 管理職"          TO  SPA-GMN-SYOKUKBN-LIST (5)
      *----(05.00.00)--UPD-START---
      *    医療機関番号が２以上は 0 マスター なし
           IF      SPA-HOSPNUM         >=  02
               MOVE    "9 ログイン中"      TO  SPA-GMN-SYOKUKBN-LIST (6)
               MOVE    6                   TO  SPA-SYOKUKBN-MAX
           ELSE
               MOVE    "0 マスター"        TO  SPA-GMN-SYOKUKBN-LIST (6)
               MOVE    "9 ログイン中"      TO  SPA-GMN-SYOKUKBN-LIST (7)
               MOVE    7                   TO  SPA-SYOKUKBN-MAX
           END-IF
      *----(05.00.00)--UPD-END-----
      *    管理者権限
           MOVE    "管理者である"      TO  SPA-GMN-KANRIAUTH-LST(1)
           MOVE    "管理者でない"      TO  SPA-GMN-KANRIAUTH-LST(2) 
           MOVE    2                   TO  SPA-KANRIAUTHFLG-MAX
      *    診療行為初期表示
           MOVE    "1 入院画面"        TO  SPA-GMN-ORCA21FLG-LST (1)
           MOVE    "2 外来画面"        TO  SPA-GMN-ORCA21FLG-LST (2)
           MOVE    2                   TO  SPA-ORCA21FLG-MAX
      *    表示区分
           MOVE    "表示"              TO  SPA-GMN-HYOJIFLG-LST (1)
           MOVE    "非表示"            TO  SPA-GMN-HYOJIFLG-LST (2)
           MOVE    2                   TO  SPA-HYOJIFLG-MAX
      *
      *    診療科をシステム日付で検索
           MOVE    SPA-SYSYMD          TO  SPA-SRYKA-YMD
           PERFORM 3201-SRYKA-HEN-SEC
      *
      *    請求書発行
           MOVE    "0 発行しない"      TO  SPA-GMN-SKYPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-SKYPRTFLG-LST (2)
           MOVE    "2 発行する（請求あり）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (4)
           MOVE    4                   TO  SPA-SKYPRTFLG-MAX
      *    院外処方せん発行フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-SYOHOPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYOHOPRTFLG-LST (2)
           MOVE    "2 院内処方発行"    TO  SPA-GMN-SYOHOPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）"
                                       TO  SPA-GMN-SYOHOPRTFLG-LST (4)
           MOVE    "4 院内処方発行（訂正時なし）" 
                                       TO  SPA-GMN-SYOHOPRTFLG-LST (5)
           MOVE    5                   TO  SPA-SYOHOPRTFLG-MAX
      *    薬剤情報発行フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-YAKUJYOFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-YAKUJYOFLG-LST (2)
           MOVE    "2 発行する（訂正時なし）" 
                                       TO  SPA-GMN-YAKUJYOFLG-LST (3)
           MOVE    3                   TO  SPA-YAKUJYOFLG-MAX
      *    診療費明細書フラグ
           MOVE    "0 発行しない"      TO  SPA-GMN-MEIPRTFLG-LST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-MEIPRTFLG-LST (2)
           MOVE    "2 発行する（請求あり）" 
                                       TO  SPA-GMN-MEIPRTFLG-LST (3)
           MOVE    "3 発行する（訂正時なし）" 
                                       TO  SPA-GMN-MEIPRTFLG-LST (4)
           MOVE    4                   TO  SPA-MEIPRTFLG-MAX
      *    検査正式名称表示
           MOVE    "0 使用しない"      TO  SPA-GMN-FORMALFLG-LST(1)
           MOVE    "1 使用する"        TO  SPA-GMN-FORMALFLG-LST(2)
           MOVE    2                   TO  SPA-FORMALFLG-MAX
      *    クライアント保存可否
           MOVE    "0 保存不可"        TO  SPA-GMN-CLIENT-SAVE-LST(1)
           MOVE    "1 保存可"          TO  SPA-GMN-CLIENT-SAVE-LST(2)
           MOVE    2                   TO  SPA-CLIENT-SAVE-MAX
      *    ユーザーログイン権限
           MOVE    "0 表示しない"      TO  SPA-GMN-USER-KANRI-LST(1)
           MOVE    "1 閲覧のみ可(自院のみ)"
                                       TO  SPA-GMN-USER-KANRI-LST(2)
           MOVE    "2 閲覧のみ可(全て)"
                                       TO  SPA-GMN-USER-KANRI-LST(3)
           MOVE    "3 更新も可(自院のみ)"
                                       TO  SPA-GMN-USER-KANRI-LST(4)
           MOVE    "4 更新も可(全て)"  TO  SPA-GMN-USER-KANRI-LST(5)
           MOVE    5                   TO  SPA-USER-KANRI-MAX
      *    プリンタへのデータ送信単位
           MOVE    "0 １ページ毎に送信"
                                       TO  SPA-GMN-PRTCONF-LST(1)
           MOVE    "1 帳票毎に送信"
                                       TO  SPA-GMN-PRTCONF-LST(2)
           MOVE    2                   TO  SPA-PRTCONF-MAX
      *    クライアント印刷可否
           MOVE    "0 しない"          TO  SPA-GMN-CLIENT-PRT-LST(1)
           MOVE    "1 する"            TO  SPA-GMN-CLIENT-PRT-LST(2)
           MOVE    2                   TO  SPA-CLIENT-PRT-MAX
      *    ダイアログ表示
           MOVE    "0 表示しない"      TO  SPA-GMN-DIALOGFLG-LST(1)
           MOVE    "1 表示する"        TO  SPA-GMN-DIALOGFLG-LST(2)
           MOVE    2                   TO  SPA-DIALOGFLG-MAX
      *    ＩＣカードリーダ／ライタ
           MOVE    "0 使用しない"      TO  SPA-GMN-ICCARDRW-LST(1)
           MOVE    "1 使用する"        TO  SPA-GMN-ICCARDRW-LST(2)
           MOVE    2                   TO  SPA-ICCARDRW-MAX
      *
      *    基本情報の発行フラグ
           PERFORM 3202-KIHON-HEN-SEC
      *
      *----(04.08.05)--UPD-START---
      *!!!!
      *H27.11
      *    入力者の管理者判定
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-STAFFCD         TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF     (SYS-1010-KANRIAUTHFLG   =   "T"  )
                   MOVE    1               TO  SPA-NAI-STAFFKANRIKBN
      *            スーパーバイザー
                   MOVE    SYS-1010-USERID TO  WRK-USERID
                   PERFORM 49001-SYSUSER-CHK-SEC
                   IF    ( FLG-USERID-UPD  =   ZERO ) AND
                         ( SYSUSER-SUPERVISOR  =   "T")
                       MOVE    2           TO  SPA-NAI-STAFFKANRIKBN
                   END-IF
               END-IF
           END-IF
      *!!!!
           IF      SPA-NAI-STAFFKANRIKBN   =   ZERO
               PERFORM 3102-INIT-GMN-IPPAN-SEC
           ELSE
               PERFORM 3102-SPASET-KANRI-SEC
           END-IF
      *----(04.08.05)--UPD-END-----
           . 
       310-SPASET-EXT.
           EXIT.
      *----(04.08.05)--ADD-START--
      *****************************************************************
      *    管理者初期表示編集処理
      *****************************************************************
       3102-SPASET-KANRI-SEC       SECTION.
      *
      *    １件目の職員情報読み込み
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL    FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) NOT =   "0"
                   MOVE    1               TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF      SYS-1010-KBNCD(1:1) =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  SPA-GMN-SYOKUKBN-G
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SYOKUKBN-G  =   SPACE
               MOVE    SPA-GMN-SYOKUKBN-LIST (1)
                                           TO  SPA-GMN-SYOKUKBN-G
           END-IF
      *
           PERFORM 320-SYOKUCOD-HENSYU-SEC
           IF      SPA-SYOKUCOD-MAX    >   ZERO
               MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                           TO  SPA-GMN-SYOKUCOD-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SYOKUCOD-G
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGENO
           . 
       3102-SPASET-KANRI-EXT.
           EXIT.
      *****************************************************************
      *    一般初期表示編集処理
      *****************************************************************
       3102-INIT-GMN-IPPAN-SEC     SECTION.
      *
           MOVE    SPA-STAFFCD(1:1)    TO  SPA-GMN-SYOKUKBN-G
           MOVE    SPA-STAFFCD(2:4)    TO  SPA-GMN-SYOKUCOD-G
           PERFORM 320-SYOKUCOD-HENSYU-SEC
      *
      *----(05.00.00)--ADD-START---
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF       SPA-GMN-SYOKUKBN   =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  SPA-GMN-SYOKUKBN-G
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *----(05.00.00)--ADD-END-----
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYOKUCOD-MAX)
               IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD(IDX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST(IDX)
                                               TO  SPA-GMN-SYOKUCOD-G
                   MOVE    SPA-SYOKUCOD-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    職員の情報を読み込み
           PERFORM 20021-SYOKUCOD-CHK-SEC
      *
           IF      SPA-GMN-TBL-MAX     >   1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >   SPA-GMN-TBL-MAX
               IF  ( SPA-NAI-TSTYUKYMD (IDX)   <=   SPA-SYSYMD )
               AND ( SPA-NAI-TEDYUKYMD (IDX)   >=   SPA-SYSYMD )
                   MOVE    IDX             TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-TBL-MAX TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-GMN-SELNUM  =   ZERO
                   MOVE    SPA-GMN-TBL-MAX     TO  SPA-GMN-SELNUM
               END-IF
               MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
               PERFORM 2003-SELNUM-CHK-SEC
           END-IF
      *
      *----(04.08.06)--UPD-START---
           MOVE    81                  TO  SPA-GMN-CUR
      *----(04.08.06)--UPD-END-----
           MOVE    1                   TO  SPA-GMN-PAGENO
           . 
       3102-INIT-GMN-IPPAN-EXT.
           EXIT.
      *****************************************************************
      *    管理者初期表示編集処理
      *****************************************************************
       3102-INIT-GMN-KANRI-SEC     SECTION.
      *
           PERFORM 320-SYOKUCOD-HENSYU-SEC
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYOKUCOD-MAX)
               IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD(IDX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST(IDX)
                                               TO  SPA-GMN-SYOKUCOD-G
                   MOVE    SPA-SYOKUCOD-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               IF      SPA-SYOKUCOD-MAX    >   ZERO
                   MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                               TO  SPA-GMN-SYOKUCOD-G
               ELSE
                   MOVE    SPACE               TO  SPA-GMN-SYOKUCOD-G
               END-IF
           END-IF
      *
           IF      SPA-GMN-CUR         =   1   OR  2
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           MOVE    1                   TO  SPA-GMN-PAGENO
           . 
       3102-INIT-GMN-KANRI-EXT.
           EXIT.
      *----(04.08.05)--ADD-END----
      *----(04.08.05)--UPD-START--
      *****************************************************************
      *    職員情報初期表示編集処理
      *****************************************************************
       3101-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYORI
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
           INITIALIZE                      SPA-PAGE2-G
           INITIALIZE                      SPA-PAGE3-G
      *
           IF      SPA-NAI-STAFFKANRIKBN   =   ZERO
               PERFORM 3102-INIT-GMN-IPPAN-SEC
           ELSE
               PERFORM 3102-INIT-GMN-KANRI-SEC
           END-IF
           . 
       3101-INIT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    職員区分毎の職員編集処理 
      *****************************************************************
       320-SYOKUCOD-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYOKUCOD-LIST-G
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-KEY-FLG
           MOVE    SPACE               TO  WRK-KEY-SYOKUCOD
           PERFORM UNTIL       (IDX             >= 100 )  OR
                               (FLG-SYSKANRI    =   1  )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(2:4) NOT =   WRK-KEY-SYOKUCOD
                   ADD     1                   TO  IDX
                   MOVE    ZERO                TO  WRK-KEY-FLG
                   MOVE    SYS-1010-KBNCD(2:4) TO  WRK-KEY-SYOKUCOD
                                               SPA-GMN-TSYOKUCOD(IDX)
               END-IF
               IF     (SYS-1010-STYUKYMD   <=  SPA-SYSYMD  ) AND
                      (SYS-1010-EDYUKYMD   >=  SPA-SYSYMD  )
                   MOVE    SYS-1010-NAME   TO
                                           SPA-GMN-TSYOKUCOD-MEI(IDX)
                   MOVE    1               TO  WRK-KEY-FLG
               END-IF
               IF      WRK-KEY-FLG         =   ZERO
                   MOVE    SYS-1010-NAME   TO
                                           SPA-GMN-TSYOKUCOD-MEI(IDX)
               END-IF
               MOVE    IDX                 TO  SPA-SYOKUCOD-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (SPA-SYOKUCOD-MAX    =   100 )  AND
                  (FLG-SYSKANRI        =   ZERO)
      *        １００件以上あり
               PERFORM 3202-SYOKUCOD-HENSYU2-SEC
           END-IF
      *
           . 
       320-SYOKUCOD-HENSYU-EXT.
           EXIT.
      *----(04.08.05)--UPD-END----
      *****************************************************************
      *    職員区分毎の職員編集処理２
      *****************************************************************
       3202-SYOKUCOD-HENSYU2-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYOKUCOD-LIST-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    "%"                 TO  SYS-1010-KBNCD (2:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       (IDX             >= 100 )  OR
                               (FLG-SYSKANRI    =   1  )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF     (SYS-1010-STYUKYMD   <=  SPA-SYSYMD  ) AND
                      (SYS-1010-EDYUKYMD   >=  SPA-SYSYMD  )
                   ADD     1                   TO  IDX
                   MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-TSYOKUCOD
                                                              (IDX)
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-TSYOKUCOD-MEI
                                                               (IDX)
                   MOVE    IDX                 TO  SPA-SYOKUCOD-MAX
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           . 
       3202-SYOKUCOD-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ専門科コード初期編集処理 
      *****************************************************************
       3201-SRYKA-HEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SEMONKACD-AREA
      *    診療科コード
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >   99  )  OR
                              (FLG-SYSKANRI  =   1 )
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD  TO  SPA-GMN-SRYKA (IDX)
               MOVE    SYS-1005-SRYKANAME 
                                       TO  SPA-GMN-SRYKAMEI (IDX)
               MOVE  IDX               TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           . 
       3201-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報の発行フラグ編集処理 
      *****************************************************************
       3202-KIHON-HEN-SEC              SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD 
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                      SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-SKYPRTFLG      TO  SPA-1001-SKYPRTFLG
           MOVE    SYS-1001-SYOHOPRTFLG    TO  SPA-1001-SYOHOPRTFLG
           MOVE    SYS-1001-YAKUJYOPRTFLG  TO  SPA-1001-YAKUJYOPRTFLG
           MOVE    SYS-1001-MEIPRTFLG      TO  SPA-1001-MEIPRTFLG
      *
      *    検査正式名称表示
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1038-STYUKYMD 
           MOVE    SPA-SYSYMD          TO  SYS-1038-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
           ELSE
               INITIALIZE                      SYS-1038-REC
           END-IF
      *
           MOVE    SYS-1038-FORMALFLG      TO  SPA-1038-FORMALFLG 
           . 
       3202-KIHON-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *----(04.08.05)--ADD-START---
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 210-BACK-USER-CHK2-SEC
      *----(04.08.05)--ADD-END-----
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
      *----(04.07.01)--UPD-START---
                   PERFORM 210-BACK-CHK-SEC
      *----(04.07.01)--UPD-END-----
      *    クリア
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM  402-CLERA-SEC
      *    削除
               WHEN    "CLICKED"      ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *!!!
      *H27.11
      *    一覧へ
               WHEN    "CLICKED"      ALSO    "B05"
                   PERFORM 460-W061-SEL-SEC
      *H27.11
      *!!!
      *    タグ切替
               WHEN    "CLICKED"      ALSO    "B08"
                   PERFORM 480-BETUGAMEN-SEC
      *    業務権限
               WHEN    "CLICKED"      ALSO    "CHK01"
               WHEN    "CLICKED"      ALSO    "CHK02"
               WHEN    "CLICKED"      ALSO    "CHK03"
               WHEN    "CLICKED"      ALSO    "CHK04"
               WHEN    "CLICKED"      ALSO    "CHK05"
               WHEN    "CLICKED"      ALSO    "CHK06"
               WHEN    "CLICKED"      ALSO    "CHK07"
               WHEN    "CLICKED"      ALSO    "CHK08"
               WHEN    "CLICKED"      ALSO    "CHK09"
               WHEN    "CLICKED"      ALSO    "CHK10"
               WHEN    "CLICKED"      ALSO    "CHK11"
               WHEN    "CLICKED"      ALSO    "CHK12"
               WHEN    "CLICKED"      ALSO    "CHK13"
               WHEN    "CLICKED"      ALSO    "CHK14"
               WHEN    "CLICKED"      ALSO    "CHK15"
               WHEN    "CLICKED"      ALSO    "CHK16"
               WHEN    "CLICKED"      ALSO    "CHK17"
               WHEN    "CLICKED"      ALSO    "CHK18"
               WHEN    "CLICKED"      ALSO    "CHK19"
               WHEN    "CLICKED"      ALSO    "CHK20"
               WHEN    "CLICKED"      ALSO    "CHK21"
               WHEN    "CLICKED"      ALSO    "CHK22"
               WHEN    "CLICKED"      ALSO    "CHK23"
               WHEN    "CLICKED"      ALSO    "CHK24"
               WHEN    "CLICKED"      ALSO    "CHK25"
      *----(01.01.05)----START------------
               WHEN    "CLICKED"      ALSO    "CHK26"
      *----(01.01.05)----END--------------
      *----(01.01.07)----START------------
               WHEN    "CLICKED"      ALSO    "CHK27"
               WHEN    "CLICKED"      ALSO    "CHK28"
               WHEN    "CLICKED"      ALSO    "CHK29"
               WHEN    "CLICKED"      ALSO    "CHK30"
               WHEN    "CLICKED"      ALSO    "CHK31"
               WHEN    "CLICKED"      ALSO    "CHK32"
      *----(01.01.07)----END--------------
      *----(02.00.01)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK33"
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK34"
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
               WHEN    "CLICKED"      ALSO    "CHK35"
      *----(02.00.03)--ADD-END-----
               WHEN    "CLICKED"      ALSO    "CHK36"
                   PERFORM 4119-GSRAUTH-CHK-SEC
      *    タグ切替え
      *    WHEN    "CLICKED"       ALSO    "TGN1"
      *    WHEN    "CLICKED"       ALSO    "TGN2"
      *    WHEN    "CLICKED"       ALSO    "TGN3"
      *        PERFORM 460-TAGUCHG-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    SPA-GMN-CUR         TO  WRK-GMN-CUR
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        職員区分入力
               WHEN    "ACTIVATE"      ALSO    "SYOKUKBN"
                   PERFORM 2001-SYOKUKBN-SYORI-SEC
      *        職員番号入力
               WHEN    "ACTIVATE"      ALSO    "SYOKUCOD"
                   PERFORM 2002-SYOKUCOD-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 2003-SELNUM-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "KIKAN_LIST"
                   PERFORM 2004-KIKANLIST-SEC
      *       タブ切替え
               WHEN    "SWITCH"        ALSO    "nt1"
                   PERFORM 460-TAGUCHG-SEC
      *        パスワード再入力
               WHEN    "ACTIVATE"      ALSO    "PASSWORD2"
                   PERFORM 4121-PASSWORD-CHK-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員区分入力処理
      *****************************************************************
       2001-SYOKUKBN-SYORI-SEC         SECTION.
      *
           MOVE    SPA-GMN-SYOKUKBN-G  TO  WRK-MAE-SYOKUKBN-G
      *
           MOVE    W06-SYOKUKBN        TO  SPA-GMN-SYOKUKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *----(05.00.00)--ADD-START---
           IF      SPA-GMN-SYOKUKBN(1:1)   =   "9"
               PERFORM 3102-INIT-GMN-IPPAN-SEC
               GO  TO  2001-SYOKUKBN-SYORI-EXT
           END-IF
      *----(05.00.00)--ADD-END-----
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF      SPA-GMN-SYOKUKBN    =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  SPA-GMN-SYOKUKBN-G
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-SYOKUKBN    NOT =   WRK-MAE-SYOKUKBN
                   INITIALIZE                      SPA-GMN-AREA2
                   INITIALIZE                      SPA-GMN-KOUMOKU
                   INITIALIZE                      SPA-PAGE2-G
                   INITIALIZE                      SPA-PAGE3-G
      *            職員編集
                   PERFORM 320-SYOKUCOD-HENSYU-SEC
                   IF      SPA-SYOKUCOD-MAX    >   ZERO
                        MOVE    SPA-GMN-SYOKUCOD-LIST(1)
                                           TO  SPA-GMN-SYOKUCOD-G
                   ELSE
                       MOVE     SPACE      TO  SPA-GMN-SYOKUCOD-G
                   END-IF
               END-IF
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           .
       2001-SYOKUKBN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員コード入力処理
      *****************************************************************
       2002-SYOKUCOD-SYORI-SEC         SECTION.
      *
           MOVE    W06-SYOKUCOD        TO  SPA-GMN-SYOKUCOD-G
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-SYOKUCOD(1:4)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-SYOKUCOD
           END-IF
      **** 9999 はエラー
      *    IF      SPA-GMN-SYOKUCOD    =   "9999"
      *        MOVE    "0015"              TO  SPA-ERRCD
      *        MOVE    2                   TO  SPA-GMN-CUR
      **** END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SYOKUCOD-MAX) OR
                              (SPA-ERRCD   NOT =   SPACE   )
               IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD
                                                         (IDX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST (IDX)
                                               TO  SPA-GMN-SYOKUCOD-G
                   MOVE    SPA-SYOKUCOD-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF     SPA-ERRCD            =   SPACE
      *        職員編集
               PERFORM 20021-SYOKUCOD-CHK-SEC
           END-IF
      *H30.10
      *    9999 はエラー（登録済みの削除を可能）
           IF      SPA-GMN-SYOKUCOD    =   "9999"
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGENO
      *
           .
       2002-SYOKUCOD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    職員コードチェック処理
      *****************************************************************
       20021-SYOKUCOD-CHK-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-GMN-KOUMOKU
           INITIALIZE                      SPA-PAGE2-G
           INITIALIZE                      SPA-PAGE3-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    "%"                 TO  SYS-1010-KBNCD (6:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >     10   )  OR
                              (FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               ADD     1                   TO  SPA-GMN-TBL-MAX
               MOVE    IDX                 TO  SPA-GMN-TBANGO    (IDX)
               MOVE    SYS-1010-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-1010-STYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-1010-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-1010-EDYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-GMN-TBL-MAX     =   ZERO
      *        新規
               MOVE    1                   TO  SPA-GMN-SYORI
               MOVE    3                   TO  SPA-GMN-CUR
               PERFORM 200212-INIT-GMN-SEC
           ELSE
               IF      FLG-OK              =   ZERO
                   IF      SPA-SYOKUCOD-MAX     <  100
                       ADD     1               TO  SPA-SYOKUCOD-MAX
                   END-IF
                   MOVE    SYS-1010-KBNCD (2:4)
                                           TO  SPA-GMN-TSYOKUCOD
                                                   (SPA-SYOKUCOD-MAX)
                   MOVE    SYS-1010-NAME   TO  SPA-GMN-TSYOKUCOD-MEI
                                                   (SPA-SYOKUCOD-MAX)
                   MOVE    SPA-GMN-SYOKUCOD-LIST (SPA-SYOKUCOD-MAX)
                                           TO  SPA-GMN-SYOKUCOD-G
               END-IF
      *
               MOVE    2                   TO  SPA-GMN-SYORI
               IF      SPA-GMN-TBL-MAX     =   1
      *            １件を選択とする
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
                   PERFORM 2003-SELNUM-CHK-SEC
               ELSE
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       20021-SYOKUCOD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新規初期表示処理
      *****************************************************************
       200212-INIT-GMN-SEC       SECTION.
      *
      *    管理者権限（管理者でない）
           MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                       TO  SPA-GMN-KANRIAUTH-MEI
           MOVE    "F"                 TO  SPA-NAI-KANRIAUTHFLG
      *    診療行為初期表示
           IF      SPA-BEDFLG          >  ZERO
               MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                          TO  SPA-GMN-ORCA21FLG-G
           ELSE
               MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                          TO  SPA-GMN-ORCA21FLG-G
           END-IF
      *    業務権限
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           PERFORM 320-SPASET-GSRAUTH-SEC
      *
      *----(04.08.01)--UPD-START---
           IF      SPA-MW      =   SPA-GINBEE
               MOVE    "1"                 TO  SPA-GMN-CLIENT-SAVE
           ELSE
               MOVE    "0"                 TO  SPA-GMN-CLIENT-SAVE
           END-IF
      *----(04.08.01)--UPD-END-----
           MOVE    "0"                 TO  SPA-GMN-USER-KANRI
           MOVE    "0"                 TO  SPA-GMN-PRTCONF
           MOVE    "0"                 TO  SPA-GMN-CLIENT-PRT
           MOVE    "0"                 TO  SPA-GMN-DIALOGFLG
           MOVE    "0"                 TO  SPA-GMN-ICCARDRW
      *    表示区分
           MOVE    "0"                 TO  SPA-NAI-HYOJIFLG
      *
           PERFORM 20091-PRTFLG-HEN-SEC
      *
           .
       200212-INIT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    番号選択チェック処理
      *****************************************************************
       2003-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    W06-SELNUM          TO  SPA-GMN-SELNUM
      *
      *H26.5
           IF      SPA-GMN-SELNUM      =   ZERO
      *        最終履歴が999999であれば、選択する
               IF     (SPA-GMN-TBL-MAX     >   ZERO    )
                 AND  (SPA-NAI-TEDYUKYMD (SPA-GMN-TBL-MAX)
                                           =   "99999999" )
                   MOVE    SPA-GMN-TBL-MAX     TO  SPA-GMN-SELNUM
               END-IF
           END-IF
      *
      *
           IF     (SPA-GMN-SELNUM      >=  1   )  AND
                  (SPA-GMN-SELNUM      <=  SPA-GMN-TBL-MAX)
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    SPA-GMN-TSTYUKYMD(IDX)  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-GMN-TEDYUKYMD(IDX)  TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-MAE-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-MAE-EDYUKYMD
      *        職員情報編集
               PERFORM 2009-SYOKUIN-HENSYU-SEC
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    2                   TO  SPA-GMN-SYORI
           ELSE
               IF      SPA-GMN-SELNUM      =   ZERO
      *            選択番号が無い時、新規追加とする
                   MOVE    SPACE           TO  SPA-MAE-STYUKYMD
                   MOVE    SPACE           TO  SPA-MAE-EDYUKYMD
                   MOVE    1               TO  SPA-GMN-SYORI
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *----(04.07.01)--ADD-START---
      *    変更項目セット
           PERFORM 4101-HENKOU-SET-SEC
      *----(04.07.01)--ADD-END-----
           .
       2003-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       2004-KIKANLIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               IF      W06-KIKAN-SELECT(IDX)   =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-TBL-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
      *
               MOVE    SPA-GMN-SELNUM      TO  W06-SELNUM
               PERFORM 2003-SELNUM-CHK-SEC
           END-IF
           .
        2004-KIKANLIST-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       2009-SYOKUIN-HENSYU-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-KOUMOKU
           INITIALIZE                      SPA-PAGE2-G
           INITIALIZE                      SPA-PAGE3-G
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1010-STYUKYMD 
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1010-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1010-REC
      *
               MOVE    SPACE               TO  SYS-1050-REC
               INITIALIZE                      SYS-1050-REC
               MOVE    "1050"              TO  SYS-1050-KANRICD
               MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1050-KBNCD (1:1)
               MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1050-KBNCD (2:4)
               MOVE    SPA-NAI-STYUKYMD    TO  SYS-1050-STYUKYMD 
               MOVE    SPA-NAI-STYUKYMD    TO  SYS-1050-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
               MOVE    SYS-1050-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-KEY-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1050-REC
               ELSE
                   MOVE    SPACE               TO  SYS-1050-REC
                   INITIALIZE                  SYS-1050-REC
               END-IF
      *
               MOVE    SYS-1010-USERID         TO  SPA-GMN-USERID
               MOVE    SYS-1010-KANANAME       TO  SPA-GMN-KANANAME
               MOVE    SYS-1010-NAME           TO  SPA-GMN-NAME
               MOVE    SYS-1010-ISEKINUM       TO  SPA-GMN-ISEKINUM
               MOVE    SYS-1010-HKNDRNUMS      TO  SPA-GMN-HKNDRNUMS
               MOVE    SYS-1010-HKNDRNUMK      TO  SPA-GMN-HKNDRNUMK
               MOVE    SYS-1010-MAYAKULICENSE  TO  SPA-GMN-MAYAKULICENSE
      *        業務権限
               PERFORM 320-SPASET-GSRAUTH-SEC
      *
               MOVE    SYS-1010-KANRIAUTHFLG   TO  SPA-NAI-KANRIAUTHFLG
      *
               IF      SPA-NAI-KANRIAUTHFLG    =  "T"
      *            管理者である
                   MOVE    SPA-GMN-KANRIAUTH-LST(1)
                                               TO  SPA-GMN-KANRIAUTH-MEI
               ELSE
      *            管理者でない
                   MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                               TO  SPA-GMN-KANRIAUTH-MEI
               END-IF
      *----(04.07.01)--ADD-START---
      *        スーパーバイザー
               MOVE    SPA-GMN-USERID      TO  WRK-USERID
               PERFORM 49001-SYSUSER-CHK-SEC
               IF      FLG-USERID-UPD  =   ZERO
                   MOVE    SYSUSER-SUPERVISOR  TO  SPA-NAI-SUPERVISOR
               ELSE
                   MOVE    SPACE               TO  SPA-NAI-SUPERVISOR
               END-IF
      *----(04.07.01)--ADD-END-----
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   MOVE    SPACE           TO  SPA-GMN-SEMONKACD-G (IDX)
                   MOVE    SYS-1010-SEMONKACD (IDX)
                                           TO  SPA-GMN-SEMONKACD (IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-SRYKA-MAX
                       IF      SPA-GMN-SEMONKACD (IDX)
                                               =   SPA-GMN-SRYKA (IDY)
                           MOVE    SPA-GMN-SEMONKACD-TBL (IDY)
                                               TO  SPA-GMN-SEMONKACD-G
                                                                 (IDX)
                           MOVE    SPA-SRYKA-MAX   TO  IDY
                       END-IF 
                   END-PERFORM
               END-PERFORM
      *        診療行為初期表示
               MOVE    SYS-1010-ORCA21FLG  TO  SPA-GMN-ORCA21FLG
               EVALUATE   SPA-GMN-ORCA21FLG
                   WHEN    "1"
                       MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                               TO  SPA-GMN-ORCA21FLG-G
                   WHEN    "2"
                       MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                               TO  SPA-GMN-ORCA21FLG-G
                   WHEN    OTHER
      *                診療行為初期表示
                       IF      SPA-BEDFLG          >  ZERO
                           MOVE    SPA-GMN-ORCA21FLG-LST (1)
                                               TO  SPA-GMN-ORCA21FLG-G
                       ELSE
                           MOVE    SPA-GMN-ORCA21FLG-LST (2)
                                               TO  SPA-GMN-ORCA21FLG-G
                       END-IF
               END-EVALUATE
      *        表示区分
               MOVE    SYS-1010-HYOJIFLG       TO  SPA-NAI-HYOJIFLG
               IF      SPA-NAI-HYOJIFLG        =   "1"
                   MOVE    SPA-GMN-HYOJIFLG-LST(2)
                                               TO  SPA-GMN-HYOJIFLG-G
               ELSE
                   MOVE    SPA-GMN-HYOJIFLG-LST(1)
                                               TO  SPA-GMN-HYOJIFLG-G
                   MOVE    "0"                 TO  SPA-NAI-HYOJIFLG
               END-IF
      *
      *        発行フラグ
               MOVE    SYS-1010-SKYPRTFLG      TO  SPA-GMN-SKYPRTFLG
               MOVE    SYS-1010-MEIPRTFLG      TO  SPA-GMN-MEIPRTFLG
               MOVE    SYS-1010-SYOHOPRTFLG    TO  SPA-GMN-SYOHOPRTFLG
               MOVE    SYS-1010-YAKUJYOPRTFLG  TO  SPA-GMN-YAKUJYOFLG
               MOVE    SYS-1010-FORMALFLG      TO  SPA-GMN-FORMALFLG
               MOVE    SYS-1010-CLIENT-SAVE    TO  SPA-GMN-CLIENT-SAVE
               MOVE    SYS-1010-USER-KANRI     TO  SPA-GMN-USER-KANRI
               MOVE    SYS-1010-CLIENT-PRT     TO  SPA-GMN-CLIENT-PRT
               MOVE    SYS-1010-DIALOGFLG      TO  SPA-GMN-DIALOGFLG
               MOVE    SYS-1050-ICCARDRW       TO  SPA-GMN-ICCARDRW
      *        入力スクロール行数
               IF      SYS-1050-GLINECNT   NOT NUMERIC
      *            外来の初期行数　２４行
                   MOVE    24                  TO  SPA-GMN-GLINECNT
               ELSE
                   MOVE    SYS-1050-GLINECNT   TO  SPA-GMN-GLINECNT
               END-IF
               IF      SYS-1050-NLINECNT   NOT NUMERIC
      *            入院の初期行数　２８行
                   MOVE    28                  TO  SPA-GMN-NLINECNT
               ELSE
                   MOVE    SYS-1050-NLINECNT   TO  SPA-GMN-NLINECNT
               END-IF
      *
               IF    ( SYS-1050-PRTCONF        =   SPACE )
                   MOVE    "0"                 TO  SPA-GMN-PRTCONF
               ELSE
                   MOVE    SYS-1050-PRTCONF    TO  SPA-GMN-PRTCONF
               END-IF
      *
               INITIALIZE                     SPA-PAGE3-G
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >   CONST-GRPHOSP-MAX )
                        OR   ( SPA-GRPHOSPNUM (IDX)    =   ZERO )
      *
                   MOVE    IDX             TO  SPA-GMN-GRPHOSP-MAX
      *
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL ( IDY    >   CONST-GRPHOSP-MAX )
                            OR   ( SPA-GMN-GRPUSERID (IDX)
                                          NOT =   SPACE )
                       IF    ( SYS-1010-GRPHOSPNUM (IDY) IS  NUMERIC )
                           IF    ( SYS-1010-GRPHOSPNUM (IDY)
                                       =   SPA-GRPHOSPNUM (IDX) )
                               MOVE    SYS-1010-GRPHOSPUSERID (IDY)
                                           TO  SPA-GMN-GRPUSERID (IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *----(04.08.01)--UPD-START---
           IF    ( SPA-GMN-CLIENT-SAVE =   SPACE )
               IF      SPA-MW      =   SPA-GINBEE
                   MOVE    "1"                 TO  SPA-GMN-CLIENT-SAVE
               ELSE
                   MOVE    "0"                 TO  SPA-GMN-CLIENT-SAVE
               END-IF
           END-IF
      *----(04.08.01)--UPD-END-----
           IF    ( SPA-GMN-USER-KANRI  =   SPACE )
               MOVE    "0"             TO  SPA-GMN-USER-KANRI
           END-IF
           IF    ( SPA-GMN-PRTCONF    =   SPACE )
               MOVE    "0"             TO  SPA-GMN-PRTCONF
           END-IF
           IF    ( SPA-GMN-CLIENT-PRT  =   SPACE )
               MOVE    "0"             TO  SPA-GMN-CLIENT-PRT
           END-IF
           IF    ( SPA-GMN-DIALOGFLG   =   SPACE )
               MOVE    "0"             TO  SPA-GMN-DIALOGFLG
           END-IF
           IF    ( SPA-GMN-ICCARDRW    =   SPACE )
               MOVE    "0"             TO  SPA-GMN-ICCARDRW
           END-IF
      *
           PERFORM 20091-PRTFLG-HEN-SEC
      *
           . 
       2009-SYOKUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    発行フラグ編集処理 
      *****************************************************************
       20091-PRTFLG-HEN-SEC              SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG   =   SPA-GMN-SKYPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                           TO  SPA-GMN-SKYPRTFLG-G
                   MOVE    SPA-SKYPRTFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-MEIPRTFLG-LST (IDX)
                                           TO  SPA-GMN-MEIPRTFLG-G
                   MOVE    SPA-MEIPRTFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-SYOHOPRTFLG-MAX
               IF      SPA-GMN-SYOHOPRTFLG =   SPA-GMN-SYOHOPRTFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-SYOHOPRTFLG-LST (IDX)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
                   MOVE    SPA-SYOHOPRTFLG-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-YAKUJYOFLG-MAX
               IF      SPA-GMN-YAKUJYOFLG   =   SPA-GMN-YAKUJYOFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-YAKUJYOFLG-LST (IDX)
                                           TO  SPA-GMN-YAKUJYOFLG-G
                   MOVE    SPA-YAKUJYOFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG   =   SPA-GMN-FORMALFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST (IDX)
                                           TO  SPA-GMN-FORMALFLG-G
                   MOVE    SPA-FORMALFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-CLIENT-SAVE-MAX
               IF      SPA-GMN-CLIENT-SAVE =   SPA-GMN-CLIENT-SAVE-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-SAVE-LST (IDX)
                                           TO  SPA-GMN-CLIENT-SAVE-G
                   MOVE    SPA-CLIENT-SAVE-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-USER-KANRI-MAX
               IF      SPA-GMN-USER-KANRI  =   SPA-GMN-USER-KANRI-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-USER-KANRI-LST (IDX)
                                           TO  SPA-GMN-USER-KANRI-G
                   MOVE    SPA-USER-KANRI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-PRTCONF-MAX
               IF      SPA-GMN-PRTCONF  =   SPA-GMN-PRTCONF-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-PRTCONF-LST (IDX)
                                           TO  SPA-GMN-PRTCONF-G
                   MOVE    SPA-PRTCONF-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-CLIENT-PRT-MAX
               IF      SPA-GMN-CLIENT-PRT  =   SPA-GMN-CLIENT-PRT-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-PRT-LST (IDX)
                                           TO  SPA-GMN-CLIENT-PRT-G
                   MOVE    SPA-CLIENT-PRT-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-DIALOGFLG-MAX
               IF      SPA-GMN-DIALOGFLG   =   SPA-GMN-DIALOGFLG-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-DIALOGFLG-LST (IDX)
                                           TO  SPA-GMN-DIALOGFLG-G
                   MOVE    SPA-DIALOGFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-ICCARDRW-MAX
               IF      SPA-GMN-ICCARDRW   =   SPA-GMN-ICCARDRW-LST
                                                      (IDX)(1:1)
                   MOVE    SPA-GMN-ICCARDRW-LST (IDX)
                                           TO  SPA-GMN-ICCARDRW-G
                   MOVE    SPA-ICCARDRW-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           . 
       20091-PRTFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ業務処理権限初期編集処理 
      *****************************************************************
       320-SPASET-GSRAUTH-SEC              SECTION.
      *医事業務
           IF  SYS-1010-GSRAUTH(1:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(1)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(1)
           END-IF
      *受付
           IF  SYS-1010-GSRAUTH(16:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(2)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(2)
           END-IF
      *登録
           IF  SYS-1010-GSRAUTH(17:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(3)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(3)
           END-IF
      *照会
           IF  SYS-1010-GSRAUTH(18:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(4)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(4)
           END-IF
      *予約
           IF  SYS-1010-GSRAUTH(19:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(5)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(5)
           END-IF
      *診療行為
           IF  SYS-1010-GSRAUTH(21:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(6)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(6)
           END-IF
      *病名
           IF  SYS-1010-GSRAUTH(22:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(7)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(7)
           END-IF
      *収納
           IF  SYS-1010-GSRAUTH(23:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(8)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(8)
           END-IF
      *会計照会
           IF  SYS-1010-GSRAUTH(24:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(9)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(9)
           END-IF
      *データチェック
           IF  SYS-1010-GSRAUTH(26:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(10)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(10)
           END-IF
      *明細書
           IF  SYS-1010-GSRAUTH(27:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(11)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(11)
           END-IF
      *請求管理
           IF  SYS-1010-GSRAUTH(28:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(12)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(12)
           END-IF
      *総括票
           IF  SYS-1010-GSRAUTH(29:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(13)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(13)
           END-IF
      *日次統計
           IF  SYS-1010-GSRAUTH(31:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(14)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(14)
           END-IF
      *月次統計
           IF  SYS-1010-GSRAUTH(32:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(15)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(15)
           END-IF
      *データ出力
           IF  SYS-1010-GSRAUTH(41:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(16)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(16)
           END-IF
      *外部媒体
           IF  SYS-1010-GSRAUTH(47:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(17)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(17)
           END-IF
      *マスタ登録
           IF  SYS-1010-GSRAUTH(51:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(18)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(18)
           END-IF
      *マスタ更新
           IF  SYS-1010-GSRAUTH(52:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(19)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(19)
           END-IF
      *システム管理
           IF  SYS-1010-GSRAUTH(61:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(20)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(20)
           END-IF
      *点数マスタ
           IF  SYS-1010-GSRAUTH(62:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(21)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(21)
           END-IF
      *チェックマスタ
           IF  SYS-1010-GSRAUTH(63:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(22)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(22)
           END-IF
      *保険番号マスタ
           IF  SYS-1010-GSRAUTH(64:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(23)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(23)
           END-IF
      *保険者マスタ
           IF  SYS-1010-GSRAUTH(65:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(24)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(24)
           END-IF
      *人名辞書マスタ
           IF  SYS-1010-GSRAUTH(66:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(25)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(25)
           END-IF
      *----(01.01.05)----START------------
      *薬剤情報マスタ
           IF  SYS-1010-GSRAUTH(67:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(26)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(26)
           END-IF
      *----(01.01.05)----END--------------
      *----(01.01.07)----START------------
      *入退院登録
           IF  SYS-1010-GSRAUTH(36:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(27)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(27)
           END-IF
      *入院会計照会
           IF  SYS-1010-GSRAUTH(37:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(28)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(28)
           END-IF
      *入院定期請求
           IF  SYS-1010-GSRAUTH(38:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(29)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(29)
           END-IF
      *退院時仮計算
           IF  SYS-1010-GSRAUTH(39:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(30)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(30)
           END-IF
      *病床管理
           IF  SYS-1010-GSRAUTH(56:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(31)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(31)
           END-IF
      *入院患者照会
           IF  SYS-1010-GSRAUTH(57:1)  =  1
               MOVE  "T"  TO  SPA-GMN-CHK(32)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(32)
           END-IF
      *----(01.01.07)----END--------------
      *----(02.00.01)--ADD-START---
      *プログラム更新
           IF      SYS-1010-GSRAUTH(3:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(33)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(33)
           END-IF
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
      *住所マスタ
           IF      SYS-1010-GSRAUTH(68:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(34)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(34)
           END-IF
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
      *ヘルプマスタ
           IF      SYS-1010-GSRAUTH(69:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(35)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(35)
           END-IF
      *----(02.00.031)--ADD-END-----
      *    外来まとめ
           IF      SYS-1010-GSRAUTH(5:1)    =  "1"
               MOVE  "T"  TO  SPA-GMN-CHK(36)
           ELSE
               MOVE  "F"  TO  SPA-GMN-CHK(36)
           END-IF
           . 
       320-SPASET-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W06-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W06-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *
           MOVE    W06-PASSWORD1       TO  SPA-GMN-PASSWORD1
           MOVE    W06-PASSWORD2       TO  SPA-GMN-PASSWORD2
      *
           MOVE    W06-USERID          TO  SPA-GMN-USERID
           MOVE    W06-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    W06-NAME            TO  SPA-GMN-NAME
           MOVE    W06-ISEKINUM        TO  SPA-GMN-ISEKINUM
           MOVE    W06-HKNDRNUMS       TO  SPA-GMN-HKNDRNUMS
           MOVE    W06-HKNDRNUMK       TO  SPA-GMN-HKNDRNUMK
           MOVE    W06-MAYAKULICENSE   TO  SPA-GMN-MAYAKULICENSE
      *
      *----(04.07.01)--ADD-START---
           MOVE    W06-CHK01           TO  SPA-GMN-CHK(1)
           MOVE    W06-CHK02           TO  SPA-GMN-CHK(2)
           MOVE    W06-CHK03           TO  SPA-GMN-CHK(3)
           MOVE    W06-CHK04           TO  SPA-GMN-CHK(4)
           MOVE    W06-CHK05           TO  SPA-GMN-CHK(5)
           MOVE    W06-CHK06           TO  SPA-GMN-CHK(6)
           MOVE    W06-CHK07           TO  SPA-GMN-CHK(7)
           MOVE    W06-CHK08           TO  SPA-GMN-CHK(8)
           MOVE    W06-CHK09           TO  SPA-GMN-CHK(9)
           MOVE    W06-CHK10           TO  SPA-GMN-CHK(10)
           MOVE    W06-CHK11           TO  SPA-GMN-CHK(11)
           MOVE    W06-CHK12           TO  SPA-GMN-CHK(12)
           MOVE    W06-CHK13           TO  SPA-GMN-CHK(13)
           MOVE    W06-CHK14           TO  SPA-GMN-CHK(14)
           MOVE    W06-CHK15           TO  SPA-GMN-CHK(15)
           MOVE    W06-CHK16           TO  SPA-GMN-CHK(16)
           MOVE    W06-CHK17           TO  SPA-GMN-CHK(17)
           MOVE    W06-CHK18           TO  SPA-GMN-CHK(18)
           MOVE    W06-CHK19           TO  SPA-GMN-CHK(19)
           MOVE    W06-CHK20           TO  SPA-GMN-CHK(20)
           MOVE    W06-CHK21           TO  SPA-GMN-CHK(21)
           MOVE    W06-CHK22           TO  SPA-GMN-CHK(22)
           MOVE    W06-CHK23           TO  SPA-GMN-CHK(23)
           MOVE    W06-CHK24           TO  SPA-GMN-CHK(24)
           MOVE    W06-CHK25           TO  SPA-GMN-CHK(25)
           MOVE    W06-CHK26           TO  SPA-GMN-CHK(26)
           MOVE    W06-CHK27           TO  SPA-GMN-CHK(27)
           MOVE    W06-CHK28           TO  SPA-GMN-CHK(28)
           MOVE    W06-CHK29           TO  SPA-GMN-CHK(29)
           MOVE    W06-CHK30           TO  SPA-GMN-CHK(30)
           MOVE    W06-CHK31           TO  SPA-GMN-CHK(31)
           MOVE    W06-CHK32           TO  SPA-GMN-CHK(32)
           MOVE    W06-CHK33           TO  SPA-GMN-CHK(33)
           MOVE    W06-CHK34           TO  SPA-GMN-CHK(34)
           MOVE    W06-CHK35           TO  SPA-GMN-CHK(35)
           MOVE    W06-CHK36           TO  SPA-GMN-CHK(36)
      *----(04.07.01)--ADD-END-----
      *
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    W06-SEMONKACD(IDX)  TO  SPA-GMN-SEMONKACD-G(IDX)
           END-PERFORM
      *    管理者権限
           MOVE    W06-KANRIAUTHFLG    TO  SPA-GMN-KANRIAUTH-MEI
           IF      SPA-GMN-KANRIAUTH-MEI   =   SPACE
               MOVE    SPA-GMN-KANRIAUTH-LST(1)
                                           TO  SPA-GMN-KANRIAUTH-MEI
           END-IF
      *
           MOVE    W06-ORCA21FLG       TO  SPA-GMN-ORCA21FLG-G
      *    表示区分
           MOVE    W06-HYOJIFLG        TO  SPA-GMN-HYOJIFLG-G
      *
      *    個別設定画面
           MOVE    W06-SKYPRTFLG       TO  SPA-GMN-SKYPRTFLG-G
           MOVE    W06-MEIPRTFLG       TO  SPA-GMN-MEIPRTFLG-G
           MOVE    W06-SYOHOPRTFLG     TO  SPA-GMN-SYOHOPRTFLG-G
           MOVE    W06-YAKUJYOFLG      TO  SPA-GMN-YAKUJYOFLG-G
           MOVE    W06-FORMALFLG       TO  SPA-GMN-FORMALFLG-G
           MOVE    W06-CLIENT-SAVE     TO  SPA-GMN-CLIENT-SAVE-G
           MOVE    W06-USER-KANRI      TO  SPA-GMN-USER-KANRI-G
           MOVE    W06-PRTCONF         TO  SPA-GMN-PRTCONF-G
           MOVE    W06-CLIENT-PRT      TO  SPA-GMN-CLIENT-PRT-G
           MOVE    W06-DIALOGFLG       TO  SPA-GMN-DIALOGFLG-G
           MOVE    W06-ICCARDRW        TO  SPA-GMN-ICCARDRW-G
      *
           MOVE    W06-GLINECNT        TO  SPA-GMN-GLINECNT
           MOVE    W06-NLINECNT        TO  SPA-GMN-NLINECNT
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONST-GRPHOSP-MAX
               MOVE    W06-GRPUSERID(IDX)  TO  SPA-GMN-GRPUSERID(IDX)
           END-PERFORM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *----(04.07.01)--ADD-START---
      *****************************************************************
      *    変更項目セット処理
      *****************************************************************
       4101-HENKOU-SET-SEC         SECTION.
      *
           INITIALIZE              SPA-MAE-AREA
      *
           MOVE    SPA-GMN-STYUKYMD        TO  SPA-MAE-STYUKYMD2
           MOVE    SPA-GMN-EDYUKYMD        TO  SPA-MAE-EDYUKYMD2
      *
           MOVE    SPA-GMN-USERID          TO  SPA-MAE-USERID
           MOVE    SPA-GMN-KANANAME        TO  SPA-MAE-KANANAME
           MOVE    SPA-GMN-NAME            TO  SPA-MAE-NAME
           MOVE    SPA-GMN-ISEKINUM        TO  SPA-MAE-ISEKINUM
           MOVE    SPA-GMN-HKNDRNUMS       TO  SPA-MAE-HKNDRNUMS
           MOVE    SPA-GMN-HKNDRNUMK       TO  SPA-MAE-HKNDRNUMK
           MOVE    SPA-GMN-MAYAKULICENSE   TO  SPA-MAE-MAYAKULICENSE
      *
           MOVE    SPA-GMN-CHK-G           TO  SPA-MAE-CHK-G
      *
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    SPA-GMN-SEMONKACD-G(IDX)
                                           TO  SPA-MAE-SEMONKACD-G(IDX)
           END-PERFORM
      *    管理者権限
           MOVE    SPA-GMN-KANRIAUTH-MEI   TO  SPA-MAE-KANRIAUTH-MEI
      *
           MOVE    SPA-GMN-ORCA21FLG-G     TO  SPA-MAE-ORCA21FLG-G
      *    表示区分
           MOVE    SPA-GMN-HYOJIFLG-G      TO  SPA-MAE-HYOJIFLG-G
      *
      *    個別設定画面
           MOVE    SPA-GMN-SKYPRTFLG-G     TO  SPA-MAE-SKYPRTFLG-G
           MOVE    SPA-GMN-MEIPRTFLG-G     TO  SPA-MAE-MEIPRTFLG-G
           MOVE    SPA-GMN-SYOHOPRTFLG-G   TO  SPA-MAE-SYOHOPRTFLG-G
           MOVE    SPA-GMN-YAKUJYOFLG-G    TO  SPA-MAE-YAKUJYOFLG-G
           MOVE    SPA-GMN-FORMALFLG-G     TO  SPA-MAE-FORMALFLG-G
           MOVE    SPA-GMN-CLIENT-SAVE-G   TO  SPA-MAE-CLIENT-SAVE-G
           MOVE    SPA-GMN-USER-KANRI-G    TO  SPA-MAE-USER-KANRI-G
           MOVE    SPA-GMN-PRTCONF-G      TO  SPA-MAE-PRTCONF-G
           MOVE    SPA-GMN-CLIENT-PRT-G    TO  SPA-MAE-CLIENT-PRT-G
           MOVE    SPA-GMN-DIALOGFLG-G     TO  SPA-MAE-DIALOGFLG-G
           MOVE    SPA-GMN-ICCARDRW-G      TO  SPA-MAE-ICCARDRW-G
      *
           MOVE    SPA-GMN-GLINECNT        TO  SPA-MAE-GLINECNT
           MOVE    SPA-GMN-NLINECNT        TO  SPA-MAE-NLINECNT
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONST-GRPHOSP-MAX
               MOVE    SPA-GMN-GRPUSERID(IDX)
                                           TO  SPA-MAE-GRPUSERID(IDX)
           END-PERFORM
      *
           PERFORM 900-PASSWD-READ-SEC
           IF      FLG-PASSWD          =   ZERO
               MOVE    1                   TO  SPA-NAI-PASSWD
           ELSE
               MOVE    ZERO                TO  SPA-NAI-PASSWD
           END-IF
           .
       4101-HENKOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更項目チェック処理
      *****************************************************************
       4102-HENKOU-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF      SPA-GMN-STYUKYMD    NOT =   SPA-MAE-STYUKYMD2
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-EDYUKYMD    NOT =   SPA-MAE-EDYUKYMD2
               MOVE    1               TO  FLG-OK
           END-IF
      *
           IF      SPA-GMN-USERID      NOT =   SPA-MAE-USERID
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-KANANAME    NOT =   SPA-MAE-KANANAME
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-NAME        NOT =   SPA-MAE-NAME
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-ISEKINUM    NOT =   SPA-MAE-ISEKINUM
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-HKNDRNUMS   NOT =   SPA-MAE-HKNDRNUMS
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-HKNDRNUMK   NOT =   SPA-MAE-HKNDRNUMK
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-MAYAKULICENSE
                                       NOT =   SPA-MAE-MAYAKULICENSE
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-CHK-G       NOT =   SPA-MAE-CHK-G
               MOVE    1               TO  FLG-OK
           END-IF
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-GMN-SEMONKACD-G(IDX)
                                       NOT =   SPA-MAE-SEMONKACD-G(IDX)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *    管理者権限
           IF      SPA-GMN-KANRIAUTH-MEI
                                       NOT =   SPA-MAE-KANRIAUTH-MEI
               MOVE    1               TO  FLG-OK
           END-IF
      *
           IF      SPA-GMN-ORCA21FLG-G NOT =   SPA-MAE-ORCA21FLG-G
               MOVE    1               TO  FLG-OK
           END-IF
      *    表示区分
           IF      SPA-GMN-HYOJIFLG-G  NOT =   SPA-MAE-HYOJIFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
      *
      *    個別設定画面
           IF      SPA-GMN-SKYPRTFLG-G NOT =   SPA-MAE-SKYPRTFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-MEIPRTFLG-G NOT =   SPA-MAE-MEIPRTFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-SYOHOPRTFLG-G
                                       NOT =   SPA-MAE-SYOHOPRTFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-YAKUJYOFLG-G
                                       NOT =   SPA-MAE-YAKUJYOFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-FORMALFLG-G NOT =   SPA-MAE-FORMALFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
      *
           IF      SPA-GMN-GLINECNT    NOT =   SPA-MAE-GLINECNT
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-NLINECNT    NOT =   SPA-MAE-NLINECNT
               MOVE    1               TO  FLG-OK
           END-IF
      *
           IF      SPA-GMN-CLIENT-SAVE-G
                                       NOT =   SPA-MAE-CLIENT-SAVE-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-USER-KANRI-G
                                       NOT =   SPA-MAE-USER-KANRI-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-PRTCONF-G
                                       NOT =   SPA-MAE-PRTCONF-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-CLIENT-PRT-G
                                       NOT =   SPA-MAE-CLIENT-PRT-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-DIALOGFLG-G NOT =   SPA-MAE-DIALOGFLG-G
               MOVE    1               TO  FLG-OK
           END-IF
           IF      SPA-GMN-ICCARDRW-G  NOT =   SPA-MAE-ICCARDRW-G
               MOVE    1               TO  FLG-OK
           END-IF
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONST-GRPHOSP-MAX
               IF      SPA-GMN-GRPUSERID(IDX)
                                       NOT =   SPA-MAE-GRPUSERID(IDX)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       4102-HENKOU-CHK-EXT.
           EXIT.
      *
      *----(4.07.01)--ADD-END-----
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    職員番号チェック
           IF     (SPA-GMN-SYOKUCOD    =   SPACE ) OR
                  (SPA-GMN-SYOKUCOD    NOT NUMERIC )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-STYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-EDYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-USERID-CHK-SEC
           END-IF
      *
      *----(04.8.03)--ADD-START---
           IF      SPA-ERRCD           =   SPACE
      *        スーパーユーザーはパスワード削除できない
               IF    ( SPA-GMN-PASSWORD1(1:2)  =   "/d" )  OR
                     ( SPA-GMN-PASSWORD1(1:2)  =   "/D" )
                   IF     (SPA-NAI-SUPERVISOR  =   "T")  OR
                          (SPA-NAI-KANRIAUTHFLG    =   "T")
                       MOVE    "0035"          TO  SPA-ERRCD
                       MOVE    81              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
      *----(04.8.03)--ADD-END-----
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-KANANAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-NAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4114-ISEKINUM-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4115-HKNDRNUMS-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4116-HKNDRNUMK-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4117-MAYAKULICENSE-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE)
              AND (SPA-GMN-PAGENO      =   1    )
               PERFORM 4118-SEMONKACD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-KANRIAUTHFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-ORCA21FLG-CHK-SEC
           END-IF
      *    表示区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-HYOJIFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-SKYPRTFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-SYOHOPRTFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-YAKUJYOFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-MEIPRTFLG-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-FORMALFLG-CHK-SEC
           END-IF
      *    入力スクロール行数
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-GLINECNT-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-CLIENT-SAVE-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-USER-KANRI-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-PRTCONF-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-CLIENT-PRT-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-DIALOGFLG-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-ICCARDRW-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX-GRP FROM    1   BY  1
                   UNTIL ( IDX-GRP >   SPA-GMN-GRPHOSP-MAX )
                    OR   ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 4122-GRPUSERID-CHK-SEC
           END-PERFORM
      *
      *    基本チェックの最後にチェックする
           IF      SPA-ERRCD           =   SPACE
               IF    ((SPA-GMN-USERID  NOT =   SPA-MAE-USERID) AND
                      (SPA-GMN-PASSWORD1   =   SPACE)  AND
                      (SPA-NAI-KANRIAUTHFLG    =   "T")  AND
                      (MCP-WIDGET      NOT =   "USERID"))    OR
                     ((SPA-GMN-PASSWORD1   =   SPACE)  AND
                      (SPA-NAI-KANRIAUTHFLG    =   "T")  AND
                      (SPA-NAI-PASSWD          =   ZERO))
                       MOVE    "0036"          TO  SPA-ERRCD
                       MOVE    81              TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効開始日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-STYUKYMD
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "STYUKYMD"
                   MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
        41021-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効終了日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-EDYUKYMD
      *
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "EDYUKYMD"
                   MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               END-IF
           END-IF
      *
           IF      SPA-GMN-EDYUKYMD    NOT =   SPACE
               MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD 
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN-STYUKYMD    NOT =   SPACE ) AND
                  (SPA-GMN-EDYUKYMD    NOT =   SPACE )
               IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    4                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
        41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オペレータＩＤチェック
      *****************************************************************
       4100-USERID-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SAME-USERID
      *
           IF      SPA-GMN-USERID      =   SPACE
               IF      WRK-MCP-WIDGET     =   "USERID"
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           ELSE 
      *    半角チェック
               INITIALIZE              ORCSKANACHKAREA
               MOVE  "1"               TO  KANACHK-SYORI
               MOVE  SPA-GMN-USERID    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING 
                                       ORCSKANACHKAREA
      *
               IF      (KANACHK-SYUBETU NOT  =  1)
                   OR  (KANACHK-RC      NOT  =  0)
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        登録済みチェック
               INITIALIZE                  SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               STRING  SPA-GMN-USERID      DELIMITED  BY  SPACE
                       "%"                 DELIMITED  BY  SIZE
                                           INTO    SYS-1010-TBL
               END-STRING
               MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD 
               MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               PERFORM UNTIL  (FLG-SYSKANRI    =   1    )  OR
                              (SPA-ERRCD   NOT =   SPACE)
                   MOVE    MCPDATA-REC             TO  SYS-1010-REC
                   IF      SYS-1010-USERID         =   SPA-GMN-USERID
                       IF     (SYS-1010-KBNCD(1:1) =   SPA-GMN-SYOKUKBN)
                         AND  (SYS-1010-KBNCD(2:4) =   SPA-GMN-SYOKUCOD)
                           MOVE    1               TO  FLG-SAME-USERID
                       ELSE
                           MOVE    "0007"              TO  SPA-ERRCD
                           MOVE    6                   TO  SPA-GMN-CUR
                       END-IF
                   END-IF
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-SYSKANRI-READ-SEC
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-USERID  NOT =   SPACE )
                AND  ( FLG-SAME-USERID     =   ZERO )
                   MOVE  FUNCTION LENGTH(
                              FUNCTION TRIM(SPA-GMN-USERID TRAILING))
                                   TO  WRK-LENGTH
                   DISPLAY "WRK-LENGTH=" WRK-LENGTH
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   WRK-LENGTH )
      *
                       EVALUATE  FUNCTION TRIM(SPA-GMN-USERID  TRAILING)
                                                              (IDX0 : 1)
                       WHEN    "A" THRU    "Z"
                       WHEN    "a" THRU    "z"
                       WHEN    "0" THRU    "9"
                       WHEN    "_"
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "0009"  TO  SPA-ERRCD
                           MOVE    6       TO  SPA-GMN-CUR
                       END-EVALUATE
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       4100-USERID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    フリガナチェック処理
      *****************************************************************
       4112-KANANAME-CHK-SEC           SECTION.
      *
      *    全角チェック
           IF      SPA-GMN-KANANAME    NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-KANANAME    TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-KANANAME
      *
               IF      (KANACHK-RC         NOT =  0)  OR
                       (KANACHK-SYUBETU    NOT =  2)
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-KANANAME
               END-IF
           END-IF
      *  
           .
       4112-KANANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       4113-NAME-CHK-SEC        SECTION.
      *
      *    全角チェック
           IF      SPA-GMN-NAME        =   SPACE
               CONTINUE
           ELSE
      *
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK2-MAE-INPUT
               MOVE    80                  TO  KANACHK2-MAX-CNT
               CALL    "ORCSKANACHK2"      USING 
                                           ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT   TO  SPA-GMN-NAME
      *
               IF      (KANACHK2-RC         NOT =  0)  OR
                       (KANACHK2-SYUBETU    NOT =  2)
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK2-OUT-INPUT  TO  SPA-GMN-NAME
               END-IF
           ELSE
      *        拡張漢字不可
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-NAME
      *
               IF      (KANACHK-RC         NOT =  0)  OR
                       (KANACHK-SYUBETU    NOT =  2)
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               END-IF
           END-IF
      *
           END-IF
           . 
       4113-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医籍登録番号チェック処理
      *****************************************************************
       4114-ISEKINUM-CHK-SEC        SECTION.
           IF      SPA-GMN-ISEKINUM    NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-ISEKINUM    TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-ISEKINUM
           END-IF
      *
           .
       4114-ISEKINUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険医登録番号（社保）チェック処理
      *****************************************************************
       4115-HKNDRNUMS-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKNDRNUMS  NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HKNDRNUMS   TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HKNDRNUMS
           END-IF
           .
       4115-HKNDRNUMS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険医登録番号（国保）チェック処理
      *****************************************************************
       4116-HKNDRNUMK-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKNDRNUMK  NOT  =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HKNDRNUMK   TO  KANACHK-MAE-INPUT
               MOVE    12                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HKNDRNUMK
           END-IF
      *
           .
       4116-HKNDRNUMk-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻薬ライセンス チェック処理
      *****************************************************************
       4117-MAYAKULICENSE-CHK-SEC        SECTION.
      *
      *    混在チェック
           IF      SPA-GMN-MAYAKULICENSE   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-MAYAKULICENSE
                                           TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING 
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-MAYAKULICENSE
      *
               IF      KANACHK-RC      NOT =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    12                  TO  SPA-GMN-CUR
               ELSE
      *            全角の時、空白変換分を編集
                   IF      KANACHK-SYUBETU     =   2
                       MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-MAYAKULICENSE
                   END-IF
               END-IF
           END-IF
      *
           .
       4117-MAYAKULICENSE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    専門科コードチェック処理
      *****************************************************************
       4118-SEMONKACD-CHK-SEC        SECTION.
      *
      *    専門科コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      SPA-GMN-SEMONKACD(IDX)  NOT =   SPACE
      *            桁数変換
                   MOVE    SPA-GMN-SEMONKACD (IDX) TO  WRK-CD
                   PERFORM 41181-NUMHEN-SEC
                   MOVE    WRK-CD              TO  SPA-GMN-SEMONKACD
                                                               (IDX)
      *            科コードチェック
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-SRYKA-MAX
                       IF      SPA-GMN-SEMONKACD (IDX)
                                               =   SPA-GMN-SRYKA (IDY)
                           MOVE    SPA-GMN-SEMONKACD-TBL (IDY)
                                               TO  SPA-GMN-SEMONKACD-G
                                                                (IDX)
                           MOVE    1               TO  FLG-CHK
                           MOVE    SPA-SRYKA-MAX   TO  IDY
                       END-IF
                   END-PERFORM
      *            登録前・タグ遷移のチェックはしない
                   IF      FLG-TOUROKU         =   1
                       CONTINUE
                   ELSE
                       IF      FLG-CHK             =   ZERO
                           MOVE    "0013"          TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR     =   IDX  +   12
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4118-SEMONKACD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    科コード数字変換処理
      *****************************************************************
       41181-NUMHEN-SEC             SECTION.
      *    数字変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-CD              TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   2     )   OR
                  (SNUM-SYOSUU         >   ZERO  )
               MOVE    "0013"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-CD
           END-IF
      *
           .
       41181-NUMHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務処理権限チェック処理
      *****************************************************************
       4119-GSRAUTH-CHK-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           IF      MCP-WIDGET(4:2)     NUMERIC
               MOVE    MCP-WIDGET(4:2)     TO  IDX2
               MOVE    IDX2                TO  IDX
           ELSE
               GO      TO      4119-GSRAUTH-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-CHK (IDX)
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-CHK (IDX)
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-CHK (IDX)
           END-EVALUATE
      *
      *    システム管理はオルカマスターの時、可能
           IF       SPA-GMN-CHK(20)    =   "F"
               IF      (SPA-GMN-SYOKUKBN   =   "0"  )  AND
                       (SPA-GMN-SYOKUCOD   =   "0001")
                    MOVE    "0100"         TO  SPA-ERRCD
               END-IF
           END-IF
           EVALUATE    IDX
               WHEN    18
      *        マスタ登録
               IF      SPA-GMN-CHK(IDX)    =   "F"
                   MOVE    "F"            TO  SPA-GMN-CHK(20)
                   MOVE    "F"            TO  SPA-GMN-CHK(21)
                   MOVE    "F"            TO  SPA-GMN-CHK(22)
                   MOVE    "F"            TO  SPA-GMN-CHK(23)
                   MOVE    "F"            TO  SPA-GMN-CHK(24)
                   MOVE    "F"            TO  SPA-GMN-CHK(25)
                   MOVE    "F"            TO  SPA-GMN-CHK(26)
                   MOVE    "F"            TO  SPA-GMN-CHK(34)
                   MOVE    "F"            TO  SPA-GMN-CHK(35)
               END-IF
               WHEN    20  THRU  26
               WHEN    34
               WHEN    35
      *        M02画面
               IF      SPA-GMN-CHK(IDX)    =   "T"
                   MOVE    "T"            TO  SPA-GMN-CHK(18)
               END-IF
           END-EVALUATE
      *
      *
      *    他の入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
           .
       4119-GSRAUTH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理者権限フラグチェック処理
      *****************************************************************
       4120-KANRIAUTHFLG-CHK-SEC        SECTION.
      *
           EVALUATE    SPA-GMN-KANRIAUTH-MEI
      *        管理者である
               WHEN    SPA-GMN-KANRIAUTH-LST(1)
                   MOVE    "T"             TO  SPA-NAI-KANRIAUTHFLG
      *        管理者でない
               WHEN    SPA-GMN-KANRIAUTH-LST(2)
                   MOVE    "F"             TO  SPA-NAI-KANRIAUTHFLG
      *        その他（管理者でない）
               WHEN    OTHER
                   MOVE    SPA-GMN-KANRIAUTH-LST(2)
                                           TO  SPA-GMN-KANRIAUTH-MEI
                   MOVE    "F"             TO  SPA-NAI-KANRIAUTHFLG
           END-EVALUATE
      *
           .
       4120-KANRIAUTHFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為初期画チェック処理
      *****************************************************************
       4120-ORCA21FLG-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-ORCA21FLG   =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-ORCA21FLG-MAX
                   IF      SPA-GMN-ORCA21FLG   =   SPA-GMN-ORCA21FLG-LST
                                                       (IDX)(1:1)
                       MOVE    SPA-GMN-ORCA21FLG-LST (IDX)
                                               TO  SPA-GMN-ORCA21FLG-G
                       MOVE    1               TO  FLG-CHK
                       MOVE    SPA-ORCA21FLG-MAX   TO  IDX
                    END-IF
               END-PERFORM
               IF     (FLG-CHK             =   ZERO )
                   MOVE    "0014"          TO  SPA-ERRCD
                   MOVE    18              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4120-ORCA21FLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示区分チェック処理
      *****************************************************************
       4120-HYOJIFLG-CHK-SEC        SECTION.
      *
           EVALUATE    SPA-GMN-HYOJIFLG-MEI
      *        表示
               WHEN    SPA-GMN-HYOJIFLG-LST(1)
                   MOVE    "0"             TO  SPA-NAI-HYOJIFLG
      *        非表示
               WHEN    SPA-GMN-HYOJIFLG-LST(2)
                   MOVE    "1"             TO  SPA-NAI-HYOJIFLG
      *        表示
               WHEN    OTHER
                   MOVE    SPA-GMN-HYOJIFLG-LST(1)
                                           TO  SPA-GMN-HYOJIFLG-MEI
                   MOVE    "0"             TO  SPA-NAI-HYOJIFLG
           END-EVALUATE
      *
           .
       4120-HYOJIFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行フラグチェック処理
      *****************************************************************
       4121-SKYPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG   =   SPA-GMN-SKYPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                               TO  SPA-GMN-SKYPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-SKYPRTFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-SKYPRTFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-SKYPRTFLG-G
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-SKYPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方せん発行フラグチェック処理
      *****************************************************************
       4121-SYOHOPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               IF      SPA-GMN-SYOHOPRTFLG =   SPA-GMN-SYOHOPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-SYOHOPRTFLG-LST (IDX)
                                               TO  SPA-GMN-SYOHOPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-SYOHOPRTFLG-MAX TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-SYOHOPRTFLG =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-SYOHOPRTFLG-G
               ELSE
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-SKYPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報発行フラグチェック処理
      *****************************************************************
       4121-YAKUJYOFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKUJYOFLG-MAX
               IF      SPA-GMN-YAKUJYOFLG   =   SPA-GMN-YAKUJYOFLG-LST
                                                       (IDX)(1:1)
                    MOVE    SPA-GMN-YAKUJYOFLG-LST (IDX)
                                               TO  SPA-GMN-YAKUJYOFLG-G
                    MOVE    1                  TO  FLG-CHK
                    MOVE    SPA-YAKUJYOFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-YAKUJYOFLG  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-YAKUJYOFLG-G
               ELSE
                   MOVE    "0018"          TO  SPA-ERRCD
                   MOVE    23              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-YAKUJYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行フラグチェック処理
      *****************************************************************
       4121-MEIPRTFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-MEIPRTFLG-LST (IDX)
                                               TO  SPA-GMN-MEIPRTFLG-G
                    MOVE    1                  TO  FLG-CHK
                   MOVE    SPA-MEIPRTFLG-MAX   TO  IDX
                END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-MEIPRTFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-MEIPRTFLG-G
               ELSE
                   MOVE    "0019"          TO  SPA-ERRCD
                   MOVE    24              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-MEIPRTFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査正式名称表示フラグチェック処理
      *****************************************************************
       4121-FORMALFLG-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FORMALFLG-MAX
               IF      SPA-GMN-FORMALFLG   =   SPA-GMN-FORMALFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-FORMALFLG-LST (IDX)
                                               TO  SPA-GMN-FORMALFLG-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-FORMALFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-FORMALFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-FORMALFLG-G
               ELSE
                   MOVE    "0022"          TO  SPA-ERRCD
                   MOVE    25              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-FORMALFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力スクロール行数チェック処理
      *****************************************************************
       4121-GLINECNT-CHK-SEC        SECTION.
      *
      *    ゼロは初期値とする
           IF      SPA-GMN-GLINECNT    =   ZERO
               MOVE    24              TO  SPA-GMN-GLINECNT
           END-IF
      *
           IF      SPA-GMN-NLINECNT    =   ZERO
               MOVE    28              TO  SPA-GMN-NLINECNT
           END-IF
      *
           .
       4121-GLINECNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クライアント保存可否チェック処理
      *****************************************************************
       4121-CLIENT-SAVE-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-SAVE-MAX
               IF      SPA-GMN-CLIENT-SAVE =   SPA-GMN-CLIENT-SAVE-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-SAVE-LST (IDX)
                                               TO  SPA-GMN-CLIENT-SAVE-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-CLIENT-SAVE-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-CLIENT-SAVE  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-CLIENT-SAVE-G
               ELSE
                   MOVE    "0023"          TO  SPA-ERRCD
                   MOVE    26              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-CLIENT-SAVE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザーログイン権限チェック処理
      *****************************************************************
       4121-USER-KANRI-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USER-KANRI-MAX
               IF      SPA-GMN-USER-KANRI  =   SPA-GMN-USER-KANRI-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-USER-KANRI-LST (IDX)
                                               TO  SPA-GMN-USER-KANRI-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-USER-KANRI-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-USER-KANRI  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-USER-KANRI-G
               ELSE
                   MOVE    "0025"          TO  SPA-ERRCD
                   MOVE    27              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-USER-KANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プリンタへのデータ送信単位チェック処理
      *****************************************************************
       4121-PRTCONF-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PRTCONF-MAX
               IF      SPA-GMN-PRTCONF =   SPA-GMN-PRTCONF-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-PRTCONF-LST (IDX)
                                               TO  SPA-GMN-PRTCONF-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-PRTCONF-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-PRTCONF  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-PRTCONF-G
               ELSE
                   MOVE    "0037"          TO  SPA-ERRCD
                   MOVE    103              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-PRTCONF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷可否チェック処理
      *****************************************************************
       4121-CLIENT-PRT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-PRT-MAX
               IF      SPA-GMN-CLIENT-PRT =   SPA-GMN-CLIENT-PRT-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-CLIENT-PRT-LST (IDX)
                                               TO  SPA-GMN-CLIENT-PRT-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-CLIENT-PRT-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-CLIENT-PRT  =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-CLIENT-PRT-G
               ELSE
                   MOVE    "0029"          TO  SPA-ERRCD
                   MOVE    28              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-CLIENT-PRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ダイアログ表示チェック処理
      *****************************************************************
       4121-DIALOGFLG-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DIALOGFLG-MAX
               IF      SPA-GMN-DIALOGFLG   =   SPA-GMN-DIALOGFLG-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-DIALOGFLG-LST (IDX)
                                               TO  SPA-GMN-DIALOGFLG-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-DIALOGFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-DIALOGFLG   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-DIALOGFLG-G
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
                   MOVE    29              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-DIALOGFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ダイアログ表示チェック処理
      *****************************************************************
       4121-ICCARDRW-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ICCARDRW-MAX
               IF      SPA-GMN-ICCARDRW   =   SPA-GMN-ICCARDRW-LST
                                                       (IDX)(1:1)
                   MOVE    SPA-GMN-ICCARDRW-LST (IDX)
                                               TO  SPA-GMN-ICCARDRW-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-ICCARDRW-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO )
               IF      SPA-GMN-ICCARDRW   =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-ICCARDRW-G
               ELSE
                   MOVE    "0032"          TO  SPA-ERRCD
                   MOVE    30              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-ICCARDRW-CHK-EXT.
           EXIT.
      *****************************************************************
      *    他院オペレータＩＤチェック処理
      *****************************************************************
       4122-GRPUSERID-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-GRPUSERID (IDX-GRP)     =   SPACE )
               CONTINUE
           ELSE
      *
               COMPUTE WRK-ERRCUR  =   IDX-GRP    +   200
      *
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-GRPUSERID (IDX-GRP)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING 
                                       ORCSKANACHKAREA
      *
               IF      (KANACHK-SYUBETU    NOT =  1)
                   OR  (KANACHK-RC         NOT =  0)
                   MOVE    "0024"          TO  SPA-ERRCD
                   MOVE    WRK-ERRCUR      TO  SPA-GMN-CUR
               END-IF
      *
      *            他院職員情報存在チェック
               IF    ( SPA-ERRCD       =   SPACE )
      *
                   MOVE    ZERO                TO  FLG-USERMATCH
                   MOVE    SPACE               TO  SYS-1010-REC
                   INITIALIZE                      SYS-1010-REC
                   MOVE    SPA-GRPHOSPNUM (IDX-GRP)
                                               TO  SYS-1010-HOSPNUM
                   MOVE    "1010"              TO  SYS-1010-KANRICD
                   MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-STYUKYMD
                   MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-EDYUKYMD
      *
                   MOVE    SYS-1010-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM   920-DBFETCH-SEC
                   PERFORM   UNTIL     (FLG-USERMATCH  =  1)  OR
                                       (MCP-RC      NOT = 0)
                       MOVE    MCPDATA-REC     TO  SYS-1010-REC
                       IF    SPA-GMN-GRPUSERID (IDX-GRP)
                                            =   SYS-1010-USERID
                           MOVE   1     TO   FLG-USERMATCH
                       END-IF
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM    920-DBFETCH-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
                   IF         FLG-USERMATCH    =   ZERO
                       MOVE    "0028"          TO  SPA-ERRCD
                       MOVE    WRK-ERRCUR      TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-GMN-GRPUSERID (IDX-GRP)
                                       =   SPA-GMN-USERID )
                       MOVE    "0026"      TO  SPA-ERRCD
                       MOVE    WRK-ERRCUR  TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               PERFORM VARYING IDX FROM    IDX-GRP BY  1
                    UNTIL( IDX         >   SPA-GMN-GRPHOSP-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( IDX     =   IDX-GRP )
                       CONTINUE
                   ELSE
                       IF    ( SPA-ERRCD   =   SPACE )
                           IF    ( SPA-GMN-GRPUSERID (IDX-GRP)
                                           =   SPA-GMN-GRPUSERID (IDX))
                               MOVE    "0027"      TO  SPA-ERRCD
                               COMPUTE SPA-GMN-CUR =   IDX +   200
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
           END-IF
      *
           .
       4122-GRPUSERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パスワードチェック処理
      *****************************************************************
       4121-PASSWORD-CHK-SEC       SECTION.
      *
           MOVE    W06-PASSWORD2   TO  SPA-GMN-PASSWORD2
      *
           IF      SPA-GMN-PASSWORD1   NOT =   SPACE
      *----(04.08.03)--ADD-START---
      *        パスワード削除するときは再入力チェックを行わない
               IF    ( SPA-GMN-PASSWORD1(1:2)  NOT =   "/d" )  AND
                     ( SPA-GMN-PASSWORD1(1:2)  NOT =   "/D" )
      *----(04.08.03)--ADD-END-----
               IF      SPA-GMN-PASSWORD2   =   SPACE
                   MOVE    "0033"          TO  SPA-ERRCD
                   MOVE    82              TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-GMN-PASSWORD1   NOT =   SPA-GMN-PASSWORD2
                       MOVE    "0034"          TO  SPA-ERRCD
                       MOVE    82              TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *----(04.08.03)--ADD-START---
               END-IF
      *----(04.08.03)--ADD-END-----
           ELSE
               MOVE    SPACE           TO  SPA-GMN-PASSWORD2
           END-IF
      *
           .
       4121-PASSWORD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    タグ切替え処理
      *****************************************************************
       460-TAGUCHG-SEC           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    W06-PAGENO          TO  WRK-PAGENO
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    WRK-PAGENO
                   WHEN    0
      *                基本画面
      *----(04.07.01)--UPD-START---
                       IF      SPA-GMN-SYOKUKBN    =   "1"
      *                    医師の場合は医籍登録番号へ
                           MOVE    9               TO  SPA-GMN-CUR
                       ELSE
      *                    スーパーバイザーの場合は診療行為初期画面へ
                           IF      SPA-NAI-SUPERVISOR  =   "T"
                               MOVE    19              TO  SPA-GMN-CUR
                           ELSE
      *                        管理者である場合は外来まとめへ
                               IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                                   MOVE    66          TO  SPA-GMN-CUR
      *                        管理者でない場合は医事業務へ
                               ELSE
                                   MOVE    31          TO  SPA-GMN-CUR
                               END-IF
                           END-IF
                       END-IF
      *----(04.07.01)--UPD-END-----
                       MOVE    1                   TO  SPA-GMN-PAGENO
                   WHEN    1
      *                個別設定
                       MOVE    21                  TO  SPA-GMN-CUR
                       MOVE    2                   TO  SPA-GMN-PAGENO
                   WHEN    2
                       IF    ( SPA-GRPHOSPKBN      =   1 )
      *                    他院設定
                           MOVE    201             TO  SPA-GMN-CUR
                           MOVE    3               TO  SPA-GMN-PAGENO
                       ELSE
                           MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
                           MOVE    "0031"          TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF
      *
           .
       460-TAGUCHG-EXT.
           EXIT.
      *----(04.07.01)--ADD-START---
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-CHK-SEC            SECTION.
      *
           IF     SPA-GMN-SYORI    NOT =   ZERO
      *        画面をＳＰＡセット
               PERFORM 4101-GMN-SPA-SET-SEC
      *        変更項目チェク
               PERFORM 4102-HENKOU-CHK-SEC
               IF      FLG-OK          NOT =   ZERO
                   MOVE    "1004"      TO  SPA-WIDCD
                   GO  TO  210-BACK-CHK-EXT
               END-IF
           END-IF
      *
      *----(04.08.05)--UPD-START---
           PERFORM 210-BACK-USER-CHK-SEC
      *----(04.08.05)--UPD-END-----
           .
       210-BACK-CHK-EXT.
           EXIT.
      *----(04.07.01)--ADD-END-----
      *----(04.08.05)--ADD-START---
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-USER-CHK-SEC       SECTION.
      *
           IF      SPA-NAI-STAFFKANRIKBN  =   ZERO
               PERFORM 210-BACK
           ELSE
      *        ログインユーザーが管理者
               PERFORM 210-KANRISYA-CHK-SEC
      *        当日有効の管理者存在エラー
               IF    ( FLG-KIKAN1          =   0 )
                   MOVE    "0038"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK-EXT
               END-IF
      *        無期限有効の管理者存在エラー
               IF    ( FLG-KIKAN2          =   0 )
                   MOVE    "0039"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK-EXT
               END-IF
      *        当日有効のシステム管理者存在エラー
               IF    ( FLG-KIKAN3          =   0 ) AND
                     ( SPA-HOSPNUM         =   1 )
                   MOVE    "0040"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK-EXT
               END-IF
      *        無期限有効のシステム管理者存在エラー
               IF    ( FLG-KIKAN4          =   0 ) AND
                     ( SPA-HOSPNUM         =   1 )
                   MOVE    "0041"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK-EXT
               END-IF
               PERFORM 210-BACK
           END-IF
           .
       210-BACK-USER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る２　処理
      *****************************************************************
       210-BACK-USER-CHK2-SEC      SECTION.
      *
      *    ログインユーザーがシステム管理者
           IF      SPA-NAI-STAFFKANRIKBN   =   2
      *        ログインユーザーが管理者
               PERFORM 210-KANRISYA-CHK-SEC
      *        当日有効のシステム管理者存在エラー
               IF    ( FLG-KIKAN3          =   0 ) AND
                     ( SPA-HOSPNUM         =   1 )
                   MOVE    "0040"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK2-EXT
               END-IF
      *        無期限有効のシステム管理者存在エラー
               IF    ( FLG-KIKAN4          =   0 ) AND
                     ( SPA-HOSPNUM         =   1 )
                   MOVE    "0041"          TO  SPA-ERRCD
                   GO  TO  210-BACK-USER-CHK2-EXT
               END-IF
               PERFORM 210-BACK
           END-IF
           .
       210-BACK-USER-CHK2-EXT.
           EXIT.
      *----(04.08.05)--ADD-END-----
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *----(04.08.05)--ADD-START---
      *****************************************************************
      *    システム管理者有効期限チェック処理
      *****************************************************************
       210-KANRISYA-CHK-SEC        SECTION.
      *
           MOVE    0               TO  FLG-KIKAN1
           MOVE    0               TO  FLG-KIKAN2
           MOVE    0               TO  FLG-KIKAN3
           MOVE    0               TO  FLG-KIKAN4
      *
      *    当日有効レコードチェック
           INITIALIZE                  SYS-1010-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
      *        管理者なら
               IF     (SYS-1010-KANRIAUTHFLG   =   "T"  )
                   MOVE    SYS-1010-USERID     TO  WRK-USERID
                   PERFORM 49001-SYSUSER-CHK-SEC
      *            システム管理者なら
                   IF    ( FLG-USERID-UPD  =   ZERO ) AND
                         ( SYSUSER-SUPERVISOR  =   "T")
      *                当日有効か？
                       IF    ( SYS-1010-STYUKYMD   <=  SPA-SYSYMD ) AND
                             ( SYS-1010-EDYUKYMD   >=  SPA-SYSYMD )
                           MOVE    1               TO  FLG-KIKAN3
                       END-IF
      *                無期限有効か？
                       IF      SYS-1010-EDYUKYMD   =   "99999999"
                           MOVE    1               TO  FLG-KIKAN4
                       END-IF
                   ELSE
      *                当日有効か？
                       IF    ( SYS-1010-STYUKYMD   <=  SPA-SYSYMD ) AND
                             ( SYS-1010-EDYUKYMD   >=  SPA-SYSYMD )
      *                    自分のみの場合
                           IF        ( SYS-1010-USERID
                                                   =   SPA-GMN-USERID )
                               AND   ( FLG-KIKAN1  =   ZERO )
                               MOVE    2               TO  FLG-KIKAN1
                           ELSE
                               MOVE    1               TO  FLG-KIKAN1
                           END-IF
                       END-IF
      *                無期限有効か？
                       IF      SYS-1010-EDYUKYMD   =   "99999999"
                           MOVE    1               TO  FLG-KIKAN2
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       210-KANRISYA-CHK-EXT.
           EXIT.
      *----(04.08.05)--ADD-END-----
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLERA-SEC                               SECTION.
      *
      *    職員情報初期表示
           PERFORM 3101-INIT-GMN-SEC
      *
           .
       402-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
      *----(04.08.05)--ADD-START---
      *    管理者であり１件しかない場合にチェック
           IF    ( SPA-NAI-STAFFKANRIKBN   NOT =   ZERO ) AND
                 ( SPA-GMN-TBL-MAX         =   1 )
               PERFORM 210-KANRISYA-CHK-SEC
      *        当日有効の管理者存在エラー
               IF    ( FLG-KIKAN1          =   0 ) OR
                     ( FLG-KIKAN1          =   2 )
                   MOVE    "0038"          TO  SPA-ERRCD
                   GO  TO  403-DEL-EXT
               END-IF
           END-IF
      *----(04.08.05)--ADD-END-----
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    "1003"              TO  SPA-WIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"                  TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN        TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD        TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-MAE-STYUKYMD        TO  SYS-1010-STYUKYMD
           MOVE    SPA-MAE-EDYUKYMD        TO  SYS-1010-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W06 DB DEL ERR SYSKANRI-KEY =>" SYS-1010-KEY 
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPACE               TO  SYS-1050-REC
               INITIALIZE                      SYS-1050-REC
               MOVE    "1050"              TO  SYS-1050-KANRICD
               MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1050-KBNCD(1:1)
               MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1050-KBNCD(2:4)
               MOVE    SPA-MAE-STYUKYMD    TO  SYS-1050-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYS-1050-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
               MOVE    SYS-1050-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO 
                 MOVE    "1002"            TO  SPA-ERRCD
                 DISPLAY "W06 DB DEL ERR SYSKANRI-KEY =>" SYS-1050-KEY
               END-IF
           END-IF
      *
      *----(04.08.04)--ADD-START---
           IF      SPA-GMN-TBL-MAX     =   1
               PERFORM 4031-PRT-DEL-SEC
           END-IF
      *----(04.08.04)--ADD-END-----
      *
      *    ユーザＩＤ登録判定
           MOVE    ZERO                TO  FLG-USERID-UPD
           IF      SPA-GMN-TBL-MAX     >   1
               PERFORM 40311-USERID-CHEK-SEC
           END-IF
      *    ユーザＩＤを削除
           IF      FLG-USERID-UPD      =   ZERO
               INITIALIZE                      SYSUSER-REC
               MOVE    SPA-GMN-USERID      TO  SYSUSER-USERID
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W06 DB DEL ERR USERID-KEY =>" SYSUSER-USERID
               END-IF
      *
               INITIALIZE                  PASSWD-REC
               MOVE    SPA-GMN-USERID  TO  PASSWD-USERID
               MOVE    PASSWD-REC      TO  MCPDATA-REC
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               MOVE    "DBDELETE"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        職員情報初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *----(04.08.04)--ADD-START---
      *****************************************************************
      *    削除処理（プリンタ設定）
      *****************************************************************
       4031-PRT-DEL-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                      SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1031-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1031-KBNCD(2:4)
           MOVE    "%"                 TO  SYS-1031-KBNCD(6:)
           MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           PERFORM 4031-PRT-DEL-SYORI-SEC
      *
           MOVE    SPACE               TO  SYS-1034-REC
           INITIALIZE                      SYS-1034-REC
           MOVE    "1034"              TO  SYS-1034-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1034-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1034-KBNCD(2:4)
           MOVE    "%"                 TO  SYS-1034-KBNCD(6:)
           MOVE    SPA-HOSPNUM         TO  SYS-1034-HOSPNUM
           MOVE    SYS-1034-REC        TO  MCPDATA-REC
           PERFORM 4031-PRT-DEL-SYORI-SEC
      *
           MOVE    SPACE               TO  SYS-1032-REC
           INITIALIZE                      SYS-1032-REC
           MOVE    "1032"              TO  SYS-1032-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1032-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1032-KBNCD(2:4)
           MOVE    "%"                 TO  SYS-1032-KBNCD(6:)
           MOVE    SPA-HOSPNUM         TO  SYS-1032-HOSPNUM
           MOVE    SYS-1032-REC        TO  MCPDATA-REC
           PERFORM 4031-PRT-DEL-SYORI-SEC
     *
           MOVE    SPACE               TO  SYS-1035-REC
           INITIALIZE                      SYS-1035-REC
           MOVE    "1035"              TO  SYS-1035-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1035-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1035-KBNCD(2:4)
           MOVE    "%"                 TO  SYS-1035-KBNCD(6:)
           MOVE    SPA-HOSPNUM         TO  SYS-1035-HOSPNUM
           MOVE    SYS-1035-REC        TO  MCPDATA-REC
           PERFORM 4031-PRT-DEL-SYORI-SEC
           .
       4031-PRT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理（プリンタ設定）
      *****************************************************************
       4031-PRT-DEL-SYORI-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4031-PRT-DEL-SYORI-EXT.
           EXIT.
      *----(04.08.04)--ADD-END-----
      *****************************************************************
      *    ユーザＩＤチェック処理
      *****************************************************************
       40311-USERID-CHEK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD (1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD (2:4)
           MOVE    "%"                 TO  SYS-1010-KBNCD (6:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL        (FLG-SYSKANRI      =   1 )
                           OR   (FLG-USERID-UPD    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-USERID     =   SPA-GMN-USERID
                   MOVE    1                   TO  FLG-USERID-UPD
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40311-USERID-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ切替処理
      *****************************************************************
       480-BETUGAMEN-SEC              SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    1                   TO  FLG-TOUROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    TRUE
               WHEN  ( SPA-GMN-PAGENO      =   1 )
                   MOVE    2                   TO  SPA-GMN-PAGENO
                   MOVE    21                  TO  SPA-GMN-CUR
               WHEN  ( SPA-GMN-PAGENO      =   2 )
                  AND( SPA-GRPHOSPKBN      =   1 )
                   MOVE    3                   TO  SPA-GMN-PAGENO
                   MOVE    201                 TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    1                   TO  SPA-GMN-PAGENO
      *            基本画面
      *----(04.07.01)--UPD-START---
                   IF      SPA-GMN-SYOKUKBN    =   "1"
      *                医師の場合は医籍登録番号へ
                       MOVE    9               TO  SPA-GMN-CUR
                   ELSE
      *                スーパーバイザーの場合は診療行為初期画面へ
                       IF      SPA-NAI-SUPERVISOR  =   "T"
                           MOVE    19              TO  SPA-GMN-CUR
                       ELSE
      *                    管理者である場合は外来まとめへ
                           IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                               MOVE    66              TO  SPA-GMN-CUR
      *                    管理者でない場合は医事業務へ
                           ELSE
                               MOVE    31              TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
      *----(04.07.01)--UPD-END-----
               END-EVALUATE
           END-IF
           .
       480-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *H30.10
      *    9999 はエラー
           IF      SPA-GMN-SYOKUCOD    =   "9999"
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1002"           TO  SPA-WIDCD
      *       有効開始日の変更は追加
              IF     (SPA-GMN-SELNUM       =   ZERO )
                  MOVE    "1001"           TO  SPA-WIDCD
               END-IF
      *H27.10
      *       有効開始日の変更は追加
               IF     (SPA-GMN-SYORI       =   1    )
                 AND  (SPA-GMN-SELNUM  NOT =   ZERO )
                   MOVE    "1005"          TO  SPA-WIDCD
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *----(04.07.01)--UPD-START---
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    職員区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               IF      SPA-GMN-SYOKUKBN    =   SPA-GMN-SYOKUKBN-LIST
                                                     (IDX)(1:1)
                   MOVE    SPA-SYOKUKBN-MAX    TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    職員コード
           IF     (SPA-GMN-SYOKUCOD    =   SPACE )  OR
                  (SPA-GMN-SYOKUCOD    NOT NUMERIC)
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-SYOKUCOD-MAX) OR
                                  (FLG-OK      =   1    )
                   IF      SPA-GMN-SYOKUCOD    =   SPA-GMN-TSYOKUCOD
                                                         (IDX)
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *        入力済みの時、選択があること
               IF      FLG-OK          =   1
                   IF      SPA-GMN-TBL-MAX =   ZERO
                       MOVE    "0021"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    医師でゼロの場合、警告
           IF     (SPA-GMN-SYOKUKBN    =   "1"  )  AND
                  (SPA-GMN-SYOKUCOD    =   ZERO )
               IF      SPA-NAI-KEIFLG      =   ZERO
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KEIFLG
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           NOT =   SPACE
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               MOVE    "00000000"         TO  SPA-NAI-STYUKYMD
           END-IF
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               MOVE    "99999999"         TO  SPA-NAI-EDYUKYMD
           END-IF
           IF      SPA-ERRCD          =   SPACE
               IF      SPA-NAI-STYUKYMD   >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *H27.10
           MOVE    ZERO                TO  FLG-CHK
      *    期間変更時に、追加可能かのチェック
           IF     (SPA-GMN-SELNUM  NOT =   ZERO )
               MOVE    2                   TO  SPA-GMN-SYORI
               IF     (SPA-NAI-STYUKYMD    =   SPA-NAI-TSTYUKYMD
                                                   (SPA-GMN-SELNUM))
                 OR   (SPA-NAI-EDYUKYMD    =   SPA-NAI-TEDYUKYMD
                                                   (SPA-GMN-SELNUM))
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-CHK
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL ( IDX     >   SPA-GMN-TBL-MAX )
                           OR    ( FLG-CHK     =   1     )
                       IF     (SPA-NAI-TEDYUKYMD (IDX)
                                           <   SPA-NAI-STYUKYMD )  OR
                              (SPA-NAI-TSTYUKYMD (IDX)
                                           >   SPA-NAI-EDYUKYMD )
                           CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-CHK
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK         =   ZERO
      *                期間重複なし 追加処理
                       MOVE    1                TO  SPA-GMN-SYORI
                   END-IF
               END-IF
           END-IF
      *    期間重複チェック
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-TBL-MAX )
                   OR    ( SPA-ERRCD       NOT =   SPACE )
                   OR    ( SPA-GMN-SYORI       =   1   )
               IF     (SPA-GMN-SELNUM  NOT =   IDX )
                   IF     (SPA-NAI-TEDYUKYMD (IDX)
                                       <   SPA-NAI-STYUKYMD )  OR
                          (SPA-NAI-TSTYUKYMD (IDX)
                                       >   SPA-NAI-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0020"           TO  SPA-ERRCD
                       MOVE    3                TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
      *    オペレータＩＤ
           IF     (SPA-GMN-USERID      =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    氏名必須チェック
           IF     (SPA-GMN-NAME        =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        システムユーザーチェック
               MOVE    SPA-GMN-USERID      TO  WRK-USERID
               PERFORM 49001-SYSUSER-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        パスワードチェック
               PERFORM 4121-PASSWORD-CHK-SEC
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *----(04.07.01)--UPD-END-----
      *****************************************************************
      *       システムユーザーチェック処理
      *****************************************************************
       49001-SYSUSER-CHK-SEC           SECTION.
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    WRK-USERID          TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           MOVE    ZERO                TO  FLG-USERID-UPD
           IF      FLG-SYSUSER         =   ZERO
      *        登録済みの時、同じ医療機関であること
               IF      SPA-HOSPNUM     NOT =   SYSUSER-HOSPNUM
                   MOVE    "0025"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    ZERO                TO  FLG-USERID-UPD
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERID-UPD
           END-IF
           .
       49001-SYSUSER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-STYUKYMD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1010-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1010-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1010-REC
               INITIALIZE                      SYS-1010-REC
               MOVE   "1010"                   TO  SYS-1010-KANRICD
               MOVE    SPA-GMN-SYOKUKBN        TO  SYS-1010-KBNCD(1:1)
               MOVE    SPA-GMN-SYOKUCOD        TO  SYS-1010-KBNCD(2:4)
           END-IF
      *
      *    ユザー
      *    システムユーザーチェック
           MOVE    SPA-GMN-USERID      TO  WRK-USERID
           PERFORM 49001-SYSUSER-CHK-SEC
      *
           MOVE    SPACE               TO  WRK-MAE-USERID
           IF      SPA-GMN-USERID  NOT =   SYS-1010-USERID
               MOVE    SYS-1010-USERID     TO  WRK-MAE-USERID
               MOVE    1                   TO  FLG-USERID-UPD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-1010-EDYUKYMD
           MOVE    SPA-GMN-USERID          TO  SYS-1010-USERID 
           MOVE    SPA-GMN-KANANAME        TO  SYS-1010-KANANAME 
           MOVE    SPA-GMN-NAME            TO  SYS-1010-NAME 
           MOVE    SPA-GMN-ISEKINUM        TO  SYS-1010-ISEKINUM 
           MOVE    SPA-GMN-HKNDRNUMS       TO  SYS-1010-HKNDRNUMS
           MOVE    SPA-GMN-HKNDRNUMK       TO  SYS-1010-HKNDRNUMK
           MOVE    SPA-GMN-MAYAKULICENSE   TO  SYS-1010-MAYAKULICENSE
      *
           PERFORM 4900-KOUSHIN-GSRAUTH-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-GMN-SEMONKACD(IDX)
                                           TO  SYS-1010-SEMONKACD (IDX)
           END-PERFORM
           MOVE    SPA-NAI-KANRIAUTHFLG    TO  SYS-1010-KANRIAUTHFLG
      *
           MOVE    SPA-GMN-ORCA21FLG       TO  SYS-1010-ORCA21FLG
      *    表示区分
           MOVE    SPA-NAI-HYOJIFLG        TO  SYS-1010-HYOJIFLG
      *
           MOVE    SPA-GMN-SKYPRTFLG       TO  SYS-1010-SKYPRTFLG
           MOVE    SPA-GMN-SYOHOPRTFLG     TO  SYS-1010-SYOHOPRTFLG
           MOVE    SPA-GMN-YAKUJYOFLG      TO  SYS-1010-YAKUJYOPRTFLG
           MOVE    SPA-GMN-MEIPRTFLG       TO  SYS-1010-MEIPRTFLG
           MOVE    SPA-GMN-FORMALFLG       TO  SYS-1010-FORMALFLG
           MOVE    SPA-GMN-CLIENT-SAVE     TO  SYS-1010-CLIENT-SAVE
           MOVE    SPA-GMN-USER-KANRI      TO  SYS-1010-USER-KANRI
           MOVE    SPA-GMN-CLIENT-PRT      TO  SYS-1010-CLIENT-PRT
           MOVE    SPA-GMN-DIALOGFLG       TO  SYS-1010-DIALOGFLG
      *
           INITIALIZE                          SYS-1010-GRPHOSPINF-G
           MOVE    ZERO                    TO  IDX-GRP
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   SPA-GMN-GRPHOSP-MAX )
      *
               IF    ( SPA-GMN-GRPUSERID (IDX) NOT =   SPACE )
                   COMPUTE IDX-GRP =   IDX-GRP +   1
                   MOVE    SPA-GRPHOSPNUM (IDX)
                                   TO  SYS-1010-GRPHOSPNUM    (IDX-GRP)
                   MOVE    SPA-GMN-GRPUSERID (IDX)
                                   TO  SYS-1010-GRPHOSPUSERID (IDX-GRP)
               END-IF
      *
           END-PERFORM
      *H26.1
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
               CONTINUE
           ELSE
      *        最大１０件の履歴とする
               IF      SPA-GMN-TBL-MAX    =   10
                   MOVE   "0050"              TO  SPA-ERRCD
                   MOVE   5                   TO  SPA-GMN-CUR
                   GO     TO      490-KOUSHIN-EXT
               END-IF
           END-IF
      *
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
      *        更新
               MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
               MOVE    SYS-1010-REC        TO  SYSKANRI-REC
      *
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-MAE-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYSUP-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB UPD ERR KEY:" SYS-1010-KEY 
               END-IF
           ELSE
      *        追加
               MOVE    SMCNDATE-YMD        TO  SYS-1010-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1010-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1010-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB INS ERR KEY:" SYS-1010-KEY 
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4905-SYS1050-UPD-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-USERID-UPD      =   1     )
      *        システムユーザー更新処理
               PERFORM 4905-SYSUSER-UPD-SEC
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-PASSWORD1   NOT =   SPACE  )
      *        パスワード更新処理
               PERFORM 4905-PASSWD-UPD-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        職員情報初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       チェック登録処理
      *****************************************************************
       4900-KOUSHIN-GSRAUTH-SEC           SECTION.
      *医事業務
           IF  SPA-GMN-CHK(1)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(1:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(1:1)
           END-IF
      *受付
           IF  SPA-GMN-CHK(2)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(16:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(16:1)
           END-IF
      *登録
           IF  SPA-GMN-CHK(3)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(17:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(17:1)
           END-IF
      *照会
           IF  SPA-GMN-CHK(4)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(18:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(18:1)
           END-IF
      *予約
           IF  SPA-GMN-CHK(5)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(19:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(19:1)
           END-IF
      *診療行為
           IF  SPA-GMN-CHK(6)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(21:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(21:1)
           END-IF
      *病名
           IF  SPA-GMN-CHK(7)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(22:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(22:1)
           END-IF
      *収納
           IF  SPA-GMN-CHK(8)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(23:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(23:1) 
           END-IF
      *会計照会
           IF  SPA-GMN-CHK(9)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(24:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(24:1)
           END-IF
      *データチェック
           IF  SPA-GMN-CHK(10)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(26:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(26:1)
           END-IF
      *明細書
           IF  SPA-GMN-CHK(11)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(27:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(27:1)
           END-IF
      *請求管理
           IF  SPA-GMN-CHK(12)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(28:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(28:1)
           END-IF
      *総括票
           IF  SPA-GMN-CHK(13)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(29:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(29:1)
           END-IF
      *日次統計
           IF  SPA-GMN-CHK(14)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(31:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(31:1)
           END-IF
      *月次統計
           IF  SPA-GMN-CHK(15)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(32:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(32:1)
           END-IF
      *データ出力
           IF  SPA-GMN-CHK(16)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(41:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(41:1)
           END-IF
      *外部媒体
           IF  SPA-GMN-CHK(17)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(47:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(47:1)
           END-IF
      *マスタ登録
           IF  SPA-GMN-CHK(18)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(51:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(51:1)
           END-IF
      *マスタ更新
           IF  SPA-GMN-CHK(19)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(52:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(52:1) 
           END-IF
      *システム管理
           IF  SPA-GMN-CHK(20)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(61:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(61:1)
           END-IF
      *点数マスタ
           IF  SPA-GMN-CHK(21)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(62:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(62:1)
           END-IF
      *チェックマスタ
           IF  SPA-GMN-CHK(22)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(63:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(63:1)
           END-IF
      *保険番号マスタ
           IF  SPA-GMN-CHK(23)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(64:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(64:1)
           END-IF
      *保険者マスタ
           IF  SPA-GMN-CHK(24)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(65:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(65:1)
           END-IF
      *人名辞書マスタ
           IF  SPA-GMN-CHK(25)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(66:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(66:1)
           END-IF
      *----(01.01.05)----START------------
      *薬剤情報マスタ
           IF  SPA-GMN-CHK(26)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(67:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(67:1)
           END-IF
      *----(01.01.05)----START------------
      *----(01.01.07)----START------------
      *入退院登録
           IF  SPA-GMN-CHK(27)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(36:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(36:1)
           END-IF
      *入院会計照会
           IF  SPA-GMN-CHK(28)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(37:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(37:1)
           END-IF
      *入院定期請求
           IF  SPA-GMN-CHK(29)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(38:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(38:1)
           END-IF
      *退院時仮計算
           IF  SPA-GMN-CHK(30)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(39:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(39:1)
           END-IF
      *病床管理
           IF  SPA-GMN-CHK(31)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(56:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(56:1)
           END-IF
      *入院患者照会
           IF  SPA-GMN-CHK(32)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(57:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(57:1)
           END-IF
      *----(01.01.07)----START------------
      *----(02.00.01)--ADD-START---
      *プログラム管理
           IF  SPA-GMN-CHK(33)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(3:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(3:1)
           END-IF
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
      *住所マスタ
           IF  SPA-GMN-CHK(34)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(68:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(68:1)
           END-IF
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
      *ヘルプマスタ
           IF  SPA-GMN-CHK(35)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(69:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(69:1)
           END-IF
      *----(02.00.03)--ADD-END-----
      *外来まとめ
           IF  SPA-GMN-CHK(36)  =  "T"
               MOVE     "1"    TO  SYS-1010-GSRAUTH(5:1)
           ELSE
               MOVE     "0"    TO  SYS-1010-GSRAUTH(5:1)
           END-IF
          .
       4900-KOUSHIN-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報２更新処理
      *****************************************************************
       4905-SYS1050-UPD-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1050-REC
           INITIALIZE                      SYS-1050-REC
           MOVE    "1050"              TO  SYS-1050-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1050-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1050-KBNCD(2:4)
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1050-STYUKYMD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1050-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
           MOVE    SYS-1050-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
      *        更新
               PERFORM 49051-SYS1050-UPD-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-1050-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1050-UPHMS
               MOVE    SYS-1050-REC        TO  SYSKANRI-REC
      *
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-MAE-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYSUP-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB UPD ERR KEY:" SYS-1050-KEY 
               END-IF
           ELSE
               PERFORM 49051-SYS1050-UPD-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-1050-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1050-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1050-UPHMS
               MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
               MOVE    SYS-1050-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB INS ERR KEY:" SYS-1050-KEY 
               END-IF
           END-IF
      *
          .
       4905-SYS1050-UPD-EXT.
           EXIT.
      *****************************************************************
      *    職員情報２更新処理
      *****************************************************************
       49051-SYS1050-UPD-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-1050-REC
           INITIALIZE                      SYS-1050-REC
           MOVE    "1050"              TO  SYS-1050-KANRICD
           MOVE    SPA-GMN-SYOKUKBN    TO  SYS-1050-KBNCD(1:1)
           MOVE    SPA-GMN-SYOKUCOD    TO  SYS-1050-KBNCD(2:4)
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1050-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SYS-1050-EDYUKYMD
           MOVE    SPA-GMN-ICCARDRW    TO  SYS-1050-ICCARDRW
      *
      *    入力スクロール行数
           MOVE    SPA-GMN-GLINECNT    TO  SYS-1050-GLINECNT
           MOVE    SPA-GMN-NLINECNT    TO  SYS-1050-NLINECNT
      *
      *    プリンタへのデータ送信単位
           MOVE    SPA-GMN-PRTCONF    TO  SYS-1050-PRTCONF
      *
          .
       49051-SYS1050-UPD-EXT.
           EXIT.
      *****************************************************************
      *    システムユーザー更新処理
      *****************************************************************
       4905-SYSUSER-UPD-SEC              SECTION.
      *
      *    前のユーザＩＤを削除
           IF      WRK-MAE-USERID  NOT =   SPACE
               INITIALIZE                      SYSUSER-REC
               MOVE    WRK-MAE-USERID      TO  SYSUSER-USERID
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W06 DB DEL ERR USERID-KEY =>" SYSUSER-USERID
               END-IF
      *----(04.08.02)--ADD-START---
      *        パスワードを削除
               INITIALIZE                  PASSWD-REC
               MOVE    WRK-MAE-USERID  TO  PASSWD-USERID
               MOVE    PASSWD-REC      TO  MCPDATA-REC
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *----(04.08.02)--ADD-END-----
           END-IF
      *
      *    新規追加
           INITIALIZE                      SYSUSER-REC
           MOVE    SYS-1010-USERID     TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   1
               INITIALIZE                      SYSUSER-REC
               MOVE    SYS-1010-USERID     TO  SYSUSER-USERID
               MOVE    SPA-HOSPNUM         TO  SYSUSER-HOSPNUM
               IF      SYS-1010-KBNCD      =   "00001"
                 MOVE    "T"                   TO  SYSUSER-SUPERVISOR
               ELSE
                 MOVE    "F"                   TO  SYSUSER-SUPERVISOR
               END-IF
               MOVE    SMCNDATE-YMD        TO  SYSUSER-UPYMD
      *
               MOVE    SYSUSER-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysuser"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               MOVE    "DBINSERT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W06 DB INS ERR USERID-KEY =>" SYSUSER-USERID
               END-IF
           END-IF
          .
       4905-SYSUSER-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    パスワード更新処理
      *****************************************************************
       4905-PASSWD-UPD-SEC         SECTION.
      *
      *----(04.08.03)--ADD-START---
      *    パスワード削除のときは更新しない
           IF    ( SPA-GMN-PASSWORD1(1:2)  =   "/d" )  OR
                 ( SPA-GMN-PASSWORD1(1:2)  =   "/D" )
      *        パスワードを削除
               INITIALIZE                  PASSWD-REC
               MOVE    SPA-GMN-USERID  TO  PASSWD-USERID
               MOVE    PASSWD-REC      TO  MCPDATA-REC
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               GO  TO  4905-PASSWD-UPD-EXT
           END-IF
      *----(04.08.03)--ADD-END-----
      *
           MOVE    SPACE           TO  WRK-PASSWORD
      *
           INITIALIZE                  ORCSAUTHAREA
           MOVE    "passwd"        TO  AUTH-COMMAND
           MOVE    1               TO  AUTH-ID
           MOVE    SPA-GMN-PASSWORD1
                                   TO  AUTH-PASSWORD1
           CALL    "orcsauth"          USING
                                       ORCSAUTHAREA
           IF      AUTH-RC         =   ZERO
                MOVE    AUTH-PASSWORD2      TO  WRK-PASSWORD
           END-IF
      *
           PERFORM 900-PASSWD-READ-SEC
      *
           IF      FLG-PASSWD      =   ZERO
               INITIALIZE                  PASSWD-REC
               MOVE    SPA-GMN-USERID  TO  PASSWD-USERID
               MOVE    WRK-PASSWORD    TO  PASSWD-PASSWORD
      *
               MOVE    PASSWD-REC      TO  MCPDATA-REC
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    SPA-GMN-USERID  TO  PASSWD-USERID
               MOVE    WRK-PASSWORD    TO  PASSWD-PASSWORD
      *
               MOVE    PASSWD-REC      TO  MCPDATA-REC
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBINSERT"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
           .
       4905-PASSWD-UPD-EXT.
           EXIT.
      *!!!
      *H27.11
      *****************************************************************
      *    職員一覧（Ｗ０６１）処理
      *****************************************************************
       460-W061-SEL-SEC           SECTION.
      *
      *
           IF     (SPA-GMN-SYOKUKBN    =   "1"
                                       OR  "2"
                                       OR  "3"
                                       OR  "4"
                                       OR  "5" )
             AND  (SPA-NAI-STAFFKANRIKBN   NOT =   ZERO )
               PERFORM 4601-W061-SYORI-SEC
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       460-W061-SEL-EXT.
           EXIT.
      *****************************************************************
      *    職員一覧画面遷移処理
      *****************************************************************
       4601-W061-SYORI-SEC        SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W06             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW061"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-W06
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
      *
           IF      SPA-W061-MAX        =   ZERO
               MOVE    "0028"              TO  SPA-ERRCD
               GO      TO      4601-W061-SYORI-EXT
           END-IF
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
           MOVE    "W061"              TO  SPA-SAKIPG
           MOVE    "W06"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W061"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4601-W061-SYORI-EXT.
           EXIT.
      *H27.11
      *!!!
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W06
      *
           INITIALIZE                      W06
           MOVE    SPA-GMN-SYOKUKBN-G      TO  W06-SYOKUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUKBN-MAX
               MOVE    SPA-GMN-SYOKUKBN-LIST (IDX)
                                           TO  W06-SYOKUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SYOKUKBN-MAX        TO  W06-SYOKUKBN-COUNT
      *
           MOVE    SPA-GMN-SYOKUCOD-G      TO  W06-SYOKUCOD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOKUCOD-MAX
               MOVE    SPA-GMN-SYOKUCOD-LIST (IDX)
                                           TO  W06-SYOKUCOD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SYOKUCOD-MAX        TO  W06-SYOKUCOD-COUNT
      *
           MOVE    SPA-GMN-STYUKYMD        TO  W06-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W06-EDYUKYMD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  W06-TNO (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX)
                                           TO  W06-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX)
                                           TO  W06-TEDYUKYMD (IDX)
               IF      SPA-GMN-SELNUM      =   IDX
                   MOVE    "T"             TO  W06-KIKAN-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W06-KIKAN-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-TBL-MAX         TO  W06-KIKAN-COUNT
           MOVE    SPA-GMN-SELNUM          TO  W06-SELNUM
      *
           MOVE    SPA-GMN-PASSWORD1       TO  W06-PASSWORD1
           MOVE    SPA-GMN-PASSWORD2       TO  W06-PASSWORD2
      *
           MOVE    SPA-GMN-USERID          TO  W06-USERID
           MOVE    SPA-GMN-KANANAME        TO  W06-KANANAME
           MOVE    SPA-GMN-NAME            TO  W06-NAME  
           MOVE    SPA-GMN-ISEKINUM        TO  W06-ISEKINUM
           MOVE    SPA-GMN-HKNDRNUMS       TO  W06-HKNDRNUMS
           MOVE    SPA-GMN-HKNDRNUMK       TO  W06-HKNDRNUMK
           MOVE    SPA-GMN-MAYAKULICENSE   TO  W06-MAYAKULICENSE
      *
           MOVE  SPA-GMN-CHK(1)            TO  W06-CHK01
           MOVE  SPA-GMN-CHK(2)            TO  W06-CHK02
           MOVE  SPA-GMN-CHK(3)            TO  W06-CHK03
           MOVE  SPA-GMN-CHK(4)            TO  W06-CHK04
           MOVE  SPA-GMN-CHK(5)            TO  W06-CHK05
           MOVE  SPA-GMN-CHK(6)            TO  W06-CHK06
           MOVE  SPA-GMN-CHK(7)            TO  W06-CHK07
           MOVE  SPA-GMN-CHK(8)            TO  W06-CHK08
           MOVE  SPA-GMN-CHK(9)            TO  W06-CHK09
           MOVE  SPA-GMN-CHK(10)           TO  W06-CHK10
           MOVE  SPA-GMN-CHK(11)           TO  W06-CHK11
           MOVE  SPA-GMN-CHK(12)           TO  W06-CHK12
           MOVE  SPA-GMN-CHK(13)           TO  W06-CHK13
           MOVE  SPA-GMN-CHK(14)           TO  W06-CHK14
           MOVE  SPA-GMN-CHK(15)           TO  W06-CHK15
           MOVE  SPA-GMN-CHK(16)           TO  W06-CHK16
           MOVE  SPA-GMN-CHK(17)           TO  W06-CHK17
           MOVE  SPA-GMN-CHK(18)           TO  W06-CHK18
           MOVE  SPA-GMN-CHK(19)           TO  W06-CHK19
           MOVE  SPA-GMN-CHK(20)           TO  W06-CHK20
           MOVE  SPA-GMN-CHK(21)           TO  W06-CHK21
           MOVE  SPA-GMN-CHK(22)           TO  W06-CHK22
           MOVE  SPA-GMN-CHK(23)           TO  W06-CHK23
           MOVE  SPA-GMN-CHK(24)           TO  W06-CHK24
           MOVE  SPA-GMN-CHK(25)           TO  W06-CHK25
      *----(01.01.05)----START------------
           MOVE  SPA-GMN-CHK(26)           TO  W06-CHK26
      *----(01.01.05)----START------------
      *----(01.01.07)----START------------
           MOVE  SPA-GMN-CHK(27)           TO  W06-CHK27
           MOVE  SPA-GMN-CHK(28)           TO  W06-CHK28
           MOVE  SPA-GMN-CHK(29)           TO  W06-CHK29
           MOVE  SPA-GMN-CHK(30)           TO  W06-CHK30
           MOVE  SPA-GMN-CHK(31)           TO  W06-CHK31
           MOVE  SPA-GMN-CHK(32)           TO  W06-CHK32
      *----(01.01.07)----START------------
      *----(02.00.01)--ADD-START---
           MOVE  SPA-GMN-CHK(33)           TO  W06-CHK33
      *----(02.00.01)--ADD-END-----
      *----(02.00.02)--ADD-START---
           MOVE  SPA-GMN-CHK(34)           TO  W06-CHK34
      *----(02.00.02)--ADD-END-----
      *----(02.00.03)--ADD-START---
           MOVE  SPA-GMN-CHK(35)           TO  W06-CHK35
      *----(02.00.03)--ADD-END-----
           MOVE  SPA-GMN-CHK(36)           TO  W06-CHK36
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYKA-MAX
                   MOVE    SPA-GMN-SEMONKACD-TBL (IDY) 
                                           TO  W06-SEMONKACD-ITEM
                                                        (IDX IDY)
               END-PERFORM
               MOVE    SPA-SRYKA-MAX       TO  W06-SEMONKACD-COUNT(IDX)
               MOVE    SPA-GMN-SEMONKACD-G(IDX)
                                           TO  W06-SEMONKACD(IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KANRIAUTHFLG-MAX
               MOVE    SPA-GMN-KANRIAUTH-LST(IDX)
                                       TO  W06-KANRIAUTHFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-KANRIAUTHFLG-MAX  TO  W06-KANRIAUTHFLG-COUNT
           MOVE  SPA-GMN-KANRIAUTH-MEI TO  W06-KANRIAUTHFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ORCA21FLG-MAX
               MOVE    SPA-GMN-ORCA21FLG-LST(IDX)
                                       TO  W06-ORCA21FLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-ORCA21FLG-MAX     TO  W06-ORCA21FLG-COUNT
           MOVE  SPA-GMN-ORCA21FLG-G   TO  W06-ORCA21FLG
      *
      *    表示区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HYOJIFLG-MAX
               MOVE    SPA-GMN-HYOJIFLG-LST(IDX)
                                       TO  W06-HYOJIFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-HYOJIFLG-MAX      TO  W06-HYOJIFLG-COUNT
           MOVE  SPA-GMN-HYOJIFLG-G    TO  W06-HYOJIFLG
      *
      *    請求書発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SKYPRTFLG-MAX
               MOVE    SPA-GMN-SKYPRTFLG-LST(IDX)
                                       TO  W06-SKYPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SKYPRTFLG-MAX     TO  W06-SKYPRTFLG-COUNT
           MOVE  SPA-GMN-SKYPRTFLG-G   TO  W06-SKYPRTFLG
      *    院外処方せん発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               MOVE    SPA-GMN-SYOHOPRTFLG-LST(IDX)
                                       TO  W06-SYOHOPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SYOHOPRTFLG-MAX     TO  W06-SYOHOPRTFLG-COUNT
           MOVE  SPA-GMN-SYOHOPRTFLG-G   TO  W06-SYOHOPRTFLG
      *    薬剤情報発行フラグ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKUJYOFLG-MAX
               MOVE    SPA-GMN-YAKUJYOFLG-LST(IDX)
                                       TO  W06-YAKUJYOFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-YAKUJYOFLG-MAX     TO  W06-YAKUJYOFLG-COUNT
           MOVE  SPA-GMN-YAKUJYOFLG-G   TO  W06-YAKUJYOFLG
      *    診療費明細書
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               MOVE    SPA-GMN-MEIPRTFLG-LST(IDX)
                                       TO  W06-MEIPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MEIPRTFLG-MAX     TO  W06-MEIPRTFLG-COUNT
           MOVE  SPA-GMN-MEIPRTFLG-G   TO  W06-MEIPRTFLG
      *    検査正式名称表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FORMALFLG-MAX
               MOVE    SPA-GMN-FORMALFLG-LST(IDX)
                                       TO  W06-FORMALFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-FORMALFLG-MAX     TO  W06-FORMALFLG-COUNT
           MOVE  SPA-GMN-FORMALFLG-G   TO  W06-FORMALFLG
      *
      *    入力スクロール行数
           MOVE    SPA-GMN-GLINECNT    TO  W06-GLINECNT
           MOVE    SPA-GMN-NLINECNT    TO  W06-NLINECNT
      *
      *    クライアント保存可否表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-SAVE-MAX
               MOVE    SPA-GMN-CLIENT-SAVE-LST(IDX)
                                       TO  W06-CLIENT-SAVE-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-CLIENT-SAVE-MAX   TO  W06-CLIENT-SAVE-COUNT
           MOVE  SPA-GMN-CLIENT-SAVE-G TO  W06-CLIENT-SAVE
      *    ユーザーログイン権限表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USER-KANRI-MAX
               MOVE    SPA-GMN-USER-KANRI-LST(IDX)
                                       TO  W06-USER-KANRI-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-USER-KANRI-MAX    TO  W06-USER-KANRI-COUNT
           MOVE  SPA-GMN-USER-KANRI-G  TO  W06-USER-KANRI
      *    プリンタへのデータ送信単位
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PRTCONF-MAX
               MOVE    SPA-GMN-PRTCONF-LST(IDX)
                                       TO  W06-PRTCONF-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-PRTCONF-MAX    TO  W06-PRTCONF-COUNT
           MOVE  SPA-GMN-PRTCONF-G  TO  W06-PRTCONF
      *    クライアント印刷可否表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-CLIENT-PRT-MAX
               MOVE    SPA-GMN-CLIENT-PRT-LST(IDX)
                                       TO  W06-CLIENT-PRT-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-CLIENT-PRT-MAX    TO  W06-CLIENT-PRT-COUNT
           MOVE  SPA-GMN-CLIENT-PRT-G  TO  W06-CLIENT-PRT
      *    ダイアログ表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DIALOGFLG-MAX
               MOVE    SPA-GMN-DIALOGFLG-LST(IDX)
                                       TO  W06-DIALOGFLG-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-DIALOGFLG-MAX     TO  W06-DIALOGFLG-COUNT
           MOVE  SPA-GMN-DIALOGFLG-G   TO  W06-DIALOGFLG
      *    ＩＣカードリーダ／ライタ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ICCARDRW-MAX
               MOVE    SPA-GMN-ICCARDRW-LST(IDX)
                                       TO  W06-ICCARDRW-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-ICCARDRW-MAX      TO  W06-ICCARDRW-COUNT
           MOVE  SPA-GMN-ICCARDRW-G    TO  W06-ICCARDRW
      *
      *    他院情報設定画面
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL (     IDX     >   SPA-GMN-GRPHOSP-MAX )
               MOVE    SPA-GRPHOSPNUM (IDX)
                                       TO  W06-GRPHOSPNUM(IDX)
               MOVE    SPA-GRPHOSPNAME(IDX)
                                       TO  W06-GRPHOSPNAME(IDX)
               MOVE    SPA-GMN-GRPUSERID(IDX)
                                       TO  W06-GRPUSERID(IDX)
           END-PERFORM
      *
           MOVE    WIDGET-INSENSITIVE  TO  W06-TGN3-STATE
           IF    ( SPA-GRPHOSPKBN      =   1 )
               MOVE    WIDGET-NORMAL   TO  W06-TGN3-STATE
           END-IF
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    1
                   MOVE    0           TO  W06-PAGENO
               WHEN    2
                   MOVE    1           TO  W06-PAGENO
               WHEN    3
                   MOVE    2           TO  W06-PAGENO
           END-EVALUATE
      *
           PERFORM 510-GSRAUTH-SEC
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *----(04.07.01)--UPD-START---
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           IF     (SPA-GMN-SYORI       =   1   ) AND
                  (SPA-GMN-SELNUM      =   ZERO)
               MOVE    WIDGET-INSENSITIVE  TO  W06-B03-STATE 
           ELSE
               MOVE    WIDGET-NORMAL       TO  W06-B03-STATE
           END-IF
      *    オルカマスターのみ削除不可能にする
           IF      (SPA-GMN-SYOKUKBN   =   "0"  )  AND
                   (SPA-GMN-SYOKUCOD   =   "0001") AND
                   (SPA-GMN-TBL-MAX    =   1    )
               MOVE    WIDGET-INSENSITIVE      TO  W06-B03-STATE
           END-IF
      *
      *    スーパーバイザーの場合
           IF      SPA-NAI-SUPERVISOR      =  "T"
               MOVE    "T"                 TO  W06-CHK01
                                               W06-CHK02
                                               W06-CHK03
                                               W06-CHK04
                                               W06-CHK05
                                               W06-CHK06
                                               W06-CHK07
                                               W06-CHK08
                                               W06-CHK09
                                               W06-CHK10
                                               W06-CHK11
                                               W06-CHK12
                                               W06-CHK13
                                               W06-CHK14
                                               W06-CHK15
                                               W06-CHK16
                                               W06-CHK17
                                               W06-CHK18
                                               W06-CHK19
                                               W06-CHK20
                                               W06-CHK21
                                               W06-CHK22
                                               W06-CHK23
                                               W06-CHK24
                                               W06-CHK25
                                               W06-CHK26
                                               W06-CHK27
                                               W06-CHK28
                                               W06-CHK29
                                               W06-CHK30
                                               W06-CHK31
                                               W06-CHK32
                                               W06-CHK33
                                               W06-CHK34
                                               W06-CHK35
                                               W06-CHK36
               MOVE    WIDGET-INSENSITIVE  TO  W06-CHK01-STATE
                                               W06-CHK02-STATE
                                               W06-CHK03-STATE
                                               W06-CHK04-STATE
                                               W06-CHK05-STATE
                                               W06-CHK06-STATE
                                               W06-CHK07-STATE
                                               W06-CHK08-STATE
                                               W06-CHK09-STATE
                                               W06-CHK10-STATE
                                               W06-CHK11-STATE
                                               W06-CHK12-STATE
                                               W06-CHK13-STATE
                                               W06-CHK14-STATE
                                               W06-CHK15-STATE
                                               W06-CHK16-STATE
                                               W06-CHK17-STATE
                                               W06-CHK18-STATE
                                               W06-CHK19-STATE
                                               W06-CHK20-STATE
                                               W06-CHK21-STATE
                                               W06-CHK22-STATE
                                               W06-CHK23-STATE
                                               W06-CHK24-STATE
                                               W06-CHK25-STATE
                                               W06-CHK26-STATE
                                               W06-CHK27-STATE
                                               W06-CHK28-STATE
                                               W06-CHK29-STATE
                                               W06-CHK30-STATE
                                               W06-CHK31-STATE
                                               W06-CHK32-STATE
                                               W06-CHK33-STATE
                                               W06-CHK34-STATE
                                               W06-CHK35-STATE
                                               W06-CHK36-STATE
      *        管理者であるにする
               MOVE    SPA-GMN-KANRIAUTH-LST(1)
                                           TO  W06-KANRIAUTHFLG
               MOVE    WIDGET-INSENSITIVE  TO  W06-KANRIAUTHFLG-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W06-CHK01-STATE
                                               W06-CHK02-STATE
                                               W06-CHK03-STATE
                                               W06-CHK04-STATE
                                               W06-CHK05-STATE
                                               W06-CHK06-STATE
                                               W06-CHK07-STATE
                                               W06-CHK08-STATE
                                               W06-CHK09-STATE
                                               W06-CHK10-STATE
                                               W06-CHK11-STATE
                                               W06-CHK12-STATE
                                               W06-CHK13-STATE
                                               W06-CHK14-STATE
                                               W06-CHK15-STATE
                                               W06-CHK16-STATE
                                               W06-CHK17-STATE
                                               W06-CHK18-STATE
                                               W06-CHK19-STATE
                                               W06-CHK20-STATE
                                               W06-CHK21-STATE
                                               W06-CHK22-STATE
                                               W06-CHK23-STATE
                                               W06-CHK24-STATE
                                               W06-CHK25-STATE
                                               W06-CHK26-STATE
                                               W06-CHK27-STATE
                                               W06-CHK28-STATE
                                               W06-CHK29-STATE
                                               W06-CHK30-STATE
                                               W06-CHK31-STATE
                                               W06-CHK32-STATE
                                               W06-CHK33-STATE
                                               W06-CHK34-STATE
                                               W06-CHK35-STATE
                                               W06-CHK36-STATE
               MOVE    WIDGET-NORMAL       TO  W06-KANRIAUTHFLG-STATE
      *
      *        管理者である時
               IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                   MOVE    "T"                 TO  W06-CHK01
                                                   W06-CHK18
                                                   W06-CHK19
                                                   W06-CHK20
                                                   W06-CHK33
                   MOVE    WIDGET-INSENSITIVE  TO  W06-CHK01-STATE
                                                   W06-CHK18-STATE
                                                   W06-CHK19-STATE
                                                   W06-CHK20-STATE
                                                   W06-CHK33-STATE
               END-IF
      *
      *        管理者でない時
               IF      SPA-NAI-KANRIAUTHFLG    =  "F"
                   MOVE    "F"                 TO  W06-CHK19
                                                   W06-CHK33
                   MOVE    WIDGET-INSENSITIVE  TO  W06-CHK19-STATE
                                                   W06-CHK33-STATE
               END-IF
           END-IF
      *
      *----(04.08.05)--ADD-START---
      *    ログインユーザーが管理者でない場合
           IF      SPA-NAI-STAFFKANRIKBN   =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  W06-SYOKUKBN-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-SYOKUCOD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-KANRIAUTHFLG-STATE
      *----(04.08.06)--ADD-START---
               MOVE    WIDGET-INSENSITIVE  TO  W06-B03-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-STYUKYMD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-EDYUKYMD-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-USERID-STATE
      *----(04.08.06)--ADD-END-----
           ELSE
               MOVE    WIDGET-NORMAL       TO  W06-SYOKUKBN-STATE
               MOVE    WIDGET-NORMAL       TO  W06-SYOKUCOD-STATE
      *----(04.08.06)--ADD-START---
               MOVE    WIDGET-NORMAL       TO  W06-STYUKYMD-STATE
               MOVE    WIDGET-NORMAL       TO  W06-EDYUKYMD-STATE
               MOVE    WIDGET-NORMAL       TO  W06-USERID-STATE
      *----(04.08.06)--ADD-END-----
           END-IF
      *----(04.08.05)--ADD-END-----
      *
      *----(04.08.01)--ADD-START---
           IF      SPA-MW      =   SPA-GINBEE
               MOVE    SPACE               TO  W06-CHK33-LABEL
               MOVE    SPACE               TO  W06-CHK16-LABEL
               MOVE    SPACE               TO  W06-CHK17-LABEL
      *        ユーザーログイン権限「表示しない」に設定
               MOVE    SPA-GMN-USER-KANRI-LST(1)
                                           TO  W06-USER-KANRI
      *        クライアント印刷可否「する」に設定
      *        MOVE    SPA-GMN-CLIENT-PRT-LST(2)
      *                                    TO  W06-CLIENT-PRT
      *        プリンタへのデータ送信単位「帳票毎に送信」に設定
               MOVE    SPA-GMN-PRTCONF-LST(2)
                                           TO  W06-PRTCONF
               MOVE    WIDGET-INSENSITIVE  TO  W06-USER-KANRI-STATE
      *        MOVE    WIDGET-INSENSITIVE  TO  W06-CLIENT-PRT-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W06-PRTCONF-STATE
           ELSE
               MOVE    "プログラム更新"    TO  W06-CHK33-LABEL
               MOVE    "データ出力"        TO  W06-CHK16-LABEL
               MOVE    "外部媒体"          TO  W06-CHK17-LABEL
               MOVE    WIDGET-NORMAL       TO  W06-USER-KANRI-STATE
               MOVE    WIDGET-NORMAL       TO  W06-CLIENT-PRT-STATE
               MOVE    WIDGET-NORMAL       TO  W06-PRTCONF-STATE
           END-IF
      *----(04.08.01)--ADD-END-----
      *!!!
      *H27.11
      *    管理者のみ、職員一覧遷移可
           IF     (SPA-NAI-STAFFKANRIKBN   NOT =   ZERO  )
              AND (SPA-GMN-SYOKUKBN    NOT =   "0")
               MOVE    "職員一覧"          TO  W06-B05-LABEL
               MOVE    WIDGET-NORMAL       TO  W06-B05-STATE
           ELSE
               MOVE    SPACE               TO  W06-B05-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  W06-B05-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *----(04.07.01)--UPD-END-----
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "職員区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "職員番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効開始日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効終了日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "選択番号がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   STRING  "既にオペレータＩＤが登録されています。"
                           "オペレータＩＤを変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   MOVE    "オペレータＩＤを入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING  "オペレータＩＤに使用可能な文字は半角の"
                                           DELIMITED BY SIZE
                           "英数字と下線符号（アンダーバー）のみです"
                                           DELIMITED BY SIZE
                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0025"
                   STRING  "オペレータＩＤがシステム内に存在します。"
                           "オペレータＩＤを変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   MOVE    "フリガナは全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "氏名を全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "麻薬施用者免許証番号は、"
                                               TO  SPA-ERRMSG
                   MOVE    "半角か全角のどちらかで入力して下さい"
                                               TO  SPA-ERRMSG(25:)
               WHEN    "0013"
                   MOVE    "専門科コード入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "診療行為初期画面入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "９９９９は入力できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "請求書発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "院外処方せん発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "薬剤情報発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "診療費明細書発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "職員の選択がしてありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "検査正式名称表示フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "クライアント保存可否入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "他院オペレータＩＤは半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "ユーザーログイン権限入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "自院と他院のオペレータＩＤが重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "他院のオペレータＩＤが重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "職員情報が未登録です"
                                               TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "クライアント印刷可否入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "ダイアログ表示フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "本院・分院機能が有効な場合に設定可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "ＩＣカードリーダ／ライタ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "パスワードを再入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "再入力されたパスワードが異なります"
                                               TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "管理者のパスワードは削除できません"
                                               TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "管理者はパスワードが必須です"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "プリンタへのデータ送信単位入力エラー"
                                               TO  SPA-ERRMSG
      *----(04.08.05)--ADD-START---
               WHEN    "0038"
                   STRING  "本日有効な管理者が不在となります。"
                           "管理者を登録してください。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0039"
                   STRING  "無期限有効な管理者が存在しません。"
                           "管理者を登録してください。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0040"
                   STRING  "本日有効なシステム管理者が不在となります。"
                           "システム管理者を登録してください。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0041"
                   STRING  "システム管理者の最終期間の終了日が"
                           "正しくありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *----(04.08.05)--ADD-END-----
               WHEN    "0050"
                   STRING    "履歴１０件登録済みです。"
                             "これ以上追加できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0100"
                   MOVE
                   "オルカマスターはシステム管理を有効にして下さい"
                                               TO  SPA-ERRMSG
               WHEN    "K001"
                   STRING    "警告！医師の職員番号がゼロです。"
                             "診療行為の複数科入力に使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W06"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "職員情報を追加登録します"
                                                     TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "職員情報を更新します"    TO  WRK-WIDMSG
      *----(04.08.03)--ADD-START---
                   IF    ( SPA-GMN-PASSWORD1(1:2)  =   "/d" )  OR
                         ( SPA-GMN-PASSWORD1(1:2)  =   "/D" )
                       MOVE    "職員情報を更新します（パスワード削除）"
                                                     TO  WRK-WIDMSG
                   END-IF
      *----(04.08.03)--ADD-END-----
               WHEN    "1003"
                   MOVE    "職員情報を削除します"    TO  WRK-WIDMSG
               WHEN    "1004"
                   MOVE    "編集中ですが中止しますか？"
                                                     TO  WRK-WIDMSG
      *H27.10
               WHEN    "1005"
                   STRING  "ＯＫで職員情報を追加登録します。"
                           "よろしいですか？"
                                         DELIMITED  BY  SIZE
                                         INTO    WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W06"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          IF    ( SPA-GMN-CUR          =   27 )
           AND  ( W06-USER-KANRI-STATE =   WIDGET-INSENSITIVE )
               MOVE    103         TO  SPA-GMN-CUR
          END-IF
      *
          IF    ( SPA-GMN-CUR          =   103 )
           AND  ( W06-PRTCONF-STATE    =   WIDGET-INSENSITIVE )
               MOVE    28          TO  SPA-GMN-CUR
          END-IF
      *
          IF    ( SPA-GMN-CUR          =   28 )
           AND  ( W06-CLIENT-PRT-STATE =   WIDGET-INSENSITIVE )
               MOVE    29          TO  SPA-GMN-CUR
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "SYOKUKBN"       TO   MCP-WIDGET
                WHEN    2
                    MOVE  "SYOKUCOD"       TO   MCP-WIDGET
                WHEN    3
                    MOVE  "STYUKYMD"       TO   MCP-WIDGET
                WHEN    4
                    MOVE  "EDYUKYMD"       TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SELNUM"         TO   MCP-WIDGET
                WHEN    6
                    MOVE  "USERID"         TO   MCP-WIDGET
                WHEN    7
                    MOVE  "KANANAME"       TO   MCP-WIDGET
                WHEN    8
                    MOVE  "NAME"           TO   MCP-WIDGET
                WHEN    9
                    MOVE  "ISEKINUM"       TO   MCP-WIDGET
                WHEN    10
                    MOVE  "HKNDRNUMS"      TO   MCP-WIDGET
                WHEN    11
                    MOVE  "HKNDRNUMK"      TO   MCP-WIDGET
                WHEN    12
                    MOVE  "MAYAKULICENSE"  TO   MCP-WIDGET
                WHEN    13
                    MOVE  "SEMONKACD1"  TO   MCP-WIDGET
                WHEN    14
                    MOVE  "SEMONKACD2"  TO   MCP-WIDGET
                WHEN    15
                    MOVE  "SEMONKACD3"  TO   MCP-WIDGET
                WHEN    16
                    MOVE  "SEMONKACD4"  TO   MCP-WIDGET
                WHEN    17
                    MOVE  "SEMONKACD5"  TO   MCP-WIDGET
      *         業務権限
                WHEN    31   THRU  70
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "CHK"             TO  MCP-WIDGET
                    COMPUTE  IDX2              =   IDX2   -   30
                    MOVE     IDX2              TO  MCP-WIDGET(4:2)
      *
                WHEN    18
                    MOVE  "KANRIAUTHFLG"   TO   MCP-WIDGET
                WHEN    19
                    MOVE  "ORCA21FLG"      TO   MCP-WIDGET
      *         表示区分
                WHEN    20
                    MOVE  "HYOJIFLG"       TO   MCP-WIDGET
      *    個別設定画面
                WHEN    21
                    MOVE  "SKYPRTFLG"      TO   MCP-WIDGET
                WHEN    22
                    MOVE  "SYOHOPRTFLG"    TO   MCP-WIDGET
                WHEN    23
                    MOVE  "YAKUJYOFLG"     TO   MCP-WIDGET
                WHEN    24
                    MOVE  "MEIPRTFLG"      TO   MCP-WIDGET
                WHEN    25
                    MOVE  "FORMALFLG"      TO   MCP-WIDGET
                WHEN    101
                    MOVE  "GLINECNT"       TO   MCP-WIDGET
                WHEN    102
                    MOVE  "NLINECNT"       TO   MCP-WIDGET
                WHEN    26
                    MOVE  "CLIENT_SAVE"    TO   MCP-WIDGET
                WHEN    27
                    MOVE  "USER_KANRI"     TO   MCP-WIDGET
                WHEN    28
                    MOVE  "CLIENT_PRT"     TO   MCP-WIDGET
                WHEN    29
                    MOVE  "DIALOGFLG"      TO   MCP-WIDGET
                WHEN    30
                    MOVE  "ICCARDRW"       TO   MCP-WIDGET
                WHEN    103
                    MOVE  "PRTCONF"       TO   MCP-WIDGET
      *
                WHEN    81
                    MOVE  "PASSWORD1"      TO   MCP-WIDGET
                WHEN    82
                    MOVE  "PASSWORD2"      TO   MCP-WIDGET
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
      *    他院設定画面
                WHEN    201
                    MOVE  "GRPUSERID01"    TO   MCP-WIDGET
                WHEN    202
                    MOVE  "GRPUSERID02"    TO   MCP-WIDGET
                WHEN    203
                    MOVE  "GRPUSERID03"    TO   MCP-WIDGET
                WHEN    204
                    MOVE  "GRPUSERID04"    TO   MCP-WIDGET
                WHEN    205
                    MOVE  "GRPUSERID05"    TO   MCP-WIDGET
                WHEN    206
                    MOVE  "GRPUSERID06"    TO   MCP-WIDGET
                WHEN    207
                    MOVE  "GRPUSERID07"    TO   MCP-WIDGET
                WHEN    208
                    MOVE  "GRPUSERID08"    TO   MCP-WIDGET
                WHEN    209
                    MOVE  "GRPUSERID09"    TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SYOKUKBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SYOKUCOD"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "USERID"
                   MOVE    81              TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "NAME"
      *----(04.07.01)--UPD-START---
                   IF      SPA-GMN-SYOKUKBN    =   "1"
      *                医師の場合は医籍登録番号へ
                       MOVE    9               TO  SPA-GMN-CUR
                   ELSE
      *                スーパーバイザーの場合は診療行為初期画面へ
                       IF      SPA-NAI-SUPERVISOR  =   "T"
                           MOVE    19              TO  SPA-GMN-CUR
                       ELSE
      *                    管理者である場合は外来まとめへ
                           IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                               MOVE    66              TO  SPA-GMN-CUR
      *                    管理者でない場合は医事業務へ
                           ELSE
                               MOVE    31              TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
      *----(04.07.01)--UPD-END-----
               WHEN    "ISEKINUM"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "HKNDRNUMS" 
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "HKNDRNUMK"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "MAYAKULICENSE"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD1"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD2"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD3"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD4"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN     "SEMONKACD5"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN     "KANRIAUTHFLG"
                   MOVE    19              TO  SPA-GMN-CUR
               WHEN     "ORCA21FLG"
     ************* MOVE    99              TO  SPA-GMN-CUR
                   MOVE    20              TO  SPA-GMN-CUR
      *        表示区分
               WHEN     "HYOJIFLG"
                   MOVE    99              TO  SPA-GMN-CUR
      *    個別設定画面
               WHEN     "SKYPRTFLG"
                   MOVE    22              TO  SPA-GMN-CUR
               WHEN     "SYOHOPRTFLG"
                   MOVE    23              TO  SPA-GMN-CUR
               WHEN     "YAKUJYOFLG"
                   MOVE    24              TO  SPA-GMN-CUR
               WHEN     "MEIPRTFLG"
                   MOVE    25              TO  SPA-GMN-CUR
               WHEN     "FORMALFLG"
      *!!!!        MOVE    26              TO  SPA-GMN-CUR
                   MOVE    101             TO  SPA-GMN-CUR
      *
               WHEN     "GLINECNT" 
                   MOVE    102             TO  SPA-GMN-CUR
               WHEN     "NLINECNT" 
                   MOVE    26              TO  SPA-GMN-CUR
      *
               WHEN     "CLIENT_SAVE"
                   MOVE    27              TO  SPA-GMN-CUR
               WHEN     "USER_KANRI"
                   MOVE    103             TO  SPA-GMN-CUR
               WHEN     "PRTCONF"
                   MOVE    28              TO  SPA-GMN-CUR
               WHEN     "CLIENT_PRT"
                   MOVE    29              TO  SPA-GMN-CUR
               WHEN     "DIALOGFLG"
                   MOVE    30              TO  SPA-GMN-CUR
               WHEN     "ICCARDRW"
                   MOVE    99              TO  SPA-GMN-CUR
      *    他院設定画面
               WHEN     "GRPUSERID01"  THRU    "GRPUSERID08"
                   MOVE    WRK-MCP-WIDGET  TO  WRK-GRPUSERID-WIDGET
                   IF    ( SPA-GRPHOSPNUM (WRK-GRPUSERIDNUM + 1)
                                           =   ZERO )
                       MOVE    99          TO  SPA-GMN-CUR
                   ELSE
                       COMPUTE SPA-GMN-CUR
                           =   WRK-GRPUSERIDNUM
                           +   201
                   END-IF
               WHEN     "GRPUSERID09"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN     "PASSWORD1"
                   MOVE    82              TO  SPA-GMN-CUR
               WHEN     "PASSWORD2"
                   MOVE    7               TO  SPA-GMN-CUR
      *
               WHEN     OTHER
                   IF      WRK-MCP-WIDGET(1:3) =   "CHK"
                       MOVE    WRK-MCP-WIDGET(4:2) TO  IDX2
                       COMPUTE SPA-GMN-CUR     =   IDX2
                                               +   31
      *----(02.00.01)--UPD-START---
      *                医事業務の後、プログラム更新へ
                       IF      IDX2            =   1
                           COMPUTE SPA-GMN-CUR     =   33
                                                   +   30
                       END-IF
      *                プログラム更新の後、外来まとめへ
                       IF      IDX2            =   33
                           MOVE    66          TO  SPA-GMN-CUR
                       END-IF
      *                外来まとめの後、受付へ
                       IF      IDX2            =   36
                           MOVE    32          TO  SPA-GMN-CUR
                       END-IF
      *                会計照会の後、入院へ
                       IF      IDX2            =   9
                           COMPUTE SPA-GMN-CUR     =   27
                                                   +   30
                       END-IF
      *                入院照会の後、日次統計へ
                       IF      IDX2            =   32
                           COMPUTE SPA-GMN-CUR     =   10
                                                   +   30
                       END-IF
      *----(02.00.02)--UPD-START---
      *                薬剤情報の後、住所マスタへ
                       IF      IDX2            =   26
                           COMPUTE SPA-GMN-CUR     =   34
                                                   +   30
                       END-IF
      *                ヘルプマスタの後、管理者へ
                       IF      IDX2            =   35
                           MOVE    18          TO  SPA-GMN-CUR
                       END-IF
      *----(02.00.01)--UPD-END-----
      *                管理者で無い時
                       IF      SPA-NAI-KANRIAUTHFLG    =  "T"
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-CUR     =   63
                               MOVE    32          TO  SPA-GMN-CUR
                           ELSE
                               IF      SPA-GMN-CUR     =   49
                                   MOVE    50          TO  SPA-GMN-CUR
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
                   MOVE    SPACE               TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               MOVE    SPACE               TO  SYSKANRI-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-PASSWD-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-PASSWD
      *
           INITIALIZE                  PASSWD-REC
           MOVE    SPA-GMN-USERID  TO  PASSWD-USERID
           MOVE    PASSWD-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_passwd"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE   ZERO             TO  FLG-PASSWD
               ELSE
                   MOVE    1               TO  FLG-PASSWD
               END-IF
           ELSE
               MOVE    1               TO  FLG-PASSWD
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PASSWD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *

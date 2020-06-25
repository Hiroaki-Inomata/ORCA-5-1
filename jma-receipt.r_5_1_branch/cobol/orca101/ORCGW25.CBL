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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGW25.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 診療行為一覧選択サブ（Ｚ９８から流用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/09    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.00    NACL-多々納  06/10/26  検索画面連絡情報変更
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.01.00    NACL-多々納  08/01/30  次頁表示の追加
      *  04.05.00    NACL-多々納  09/07/24  診療行為一覧と同期
      *  04.07.00    NACL-多々納  12/07/09  労災等名称編集追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "W24SCR-SPA".
      *
           COPY    "W25SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
      *
           03  FLG-SYURYOU         PIC 9(01).
           03  FLG-MAX             PIC 9(01).
      *
           03  FLG-HENSYU          PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-KIKAN           PIC 9(01).
           03  FLG-KENSAKU01       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS                 PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-N               PIC 9(02).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXC                PIC 9(04).
      *
           03  ID1                 PIC 9(04).
           03  ID2                 PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
      *
           03  IDX-KAI             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SEYMD.
               05  WRK-SEYY        PIC 9(04).
               05  WRK-SEMM        PIC 9(02).
               05  WRK-SEDD        PIC 9(02).
      *
           03  WRK-STR             PIC 9(06).
      *
           03  WRK-CNT-ENT         PIC 9(04).
           03  WRK-MOJI            PIC 9(04).
           03  WRK-MAP-INDEX       PIC 9(03).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-GMN-SELNO       PIC 9(03).
           03  WRK-NOZ             PIC ZZZ.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS      PIC ZZZZZZ9.99.
      *
           03  WRK-MOJI1           PIC X(22).
           03  WRK-MOJI2           PIC X(22).
           03  WRK-TENSU1          PIC 9(09).
           03  WRK-TENSU2          PIC 9(09).
      *
           03  WRK-INPUTCD         PIC X(22).
           03  WRK-GRP-MEI         PIC X(40).
           03  WRK-GRP-MEI-X       PIC X(40).
      *
           03  WRK-NAI-SELNUM-G.
               05  WRK-NAI-SELNUM      PIC 9(03)   OCCURS   10.
           03  WRK-GMN-SELNUM-G.
               05  WRK-GMN-SELNUM      PIC X(03)   OCCURS   10.
           03  WRK-W25-SRYCD-G.
               05  WRK-W25-SRYCD       PIC X(09)  OCCURS    10.
      *
           03  WRK-LISET-SEL-G.
               05  WRK-LISET-SEL       OCCURS   10.
                   07  WRK-SEL            PIC X(01).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-KETA            PIC 9(04).
           03  WRK-KEY-INPUT       PIC X(22).
      *
           03  WRK-SENTAKU         PIC X(70).
           03  WRK-HEN-NAME        PIC X(70).
           03  WRK-KENSA-HEN       PIC X(02).
      *
           03  WRK-W25-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
           03  WRK-ERRMSG          PIC  X(80).
           03  WRK-ERRMSG2         PIC  X(80).
      *
           03  WRK-NAME1-G.
               05  WRK-NAME11           PIC X(20).
               05  WRK-NAME12           PIC X(50).
           03  WRK-NAME2-G.
               05  WRK-NAME21           PIC X(30).
               05  WRK-NAME22           PIC X(40).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    検査等実施判断グループ区分テーブル
       01  WRK-TBL-GRP.
           03  FILLER              PIC X(02)   VALUE   "01".
           03  FILLER              PIC X(40)   VALUE
                     "尿・糞便等検査".
           03  FILLER              PIC X(20)   VALUE
                     "尿・糞便等検査".
           03  FILLER              PIC X(02)   VALUE   "02".
           03  FILLER              PIC X(40)   VALUE   "血液学的検査".
           03  FILLER              PIC X(20)   VALUE   "血液学的検査".
           03  FILLER              PIC X(02)   VALUE   "31".
           03  FILLER              PIC X(40)   VALUE
                     "生化学的検査（１）（包括）".
           03  FILLER              PIC X(20)   VALUE
                     "生（１）（包括）".
           03  FILLER              PIC X(02)   VALUE   "32".
           03  FILLER              PIC X(40)   VALUE
                     "生化学的検査（１）（包括以外）".
           03  FILLER              PIC X(20)   VALUE
                     "生（１）（包括以外）".
           03  FILLER              PIC X(02)   VALUE   "41".
           03  FILLER              PIC X(40)   VALUE
                     "生化学的検査（２）（包括）".
           03  FILLER              PIC X(20)   VALUE
                     "生（２）（包括）".
           03  FILLER              PIC X(02)   VALUE   "42".
           03  FILLER              PIC X(40)   VALUE
                     "生化学的検査（２）（包括以外）".
           03  FILLER              PIC X(20)   VALUE
                     "生（２）（包括以外）".
           03  FILLER              PIC X(02)   VALUE   "05".
           03  FILLER              PIC X(40)   VALUE   "免疫学的検査".
           03  FILLER              PIC X(20)   VALUE   "免疫学的検査".
           03  FILLER              PIC X(02)   VALUE   "06".
           03  FILLER              PIC X(40)   VALUE
                     "微生物学的検査".
           03  FILLER              PIC X(20)   VALUE
                     "微生物学的検査".
           03  FILLER              PIC X(02)   VALUE   "07".
      **** 03  FILLER              PIC X(40)   VALUE   "病理学的検査".
      **** 03  FILLER              PIC X(20)   VALUE   "病理学的検査".
           03  FILLER              PIC X(40)   VALUE   "病理診断".
           03  FILLER              PIC X(20)   VALUE   "病理診断".
           03  FILLER              PIC X(02)   VALUE   "08".
           03  FILLER              PIC X(40)   VALUE
                     "基本的検体検査実施料".
           03  FILLER              PIC X(20)   VALUE
                     "基本的検体検査".
           03  FILLER              PIC X(02)   VALUE   "11".
           03  FILLER              PIC X(40)   VALUE
                     "呼吸循環機能検査".
           03  FILLER              PIC X(20)   VALUE
                     "呼吸循環機能検査".
           03  FILLER              PIC X(02)   VALUE   "12".
           03  FILLER              PIC X(40)   VALUE
                     "超音波検査".
           03  FILLER              PIC X(20)   VALUE
                     "超音波検査".
           03  FILLER              PIC X(02)   VALUE   "13".
           03  FILLER              PIC X(40)   VALUE
                     "監視装置による諸検査".
           03  FILLER              PIC X(20)   VALUE
                     "監視装置による諸検査".
           03  FILLER              PIC X(02)   VALUE   "14".
           03  FILLER              PIC X(40)   VALUE
                     "脳波検査".
           03  FILLER              PIC X(20)   VALUE
                     "脳波検査".
           03  FILLER              PIC X(02)   VALUE   "15".
           03  FILLER              PIC X(40)   VALUE
                     "神経・筋検査".
           03  FILLER              PIC X(20)   VALUE
                     "神経・筋検査".
           03  FILLER              PIC X(02)   VALUE   "16".
           03  FILLER              PIC X(40)   VALUE
                     "耳鼻咽喉科学的検査".
           03  FILLER              PIC X(20)   VALUE
                     "耳鼻咽喉科学的検査".
           03  FILLER              PIC X(02)   VALUE   "17".
           03  FILLER              PIC X(40)   VALUE
                     "眼科学的検査".
           03  FILLER              PIC X(20)   VALUE
                     "眼科学的検査".
           03  FILLER              PIC X(02)   VALUE   "23".
           03  FILLER              PIC X(40)   VALUE
                     "皮膚科学的検査".
           03  FILLER              PIC X(20)   VALUE
                     "皮膚科学的検査".
           03  FILLER              PIC X(02)   VALUE   "18".
           03  FILLER              PIC X(40)   VALUE
                     "臨床心理神経心理検査".
           03  FILLER              PIC X(20)   VALUE
                     "臨床心理神経心理検査".
           03  FILLER              PIC X(02)   VALUE   "19".
           03  FILLER              PIC X(40)   VALUE
                     "負荷試験".
           03  FILLER              PIC X(20)   VALUE
                     "負荷試験".
           03  FILLER              PIC X(02)   VALUE   "20".
           03  FILLER              PIC X(40)   VALUE
                     "ラジオアイソトープを用いた諸検査".
           03  FILLER              PIC X(20)   VALUE
                     "ラジオアイソトープ".
           03  FILLER              PIC X(02)   VALUE   "21".
           03  FILLER              PIC X(40)   VALUE
                     "内視鏡検査".
           03  FILLER              PIC X(20)   VALUE
                     "内視鏡検査".
           03  FILLER              PIC X(02)   VALUE   "22".
           03  FILLER              PIC X(40)   VALUE
                     "診断穿刺検体採取料".
           03  FILLER              PIC X(20)   VALUE
                     "診断穿刺検体採取料".
           03  FILLER              PIC X(02)   VALUE   "98".
           03  FILLER              PIC X(40)   VALUE
                     "労災検査".
           03  FILLER              PIC X(20)   VALUE
                     "労災検査".
           03  FILLER              PIC X(02)   VALUE   "99".
           03  FILLER              PIC X(40)   VALUE   "その他の検査".
           03  FILLER              PIC X(20)   VALUE   "その他の検査".
       01  WRK-TBL-GRP-R           REDEFINES   WRK-TBL-GRP.
           03  TBL-GRP-OCC         OCCURS  25  INDEXED  BY  TBL-IDX.
               05  TBL-GRP-KBN     PIC X(02).
               05  TBL-GRP-MEI     PIC X(40).
               05  TBL-GRP-RMEI    PIC X(20).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *    検索記号設定
           COPY    "CPORCSADDSIGN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE(1:30000)   TO  SPA-W25-GMN-AREA
      **** MOVE    SPA-W25-KYOUTU      TO  SPA-W25-GMN-AREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-W25-GMN-AREA    TO  SPA-FREE(1:30000)
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           PERFORM 5001-MAPCUR-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "W25"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *    Z01 で初期化する
      *D   INITIALIZE                  SPA-W25GMN-AREA
      *
      *****MOVE    ZERO                TO  SPA-W25-RC
           MOVE    SPA-W25-INPUTCD     TO  SPA-W25-GMN-INPUT
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           IF    ( SPA-W25-SYSYMD      =   SPACE )
               MOVE    SPA-SYSYMD      TO  SPA-W25-SYSYMD
           END-IF
      *
      *    リストボックス
           MOVE    "1:内服薬"          TO  SPA-W25-JIINLST (1)
           MOVE    "2:外用薬"          TO  SPA-W25-JIINLST (2)
           MOVE    "3:注射薬"          TO  SPA-W25-JIINLST (3)
           MOVE    "4:処置"            TO  SPA-W25-JIINLST (4)
           MOVE    "5:診察"            TO  SPA-W25-JIINLST (5)
           MOVE    "6:注射"            TO  SPA-W25-JIINLST (6)
           MOVE    "7:手術・麻酔"      TO  SPA-W25-JIINLST (7)
           MOVE    "8:画像診断"        TO  SPA-W25-JIINLST (8)
           MOVE    "9:その他"          TO  SPA-W25-JIINLST (9)
           MOVE    SPACE               TO  SPA-W25-JIIN-G
      *
      *    ユーザー登録他
           MOVE    "1:用法"            TO  SPA-W25-USERLST (1)
           MOVE    "2:部位"            TO  SPA-W25-USERLST (2)
           MOVE    "3:その他"          TO  SPA-W25-USERLST (3)
           MOVE    "4:コメント"        TO  SPA-W25-USERLST (4)
           MOVE    "5:労災"            TO  SPA-W25-USERLST (5)
           MOVE    "6:システム予約"    TO  SPA-W25-USERLST (6)
           MOVE    SPACE               TO  SPA-W25-USER-G
      *
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *    入力項目へ
           MOVE    21                  TO  SPA-W25-GMN-CUR
           IF      (SPA-W25-GMN-INPUT  =   SPACE )  AND
                   (SPA-W25-GMN-SYORI  =   SPACE )
               CONTINUE
           ELSE
               PERFORM                     310-SPA-SET-SEC
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
      *
           INITIALIZE                  SPA-W25-GMN-TBL
           MOVE    ZERO                TO  SPA-W25-GMN-MAX
           MOVE    ZERO                TO  FLG-MAX
      *    開始
           IF     (SPA-W25-PAGE        =   ZERO )
               MOVE    1                   TO  SPA-W25-PAGE
           END-IF
           COMPUTE WRK-STR             =  (SPA-W25-PAGE   -   1  )
                                       *   200
                                       +   1
      *
           EVALUATE    SPA-W25-GMN-INPUT(1:1)
      ***      WHEN    "."
      *            診療種別
      ****         PERFORM 31009-SRYSYUKBN-SET-SEC
               WHEN    "*"
      *            区分番号
                   PERFORM 310010-KUBUNNUM-SET-SEC
               WHEN    "/"
                   IF      SPA-W25-GMN-INPUT(2:1) =   "/"
                                                  OR  "U"
      *                ユーザ登録分
                       PERFORM 310012-USER-TENSET-SEC
                   ELSE
      *                検査点数
                       PERFORM 310011-KENSA-TENSET-SEC
                   END-IF
               WHEN    OTHER
      *            その他
                   PERFORM 3100-KENSAKU-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-W25-GMN-MAX     =   ZERO
                   MOVE    21                  TO  SPA-W25-GMN-CUR
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   MOVE    1                   TO  SPA-W25-GMN-CUR
                   IF      FLG-MAX         NOT =   ZERO
                       CONTINUE
                  ELSE
                       MOVE    ZERO                TO  SPA-W25-NEXT
                   END-IF
      *            処理区分保存
                   MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
               END-IF
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       3100-KENSAKU-SEC            SECTION.
      *
           MOVE     ZERO               TO  FLG-KENSAKU01
      *
           IF       SPA-W25-TYPE         =   "1"
               MOVE    1                 TO  FLG-KENSAKU01
               PERFORM 3101-INPUTCD-SET-SEC
               IF      SPA-W25-GMN-MAX   >   ZERO
                   MOVE    SPACE         TO  SPA-W25-OLD-SYORI
               END-IF
           END-IF
           IF     (SPA-W25-GMN-MAX     =   ZERO  )  AND
                 ((SPA-W25-CDSYUBETU   =   "J"   ) OR
                  (SPA-W25-GMN-SYORI   =   "9"  OR  "U"))
               IF     (SPA-W25-CDSYUBETU   =   "J" ) AND
                      (SPA-W25-GMN-SYORI   =   SPACE) AND
                      (SPA-W25-OLD-SYORI   NOT =   SPACE)
                   MOVE    SPA-W25-OLD-SYORI   TO  SPA-W25-GMN-SYORI
               END-IF
               PERFORM 3102-TENSU-SET-SEC
      *        点数マスタ検索時、自院がゼロ件の時、全体を検索
               IF      SPA-W25-GMN-ALL     =   SPACE  AND
                       SPA-W25-GMN-MAX     =   ZERO
                   MOVE    "1"             TO  SPA-W25-GMN-ALL
                   PERFORM 3102-TENSU-SET-SEC
               END-IF
           END-IF
           .
       3100-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別編集
      *****************************************************************
       31009-SRYSYUKBN-SET-SEC            SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           MOVE    ZERO                TO  SPA-W25-GMN-MAX
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               SET     MSYU-IDX                TO  IDX
      *        入力できない診療種別は対象外とする
               EVALUATE    MSYU-SRYSYUKBN (MSYU-IDX)
      *            ギブス
                   WHEN    "520"
      *            Ｘ線
                   WHEN    "710"
                   WHEN    "711"
                   WHEN    "712"
                   WHEN    "713"
                   WHEN    "720"
                   WHEN    "721"
                   WHEN    "723"
                   WHEN    "724"
                   WHEN    "724"
      *            リハビリ
                   WHEN    "810"
                       MOVE    1               TO  FLG-ERR
                   WHEN    OTHER
                       MOVE    ZERO            TO  FLG-ERR
               END-EVALUATE
               IF      MSYU-SRYKBN (MSYU-IDX)  =   SPACE
                   MOVE    200                 TO  IDX
               ELSE
                   IF      FLG-ERR             =   ZERO
                       ADD     1               TO  IDX2
                       MOVE    IDX2            TO  SPA-W25-GMN-MAX
                       MOVE    IDX2            TO  SPA-W25-GMN-NO
                                                                (IDX2)
                       MOVE    "."             TO  SPA-W25-GMN-INPUTCD
                                                                (IDX2)
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                               TO  SPA-W25-GMN-INPUTCD
                                                        (IDX2)(2:3)
                       MOVE    "."             TO  SPA-W25-GMN-SRYCD
                                                                (IDX2)
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                               TO  SPA-W25-GMN-SRYCD
                                                        (IDX2)(2:3)
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                               TO  SPA-W25-GMN-NAME
                                                                (IDX2)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  SPA-W25-GMN-NAME
                                                         (IDX2)(5:)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       31009-SRYSYUKBN-SET-EXT.
           EXIT.
      *TEST
      *****************************************************************
      *    区分番号編集
      *****************************************************************
       310010-KUBUNNUM-SET-SEC            SECTION.
      *
           IF      SPA-W25-GMN-INPUT(2:1)  =   SPACE
               MOVE    21                  TO  SPA-W25-GMN-CUR
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
      *
      *    IF      SPA-W25-GMN-INPUT(2:1)  >=  "A"  AND
      *                                    <=  "R"
      *        CONTINUE
      *    ELSE
      *        MOVE    21                  TO  SPA-W25-GMN-CUR
      *        MOVE    "0004"              TO  SPA-ERRCD
      ***  END-IF
           EVALUATE    SPA-W25-GMN-INPUT(2:1)
               WHEN    "A"
               WHEN    "a"
                   MOVE    "初診料・再診料・入院料"
                                               TO  SPA-W25-SENTAKU
                   MOVE    "A"                 TO  SPA-W25-GMN-INPUT
                                                              (2:1)
               WHEN    "B"
               WHEN    "b"
                   MOVE    "指導管理等"        TO  SPA-W25-SENTAKU
                   MOVE    "B"                 TO  SPA-W25-GMN-INPUT
                                                              (2:1)
               WHEN    "C"
               WHEN    "c"
                   MOVE    "在宅医療"          TO  SPA-W25-SENTAKU
                   MOVE    "C"                 TO  SPA-W25-GMN-INPUT
                                                              (2:1)
               WHEN    "D"
               WHEN    "d"
                   MOVE    "検査"              TO  SPA-W25-SENTAKU
                   MOVE    "D"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "E"
               WHEN    "e"
                   MOVE    "画像診断"          TO  SPA-W25-SENTAKU
                   MOVE    "E"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "F"
               WHEN    "f"
                   MOVE    "投薬"              TO  SPA-W25-SENTAKU
                   MOVE    "F"                 TO  SPA-W25-GMN-INPUT
                                                            (2:1)
               WHEN    "G"
               WHEN    "g"
                   MOVE    "注射"              TO  SPA-W25-SENTAKU
                   MOVE    "G"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "H"
               WHEN    "h"
                   MOVE    "リハビリテーション"    TO  SPA-W25-SENTAKU
                   MOVE    "H"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "I"
               WHEN    "i"
                   MOVE    "精神料専門療法"    TO  SPA-W25-SENTAKU
                   MOVE    "I"                 TO  SPA-W25-GMN-INPUT
                                                              (2:1)
               WHEN    "J"
               WHEN    "j"
                   MOVE    "処置"              TO  SPA-W25-SENTAKU
                   MOVE    "J"                 TO  SPA-W25-GMN-INPUT
                                                              (2:1)
               WHEN    "K"
               WHEN    "k"
                   MOVE    "手術"              TO  SPA-W25-SENTAKU
                   MOVE    "K"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "L"
               WHEN    "l"
                   MOVE    "麻酔"              TO  SPA-W25-SENTAKU
                   MOVE    "L"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "M"
               WHEN    "m"
                   MOVE    "放射線治療"        TO  SPA-W25-SENTAKU
                   MOVE    "M"                 TO  SPA-W25-GMN-INPUT
                                                             (2:1)
               WHEN    "N"
               WHEN    "n"
                   MOVE    "病理診断"          TO  SPA-W25-SENTAKU
                   MOVE    "N"                 TO  SPA-W25-GMN-INPUT
                                                                (2:1)
               WHEN    "R"
               WHEN    "r"
                   MOVE    "老人、その他"      TO  SPA-W25-SENTAKU
                   MOVE    "R"                 TO  SPA-W25-GMN-INPUT
                                                                (2:1)
               WHEN    OTHER
                   MOVE    SPACE               TO  SPA-W25-SENTAKU
                   MOVE    21                  TO  SPA-W25-GMN-CUR
                   MOVE    "0004"              TO  SPA-ERRCD
            END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
      *        /　があれば点数検索（老人はなし）
               IF     (SPA-W25-GMN-INPUT(3:1)  =   "/"  )
                 AND  (SPA-W25-GMN-INPUT(2:1)  NOT =   "R"  )
      *            区分＋点数検索
                   PERFORM 3100102-KUBUNNUM-TBLSET03-SEC
               ELSE
      *            区分番号編集
                   PERFORM 310010-KUBUNNUM-TBLSET-SEC
               END-IF
           END-IF
           .
       310010-KUBUNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    区分番号編集
      *****************************************************************
       310010-KUBUNNUM-TBLSET-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-W25-GMN-SYORI
      *
      *    最後にＳがあれば、自院
           IF     (SPA-W25-GMN-INPUT(3:3)  NUMERIC  )  AND
                  (SPA-W25-GMN-INPUT(6:1)  =   "S"  )
              IF      (SPA-W25-GMN-ALL     =   SPACE )
      *            自院分
                   PERFORM 3100102-KUBUNNUM-TBLSET02-SEC
                   IF      SPA-W25-GMN-MAX     >   ZERO
                       GO      TO  310010-KUBUNNUM-TBLSET-EXT
                   END-IF
               END-IF
           END-IF
      *!
           MOVE    "1"                 TO  SPA-W25-GMN-ALL
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-W25-GMN-INPUT(2:1)  TO  TNS-CDKBN-KBN
           IF      SPA-W25-GMN-INPUT(3:3)  NUMERIC
               MOVE    SPA-W25-GMN-INPUT(3:3)   TO  TNS-CDKBN-KBNNUM
               MOVE    "key91"              TO  WRK-PATH
           ELSE
               MOVE    SPACE                TO  SPA-W25-GMN-INPUT
                                                                (3:)
               MOVE    "key90"              TO  WRK-PATH
           END-IF
      *    老人
           IF      SPA-W25-GMN-INPUT(2:1)  =   "R"
               MOVE    "key92"            TO  WRK-PATH
           END-IF
      *
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX         >    200)   OR
                          (FLG-TENSU   =   1   )   OR
                          (SPA-ERRCD   NOT =   SPACE )
      *        点数マスタチェック
               PERFORM 31021-TENSU-CHK-SEC
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  WRK-STR
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
      *
      *    点数マスタで対象が有った時、タイプを変更する
           IF      SPA-W25-GMN-MAX     >   ZERO
               MOVE    "2"             TO  SPA-W25-TYPE
           END-IF
           .
       310010-KUBUNNUM-TBLSET-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    区分番号（自院）編集
      *****************************************************************
       3100102-KUBUNNUM-TBLSET02-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-W25-GMN-INPUT(2:1)  TO  TNS-CDKBN-KBN
           MOVE    SPA-W25-GMN-INPUT(3:3)  TO  TNS-CDKBN-KBNNUM
           MOVE    "key91m"                TO  WRK-PATH
      *
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX         >    200)   OR
                          (FLG-TENSU   =   1   )   OR
                          (SPA-ERRCD   NOT =   SPACE )
      *        点数マスタチェック
               PERFORM 31021-TENSU-CHK-SEC
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  WRK-STR
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
           .
       33100102-KUBUNNUM-TBLSET02-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    区分＋点数編集
      *****************************************************************
       3100102-KUBUNNUM-TBLSET03-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-W25-GMN-SYORI
      *    内容チェック
           MOVE    SPACE               TO  WRK-MOJI1
           MOVE    SPACE               TO  WRK-MOJI2
           MOVE    ZERO                TO  ID1
           MOVE    ZERO                TO  ID2
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-TENSU1
           MOVE    ZERO                TO  WRK-TENSU2
           PERFORM VARYING     IDX     FROM    4   BY  1
                   UNTIL      (IDX     >   22  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-W25-GMN-INPUT(IDX:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "-"
                       IF      FLG-ARI             =    1
                           MOVE    "0001"          TO  SPA-ERRCD
                       ELSE
                           MOVE    1               TO  FLG-ARI
                       END-IF
                   WHEN    OTHER
                       IF      FLG-ARI             =   ZERO
                           ADD     1               TO  ID1
                           MOVE    SPA-W25-GMN-INPUT(IDX:1)
                                                   TO  WRK-MOJI1(ID1:1)
                       ELSE
                           ADD     1               TO  ID2
                           MOVE    SPA-W25-GMN-INPUT(IDX:1)
                                                   TO  WRK-MOJI2(ID2:1)
                       END-IF
               END-EVALUATE
           END-PERFORM
           IF      WRK-MOJI1           =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-MOJI1           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   9     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0013"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TENSU1
               END-IF
           END-IF
           IF      WRK-MOJI2           =   SPACE
               MOVE    ZERO                TO  WRK-TENSU2
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-MOJI2           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   9     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0013"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TENSU2
               END-IF
           END-IF
      *
           IF     (WRK-TENSU1          >   ZERO )  AND
                  (WRK-TENSU2          >   ZERO )
               IF      WRK-TENSU1          >   WRK-TENSU2
                   MOVE    "0012"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 31001112-KUBUNNUM-TENSU-SEC 
           ELSE
               MOVE    21                  TO  SPA-W25-GMN-CUR
           END-IF
           .
       3100102-KUBUNNUM-TBLSET03-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分＋点数編集
      *****************************************************************
       31001112-KUBUNNUM-TENSU-SEC            SECTION.
      *
           IF      WRK-TENSU2          =   ZERO
               MOVE    WRK-TENSU1          TO  WRK-TENSU2
           END-IF
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TENSU1          TO  TNSXX-TK-TEN
           MOVE    WRK-TENSU2          TO  TNSXX-TK-EDTEN
           MOVE    SPA-W25-GMN-INPUT(2:1)  TO  TNS-CDKBN-KBN
           MOVE    "key95"             TO   WRK-PATH
      *
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX         >    200 )   OR
                          (FLG-TENSU   =   1    )
                     OR   (SPA-ERRCD   NOT =   SPACE )
      *
               IF     (TNS-SRYCD(1:1)      =   "1" )  AND
                      (TNS-TENSIKIBETU NOT =   5  AND  6 )
      *            点数マスタチェック
                   PERFORM 31021-TENSU-CHK-SEC
               ELSE
                   MOVE    1                   TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  WRK-STR
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
      *
      *    点数マスタで対象が有った時、タイプを変更する
           IF      SPA-W25-GMN-MAX     >   ZERO
               MOVE    "2"             TO  SPA-W25-TYPE
           END-IF
           .
       31001112-KUBUNNUM-TENSU-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    検査点数編集
      *****************************************************************
       310011-KENSA-TENSET-SEC            SECTION.
      *
      *    内容チェック
           MOVE    SPACE               TO  WRK-MOJI1
           MOVE    SPACE               TO  WRK-MOJI2
           MOVE    ZERO                TO  ID1
           MOVE    ZERO                TO  ID2
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-TENSU1
           MOVE    ZERO                TO  WRK-TENSU2
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL      (IDX     >   22  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-W25-GMN-INPUT(IDX:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "-"
                       IF      FLG-ARI             =    1
                           MOVE    "0001"          TO  SPA-ERRCD
                       ELSE
                           MOVE    1               TO  FLG-ARI
                       END-IF
                   WHEN    OTHER
                       IF      FLG-ARI             =   ZERO
                           ADD     1               TO  ID1
                           MOVE    SPA-W25-GMN-INPUT(IDX:1)
                                                   TO  WRK-MOJI1(ID1:1)
                       ELSE
                           ADD     1               TO  ID2
                           MOVE    SPA-W25-GMN-INPUT(IDX:1)
                                                   TO  WRK-MOJI2(ID2:1)
                       END-IF
               END-EVALUATE
           END-PERFORM
           IF      WRK-MOJI1           =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-MOJI1           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   9     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0013"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TENSU1
               END-IF
           END-IF
           IF      WRK-MOJI2           =   SPACE
               MOVE    ZERO                TO  WRK-TENSU2
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-MOJI2           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   9     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0013"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TENSU2
               END-IF
           END-IF
      *
           IF     (WRK-TENSU1          >   ZERO )  AND
                  (WRK-TENSU2          >   ZERO )
               IF      WRK-TENSU1          >   WRK-TENSU2
                   MOVE    "0012"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3100111-KENSA-TENHENSYU-SEC 
           ELSE
               MOVE    21                  TO  SPA-W25-GMN-CUR
           END-IF
      *
           .
       310011-KENSA-TENSET-EXT.
           EXIT.
      *
      *****************************************************************
      *     検査点数編集
      *****************************************************************
       3100111-KENSA-TENHENSYU-SEC            SECTION.
      *
           IF      WRK-TENSU2          =   ZERO
               MOVE    WRK-TENSU1          TO  WRK-TENSU2
           END-IF
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TENSU1          TO  TNSXX-TK-TEN
           MOVE    WRK-TENSU2          TO  TNSXX-TK-EDTEN
           MOVE    "60"                TO  TNSXX-STSRYKBN
           MOVE    "69"                TO  TNSXX-EDSRYKBN
      *
           MOVE    "key93"             TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    "K"                 TO   SPA-W25-GMN-SYORI
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX         >    200 )   OR
                          (FLG-TENSU   =   1   )
      *
               IF     (TNS-SRYCD(1:1)      =   "1" )  AND
                      (TNS-TENSIKIBETU NOT =   5  AND  6 )
      *            点数マスタチェック
                   PERFORM 31021-TENSU-CHK-SEC
               ELSE
                   MOVE    1                   TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  WRK-STR
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "key93"             TO  MCP-PATHNAME
               PERFORM                     910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key93"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
      *
      *    点数マスタで対象が有った時、タイプを変更する
           IF      SPA-W25-GMN-MAX     >   ZERO
               MOVE    "2"             TO  SPA-W25-TYPE
           END-IF
      *
           .
       3100111-KENSA-TENHENSYU-EXT.
           EXIT.
      *TEST
      *****************************************************************
      *     ユーザ登録分編集
      *****************************************************************
       310012-USER-TENSET-SEC            SECTION.
      *
      *    ユーザ登録分
           EVALUATE    SPA-W25-GMN-INPUT(1:3)
               WHEN    "//Y"
               WHEN    "//y"
      *        用法
                   MOVE    "U1"            TO  SPA-W25-GMN-SYORI3
               WHEN    "//C"
               WHEN    "//c"
      *        コメント
                   MOVE    "U4"            TO  SPA-W25-GMN-SYORI3
               WHEN    OTHER
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-W25-GMN-CUR
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
      *
           MOVE    "U"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPACE               TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
      *D   MOVE    1                   TO  SPA-W25-PAGE
           PERFORM 3102-TENSU-SET-SEC
      *
           END-IF
           .
       310012-USER-TENSET-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *     入力コードマスタ−より編集
      *****************************************************************
       3101-INPUTCD-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-ENT
           MOVE    ZERO                TO  FLG-MAX
      *    セットの時、種別コードは６とする
           IF      SPA-W25-GMN-INPUT(1:1)  =   "P"  OR  "S"
               MOVE    "6"                 TO  SPA-W25-CDSYUBETU
           END-IF
      *
      *    入力コードの桁数計算
           MOVE    ZERO                TO  WRK-KETA
           PERFORM VARYING     IDX-K   FROM    20   BY  -1
                   UNTIL       IDX-K   <   1
               IF      SPA-W25-GMN-INPUT(IDX-K:1)  NOT =   SPACE
                   MOVE    IDX-K           TO  WRK-KETA
                   MOVE    1               TO  IDX-K
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-W25-CDSYUBETU   TO  ICD-CDSYU
           MOVE    SPA-W25-GMN-INPUT   TO  ICD-INPUTCD
           IF      WRK-KETA            =   20
               MOVE    "key2"              TO  WRK-PATH
           ELSE
               COMPUTE IDX-K               =   WRK-KETA  +  1
               MOVE    "%"                 TO  ICD-INPUTCD (IDX-K:1)
               MOVE    "key"               TO  WRK-PATH
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           PERFORM
               UNTIL      (IDX             >  200  )   OR
                          (FLG-INPUTCD     =   1   )
      *
               PERFORM 31011-INPUTCD-CHK-SEC
               IF      FLG-OK              =   1
      *
                   IF     (ICD-DSPNAME     NOT =   SPACE )  AND
                          (ICD-SRYCD(1:1)      =   "S"  OR  "P")
                       PERFORM 31013-SET-HEN-SEC
                   ELSE
      *                点数マスタより編集
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    ICD-SRYCD           TO  TNS-SRYCD
                       PERFORM 9110-TENSU-KEYREAD-SEC
      *
                       IF      FLG-TENSU           =   ZERO
                           PERFORM 31021-TENSU-CHK-SEC
                           IF      FLG-CHK         =   ZERO
                               ADD     1               TO  IDX2
      *                        次ページの時、表示分は読み飛ばし
                               IF      IDX2            >=  WRK-STR
                                   PERFORM 31012-INPUTCD-HEN-SEC
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
           IF      FLG-MAX         NOT =   ZERO
      ******** MOVE    "0005"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W25-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-W25-NEXT
           END-IF
      *
           .
       3101-INPUTCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   セット一覧編集処理
      *****************************************************************
       31013-SET-HEN-SEC                 SECTION.
      *
           PERFORM 310131-INPUTSET-CHK-SEC
           IF      FLG-KIKAN           =   ZERO
               GO      TO      31013-SET-HEN-EXT
           END-IF
      *
           ADD     1               TO  IDX2
      *    次ページの時、表示分は読み飛ばし
           IF      IDX2            >=  WRK-STR
               CONTINUE
           ELSE
               GO      TO      31013-SET-HEN-EXT
           END-IF
      *
           ADD     1                   TO  IDX
           IF      IDX                 >   200
               MOVE    1                   TO  SPA-W25-NEXT
               MOVE    1                   TO  FLG-MAX
           ELSE
               ADD     1                   TO  WRK-CNT-ENT
               MOVE    IDX                 TO  SPA-W25-GMN-NO  (IDX)
               MOVE    ICD-INPUTCD         TO  SPA-W25-GMN-INPUTCD
                                                                (IDX)
               MOVE    ICD-SRYCD           TO  SPA-W25-GMN-SRYCD
                                                                (IDX)
               MOVE    ICD-DSPNAME         TO  SPA-W25-GMN-NAME
                                                               (IDX)
               IF      FLG-KIKAN           =   2
                   MOVE    SPACE           TO  SPA-W25-GMN-NAME(IDX)
                   STRING  "【期間外】"    DELIMITED  BY  SIZE
                           ICD-DSPNAME     DELIMITED  BY  SIZE
                                           INTO  SPA-W25-GMN-NAME
                                                               (IDX)
                   END-STRING
               END-IF
               MOVE    ICD-SRYCD           TO  SPA-W25-GMN-SETCD (IDX)
           END-IF
      *
           .
       31013-SET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力セットマスタチェック処理
      *****************************************************************
       310131-INPUTSET-CHK-SEC                 SECTION.
      *
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    ICD-SRYCD           TO  ISET-SETCD
           MOVE    SPA-W25-SYSYMD      TO  ISET-YUKOSTYMD
           MOVE    SPA-W25-SYSYMD      TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           IF      SPA-W25-MOTOPG      =   "K05"
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  FLG-KIKAN
           PERFORM
               UNTIL    FLG-INPUTSET    =   1
               IF      (ISET-YUKOSTYMD     <=  SPA-W25-SYSYMD ) AND
                       (ISET-YUKOEDYMD     >=  SPA-W25-SYSYMD )
                   MOVE    1               TO  FLG-KIKAN
                   MOVE    1               TO  FLG-INPUTSET
               ELSE
                   MOVE    2               TO  FLG-KIKAN
               END-IF
               IF      FLG-INPUTSET        =   ZERO
                   MOVE    "tbl_inputset"      TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 975-INPUTSET-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       310131-INPUTSET-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードより編集処理
      *****************************************************************
       31012-INPUTCD-HEN-SEC                 SECTION.
      *
           ADD     1                   TO  IDX
      *    ２００件オーバーチェック
           IF      IDX                 >   200
               MOVE    1                   TO  FLG-MAX
               MOVE    1                   TO  SPA-W25-NEXT
               GO      TO      31012-INPUTCD-HEN-EXT
           END-IF
      *
           ADD     1                   TO  WRK-CNT-ENT
           MOVE    IDX                 TO  SPA-W25-GMN-NO     (IDX)
           MOVE    ICD-INPUTCD         TO  SPA-W25-GMN-INPUTCD (IDX)
           MOVE    ICD-SRYCD           TO  SPA-W25-GMN-SRYCD   (IDX)
           MOVE    TNS-NAME            TO  SPA-W25-GMN-NAME    (IDX)
           MOVE    TNS-TEN             TO  SPA-W25-GMN-KISOTEN (IDX)
      *    薬剤・器材の時単位
           IF      TNS-SRYCD (1:1) =   "6"  OR  "7"
                MOVE    TNS-TANINAME       TO  SPA-W25-GMN-TANI(IDX)
           END-IF
      *
      *    診療コードの時、種別
           IF      TNS-SRYCD (1:1)     =   "1"
               MOVE    TNS-SRYKBN          TO  SPA-W25-GMN-TANI(IDX)
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    診療行為で老人の時、再編集
           IF     (TNS-SRYCD(1:1)      =   "1") AND
                  (TNS-ROUTEKKBN       =   2  )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  "［老］"      DELIMITED   BY  SIZE
                        TNS-NAME      DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
           END-IF
      *    後発医薬品再編集
           IF     (TNS-SRYCD(1:1)      =   "6") AND
                  (TNS-KOUHATUKBN      =   1  )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  "【後発】"    DELIMITED   BY  SIZE
                        TNS-NAME      DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
           END-IF
      *    後発医薬品再編集
           IF     (TNS-SRYCD(1:1)      =   "6") AND
                  (TNS-KOUHATUKBN      =   2  )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  "【後発有】"  DELIMITED   BY  SIZE
                        TNS-NAME      DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
           END-IF
      *!!
      *    検査正式名称
      *    IF     (TNS-SRYCD(1:1)      =   "1") AND
      *           (TNS-SRYKBN          =   "60"
      *                                OR  "64"  )  AND
      *           (TNS-FORMALNAME  NOT =   SPACE )  AND
      *           (TNS-FORMALNAME  NOT =   TNS-NAME )
      *         MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
      *         STRING  TNS-NAME           DELIMITED   BY  SPACE
      *                 "　【"             DELIMITED   BY  SIZE
      *                 TNS-FORMALNAME     DELIMITED   BY  SPACE
      *                 "】"               DELIMITED   BY  SIZE
      *                        INTO        SPA-W25-GMN-NAME(IDX)
      *         END-STRING
      *         PERFORM 310211-KENSA-NAME-SEC
      **** END-IF
      *
      *H24.6
      *    共通編集処理
           PERFORM 310211-TENSU-KYOHEN-SEC
      *
      *    入力ＣＤ
           IF      ICD-DSPSEQ          =   1
               MOVE    ICD-INPUTCD         TO  SPA-W25-GMN-SETCD (IDX)
           ELSE
               PERFORM 3200-SETCD-HEN-SEC
           END-IF
      *
           .
       31012-INPUTCD-HEN-EXT.
           EXIT.
      *
      *H24.6
      *****************************************************************
      *    点数マスタ共通編集処理
      *****************************************************************
       310211-TENSU-KYOHEN-SEC             SECTION.
      *
      *    検査正式名称
           IF     (TNS-SRYCD(1:1)      =   "1") AND
                  (TNS-SRYKBN          =   "60"
                                       OR  "64"  )  AND
                  (TNS-FORMALNAME  NOT =   SPACE )  AND
                  (TNS-FORMALNAME  NOT =   TNS-NAME )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  TNS-NAME           DELIMITED   BY  SPACE
                        "　【"             DELIMITED   BY  SIZE
                        TNS-FORMALNAME     DELIMITED   BY  SPACE
                        "】"               DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
                PERFORM 310211-KENSA-NAME-SEC
           END-IF
      *
      *    医療観察コード名称編集
           IF      (TNS-SRYCD(1:1)     =   "1"  )  AND
                   (TNS-IRYOKANSATUKBN =   4    )
      *        医療観察区分
               MOVE    SPACE               TO  SPA-W25-GMN-NAME (IDX)
               STRING  "【医観】"          DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      INTO                 SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
      *
      *    労災コード名称編集
           IF      (TNS-SRYCD(1:3)     =   "101")
      *    労災（器材）
           OR     ((TNS-SRYCD(1:1)     =   "7"    )  AND
                   (TNS-SRYCD(2:5)     =   "88888"))
      *    労災（コメント）
           OR     ((TNS-SRYCD(1:1)     =   "8"  )  AND
                   (TNS-SRYCD(4:1)     =   "8"  ))
               MOVE    SPACE               TO  SPA-W25-GMN-NAME (IDX)
               STRING  "【労】"            DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      INTO                 SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
      *
      *    公害のコード
           IF      (TNS-SRYCD (1:3)    =   "102")
               MOVE    SPACE               TO  SPA-W25-GMN-NAME (IDX)
               STRING  "【公害】"          DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      INTO                 SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
      *
           .
       310211-TENSU-KYOHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ編集処理
      *****************************************************************
       3200-SETCD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    TNS-SRYCD           TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-INPUTCD     TO  SPA-W25-GMN-SETCD (IDX)
           END-IF
      *
           .
       3200-SETCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一コードチェック処理
      *****************************************************************
       31011-INPUTCD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ERR
      *
           PERFORM VARYING     IDXC    FROM    1   BY  1
               UNTIL      (IDXC        >   200 )   OR
                          (SPA-W25-GMN-SRYCD(IDXC)
                                       =   SPACE)   OR
                          (FLG-ERR     =   1   )
               IF      ICD-SRYCD       =   SPA-W25-GMN-SRYCD(IDXC)
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   ZERO
               MOVE    1                   TO  FLG-OK
           END-IF
      *
           .
       31011-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ−より編集
      *****************************************************************
       3102-TENSU-SET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-ENT
           MOVE    ZERO                TO  FLG-MAX
           MOVE    SPACE               TO  WRK-KEY-INPUT
      *
      *    ユーザー登録分
           IF      SPA-W25-GMN-SYORI   =   "U"
               CONTINUE
           ELSE
      *
           IF      SPA-W25-GMN-ALL     =   SPACE
      *        自院分のみ
               EVALUATE    SPA-W25-GMN-SYORI
      *            内服
                   WHEN    "3"
                       MOVE    "key51j"       TO  WRK-PATH
      *            外用
                   WHEN    "4"
                       MOVE    "key52j"       TO  WRK-PATH
      *            注射
                   WHEN    "5"
                       MOVE    "key53j"       TO  WRK-PATH
      *            器材
                   WHEN    "7"
                       MOVE    "key6j"        TO  WRK-PATH
      *            診療行為
                   WHEN    "8"
                       MOVE    "key7j1"       TO  WRK-PATH
      *            検査
                   WHEN    "9"
                       MOVE    "key42jk"       TO  WRK-PATH
      *            内服
                   WHEN    OTHER
                       MOVE    "3"              TO  SPA-W25-GMN-SYORI
                       MOVE    "key51j"         TO  WRK-PATH
               END-EVALUATE
           ELSE
      *        全体分
               EVALUATE    SPA-W25-GMN-SYORI
      *            内服
                   WHEN    "3"
                       MOVE    "key51"       TO  WRK-PATH
      *            外用
                   WHEN    "4"
                       MOVE    "key52"      TO  WRK-PATH
      *            注射
                   WHEN    "5"
                       MOVE    "key53"       TO  WRK-PATH
      *            器材
                   WHEN    "7"
                       MOVE    "key6"        TO  WRK-PATH
      *            診療行為
                   WHEN    "8"
                       MOVE    "key71"        TO  WRK-PATH
      *            検査
                   WHEN    "9"
                       MOVE    "key42k"      TO  WRK-PATH
      *            内服
                   WHEN    OTHER
                       MOVE    "3"            TO  SPA-W25-GMN-SYORI
                       MOVE    "key51"        TO  WRK-PATH
               END-EVALUATE
      *
           END-IF
      *        検索キー編集
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    "1"                 TO  SADDSIGN-KBN
               MOVE    SPA-W25-GMN-INPUT   TO  SADDSIGN-STR
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR        TO  WRK-KEY-INPUT
      *
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           IF      SPA-W25-GMN-SYORI   =   "9"
               MOVE    SPA-W25-NAI-KENSANO TO  TNS-KNSJISGRPKBN
           ELSE
               MOVE    WRK-KEY-INPUT       TO  TNS-NAME
               MOVE    WRK-KEY-INPUT       TO  TNS-KANANAME
           END-IF
      *
      *    ユーザー登録分
           IF      SPA-W25-GMN-SYORI   =   "U"
               PERFORM 31021-ETC-PATH-SET-SEC
           ELSE
               IF     (SPA-W25-GMN-SYORI   NOT =   "9"  )  AND
                      (SPA-W25-GMN-INPUT       =   SPACE)
                   MOVE    ZERO                TO  SPA-W25-GMN-MAX
                   GO      TO      3102-TENSU-SET-EXT
               END-IF
           END-IF
      *
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX         >    200 )   OR
                          (FLG-TENSU   =   1   )
      *
      *        点数マスタチェック
               PERFORM 31021-TENSU-CHK-SEC
      *        検査で包括判定がある時の判定
               IF      SPA-W25-GMN-SYORI   =   "9"
                   EVALUATE    SPA-W25-NAI-HOUKATU
                       WHEN    "1"
      *                    包括あり
                           IF      TNS-HOUKNSKBN       =   ZERO
                               MOVE    1               TO  FLG-CHK
                           END-IF
                       WHEN    "2"
      *                    包括なし
                           IF      TNS-HOUKNSKBN   NOT =   ZERO
                               MOVE    1               TO  FLG-CHK
                           END-IF
                   END-EVALUATE
               END-IF
      *        システム予約の入力不可分
               IF      SPA-W25-GMN-SYORI3      =   "U6"
                   IF      TNS-SRYCD           >=  "099999911"
                       MOVE    1               TO  FLG-CHK
                   END-IF
      *            リハビリ算定
                   IF      TNS-SRYCD           =   "099800201"
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF      TNS-SRYCD(1:4)      =   "0996"
                                               OR  "0997"
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  WRK-STR
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
      *
      *****MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *
      *    労災分表示の時、残りのマスタを表示する
           EVALUATE    SPA-W25-GMN-SYORI3
               WHEN    "U5"
                   PERFORM 31022-RSI-HENSYU-SEC
           END-EVALUATE
      *
      *    点数マスタで対象が有った時、タイプを変更する
      *    IF      SPA-W25-GMN-MAX     >   ZERO
               MOVE    "2"             TO  SPA-W25-TYPE
      *    END-IF
           .
       3102-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー登録、自院採用分処理
      *****************************************************************
       31021-ETC-PATH-SET-SEC          SECTION.
      *
           EVALUATE    SPA-W25-GMN-SYORI3
               WHEN    "J1"
      *            自院・内服
                   MOVE    "key11m1"            TO  WRK-PATH
                   MOVE    "600000000"          TO  TNS-SRYCD
                   MOVE    "699999999"          TO  TNSUP-SRYCD
                   MOVE    1                    TO  TNS-YKZKBN
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "J2"
      *            自院・外用
                   MOVE    "key11m1"            TO  WRK-PATH
                   MOVE    "600000000"          TO  TNS-SRYCD
                   MOVE    "699999999"          TO  TNSUP-SRYCD
                   MOVE    6                    TO  TNS-YKZKBN
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "J3"
      *            自院・注射 
                   MOVE    "key11m1"            TO  WRK-PATH
                   MOVE    "600000000"          TO  TNS-SRYCD
                   MOVE    "699999999"          TO  TNSUP-SRYCD
                   MOVE    4                    TO  TNS-YKZKBN
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "J4"
      *            自院・処置
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "140000000"          TO  TNS-SRYCD
                   MOVE    "149999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "J5"
      *            自院・診察
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "110000000"          TO  TNS-SRYCD
                   MOVE    "119999999"          TO  TNSUP-SRYCD
               WHEN    "J6"
      *            自院・注射
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "130000000"          TO  TNS-SRYCD
                   MOVE    "139999999"          TO  TNSUP-SRYCD
               WHEN    "J7"
      *            自院・手術
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "150000000"          TO  TNS-SRYCD
                   MOVE    "159999999"          TO  TNSUP-SRYCD
               WHEN    "J8"
      *            自院・画像診断
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "170000000"          TO  TNS-SRYCD
                   MOVE    "179999999"          TO  TNSUP-SRYCD
               WHEN    "J9"
      *            自院・その他
                   MOVE    "key12m1"            TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "180000000"          TO  TNS-SRYCD
                   MOVE    "199999999"          TO  TNSUP-SRYCD
               WHEN    "U4"
      *            自院・コメント
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
                   MOVE    "800000000"          TO  TNS-SRYCD
                   MOVE    "899999999"          TO  TNSUP-SRYCD
               WHEN    "U1"
      *            用法
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "001000000"          TO  TNS-SRYCD
                   MOVE    "001999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "U2"
      *            部位
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "002000000"          TO  TNS-SRYCD
                   MOVE    "002999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "U3"
      *            その他
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "003000000"          TO  TNS-SRYCD
                   MOVE    "098999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "U5"
      *            労災診察
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "100000000"          TO  TNS-SRYCD
                   MOVE    "109999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
               WHEN    "U6"
      *            予約コード
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "099000000"          TO  TNS-SRYCD
                   MOVE    "099999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-W25-GMN-ALL
      *TEST
               WHEN    "JK"
      *            自院・器材
               MOVE    "key13m1"            TO  WRK-PATH
               MOVE    "1"                  TO  SPA-W25-GMN-ALL
               MOVE    "700000000"          TO  TNS-SRYCD
               MOVE    "799999999"          TO  TNSUP-SRYCD
               WHEN    "UK"
      *            自院・器材
               MOVE    "key13s"             TO  WRK-PATH
               MOVE    "1"                  TO  SPA-W25-GMN-ALL
               MOVE    "700000000"          TO  TNS-SRYCD
               MOVE    "799999999"          TO  TNSUP-SRYCD
      *TEST
           END-EVALUATE
      *
           .
       31021-ETC-PATH-SET-EXT.
           EXIT.
      *****************************************************************
      *    労災分表示処理
      *****************************************************************
       31022-RSI-HENSYU-SEC                  SECTION.
      *
           PERFORM VARYING     IDX-KAI     FROM    1   BY  1
                   UNTIL       IDX-KAI     >   4
               INITIALIZE                      TNS-TENSU-REC
               EVALUATE    IDX-KAI
                   WHEN    1
      *            器材
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "788888000"          TO  TNS-SRYCD
                       MOVE    "788888999"          TO  TNSUP-SRYCD
                   WHEN    2
      *            コメント１
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "820800000"          TO  TNS-SRYCD
                       MOVE    "820899999"          TO  TNSUP-SRYCD
                   WHEN    3
      *            コメント２
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "830800000"          TO  TNS-SRYCD
                       MOVE    "830899999"          TO  TNSUP-SRYCD
                   WHEN    4
      *            コメント３
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "840800000"          TO  TNS-SRYCD
                       MOVE    "840899999"          TO  TNSUP-SRYCD
               END-EVALUATE
      *
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 910-TENSU-SELECT-SEC
      *
               PERFORM
                    UNTIL      (IDX         >    200 )   OR
                               (FLG-TENSU   =   1   )
      *
      *            点数マスタチェック
                   PERFORM 31021-TENSU-CHK-SEC
      *
                   IF      FLG-CHK             =   ZERO
                       ADD     1               TO  IDX2
      *                次ページの時、表示分は読み飛ばし
                       IF      IDX2            >=  WRK-STR
                           PERFORM 31021-TENSU-HEN-SEC
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PATH            TO  MCP-PATHNAME
                   PERFORM                     910-TENSU-READ-SEC
               END-PERFORM
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           MOVE    WRK-CNT-ENT         TO  SPA-W25-GMN-MAX
      *
           .
       31022-RSI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       31021-TENSU-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    病院診療所区分判定
           IF      (TNS-HOSPSRYKBN     =   ZERO        )  OR
                   (TNS-HOSPSRYKBN     =   SPA-HOSPSBT )
      *H30.4
      *       短期滞在手術基本２（病院・診療所）
              OR   (TNS-HOSPSRYKBN     =   3   )
      *       短期滞在手術基本３（病院のみ）
              OR  ((TNS-HOSPSRYKBN     =   4   )   AND
                   (SPA-HOSPSBT        =   1   ))
               CONTINUE
           ELSE
               MOVE   1                TO  FLG-CHK
           END-IF
      *
      *    老人区分判定
      *    IF      (TNS-ROUTEKKBN      =   ZERO        )  OR
      *            (TNS-ROUTEKKBN      =   1    AND
      *             SPA-NAI-ROUJIN     =   ZERO        )  OR
      *            (TNS-ROUTEKKBN      =   2    AND
      *             SPA-NAI-ROUJIN     =   1           )
      *        CONTINUE
      *    ELSE
      *        MOVE   1                TO  FLG-CHK
      *    END-IF
      *
      *    入外区分
      *    IF      (TNS-NYUGAITEKKBN      =   ZERO        )  OR
      *            (TNS-NYUGAITEKKBN      =   SPA-NYUGAIKBN)
      *        CONTINUE
      *    ELSE
      *        MOVE   1                TO  FLG-CHK
      *    END-IF
      *
           .
       31021-TENSU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       31021-TENSU-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDX
      *    ２００件オーバーチェック
           IF      IDX                 >   200
               MOVE    1                   TO  FLG-MAX
               MOVE    1                   TO  SPA-W25-NEXT
               GO      TO      31021-TENSU-HEN-EXT
           END-IF
      *
           ADD     1                   TO  WRK-CNT-ENT
      **** MOVE    IDX2                TO  SPA-W25-GMN-NO   (IDX)
           MOVE    IDX                 TO  SPA-W25-GMN-NO   (IDX)
           MOVE    TNS-SRYCD           TO  SPA-W25-GMN-INPUTCD
                                                            (IDX)
           MOVE    TNS-SRYCD           TO  SPA-W25-GMN-SRYCD(IDX)
           MOVE    TNS-NAME            TO  SPA-W25-GMN-NAME
                                                             (IDX)
           MOVE    TNS-TEN             TO  SPA-W25-GMN-KISOTEN
                                                            (IDX)
      *     薬剤・器材の時単位
           IF      TNS-SRYCD (1:1)     =   "6"  OR  "7"
               MOVE    TNS-TANINAME        TO  SPA-W25-GMN-TANI(IDX)
           END-IF
      *
      *    診療コードの時、種別
           IF      SPA-W25-GMN-SYORI   =   "9"
               CONTINUE
           ELSE
               IF      TNS-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SRYKBN          TO  SPA-W25-GMN-TANI(IDX)
               END-IF
               IF      TNS-SRYCD (1:3)     =   "099"
                   MOVE    TNS-SRYKBN          TO  SPA-W25-GMN-TANI(IDX)
               END-IF
           END-IF
      *
      *    診療行為で老人の時、再編集
      *****IF     (SPA-W25-GMN-SYORI   =   "8" )  AND
           IF     (TNS-ROUTEKKBN       =   2   )
               MOVE    SPACE               TO  SPA-W25-GMN-NAME(IDX)
               STRING  "［老］"
                       TNS-NAME            DELIMITED   BY  SIZE
                       INTO                    SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
      *    後発医薬品再編集
           IF     (TNS-SRYCD(1:1)      =   "6") AND
                  (TNS-KOUHATUKBN      =   1  )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  "【後発】"    DELIMITED   BY  SIZE
                        TNS-NAME      DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
           END-IF
      *    後発医薬品再編集
           IF     (TNS-SRYCD(1:1)      =   "6") AND
                  (TNS-KOUHATUKBN      =   2  )
                MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
                STRING  "【後発有】"  DELIMITED   BY  SIZE
                        TNS-NAME      DELIMITED   BY  SIZE
                               INTO        SPA-W25-GMN-NAME(IDX)
                END-STRING
           END-IF
      *!!
      *    検査正式名称
      *    IF     (TNS-SRYCD(1:1)      =   "1") AND
      *           (TNS-SRYKBN          =   "60"
      *                                OR  "64" )  AND
      *           (TNS-FORMALNAME  NOT =   SPACE )  AND
      *           (TNS-FORMALNAME  NOT =   TNS-NAME )
      *         MOVE    SPACE              TO  SPA-W25-GMN-NAME (IDX)
      *         STRING  TNS-NAME           DELIMITED   BY  SPACE
      *                 "　【"             DELIMITED   BY  SIZE
      *                 TNS-FORMALNAME     DELIMITED   BY  SPACE
      *                 "】"               DELIMITED   BY  SIZE
      *                        INTO        SPA-W25-GMN-NAME(IDX)
      *         END-STRING
      *         PERFORM 310211-KENSA-NAME-SEC
      **** END-IF
      *H24.6
      *    共通編集処理
           PERFORM 310211-TENSU-KYOHEN-SEC
      *
      *    入力ＣＤ
           PERFORM 3200-SETCD-HEN-SEC
      *
      *    検査の時、包括の編集
           IF      SPA-W25-GMN-SYORI   =   "9"
               EVALUATE    TNS-HOUKNSKBN
                   WHEN    01
      *                血液化学検査の包括
                       MOVE    "★"            TO  WRK-KENSA-HEN
                   WHEN    02
      *                内分泌学検査の包括
                       MOVE    "★"            TO  WRK-KENSA-HEN
                   WHEN    03
      *                肝炎ウイルス関連検査の包括
                       MOVE    "★"            TO  WRK-KENSA-HEN
                   WHEN    04
      *                腫瘍マーカーの包括
                       MOVE    "●"            TO  WRK-KENSA-HEN
                   WHEN    05
      *                ４以外の腫瘍マーカーの包括
                       MOVE    "◆"            TO  WRK-KENSA-HEN
                   WHEN    06
      *                出血・凝固検査の包括
                       MOVE    "★"            TO  WRK-KENSA-HEN
                   WHEN    07
      *                自己抗体検査の包括
                       MOVE    "◆"            TO  WRK-KENSA-HEN
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-KENSA-HEN
               END-EVALUATE
               MOVE    SPA-W25-GMN-NAME(IDX)   TO  WRK-HEN-NAME
               MOVE    SPACE               TO  SPA-W25-GMN-NAME(IDX)
               STRING  WRK-KENSA-HEN       DELIMITED   BY  SIZE
                       WRK-HEN-NAME        DELIMITED   BY  SIZE
                       INTO                    SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
           IF      SPA-W25-GMN-SYORI   =   "K"
               EVALUATE    TNS-HOUKNSKBN
                   WHEN    01
      *                血液化学検査の包括
                       MOVE    "★"            TO  WRK-KENSA-HEN
                   WHEN    02
      *                内分泌学検査の包括
                       MOVE    "▼"            TO  WRK-KENSA-HEN
                   WHEN    03
      *                肝炎ウイルス関連検査の包括
                       MOVE    "▲"            TO  WRK-KENSA-HEN
                   WHEN    04
      *                腫瘍マーカーの包括
                       MOVE    "●"            TO  WRK-KENSA-HEN
                   WHEN    05
      *                ４以外の腫瘍マーカーの包括
                       MOVE    "◆"            TO  WRK-KENSA-HEN
                   WHEN    06
      *                出血・凝固検査の包括
                       MOVE    "■"            TO  WRK-KENSA-HEN
                   WHEN    07
      *                自己抗体検査の包括
                       MOVE    "◇"            TO  WRK-KENSA-HEN
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-KENSA-HEN
               END-EVALUATE
               MOVE    SPA-W25-GMN-NAME(IDX)   TO  WRK-HEN-NAME
               MOVE    SPACE               TO  SPA-W25-GMN-NAME(IDX)
               STRING  WRK-KENSA-HEN       DELIMITED   BY  SIZE
                       WRK-HEN-NAME        DELIMITED   BY  SIZE
                       INTO                    SPA-W25-GMN-NAME(IDX)
               END-STRING
           END-IF
      *
           .
       31021-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査名称再編集処理
      *****************************************************************
       310211-KENSA-NAME-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME1-G
           MOVE    TNS-NAME            TO  WRK-NAME1-G
           IF      WRK-NAME12          =   SPACE
               STRING  "【"                DELIMITED   BY  SIZE
                        TNS-FORMALNAME     DELIMITED   BY  SPACE
                        "】"               DELIMITED   BY  SIZE
                               INTO        WRK-NAME12
               END-STRING
               INSPECT WRK-NAME1-G         REPLACING  ALL "  "  BY  "　"
               MOVE    WRK-NAME1-G         TO  SPA-W25-GMN-NAME (IDX)
           ELSE
               MOVE    SPACE               TO  WRK-NAME2-G
               MOVE    TNS-NAME            TO  WRK-NAME2-G
               IF      WRK-NAME22          =   SPACE
                   STRING  "【"                DELIMITED   BY  SIZE
                            TNS-FORMALNAME     DELIMITED   BY  SPACE
                            "】"               DELIMITED   BY  SIZE
                                   INTO        WRK-NAME22
                   END-STRING
                   INSPECT WRK-NAME2-G     REPLACING  ALL "  "  BY  "　"
                   MOVE    WRK-NAME2-G     TO  SPA-W25-GMN-NAME (IDX)
               END-IF
           END-IF
      *
           .
       310211-KENSA-NAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-W25-GMN-MAX     =   ZERO )
               MOVE    ZERO                TO  WRK-W25-ROW
           ELSE
               MOVE    W25-ROW             TO  WRK-W25-ROW
           END-IF
      *
           MOVE    SPACE               TO  W25
           INITIALIZE                      W25
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  W25-ROW
           MOVE    ZERO                TO  W25-ROWATTR
           IF      WRK-W25-ROW     NOT =   ZERO
               MOVE    WRK-W25-ROW         TO  W25-ROW
           END-IF
      *
           MOVE    SPA-W25-GMN-INPUT   TO  W25-INPUT
      *
           IF     (SPA-W25-TYPE        =   "1" )  AND
                  (SPA-W25-CDSYUBETU   =   "6" )
               MOVE    SPACE               TO  SPA-W25-GMN-SYORI
               IF      SPA-W25-GMN-INPUT(1:1)  =   "P"  OR  "S"
                   MOVE    "P"                 TO  SPA-W25-GMN-SYORI
               END-IF
           END-IF
           IF     (SPA-W25-TYPE        =   "1" )  AND
                  (SPA-W25-CDSYUBETU   =   "A" )
               MOVE    SPACE               TO  SPA-W25-GMN-SYORI
           END-IF
      *
           EVALUATE    SPA-W25-GMN-SYORI
      *        内服
               WHEN    "3"
                   IF      SPA-W25-GMN-ALL     =   SPACE
                       MOVE    "内服（自院）" 
                                               TO  W25-SENTAKU
                   ELSE
                       MOVE    "内服（全体）" 
                                               TO  W25-SENTAKU
                   END-IF
      *        外用
               WHEN    "4"
                   IF      SPA-W25-GMN-ALL     =   SPACE
                       MOVE    "外用（自院）" 
                                               TO  W25-SENTAKU
                   ELSE
                       MOVE    "外用（全体）" 
                                               TO  W25-SENTAKU
                   END-IF
      *        注射
               WHEN    "5"
                   IF      SPA-W25-GMN-ALL     =   SPACE
                       MOVE    "注射（自院）" 
                                               TO  W25-SENTAKU
                   ELSE
                       MOVE    "注射（全体）" 
                                               TO  W25-SENTAKU
                   END-IF
      *        器材
               WHEN    "7"
                   IF      SPA-W25-GMN-ALL     =   SPACE
                       MOVE    "器材（自院）" 
                                               TO  W25-SENTAKU
                   ELSE
                       MOVE    "器材（全体）" 
                                               TO  W25-SENTAKU
                   END-IF
      *        診療行為
               WHEN    "8"
                   IF      SPA-W25-GMN-ALL     =   SPACE
                       MOVE    "診療行為（自院）" 
                                               TO  W25-SENTAKU
                   ELSE
                       MOVE    "診療行為（全体）" 
                                               TO  W25-SENTAKU
                   END-IF
      *        検査
               WHEN    "9"
                   MOVE    "検査"              TO  W25-SENTAKU
                   IF      SPA-W25-NAI-KENSANO     NOT =   ZERO
                       PERFORM 5010-GRPMEI-SET-SEC
                       MOVE    WRK-GRP-MEI-X       TO  W25-SENTAKU
                   END-IF
      *        セット
               WHEN    "P"
                   MOVE    "セット"            TO  W25-SENTAKU
      *        ユーザー登録
               WHEN    "U"
                   EVALUATE    SPA-W25-GMN-SYORI3
                       WHEN    "J1"
                           MOVE    "自院採用（内服）"
                                               TO  W25-SENTAKU
                       WHEN    "J2"
                           MOVE    "自院採用（外用）"
                                               TO  W25-SENTAKU
                       WHEN    "J3"
                           MOVE    "自院採用（注射）"
                                               TO  W25-SENTAKU
                       WHEN    "J4"
                           MOVE    "自院採用（処置）"
                                               TO  W25-SENTAKU
                       WHEN    "J5"
                           MOVE    "自院採用（診察）"
                                               TO  W25-SENTAKU
                       WHEN    "J6"
                           MOVE    "自院採用（注射）"
                                               TO  W25-SENTAKU
                       WHEN    "J7"
                           MOVE    "自院採用（手術・麻酔）"
                                               TO  W25-SENTAKU
                       WHEN    "J8"
                           MOVE    "自院採用（画像診断）"
                                               TO  W25-SENTAKU
                       WHEN    "J9"
                           MOVE    "自院採用（その他）"
                                               TO  W25-SENTAKU
                       WHEN    "U1"
                           MOVE    "用法（全体）"
                                               TO  W25-SENTAKU
                       WHEN    "U2"
                           MOVE    "撮影部位（全体）"
                                               TO  W25-SENTAKU
                       WHEN    "U3"
                           MOVE    "ユーザー登録（その他）"
                                               TO  W25-SENTAKU
                       WHEN    "U4"
                           MOVE    "コメント（全体）"
                                               TO  W25-SENTAKU
                       WHEN    "U5"
                           MOVE    "労災"
                                               TO  W25-SENTAKU
                       WHEN    "U6"
                           MOVE    "システム予約コード"
                                               TO  W25-SENTAKU
                       WHEN    "JK"
                           MOVE    "器材（自院）"
                                               TO  W25-SENTAKU
                       WHEN    "UK"
                           MOVE    "器材（全体）"
                                               TO  W25-SENTAKU
                   END-EVALUATE
      *        その他
               WHEN    OTHER
                   MOVE    "入力コード全体（自院）"
                                              TO  W25-SENTAKU
                   IF      SPA-W25-GMN-INPUT(1:1) =   "."
                       MOVE    "診療種別"     TO  W25-SENTAKU
                   END-IF
      *TEST
                   IF      SPA-W25-GMN-INPUT(1:1) =   "*"
                       PERFORM 5001-SENTAKU-MEI-SEC
                   END-IF
                   IF      SPA-W25-GMN-INPUT(1:1) =   "/"
                       MOVE    "検査点数別"   TO  W25-SENTAKU
                   END-IF
      *TEST
           END-EVALUATE
      *
      *    漢字入力で、入力コードで検索
           IF     (SPA-W25-TYPE        =   "1" )  AND
                  (SPA-W25-CDSYUBETU   =   "J" )
               MOVE    "入力コード全体（自院）"
                                              TO  W25-SENTAKU
           END-IF
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-W25-GMN-MAX
      *
               MOVE    IDX                 TO  WRK-NOZ
               MOVE    WRK-NOZ             TO  W25-NO  (IDX)
               MOVE    IDX                 TO  SPA-W25-NAI-NO(IDX)
      *        検査判断の時、区分を編集
               IF     (SPA-W25-GMN-SYORI       =   "9" )  AND
                      (SPA-W25-GMN-SYORI2      =   SPACE)
                   MOVE    SPA-W25-GMN-NO-X (IDX)
                                               TO  W25-NO (IDX)
                   IF      SPA-W25-GMN-NO-X (IDX)(3:1)   =   SPACE
                       MOVE    ZERO            TO  SPA-W25-NAI-NO(IDX)
                       MOVE    SPA-W25-GMN-NO-X (IDX)(1:2)
                                               TO  SPA-W25-NAI-NO(IDX)
                                                                (2:2)
                   ELSE
                       MOVE    SPA-W25-GMN-NO-X (IDX)
                                               TO  SPA-W25-NAI-NO(IDX)
                   END-IF
               END-IF
      *
               MOVE    SPA-W25-GMN-NAME (IDX)  TO  W25-NAME (IDX)
      *        単価
               IF     (SPA-W25-GMN-KISOTEN(IDX)    >   ZERO  )  OR 
                      (SPA-W25-GMN-SRYCD(IDX)(1:1) =   "1"
                                                   OR  "6"
                                                   OR  "7"   )  OR
                      (SPA-W25-GMN-SRYCD(IDX)(1:3) =   "059"
                                                   OR  "095"
                                                   OR  "096" )
                   MOVE    SPA-W25-GMN-KISOTEN(IDX)
                                               TO  WRK-TENSUS
                   MOVE    WRK-TENSUS          TO  W25-KISOTEN (IDX)
               END-IF
      *                                            
      *        IF    ((SPA-W25-GMN-KISOTEN(IDX)    =   ZERO  )  AND
      *               (SPA-W25-GMN-SRYCD(IDX)(1:1) =   "S" OR "P")) OR
      *              ((SPA-W25-GMN-SYORI           =   "9"   )  AND
      *               (SPA-W25-GMN-SYORI2          =   SPACE ))
      *            セット一覧の時、セットコードを単価へ
      *            IF      SPA-W25-GMN-SRYCD (IDX)(1:1)
      *                                            =   "P" OR "S"
      *                 MOVE    SPA-W25-GMN-SRYCD (IDX)
      *                                        TO  W25-KISOTEN (IDX)
      *            END-IF
      *        ELSE
      *            MOVE    SPA-W25-GMN-KISOTEN(IDX)
      *                                        TO  WRK-TENSUS
      *            MOVE    WRK-TENSUS          TO  W25-KISOTEN (IDX)
      *****    END-IF
      *        セット名称
               MOVE    SPA-W25-GMN-SETCD(IDX)  TO  W25-SETCD (IDX)
      *
      *!!      MOVE    SPA-W25-GMN-TANI (IDX)  TO  W25-TANI (IDX)
               IF     (SPA-W25-GMN-SRYCD (IDX) (1:1)   =   "1" ) OR
                      (SPA-W25-GMN-SRYCD (IDX) (1:3)   =   "099")
                   MOVE    SPA-W25-GMN-TANI (IDX)  TO  W25-SRYKBN(IDX)
               ELSE
                   MOVE    SPA-W25-GMN-TANI (IDX)  TO  W25-TANI (IDX)
               END-IF
      *        MOVE    "F"             TO  W25-SELECT  (IDX)
      *        PERFORM VARYING     IDS     FROM    1   BY  1
      *                UNTIL       IDS     >   1
      *            IF      SPA-W25-NAI-SELNUM(01) NOT =   ZERO
      *                IF      SPA-W25-NAI-SELNUM(01) =   IDX
      *                    MOVE    "T"             TO  W25-SELECT  (IDX)
      *                    MOVE    10              TO  IDS
      *                END-IF
      *            END-IF
      *        END-PERFORM
           END-PERFORM
      *
           MOVE    SPA-W25-GMN-MAX     TO  W25-COUNT
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   10
      *        IF      SPA-W25-NAI-SELNUM(IDX) NOT =   ZERO
      *            MOVE    SPA-W25-NAI-SELNUM(IDX)     TO  WRK-NOZ
      *            MOVE    WRK-NOZ                 TO  W25-SELNUM (IDX)
      *        END-IF
      *        MOVE    SPA-W25-GMN-SELNUM (IDX)    TO  W25-SELNUM (IDX)
      *    END-PERFORM
           MOVE    SPA-W25-GMN-SELNUM  TO  W25-SELNUM1
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-W25-JIINLST (IDX)   NOT =   SPACE
                   MOVE    SPA-W25-JIINLST(IDX) TO  W25-JIIN-ITEM(IDX)
                   MOVE    IDX                  TO  W25-JIIN-COUNT
               END-IF
           END-PERFORM
           IF     (SPA-W25-GMN-SYORI   =   "U"  )  AND
                  (SPA-W25-GMN-SYORI3(1:1)  =   "J")
               MOVE    SPA-W25-JIIN-G      TO  W25-JIIN
           ELSE
               MOVE    SPACE               TO  SPA-W25-JIIN-G
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-W25-USERLST (IDX)   NOT =   SPACE
                   MOVE    SPA-W25-USERLST(IDX) TO  W25-USER-ITEM(IDX)
                   MOVE    IDX                  TO  W25-USER-COUNT
               END-IF
           END-PERFORM
           IF     (SPA-W25-GMN-SYORI   =   "U"  )  AND
                  (SPA-W25-GMN-SYORI3(1:1)  =   "U")
               MOVE    SPA-W25-USER-G      TO  W25-USER
           ELSE
               MOVE    SPACE               TO  SPA-W25-USER-G
           END-IF
      *
      *    次頁あり
           IF      SPA-W25-NEXT        =   1
               IF      SPA-W25-PAGE        =   1
                   MOVE    "次頁有り"          TO  W25-NEXT-VALUE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  W25-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  W25-NEXT-STYLE
           ELSE
               IF      SPA-W25-PAGE        >   1
                   MOVE    "前頁有り"          TO  W25-NEXT-VALUE
                   MOVE    "red"               TO  W25-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  W25-NEXT-VALUE
                   MOVE    "black"             TO  W25-NEXT-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査グループ名称編集処理
      *****************************************************************
       5001-SENTAKU-MEI-SEC            SECTION.
      *
           EVALUATE    SPA-W25-GMN-INPUT(2:1)
               WHEN    "A"
                   MOVE    "初診料・再診料・入院料"    TO  W25-SENTAKU
               WHEN    "B"
                   MOVE    "指導管理等"                TO  W25-SENTAKU
               WHEN    "C"
                   MOVE    "在宅医療"                  TO  W25-SENTAKU
               WHEN    "D"
                   MOVE    "検査"                      TO  W25-SENTAKU
               WHEN    "E"
                   MOVE    "画像診断"                  TO  W25-SENTAKU
               WHEN    "F"
                   MOVE    "投薬"                      TO  W25-SENTAKU
               WHEN    "G"
                   MOVE    "注射"                      TO  W25-SENTAKU
               WHEN    "H"
                   MOVE    "リハビリテーション"        TO  W25-SENTAKU
               WHEN    "I"
                   MOVE    "精神料専門療法"            TO  W25-SENTAKU
               WHEN    "J"
                   MOVE    "処置"                      TO  W25-SENTAKU
               WHEN    "K"
                   MOVE    "手術"                      TO  W25-SENTAKU
               WHEN    "L"
                   MOVE    "麻酔"                      TO  W25-SENTAKU
               WHEN    "M"
                   MOVE    "放射線治療"                TO  W25-SENTAKU
               WHEN    "R"
                   MOVE    "老人、その他"              TO  W25-SENTAKU
               WHEN    OTHER
                   MOVE    SPACE                       TO  W25-SENTAKU
            END-EVALUATE
      *!
           MOVE    SPA-W25-SENTAKU     TO  W25-SENTAKU
           IF      W25-SENTAKU     NOT =   SPACE
      *!
               IF      SPA-W25-GMN-INPUT(3:1)  =   "/"
                   MOVE    W25-SENTAKU         TO  WRK-SENTAKU
                   MOVE    SPACE               TO  W25-SENTAKU
                   STRING  WRK-SENTAKU     DELIMITED  BY  SPACE
                           "（点数）"          DELIMITED  BY  SPACE
                                               INTO  W25-SENTAKU
                   END-STRING
               ELSE
      *!
      *!
               IF      SPA-W25-GMN-ALL     =   SPACE
                   MOVE    W25-SENTAKU         TO  WRK-SENTAKU
                   MOVE    SPACE               TO  W25-SENTAKU
                   STRING  WRK-SENTAKU     DELIMITED  BY  SPACE
                           "（自院）"          DELIMITED  BY  SPACE
                                               INTO  W25-SENTAKU
                   END-STRING
               END-IF
             END-IF
           END-IF
           .
       5001-SENTAKU-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査グループ名称編集処理
      *****************************************************************
       5010-GRPMEI-SET-SEC             SECTION.
      *
      *
           SET     TBL-IDX            TO  1
           SEARCH      TBL-GRP-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-GRP-MEI
                   WHEN    SPA-W25-NAI-KENSANO =
                                               TBL-GRP-KBN (TBL-IDX)
                       MOVE    TBL-GRP-RMEI  (TBL-IDX)
                                                TO  WRK-GRP-MEI
           END-SEARCH 
           MOVE    SPACE               TO  WRK-GRP-MEI-X
           IF      SPA-W25-GMN-ALL     =   SPACE
               STRING      WRK-GRP-MEI     DELIMITED  BY  SPACE
                           "（自院）"      DELIMITED  BY  SIZE
                           INTO            WRK-GRP-MEI-X
               END-STRING
           ELSE
               STRING      WRK-GRP-MEI     DELIMITED  BY  SPACE
                           "（全体）"      DELIMITED  BY  SIZE
                           INTO            WRK-GRP-MEI-X
               END-STRING
           END-IF
      *
           .
       5010-GRPMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-W25-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM1"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "INPUT"         TO  MCP-WIDGET
               WHEN    22
                   MOVE    "JIIN"          TO  MCP-WIDGET
               WHEN    23
                   MOVE    "USER"          TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-W25-SRYCD
                   PERFORM 210-BACK
      *        内服
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 230-NAIFUKU-SEC
      *        外用
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 240-GAIYOU-SEC
      *        注射
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 250-CHUUSYA-SEC
      *        器材
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 270-KIZAI-SEC
      *        診療行為
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 280-SINRYOU-SEC
      *        検査
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 290-KENSA-SEC
      *        全体
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 290-ALL-SEL-SEC
      *        前方
      *        WHEN    "CLICKED"       ALSO    "B09"
      ************ PERFORM 290-MAE-SEC
      *        部分
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 291-BUB-SEC
      *        後方
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 292-ATO-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-SEC
      *        前ページ
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 270-BACK-SEC
      *        確定
      *        WHEN    "CLICKED"       ALSO    "B12"
      ****         PERFORM 290-KAKUTEI-SEC
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        キー入力
               WHEN    "ACTIVATE"      ALSO    "INPUT"
                   PERFORM 41013-INPUT-CHK-SEC
      *        番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   MOVE    W25-ROW             TO  WRK-W25-ROW
                   PERFORM 41012-LISTSEL-SEC
      *        自院入力
               WHEN    "ACTIVATE"      ALSO    "JIIN"
                   PERFORM 41014-JIIN-CHK-SEC
      *        ユーザー
               WHEN    "ACTIVATE"      ALSO    "USER"
                   PERFORM 41014-USER-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力　処理
      *****************************************************************
       41013-INPUT-CHK-SEC                SECTION.
      *
           MOVE    21              TO  SPA-W25-GMN-CUR
      *
           MOVE    W25-INPUT           TO  SPA-W25-GMN-INPUT
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  WRK-MOJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-W25-GMN-INPUT   TO  KANACHK-MAE-INPUT
           MOVE    22                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-W25-GMN-INPUT
           IF      KANACHK-RC2         =   7
               MOVE    KANACHK-OUT-INPUT   TO  SPA-W25-GMN-INPUT
           END-IF
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX     TO  WRK-MOJI
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
           IF     (WRK-MOJI            =   ZERO  )  OR
                  (FLG-KANJI           =   1     AND
                   FLG-KANA            =   1     )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41013-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-KANJI           =   1
      *        全角入力（点数マスタより）
               MOVE    "J"                 TO  SPA-W25-CDSYUBETU
               MOVE    "1"                 TO  SPA-W25-TYPE
      *
               MOVE    SPACE               TO  SPA-W25-GMN-SYORI
      *!Ｚ０８からは器材とする
               IF      SPA-W25-MOTOPG      =   "Z08"
                   MOVE    "7"                 TO  SPA-W25-GMN-SYORI
               END-IF
           ELSE
      *        半角入力（入力セットマスタより）
               MOVE    "1"                 TO  SPA-W25-TYPE
      *
      *       全桁数字、”Ｐ、Ｓ”で始まるものは、コード種別を桁数で決定
               MOVE    SPACE               TO  SPA-W25-GMN-SYORI
               IF     (SPA-W25-GMN-INPUT(1:WRK-MOJI)   NUMERIC)  OR
                      (SPA-W25-GMN-INPUT(1:1) =   "P"  OR "S")
                   EVALUATE    WRK-MOJI
                       WHEN    4
                           MOVE    "4"         TO  SPA-W25-CDSYUBETU
                       WHEN    5
                           MOVE    "5"         TO  SPA-W25-CDSYUBETU
                       WHEN    3
                       WHEN    6
                           MOVE    "6"         TO  SPA-W25-CDSYUBETU
      ****             WHEN    1
      ***                  MOVE    "6"         TO  SPA-W25-CDSYUBETU
                       WHEN    OTHER
                           MOVE    "A"         TO  SPA-W25-CDSYUBETU
                   END-EVALUATE
               ELSE
                   MOVE    "A"             TO  SPA-W25-CDSYUBETU
               END-IF
      *----(01.00.01) LINE UPD END   ----------------------------------
           END-IF
           MOVE    1                   TO  SPA-W25-GMN-CUR
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
           PERFORM                     310-SPA-SET-SEC
      *T2
           IF      SPA-W25-GMN-MAX     =   ZERO
      *        全角で対象がない時、前の条件で検索
               IF     (SPA-W25-CDSYUBETU   =   "J" )  AND
                      (SPA-W25-OLD-SYORI   NOT =   SPACE )
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    SPA-W25-OLD-SYORI   TO  SPA-W25-GMN-SYORI
                   MOVE    SPACE               TO  SPA-W25-GMN-ALL
                   MOVE    ZERO                TO  SPA-W25-NEXT
                   MOVE    1                   TO  SPA-W25-PAGE
                   PERFORM                     310-SPA-SET-SEC
      *            検索対象がない時
                   IF      SPA-W25-GMN-MAX     =   ZERO
                       MOVE    1                 TO  FLG-KENSAKU01
                   END-IF
               END-IF
           END-IF
      *T2
      *
           .
       41013-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　処理（全体）
      *****************************************************************
       41011-SELNUM-ALLCHK-SEC                SECTION.
      *
      *
      ***  PERFORM VARYING     IDX-1   FROM   1   BY   1
      *            UNTIL      (IDX-1       >   1   )
               PERFORM 410110-GMN-SPA-SEC
      ***  END-PERFORM
      *
           MOVE    ZERO                TO  FLG-HENSYU
      *    PERFORM VARYING     IDX-1   FROM   1   BY   1
      *            UNTIL      (IDX-1       >   10  )  OR
      *                       (FLG-HENSYU  =   1  )  OR
      *                       (SPA-ERRCD   NOT =   SPACE )
               PERFORM 41011-SELNUM-CHK-SEC
      *    END-PERFORM
      *
           .
       41011-SELNUM-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択ＳＰＡ処理
      *****************************************************************
       410110-GMN-SPA-SEC                SECTION.
      *
           MOVE    W25-SELNUM1       TO  SPA-W25-GMN-SELNUM
      *    ＊ 入力時、全対象とする
           IF      W25-SELNUM1        =   "*  "  OR
                                           " * "  OR
                                           "  *" 
               GO      TO      410110-GMN-SPA-EXT
           END-IF
           IF      SPA-W25-GMN-SELNUM  =   SPACE
               MOVE    ZERO                TO  SPA-W25-NAI-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-W25-GMN-SELNUM  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SYOKBN         =   "1"   )
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    IDX-1           TO  SPA-W25-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-W25-NAI-SELNUM
                   MOVE    SPA-W25-NAI-SELNUM
                                           TO  WRK-NOZ
                   MOVE    WRK-NOZ         TO  SPA-W25-GMN-SELNUM
               END-IF
           END-IF
      *
           .
       410110-GMN-SPA-EXT.
           EXIT.
      *****************************************************************
      *    番号選択　処理
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-W25-GMN-CUR
      *
      *    ＊ 入力時、全対象とする
           IF      W25-SELNUM1        =   "*  "  OR
                                           " * "  OR
                                           "  *" 
               IF     (SPA-W25-GMN-SYORI   =   "9"   AND
                       SPA-W25-GMN-SYORI2  =   SPACE )
                   CONTINUE
               ELSE
      *            入力コードより編集の時、内服（Ｆ２）とする
                   IF      SPA-W25-TYPE        =   "1" 
                       PERFORM 230-NAIFUKU-SEC
                   ELSE
                       MOVE    "1"                 TO  SPA-W25-GMN-ALL
      *
                       INITIALIZE                  SPA-W25-NAI-SELNUM-G
                       INITIALIZE                  SPA-W25-GMN-SELNUM-G
                       MOVE    SPACE               TO  SPA-W25-SRYCD
      *                ＳＰＡセット
                       MOVE    ZERO                TO  SPA-W25-NEXT
                       MOVE    1                   TO  SPA-W25-PAGE
                       PERFORM                     310-SPA-SET-SEC
                   END-IF
                   MOVE    1               TO  FLG-HENSYU
                   GO      TO      41011-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
      *
           MOVE    W25-SELNUM1        TO  SPA-W25-GMN-SELNUM
      *    選択番号入力
           INITIALIZE                      ORCSNUMAREA
           MOVE    W25-SELNUM1        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    IDX-1               TO  SPA-W25-GMN-CUR
               GO      TO      41011-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-W25-NAI-SELNUM
           END-IF
      *    １件のみとき、１とする
           IF     (SPA-W25-GMN-MAX     =   1  )  AND
                  (SPA-W25-NAI-SELNUM  =  ZERO)
               MOVE    1               TO  SPA-W25-NAI-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W25-GMN-MAX
               IF      SPA-W25-NAI-NO (IDX)    =   SPA-W25-NAI-SELNUM
                   MOVE    IDX                 TO  WRK-SELNO
                   MOVE    SPA-W25-GMN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    検査チェック
           IF      SPA-W25-GMN-SYORI   =   "9"
               IF      SPA-W25-GMN-SYORI2  =   SPACE
                   PERFORM 41013-KENSA-GRP-SEC
                   MOVE    1               TO  FLG-HENSYU
               ELSE
      *********    PERFORM 41014-KENSA-SEL-SEC
                   PERFORM 41012-SRYCD-SEL-SEC
               END-IF
           ELSE
      ******   PERFORM 41014-KENSA-SEL-SEC
               PERFORM 41012-SRYCD-SEL-SEC
      *        診療行為の時１件以上入力可
      *********IF      SPA-W25-GMN-SYORI   =   "8"
      *            PERFORM 41014-KENSA-SEL-SEC
      *        ELSE
      *            PERFORM 41012-SRYCD-SEL-SEC
      ******** END-IF
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード選択処理
      *****************************************************************
       41012-SRYCD-SEL-SEC       SECTION.
      *
           MOVE    1                   TO  SPA-W25-GMN-CUR
      *    IF     (SPA-W25-NAI-SELNUM  NOT =   ZERO )
      *        MOVE    "0003"          TO  SPA-ERRCD
      *    ELSE
               PERFORM 41011-SELNO-SEC
      *    END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *         戻るへ
                PERFORM 210-BACK
           END-IF
           .
       41012-SRYCD-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査グループ選択処理
      *****************************************************************
       41013-KENSA-GRP-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-W25-GMN-CUR
      *
      *    IF     (SPA-W25-NAI-SELNUM
      *                                NOT =   ZERO ) AND
      *           (IDX-1               NOT =   1     ) 
      *        MOVE    "0003"          TO  SPA-ERRCD
      *        GO      TO      41013-KENSA-GRP-EXT
      *    ELSE
           IF      SPA-W25-NAI-SELNUM      =   ZERO
               GO      TO      41013-KENSA-GRP-EXT
           END-IF
      *    END-IF
      *
           IF      WRK-SELNO           =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41013-KENSA-GRP-EXT
           END-IF
           MOVE    SPA-W25-GMN-NO-X(WRK-SELNO)  TO  SPA-W25-NAI-KENSA
           EVALUATE    SPA-W25-NAI-KENSANO
               WHEN    31
               WHEN    41
      *            包括
                   MOVE    "1"                 TO  SPA-W25-NAI-HOUKATU
               WHEN    32
               WHEN    42
      *            包括外
                   MOVE    "2"                 TO  SPA-W25-NAI-HOUKATU
               WHEN    OTHER
      *            その他
                   MOVE    SPACE               TO  SPA-W25-NAI-HOUKATU
           END-EVALUATE
      *
           PERFORM 5010-GRPMEI-SET-SEC
           IF      WRK-GRP-MEI         =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    "1"                 TO  SPA-W25-GMN-SYORI2
               MOVE    SPACE               TO  SPA-W25-TYPE
               INITIALIZE                      SPA-W25-GMN-SELNUM-G
               INITIALIZE                      SPA-W25-NAI-SELNUM-G
               MOVE    SPACE               TO  SPA-W25-SRYCD
      *
               MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *        ＳＰＡセット
               MOVE    ZERO                TO  SPA-W25-NEXT
               MOVE    1                   TO  SPA-W25-PAGE
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       41013-KENSA-GRP-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査選択処理
      *****************************************************************
       41014-KENSA-SEL-SEC             SECTION.
      *
      *    MOVE    ZERO                TO  FLG-SYURYOU
      *    IF      SPA-W25-NAI-SELNUM      =   ZERO
      *        IF     (IDX-1               =   IDX-N)
      *            IF      IDX-1           >   1
      *                MOVE    1               TO  FLG-SYURYOU
      *            END-IF
      *        END-IF
      *        PERFORM 410141-TBL-SAIHEN-SEC
      *    ELSE
      *        PERFORM 41011-SELNO-SEC
      *        IF      SPA-ERRCD           =  SPACE
      *            IF     (IDX-1           =   10   )  OR
      *                   (IDX-1           =   SPA-W25-GMN-MAX)
      *               OR  (SPA-W25-GMN-INPUT(1:1) =   "." )
      *                MOVE    1               TO  FLG-SYURYOU
      *            ELSE
      *                COMPUTE SPA-W25-GMN-CUR     =   IDX-1   +   1
      *            END-IF
      *        ELSE
      *            MOVE    IDX-1               TO  SPA-W25-GMN-CUR
      *        END-IF
      *    END-IF
      *
      *    IF      SPA-ERRCD           =   SPACE
      *        IF      FLG-SYURYOU         =   1
      *             戻るへ
      *             PERFORM 210-BACK
      *        END-IF
      *    END-IF
           .
       41014-KENSA-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択空白削除処理
      *****************************************************************
       410141-TBL-SAIHEN-SEC       SECTION.
      *
      *    MOVE    ZERO                TO  IDX1
      *    MOVE    SPA-W25-NAI-SELNUM-G    TO  WRK-NAI-SELNUM-G
      *    MOVE    SPA-W25-GMN-SELNUM-G    TO  WRK-GMN-SELNUM-G
      *    MOVE    SPA-W25-SRYCD       TO  WRK-W25-SRYCD
      *
      *    INITIALIZE                  SPA-W25-GMN-SELNUM-G
      *    INITIALIZE                  SPA-W25-NAI-SELNUM-G
      *    INITIALIZE                  SPA-W25-SRYCD-G
      *
      *    PERFORM VARYING     IDX2    FROM    1   BY  1
      *            UNTIL       IDX2    >   10
      *        IF      WRK-NAI-SELNUM (IDX2)   =   ZERO
      *            CONTINUE
      *        ELSE
      *            ADD     1               TO  IDX1
      *            MOVE    WRK-GMN-SELNUM (IDX2)
      *                                    TO  SPA-W25-GMN-SELNUM(IDX1)
      *            MOVE    WRK-NAI-SELNUM (IDX2)
      *                                    TO  SPA-W25-NAI-SELNUM(IDX1)
      *            MOVE    WRK-W25-SRYCD (IDX2)
      *                                    TO  SPA-W25-SRYCD   (IDX1)
      *            MOVE    IDX1            TO  IDX
      *        END-IF
      *    END-PERFORM
      *
      *    COMPUTE SPA-W25-GMN-CUR     =   IDX1    +   1
      *
           .
       410141-TBL-SAIHEN-EXT.
           EXIT.
      *
      *選択反転無しの選択
      *****************************************************************
      *    行選択処理
      *****************************************************************
      *41012-LISTSEL-SEC       SECTION.
      *
      *    行選択
      *    MOVE    ZERO                TO  WRK-SELNO
      *    MOVE    ZERO                TO  WRK-GMN-SELNO
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   200
      *        IF      W25-SELECT (IDX)    =   "T"
      *            MOVE    SPA-W25-GMN-NO(IDX)  TO  WRK-GMN-SELNO
      *            MOVE    IDX                  TO  WRK-SELNO
      *            MOVE    200             TO  IDX
      *        END-IF
      *    END-PERFORM
      *
      *    IF      WRK-SELNO           <   01  OR
      *                                >   SPA-W25-GMN-MAX
      *        MOVE    "0002"          TO  SPA-ERRCD
      *        GO      TO              41012-LISTSEL-EXT
      *    END-IF
      *
      *    MOVE    ZERO                TO  IDX-1
      *    PERFORM VARYING     IDX2    FROM    1   BY  1
      *            UNTIL       IDX2    >   10
      *        IF      SPA-W25-NAI-SELNUM(IDX2)    =   ZERO
      *            MOVE    IDX2                TO  IDX-1
      *            MOVE    IDX-1               TO  IDX-N
      *            MOVE    WRK-SELNO           TO  SPA-W25-NAI-SELNUM
      *                                                         (IDX-1)
      *            MOVE    WRK-GMN-SELNO       TO  WRK-NOZ
      *            MOVE    WRK-NOZ             TO  SPA-W25-GMN-SELNUM
      *                                                         (IDX-1)
      *            MOVE    10                  TO  IDX2
      *        ELSE
      *            IF      SPA-W25-NAI-SELNUM(IDX2)    =   WRK-SELNO
      *                MOVE    IDX2            TO  IDX-1
      *                MOVE    SPACE           TO  SPA-W25-GMN-SELNUM
      *                                                         (IDX-1)
      *                MOVE    ZERO            TO  SPA-W25-NAI-SELNUM
      *                                                         (IDX-1)
      *                MOVE    10               TO  IDX2
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *    IF      IDX-1               =  ZERO
      *        MOVE    "0002"          TO  SPA-ERRCD
      *        GO      TO              41012-LISTSEL-EXT
      *    END-IF
      *
      *    IF      SPA-W25-GMN-SELNUM(IDX-1)   =   SPACE
      *        PERFORM 410141-TBL-SAIHEN-SEC
      *    ELSE
      *    検査チェック
      *    IF      SPA-W25-GMN-SYORI   =   "9"
      *        IF      SPA-W25-GMN-SYORI2  =   SPACE
      *            PERFORM 41013-KENSA-GRP-SEC
      *        ELSE
      *            PERFORM 41014-KENSA-SEL-SEC
      *        END-IF
      *    ELSE
      *        PERFORM 41014-KENSA-SEL-SEC
      *!!!     PERFORM 41012-SRYCD-SEL-SEC
      *    END-IF
      *    END-IF
      *    .
      *41012-LISTSEL-EXT.
      *****EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
      *    行選択
           INITIALIZE                      WRK-LISET-SEL-G
           MOVE    ZERO                TO  WRK-SELNO
           MOVE    ZERO                TO  WRK-GMN-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      W25-SELECT (IDX)    =   "T"
                   MOVE    SPA-W25-GMN-NO(IDX)  TO  WRK-GMN-SELNO
                   MOVE    IDX                  TO  WRK-SELNO
                   PERFORM 410121-LISTSEL-HEN-SEC 
               END-IF
           END-PERFORM
      *
      *    MOVE    ZERO                TO  IDX-1
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   10
      *        IF      SPA-W25-GMN-SELNUM(IDX)     =   SPACE
      *            CONTINUE
      *        ELSE
      *            削除
      *            IF      WRK-SEL (IDX)   =   SPACE
      *                MOVE    SPACE       TO  SPA-W25-GMN-SELNUM(IDX)
      *                MOVE    ZERO        TO  SPA-W25-NAI-SELNUM(IDX)
      *            END-IF
      *            今回追加
      *            IF      WRK-SEL (IDX)   =   "2"
      *                MOVE    IDX                 TO  IDX-1
      *                MOVE    IDX-1               TO  IDX-N
      *                MOVE    SPA-W25-NAI-SELNUM(IDX)
      *                                            TO  WRK-SELNO
      *                MOVE    SPA-W25-GMN-SELNUM(IDX)
      *                                            TO  WRK-GMN-SELNO
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *    削除クリア
      *    PERFORM 410141-TBL-SAIHEN-SEC
      *    今回選択なし
           IF      SPA-W25-NAI-SELNUM  =   ZERO
               CONTINUE
           ELSE
      *         検査チェック
               IF      SPA-W25-GMN-SYORI   =   "9"
                   IF      SPA-W25-GMN-SYORI2  =   SPACE
                       PERFORM 41013-KENSA-GRP-SEC
                   ELSE
      **************   PERFORM 41014-KENSA-SEL-SEC
                       PERFORM 41012-SRYCD-SEL-SEC
                   END-IF
               ELSE
      ************ PERFORM 41014-KENSA-SEL-SEC
                   PERFORM 41012-SRYCD-SEL-SEC
               END-IF
           END-IF
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択編集処理
      *****************************************************************
       410121-LISTSEL-HEN-SEC      SECTION.
      *
           IF      WRK-SELNO           <   01  OR
                                       >   SPA-W25-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-SELNO           TO  SPA-W25-NAI-SELNUM
               MOVE    WRK-GMN-SELNO       TO  WRK-NOZ
               MOVE    WRK-NOZ             TO  SPA-W25-GMN-SELNUM
           END-IF
      *
      *    MOVE    ZERO                TO  IDX-1
      *    PERFORM VARYING     IDX2    FROM    1   BY  1
      *            UNTIL      (IDX2    >   10 )
      *                OR     (SPA-ERRCD   NOT =   SPACE )
      *        IF      SPA-W25-NAI-SELNUM(IDX2)    =   ZERO
      *            MOVE    IDX2                TO  IDX-1
      *            MOVE    IDX-1               TO  IDX-N
      *            MOVE    WRK-SELNO           TO  SPA-W25-NAI-SELNUM
      *                                                         (IDX-1)
      *            MOVE    WRK-GMN-SELNO       TO  WRK-NOZ
      *            MOVE    WRK-NOZ             TO  SPA-W25-GMN-SELNUM
      *                                                         (IDX-1)
      *            MOVE    "2"                 TO  WRK-SEL (IDX-1)
      *            MOVE    10                  TO  IDX2
      *        ELSE
      *            IF      SPA-W25-NAI-SELNUM(IDX2)    =   WRK-SELNO
      *                MOVE    IDX2            TO  IDX-1
      *                MOVE    "1"             TO  WRK-SEL (IDX-1)
      *                MOVE    10              TO  IDX2
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *    IF      IDX-1               =  ZERO
      *        MOVE    "0002"          TO  SPA-ERRCD
      *    END-IF
           .
       410121-LISTSEL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択編集処理
      *****************************************************************
       41011-SELNO-SEC       SECTION.
      *
           IF      WRK-SELNO           <   01  OR
                                       >   SPA-W25-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               GO      TO              41011-SELNO-EXT
           END-IF
      *
           MOVE    WRK-SELNO           TO  IDX
      *
      *    セット・診療種別はエラー
           IF      SPA-W25-GMN-SRYCD (IDX)(1:1)    =   "S"
                                                   OR  "P"
               MOVE    "0008"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-W25-GMN-SRYCD (IDX)
                                       TO  SPA-W25-SRYCD
           END-IF
      *
           .
       41011-SELNO-EXT.
           EXIT.
      *****************************************************************
      *    内服薬剤選択処理
      *****************************************************************
       230-NAIFUKU-SEC                   SECTION.
      *
           MOVE    "3"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           IF      SPA-W25-GMN-INPUT   =   SPACE
               INITIALIZE                  SPA-W25-GMN-TBL
               MOVE    ZERO                TO  SPA-W25-GMN-MAX
               MOVE    21                  TO  SPA-W25-GMN-CUR
           ELSE
      *    ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           END-IF
      *
          .
       230-NAIFUKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外用薬選択処理
      *****************************************************************
       240-GAIYOU-SEC                   SECTION.
      *
           MOVE    "4"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           IF      SPA-W25-GMN-INPUT   =   SPACE
               INITIALIZE                  SPA-W25-GMN-TBL
               MOVE    ZERO                TO  SPA-W25-GMN-MAX
               MOVE    21                  TO  SPA-W25-GMN-CUR
           ELSE
      *    ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           END-IF
      *
          .
       240-GAIYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射薬選択処理
      *****************************************************************
       250-CHUUSYA-SEC                   SECTION.
      *
           MOVE    "5"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           IF      SPA-W25-GMN-INPUT   =   SPACE
               INITIALIZE                  SPA-W25-GMN-TBL
               MOVE    ZERO                TO  SPA-W25-GMN-MAX
               MOVE    21                  TO  SPA-W25-GMN-CUR
           ELSE
               PERFORM                     310-SPA-SET-SEC
           END-IF
      *
          .
       250-CHUUSYA-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材選択処理
      *****************************************************************
       270-KIZAI-SEC                   SECTION.
      *
           MOVE    "7"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
      *
           IF     SPA-W25-GMN-INPUT    =   SPACE
               INITIALIZE                  SPA-W25-GMN-TBL
               MOVE    ZERO                TO  SPA-W25-GMN-MAX
               MOVE    21                  TO  SPA-W25-GMN-CUR
      *TEST(全体）
      *        自院・器材
               MOVE    "U"                  TO  SPA-W25-GMN-SYORI
               MOVE    "JK"                 TO  SPA-W25-GMN-SYORI3
               MOVE    SPACE                TO  SPA-ERRCD
               PERFORM 3102-TENSU-SET-SEC
      *TEST
           ELSE
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       270-KIZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為選択処理
      *****************************************************************
       280-SINRYOU-SEC                   SECTION.
      *
           MOVE    "8"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPA-W25-GMN-SYORI   TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           IF      SPA-W25-GMN-INPUT   =   SPACE
               INITIALIZE                  SPA-W25-GMN-TBL
               MOVE    ZERO                TO  SPA-W25-GMN-MAX
               MOVE    21                  TO  SPA-W25-GMN-CUR
           ELSE
               PERFORM                     310-SPA-SET-SEC
           END-IF
      *
          .
       280-SINRYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院入力分処理
      *****************************************************************
       41014-JIIN-CHK-SEC              SECTION.
      *
           MOVE    22                  TO  SPA-W25-GMN-CUR
      *
           MOVE    W25-JIIN            TO  SPA-W25-JIIN-G
      *
           EVALUATE    SPA-W25-JIIN
               WHEN    "1"
                   MOVE    "J1"            TO  SPA-W25-GMN-SYORI3
               WHEN    "2"
                   MOVE    "J2"            TO  SPA-W25-GMN-SYORI3
               WHEN    "3"
                   MOVE    "J3"            TO  SPA-W25-GMN-SYORI3
               WHEN    "4"
                   MOVE    "J4"            TO  SPA-W25-GMN-SYORI3
               WHEN    "5"
                   MOVE    "J5"            TO  SPA-W25-GMN-SYORI3
               WHEN    "6"
                   MOVE    "J6"            TO  SPA-W25-GMN-SYORI3
               WHEN    "7"
                   MOVE    "J7"            TO  SPA-W25-GMN-SYORI3
               WHEN    "8"
                   MOVE    "J8"            TO  SPA-W25-GMN-SYORI3
               WHEN    "9"
                   MOVE    "J9"            TO  SPA-W25-GMN-SYORI3
               WHEN    OTHER
                   MOVE    "0010"          TO  SPA-ERRCD
                   GO      TO      41014-JIIN-CHK-EXT
           END-EVALUATE
      *
           MOVE    "U"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPACE               TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
           MOVE    SPACE               TO  SPA-W25-GMN-INPUT
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       41014-JIIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー登録分処理
      *****************************************************************
       41014-USER-CHK-SEC              SECTION.
      *
           MOVE    23                  TO  SPA-W25-GMN-CUR
      *
           MOVE    W25-USER            TO  SPA-W25-USER-G
      *
           EVALUATE    SPA-W25-USER
               WHEN    "1"
                   MOVE    "U1"            TO  SPA-W25-GMN-SYORI3
               WHEN    "2"
                   MOVE    "U2"            TO  SPA-W25-GMN-SYORI3
               WHEN    "3"
                   MOVE    "U3"            TO  SPA-W25-GMN-SYORI3
               WHEN    "4"
                   MOVE    "U4"            TO  SPA-W25-GMN-SYORI3
               WHEN    "5"
                   MOVE    "U5"            TO  SPA-W25-GMN-SYORI3
               WHEN    "6"
                   MOVE    "U6"            TO  SPA-W25-GMN-SYORI3
               WHEN    OTHER
                   MOVE    "0011"          TO  SPA-ERRCD
                   GO      TO      41014-USER-CHK-EXT
           END-EVALUATE
      *
           MOVE    "U"                 TO  SPA-W25-GMN-SYORI
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPACE               TO  SPA-W25-OLD-SYORI
      *
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    SPACE               TO  SPA-W25-GMN-ALL
      *
           MOVE    SPACE               TO  SPA-W25-GMN-INPUT
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       41014-USER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査選択処理
      *****************************************************************
       290-KENSA-SEC                   SECTION.
      *
           MOVE    "9"                 TO  SPA-W25-GMN-SYORI
           MOVE    SPACE               TO  SPA-W25-GMN-SYORI2
           MOVE    SPACE               TO  SPA-W25-GMN-SYORI3
           MOVE    "2"                 TO  SPA-W25-TYPE
           MOVE    SPACE               TO  SPA-W25-OLD-SYORI
      *
      *    ＳＰＡセット
           INITIALIZE                  SPA-W25-GMN-TBL
           MOVE    ZERO                TO  SPA-W25-GMN-MAX
           INITIALIZE                      SPA-W25-NAI-SELNUM-G
           INITIALIZE                      SPA-W25-GMN-SELNUM-G
           MOVE    SPACE               TO  SPA-W25-SRYCD
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
      *
           MOVE    SPACE               TO  SPA-W25-GMN-INPUT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   25
      *
               SET     TBL-IDX                 TO  IDX
               MOVE    TBL-GRP-KBN (TBL-IDX)   TO  SPA-W25-GMN-NO-X
                                                               (IDX)
               MOVE    TBL-GRP-MEI (TBL-IDX)   TO  SPA-W25-GMN-NAME
                                                               (IDX)
           END-PERFORM
      *
           MOVE    25                  TO  SPA-W25-GMN-MAX
      *
           MOVE    1                   TO  SPA-W25-GMN-CUR
      *
          .
       290-KENSAU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体一致処理
      *****************************************************************
       290-ALL-SEL-SEC                   SECTION.
      *
           IF     (SPA-W25-GMN-SYORI   =   "9"   AND
                   SPA-W25-GMN-SYORI2  =   SPACE )
               CONTINUE
           ELSE
      *        入力コードより編集の時、内服（Ｆ２）とする
               IF      SPA-W25-TYPE        =   "1"
                   IF      SPA-W25-GMN-SYORI   =   "3"  OR  "4"  OR  "5"
                       CONTINUE
                   ELSE
      *                内服
                       MOVE    "3"             TO  SPA-W25-GMN-SYORI
                   END-IF
                   MOVE    "2"                 TO  SPA-W25-TYPE
      *
                   INITIALIZE                      SPA-W25-NAI-SELNUM-G
                   INITIALIZE                      SPA-W25-GMN-SELNUM-G
                   MOVE    SPACE               TO  SPA-W25-SRYCD
                   MOVE    SPACE               TO  SPA-W25-GMN-ALL
                   MOVE    ZERO                TO  SPA-W25-NEXT
                   MOVE    1                   TO  SPA-W25-PAGE
      *            ＳＰＡセット
                   PERFORM                     310-SPA-SET-SEC
               ELSE
                   MOVE    "1"                 TO  SPA-W25-GMN-ALL
      *
                   INITIALIZE                  SPA-W25-NAI-SELNUM-G
                   INITIALIZE                  SPA-W25-GMN-SELNUM-G
                   MOVE    SPACE               TO  SPA-W25-SRYCD
                   MOVE    ZERO                TO  SPA-W25-NEXT
                   MOVE    1                   TO  SPA-W25-PAGE
      *TEST
                   IF      SPA-W25-GMN-SYORI3  =   "JK"
                       MOVE    "UK"            TO  SPA-W25-GMN-SYORI3
                   END-IF
      *TEST
      *            ＳＰＡセット
                   PERFORM                     310-SPA-SET-SEC
               END-IF
           END-IF
          .
       290-ALL-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ処理
      *****************************************************************
       270-NEXT-SEC                   SECTION.
      *
           IF      SPA-W25-NEXT        >   ZERO
               ADD     1                   TO  SPA-W25-PAGE
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           ELSE
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *
          .
       270-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前ページ処理
      *****************************************************************
       270-BACK-SEC                   SECTION.
      *
           IF      SPA-W25-PAGE        =   1
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-W25-PAGE    =   SPA-W25-PAGE  -   1
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
          .
       270-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前方一致処理
      *****************************************************************
       290-MAE-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-W25-CDSYUBETU   NOT =   "J"
               GO      TO      290-MAE-EXT
           END-IF
      *    検査以外の時のみ
           IF      SPA-W25-GMN-SYORI   =   "9"
               GO      TO      290-MAE-EXT
           END-IF
      *    区分番号以外の時のみ
           IF      SPA-W25-GMN-INPUT(1:1)  =   "*"
                                           OR  "/"
               GO      TO      290-MAE-EXT
           END-IF
      *
           INSPECT SPA-W25-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-W25-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-W25-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    WRK-INPUTCD         TO  SPA-W25-GMN-INPUT
      *
           MOVE    SPACE               TO  WRK-KEY-INPUT
           STRING  WRK-INPUTCD         DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    WRK-KEY-INPUT
           END-STRING
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       290-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    部分一致処理
      *****************************************************************
       291-BUB-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-W25-CDSYUBETU   NOT =   "J"
               GO      TO      291-BUB-EXT
           END-IF
      *    検査以外の時のみ
           IF      SPA-W25-GMN-SYORI   =   "9"
               GO      TO      291-BUB-EXT
           END-IF
      *    区分番号以外の時のみ
           IF      SPA-W25-GMN-INPUT(1:1)  =   "*"
                                           OR  "/"
               GO      TO      291-BUB-EXT
           END-IF
      *
           INSPECT SPA-W25-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-W25-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-W25-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-W25-GMN-INPUT
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                   "＊"                DELIMITED  BY  SIZE
                                       INTO    SPA-W25-GMN-INPUT
           END-STRING
      *
           MOVE    SPACE               TO  WRK-KEY-INPUT
           STRING  "%"                 DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    WRK-KEY-INPUT
           END-STRING
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       291-BUB-EXT.
           EXIT.
      *****************************************************************
      *    後分一致処理
      *****************************************************************
       292-ATO-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-W25-CDSYUBETU   NOT =   "J"
               GO      TO      292-ATO-EXT
           END-IF
      *    検査以外の時のみ
           IF      SPA-W25-GMN-SYORI   =   "9"
               GO      TO      292-ATO-EXT
           END-IF
      *    区分番号以外の時のみ
           IF      SPA-W25-GMN-INPUT(1:1)  =   "*"
                                           OR  "/"
               GO      TO      292-ATO-EXT
           END-IF
      *
           INSPECT SPA-W25-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-W25-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-W25-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-W25-GMN-INPUT
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                                       INTO    SPA-W25-GMN-INPUT
           END-STRING
      *
           MOVE    SPACE               TO  WRK-KEY-INPUT
           STRING  "%"                 DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                                       INTO    WRK-KEY-INPUT
           END-STRING
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-W25-NEXT
           MOVE    1                   TO  SPA-W25-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       292-ATO-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-W25-MOTOPG      TO  SPA-SAKIPG
           MOVE    "W25"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       290-KAKUTEI-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
           MOVE    ZERO                TO  IDX-N
      *
           PERFORM 41011-SELNUM-ALLCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      290-KAKUTEI-EXT
           END-IF
      *
      *    戻るへ
           PERFORM 210-BACK
      *
           .
       290-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W25"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                   "検査以外の時、最初の選択番号位置のみの入力です"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   PERFORM 5101-ERRMSG-HEN-SEC
      *            MOVE
      *            "対象データがありません。"
      **                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "セットコードは選択できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE
                   "対象データが２００件以上あります。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
                   "次ページはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE
                   "前ページはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                   "自院採用区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "ユーザー登録区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "開始点数≦終了点数で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "点数は数字９桁で入力して下さい。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W25"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    対象なしエラーメッセージ編集処理
      *****************************************************************
       5101-ERRMSG-HEN-SEC         SECTION.
      *
           EVALUATE    SPA-W25-GMN-SYORI
      *        内服
               WHEN    "3"
                   IF      FLG-KENSAKU01       =   ZERO
                       STRING  "内服薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   ELSE
                       STRING  "入力ＣＤ・内服薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *        外用
               WHEN    "4"
                   IF      FLG-KENSAKU01       =   ZERO
                       STRING  "外用薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   ELSE
                       STRING  "入力ＣＤ・外用薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *        注射
               WHEN    "5"
                   IF      FLG-KENSAKU01       =   ZERO
                       STRING  "注射薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   ELSE
                       STRING  "入力ＣＤ・注射薬名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *        器材
               WHEN    "7"
                   IF      FLG-KENSAKU01       =   ZERO
                       STRING  "器材名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   ELSE
                       STRING  "入力ＣＤ・器材名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *        診療行為
               WHEN    "8"
                   IF      FLG-KENSAKU01       =   ZERO
                       STRING  "診療行為名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   ELSE
                       STRING  "入力ＣＤ・診療行為名称を検索しました。"
                               "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *        検査
               WHEN    "9"
                   STRING W25-SENTAKU          DELIMITED  BY  SPACE
                          "の対象はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *        セット
               WHEN    "P"
                   MOVE    "セットコードの対象がありません"
                                               TO  SPA-ERRMSG
      *        ユーザー登録
               WHEN    "U"
                   EVALUATE    SPA-W25-GMN-SYORI3
                       WHEN    "J1"
                           MOVE    "自院採用の内服薬はありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J2"
                           MOVE    "自院採用の外用薬はありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J3"
                           MOVE    "自院採用の注射薬はありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J4"
                           MOVE    "自院採用の処置コードはありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J5"
                           MOVE    "自院採用の診察コードはありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J6"
                           MOVE    "自院採用の注射コードはありません"
                                               TO  SPA-ERRMSG
                       WHEN    "J7"
                           STRING   "自院採用の手術・麻酔コードは"
                                    "ありません"     DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                           END-STRING
                       WHEN    "J8"
                           STRING   "自院採用の画像診断コードは"
                                    "ありません"     DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                           END-STRING
                       WHEN    "J9"
                           MOVE    "自院採用のその他コードはありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U1"
                           MOVE    "用法コードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U2"
                           MOVE    "撮影部位コードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U3"
                           MOVE    "ユーザー登録コードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U4"
                           MOVE    "コメントコードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U5"
                           MOVE    "労災コードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "U6"
                           MOVE
                           "システム予約コードコードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "JK"
                           MOVE    "自院採用の器材コードがありません"
                                               TO  SPA-ERRMSG
                       WHEN    "UK"
                           MOVE    "器材コードがありません"
                                               TO  SPA-ERRMSG
                   END-EVALUATE
      *        その他
               WHEN    OTHER
                   EVALUATE     SPA-W25-GMN-INPUT(1:1)
                   WHEN    "*"
                       IF      SPA-W25-SENTAKU     =   SPACE
                           STRING  "この区分番号の診療行為コードは"
                                   "ありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                           END-STRING
                       ELSE
                           IF      SPA-W25-GMN-INPUT(3:1)  =   "/"
                           STRING  "この点数の"
                                   SPA-W25-SENTAKU DELIMITED  BY  SPACE
                                   "コードはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                           END-STRING
                           ELSE
                           STRING  "この区分番号の"
                                   SPA-W25-SENTAKU DELIMITED  BY  SPACE
                                   "コードはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                           END-STRING
                           END-IF
                       END-IF
                   WHEN    "/"
                       MOVE    "この点数の検査はありません。"
                                                   TO  SPA-ERRMSG
                   WHEN    OTHER
                       STRING  "対象の入力コードはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                       END-STRING
               END-EVALUATE
           END-EVALUATE
      *    漢字入力で、入力コードで検索
           IF     (SPA-W25-TYPE        =   "1" )  AND
                  (SPA-W25-CDSYUBETU   =   "J" )
               MOVE    SPACE                   TO  SPA-ERRMSG
               STRING  "入力コードに"
                       "対象データはありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
               END-STRING
           END-IF
           .
       5101-ERRMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索
      *****************************************************************
       910-TENSU-SELECT-SEC         SECTION.
      *
           MOVE    SPA-W25-SYSYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-W25-SYSYMD      TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM                     910-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター主キー読込処理
      *****************************************************************
       9110-TENSU-KEYREAD-SEC         SECTION.
      *
           MOVE    SPA-W25-SYSYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-W25-SYSYMD      TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9110-TENSU-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       975-INPSET-READ-EXT.
           EXIT.
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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

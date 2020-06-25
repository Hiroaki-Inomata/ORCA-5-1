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
       PROGRAM-ID.             ORCGK062.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : リハビリ開始コメント登録（Ｋ０６２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/11/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  11/08/XX  リハビリコメント終了日対応
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    算定履歴更新パラメタ
           COPY    "K06SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-NEXT            PIC 9(01).
           03  FLG-SRY-OK          PIC 9(01).
           03  FLG-CHKCOM          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDXS                PIC 9(02).
      *
           03  IDX-ERR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-YMD             PIC X(09).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    保険適用期間
           03  WRK-ST-SRYYMD.
               05  WRK-STYY            PIC 9(04).
               05  WRK-STMM            PIC 9(02).
               05  WRK-STDD            PIC 9(02).
           03  WRK-ED-SRYYMD.
               05  WRK-EDYY            PIC 9(04).
               05  WRK-EDMM            PIC 9(02).
               05  WRK-EDDD            PIC 9(02).
      *
           03  WRK-KIDMSG          PIC X(80).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-ZZ                 PIC ZZ.
           03  WRK-Z9                 PIC Z9.
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM         PIC X(06).
               05  WRK-SRYDD         PIC X(02).
           03  WRK-DD                PIC 9(02).
      *
           03  WRK-AMARI              PIC 9(03).
           03  WRK-SHO                PIC 9(03).
      *
      *    診療コード
           03  WRK-SRYCD              PIC X(09).
      *    保険組合せ名称
           03  WRK-HKNCOMBI           PIC X(04).
           03  WRK-HKNCOMBIMEI        PIC X(100).
           03  WRK-HKNKBN-MEI         PIC X(02).
           03  WRK-COMB-TEKSTYMD      PIC X(08).
           03  WRK-COMB-TEKEDYMD      PIC X(08).
      *
           03  WRK-STR-YMD.
               05  WRK-STR-YM          PIC X(06).
               05  WRK-STR-DD          PIC X(02).
      *
           03  WRK-ENDYMD              PIC X(08).
           03  WRK-ENDYMDG             PIC X(11).
      *入力タイプ
           03  WRK-INTYPE             PIC X(01).
      *
           03  WRK-MIDASI             PIC X(160).
      *H28.11
      *    空白削除
           03  TBL-K062-COMMNT-AREA.
               07  TBL-K062-COMMENT-TBL    OCCURS   50.
                   09  TBL-K062-COMMENT        PIC X(80).
                   09  TBL-K062-TENDYMDG       PIC X(10).
                   09  TBL-K062-TENDYMD        PIC X(08).
                   09  TBL-K062-INFLG          PIC X(01).
      *
           03  WRK-YUKOSTYMD           PIC X(08).
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
      *算定履歴作成
       01  TBL-IKOU-SRYCD-G.
      *    心大血管疾患リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800111".
      *    脳血管疾患リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800121".
      *    運動器リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800131".
      *    呼吸器リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800141".
      *    摂食機能リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800151".
      *    難病患者リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800161".
      *    障害児リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800171".
      *    がん患者リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800181".
      *    廃用症候群リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800191".
      *    心大血管疾患　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800211".
      *    脳血管疾患　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800221".
      *    運動器　早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800231".
      *    呼吸器早期・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800241".
      *    廃用症候群・初期リハビリ開始日
           03  FILLER          PIC X(09)   VALUE  "099800291".
      *
       01  TBL-IKOU-SRYCD-GR   REDEFINES   TBL-IKOU-SRYCD-G.
           03  TBL-IKOU-SRYCD          PIC X(09)
                                       OCCURS    14.
       01  TBL-IKOU-MAX                PIC 9(02)   VALUE   14.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    算定履歴付加情報
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
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
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K06
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K06         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
      ******   MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "K062   "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        削除確認
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
      *                削除処理
                       PERFORM 4401-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-K062-CUR
                   END-IF
      *        登録確認
               WHEN    "0102"
                   IF      SPA-KID1-FLG        =   "OK"
      *                登録処理
                       PERFORM 4501-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-K062-CUR
                   END-IF
      *        終了日付設定
               WHEN    "0110"
                   IF      SPA-KID1-FLG        =   "OK"
      *                終了日付設定
                       PERFORM 30012-ENDYMD-AUTO-SEC
                   ELSE
                       MOVE    2               TO  SPA-K062-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KID1-SET-EXT.
           EXIT. 
      *
      *****************************************************************
      *    終了日付設定処理
      *****************************************************************
       30012-ENDYMD-AUTO-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ENDYMD
           MOVE    SPACE               TO  WRK-ENDYMDG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   50   )
                          OR  (SPA-K062-COMMENT(IDX)   =   SPACE )
               IF      SPA-K062-TENDYMD (IDX)      >   WRK-ENDYMD
                   MOVE    SPA-K062-TENDYMD (IDX)  TO  WRK-ENDYMD
                   MOVE    SPA-K062-TENDYMDG(IDX)  TO  WRK-ENDYMDG
               END-IF
           END-PERFORM
           MOVE    WRK-ENDYMD          TO  SPA-K062-ENDYMD
           MOVE    WRK-ENDYMDG         TO  SPA-K062-ENDYMDG
           MOVE    99                  TO  SPA-K062-CUR
           .
       30012-ENDYMD-AUTO-EXT.
           EXIT. 
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-K062-AREA
      *
           PERFORM 3101-INIT-HENSYU-SEC
      *    コメント初期
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3101-PLSU-HENSYU-SEC
           END-IF
      *    コメント初期
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  SPA-K062-PAGE
               MOVE    ZERO            TO  SPA-K062-SELNUM
               MOVE    1               TO  FLG-NEXT
               PERFORM 3102-COMMENT-AUTO-SEC
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3101-INIT-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K062-SRYYM      TO  SANTEI-SRYYM
           MOVE    SPA-K062-SRYCD      TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           IF      SPA-K062-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    SPA-K062-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF     (FLG-SANTEI          =   ZERO )
             AND  (SANTEI-MSANTEICNT   >   ZERO )
               PERFORM 31011-SANTEI-SPA-SEC
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
       3101-INIT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       31011-SANTEI-SPA-SEC         SECTION.
      *
           MOVE    SANTEI-DAY-AREA     TO  SPA-K062-DAY-G
      *    初回日
           MOVE    SPA-K062-SRYYM      TO  WRK-SRYYM
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31   )
               IF      SANTEI-DAY (IDX2)   >   ZERO
                   MOVE    IDX2            TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-K062-SRYCD      TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-K062-MEISYO
           END-IF
      *    算定年月
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-K062-SRYYMG
      *    保険組合せ名
           IF      SPA-K062-HKNCOMBI   NOT =   SPACE
               MOVE    SPA-K062-HKNCOMBI   TO  WRK-HKNCOMBI
               PERFORM 31011-HKNCOMBI-SET-SEC
               MOVE    WRK-HKNCOMBIMEI     TO  SPA-K062-HKNCOMBIMEI
           END-IF
      *    診療行為有無判定
           PERFORM 31032-SRYACT-CHK-SEC
           .
       31011-SANTEI-SPA-EXT.
           EXIT.
      *****************************************************************
      *    診療行為有無判定処理
      *****************************************************************
       31032-SRYACT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-K062-SYORI2
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (1)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (2)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (3)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (4)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-SRY-OK
           PERFORM UNTIL      (FLG-SRYACT  =   1 )
                         OR   (FLG-SRY-OK  =   1 )
               IF      SRY-RENNUM      =   1
                   PERFORM 310321-SRYACCT-CHK-SEC
               END-IF
               IF      FLG-SRY-OK          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-SRY-OK          =   1     )
              AND (SRY-SRYKBN          =   "80"  )
               IF    ((SRY-SRYCD (1) (1:1) =   "8"   ) OR
                      (SRY-SRYCD (1) (1:3) =   "008" ))
               OR    ((SRY-SRYCD (2) (1:1) =   "8"   ) OR
                      (SRY-SRYCD (2) (1:3) =   "008" ))
               OR    ((SRY-SRYCD (3) (1:1) =   "8"   ) OR
                      (SRY-SRYCD (3) (1:3) =   "008" ))
               OR    ((SRY-SRYCD (4) (1:1) =   "8"   ) OR
                      (SRY-SRYCD (4) (1:3) =   "008" ))
               OR    ((SRY-SRYCD (5) (1:1) =   "8"   ) OR
                      (SRY-SRYCD (5) (1:3) =   "008" ))
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   31   )
                       IF      ACCT-DAY (1 IDX2)   >   ZERO
                           MOVE    "1"         TO  SPA-K062-DAYA (IDX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       31032-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       310321-SRYACCT-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF     (FLG-SRYACCT         =   ZERO  )
               IF     (SANTEI-HKNCOMBINUM  NOT =   ZERO )  AND
                      (ACCT-HKNCOMBI       =   SANTEI-HKNCOMBINUM)
                   MOVE    1               TO  FLG-SRY-OK
               END-IF
               IF     (SANTEI-HKNCOMBINUM  =   ZERO )
      *            労災・自賠責保険以外の保険判定
                   MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
                   PERFORM 31011-HKNCOMBI-SET-SEC
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
      *             第三者行為
                    OR    (COMB-KOH1HKNNUM     =   "970"  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SRY-OK
                   END-IF
               END-IF
           END-IF
           .
       310321-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3101-PLSU-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  SPA-K062-COM-DAY-G
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-K062-SRYCD      TO  SANTEIPLUS-SRYCD
           MOVE    SPA-K062-SRYYM      TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           IF      SPA-K062-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    SPA-K062-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           PERFORM
                   UNTIL       (FLG-SANTEIPLUS     =   1    )
               IF      SANTEIPLUS-RENNUM   =   1
                                           OR  ZERO
                   MOVE    SANTEIPLUS-DAYKEY   TO  IDX
                   MOVE    "1"                 TO  SPA-K062-DAYC (IDX)
               END-IF
      *
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-PLSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3102-COMMENT-AUTO-SEC         SECTION.
      *
           MOVE    SPA-K062-SELNUM     TO  IDX-STR
           IF      FLG-NEXT            =   1
      *        次
               ADD     1                   TO  IDX-STR
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX2    FROM    IDX-STR   BY  1
                       UNTIL      (IDX2    >   31   )
                              OR  (IDX-T   >   ZERO )
                   IF      SPA-K062-DAY (IDX2)     >   ZERO
                       MOVE    IDX2            TO  IDX-T
                   END-IF
               END-PERFORM
               IF      IDX-T           =   ZERO
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   31   )
                                  OR  (IDX-T   >   ZERO )
                       IF      SPA-K062-DAY (IDX2)     >   ZERO
                           MOVE    IDX2            TO  IDX-T
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        前
               COMPUTE IDX-STR         =   IDX-STR   -  1
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX2    FROM    IDX-STR   BY  -1
                       UNTIL      (IDX2    =   0    )
                              OR  (IDX-T   >   ZERO )
                   IF      SPA-K062-DAY (IDX2)     >   ZERO
                       MOVE    IDX2            TO  IDX-T
                   END-IF
               END-PERFORM
               IF      IDX-T           =   ZERO
                   PERFORM VARYING     IDX2    FROM    31   BY  -1
                       UNTIL      (IDX2    =   0    )
                                  OR  (IDX-T   >   ZERO )
                       IF      SPA-K062-DAY (IDX2)     >   ZERO
                           MOVE    IDX2            TO  IDX-T
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           MOVE    IDX-T               TO  SPA-K062-SELNUM
      *
      *
           INITIALIZE                      SPA-K062-INPUT-G
           IF      SPA-K062-SELNUM     >   ZERO
               PERFORM 31021-COMMENT-HEN-SEC
           END-IF
           .
       3102-COMMENT-AUTO-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴付加編集処理
      *****************************************************************
       31021-COMMENT-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-K062-SYORI
           MOVE    ZERO                TO  SPA-K062-SYORI2
           MOVE    ZERO                TO  SPA-K062-SYORI-KEI
           IF      SPA-K062-DAYA (SPA-K062-SELNUM)   =   "1"
               MOVE    1                   TO  SPA-K062-SYORI2
           END-IF
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-K062-SRYCD      TO  SANTEIPLUS-SRYCD
           MOVE    SPA-K062-SRYYM      TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           IF      SPA-K062-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    SPA-K062-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
           MOVE    SPA-K062-SELNUM     TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       (FLG-SANTEIPLUS     =   1    )
               IF      SANTEIPLUS-RENNUM   =   ZERO
      *            開始日終了日
                   MOVE    SANTEIPLUS-ENDYMD   TO  SPA-K062-ENDYMD
                   IF      SANTEIPLUS-ENDYMD   NOT =   SPACE
                       MOVE    SANTEIPLUS-ENDYMD   TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-K062-ENDYMDG
                   END-IF
               ELSE
                   ADD     1                   TO  IDX
                   MOVE    SANTEIPLUS-COMMENT  TO  SPA-K062-COMMENT(IDX)
                   MOVE    "1"                 TO  SPA-K062-INFLG  (IDX)
                   MOVE    SANTEIPLUS-ENDYMD   TO  SPA-K062-TENDYMD(IDX)
                   IF      SANTEIPLUS-ENDYMD   NOT =   SPACE
                       MOVE    SANTEIPLUS-ENDYMD   TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-K062-TENDYMDG
                                                                   (IDX)
                   END-IF
               END-IF
      *
               MOVE    1                   TO  SPA-K062-SYORI
      *
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    11                  TO  SPA-K062-CUR 
      *
           IF      SPA-K062-SYORI      =   1
               MOVE    02                  TO  SPA-K062-CUR
           END-IF
           .
       31021-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       31011-HKNCOMBI-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
           MOVE    COMB-TEKSTYMD       TO  WRK-COMB-TEKSTYMD
           MOVE    COMB-TEKEDYMD       TO  WRK-COMB-TEKEDYMD
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
           END-IF
      *
      *    保険組合せ名は２０バイト
           IF      WRK-HKNCOMBIMEI NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
               MOVE    20                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
           END-IF
      *
      *ver.4.7
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    ZERO                TO  PTRSI-HKNID
               MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        名称編集
               IF      PTRSI-COMMENT   NOT =   SPACE
      *            保険＋公費１のみとなる
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
      *            保険組合せ名は２０バイト
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
                   MOVE    20                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
          END-IF
           .
       31011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *     クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLERA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 440-SAKUJYO-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-BACK-PAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-NEXT-PAGE-SEC
      *    前日
      *        WHEN    "CLICKED"       ALSO    "B09"
      *            PERFORM 490-BACK-DAY-SEC
      *    次日
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-NEXT-DAY-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
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
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-K062-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        算定日選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-RENNUM-HEN-SEC
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41011-RENNUM-HEN-SEC         SECTION.
      *
           MOVE    K062-SELNUM        TO  SPA-K062-SELNUM
      *
           IF      SPA-K062-SELNUM     >=  1   AND    <=  31
               IF      SPA-K062-DAY (SPA-K062-SELNUM)
                                           =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K062-CUR
               ELSE
                   INITIALIZE                      SPA-K062-INPUT-G
                   MOVE    1                   TO  SPA-K062-PAGE
                   PERFORM 31021-COMMENT-HEN-SEC
               END-IF
           ELSE
               IF      SPA-K062-SELNUM    =   ZERO
                   PERFORM 4100-NEXT-DAY-SEC
               ELSE
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K062-CUR
               END-IF
           END-IF
           .
       41011-RENNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    K062-ENDYMD         TO  SPA-K062-ENDYMDG
      *
           COMPUTE IDX-STR             =  (SPA-K062-PAGE   -   1 )
                                       *   5
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               ADD     1                   TO  IDX-STR
               MOVE    K062-COMMENT (IDX)  TO  SPA-K062-COMMENT(IDX-STR)
               MOVE    K062-TENDYMD (IDX)  TO  SPA-K062-TENDYMDG
                                                             (IDX-STR)
           END-PERFORM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    終了日付チェック
           PERFORM 41012-ENDYMD-CHK-SEC
      *    コメント・終了日付チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-COMMENT-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日付チェック処理
      *****************************************************************
       41012-ENDYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-K062-SRYYM      TO  WRK-STR-YM
           MOVE    SPA-K062-SELNUM     TO  WRK-STR-DD
      *
           IF     SPA-K062-ENDYMDG     =   SPACE
               MOVE    SPACE               TO  SPA-K062-ENDYMD
           ELSE
               MOVE    SPA-K062-ENDYMDG    TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K062-ENDYMD
                   MOVE    WRK-HENYMDG         TO  SPA-K062-ENDYMDG
                   IF      WRK-STR-YMD         >   SPA-K062-ENDYMD
      *                開始日以降
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
      *            終了日算定日より前のみ
                   IF     (SPA-K062-RRKENDYMD  NOT =   SPACE )
                   AND    (SPA-ERRCD               =   SPACE )
                       IF     SPA-K062-RRKENDYMD   <   SPA-K062-ENDYMD
                           MOVE    "0010"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            Ｈ２２．４から
                   IF     (SPA-K062-ENDYMD     <   "20100401")
                   AND    (SPA-ERRCD               =   SPACE )
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0007"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-K062-CUR
           END-IF
      *
           .
       41012-ENDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       41012-COMMENT-CHK-SEC          SECTION.
      *
      *    空白削除
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   50
      *        IF     (IDX                 <   50   )  AND
      *               (SPA-K062-COMMENT(IDX)   =   SPACE )
      *            MOVE    SPA-K062-COMMENT-TBL (IDX + 1)
      *                                    TO  SPA-K062-COMMENT-TBL(IDX)
      *            MOVE    SPACE           TO  SPA-K062-COMMENT-TBL
      *                                                        (IDX + 1)
      *        END-IF
      **** END-PERFORM
      *
      *H28.11
      *    空白削除
           INITIALIZE                  TBL-K062-COMMNT-AREA
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
               IF     (SPA-K062-COMMENT(IDX)   NOT =   SPACE )
                   ADD     1               TO  IDX2
                   MOVE    SPA-K062-COMMENT-TBL (IDX)
                                           TO  TBL-K062-COMMENT-TBL
                                                                (IDX2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
               MOVE    TBL-K062-COMMENT-TBL (IDX)
                                           TO  SPA-K062-COMMENT-TBL(IDX)
           END-PERFORM
      *
      *    全角チェック
           MOVE    ZERO                TO  IDX-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50   )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-K062-COMMENT(IDX)   NOT =   SPACE
                   PERFORM 410121-MEISYO-HENKAN-SEC
      ***********  IF     (SPA-K062-TENDYMDG(IDX)  NOT =   SPACE )
                   IF     (SPA-ERRCD                   =   SPACE )
                       PERFORM 410122-TENDYMD-CHK-SEC
                   END-IF
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    IDX                 TO  IDX-ERR
               END-IF
           END-PERFORM
      *    カーソル
           IF      IDX-ERR             >   ZERO
               DIVIDE 5       INTO  IDX-ERR    GIVING WRK-SHO
                              REMAINDER WRK-AMARI
               COMPUTE SPA-K062-CUR    =   WRK-AMARI   +  10
               COMPUTE SPA-K062-PAGE   =   WRK-SHO     +  1
      *
               IF      SPA-ERRCD       =   "0007"
                                       OR  "0008"
                                       OR  "0009"
                                       OR  "0011"
                   COMPUTE SPA-K062-CUR    =   SPA-K062-CUR  +   10
               END-IF
           END-IF
           .
       41012-COMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    名称チェック・全角置換処理
      *****************************************************************
       410121-MEISYO-HENKAN-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    80                  TO  KANACHK-MAX-CNT
           MOVE    SPA-K062-COMMENT(IDX)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    KANACHK-OUT-INPUT   TO  SPA-K062-COMMENT(IDX)
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    IDX                 TO  IDX-ERR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-K062-COMMENT(IDX)
           END-IF
      *
           .
       401131-MEISYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日付チェック処理
      *****************************************************************
       410122-TENDYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-K062-SRYYM      TO  WRK-STR-YM
           MOVE    SPA-K062-SELNUM     TO  WRK-STR-DD
      *
           IF      SPA-K062-TENDYMDG(IDX)  =   SPACE
               MOVE    SPACE               TO  SPA-K062-TENDYMD (IDX)
           ELSE
               MOVE    SPA-K062-TENDYMDG(IDX)  TO  WRK-YMD
               MOVE    SPACE               TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-K062-TENDYMD
                                                                 (IDX)
                   MOVE    WRK-HENYMDG         TO  SPA-K062-TENDYMDG
                                                                 (IDX)
                   IF      WRK-STR-YMD         >   SPA-K062-TENDYMD(IDX)
      *                開始日以降
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
      *            Ｈ２２．４から
                   IF     (SPA-K062-TENDYMD(IDX)   <   "20100401")
                   AND    (SPA-ERRCD               =   SPACE )
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
               END-IF
      *
               IF     (SPA-ERRCD               =   SPACE )
                  AND (SPA-K062-COMMENT(IDX)   =   SPACE)
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       410122-TENDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-K062-MOTOPG     TO  SPA-SAKIPG
           MOVE    "K062"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *    クリア　処理
      *****************************************************************
       420-CLERA-SEC                SECTION.
      *
           INITIALIZE                  SPA-K062-AREA
           MOVE    ZERO                TO  SPA-K062-SELNUM
           MOVE    1                   TO  SPA-K062-PAGE
           MOVE    1                   TO  SPA-K062-CUR
           .
       420-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC                SECTION.
      *
           IF      SPA-K062-SELNUM     >   ZERO
               MOVE    "0101"          TO  SPA-KIDCD
           END-IF
           .
       440-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    削除　処理
      *****************************************************************
       4401-DELETE-SYORI-SEC                SECTION.
      *
      *    削除処理
           PERFORM 4501-SANTEIPLUS-DEL-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        一覧初期表示
               PERFORM 420-CLERA-SEC
      *        コメント初期
               PERFORM 3101-PLSU-HENSYU-SEC
           END-IF
           .
       4401-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
           IF     (SPA-K062-SRYCD      =   SPACE )
            OR    (SPA-K062-SRYYM      =   SPACE )
            OR    (SPA-K062-SELNUM     =   ZERO  )
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K062-CUR
           END-IF
           IF     (SPA-K062-SYORI2     =   1     )
               IF      SPA-K062-SYORI-KEI  =   ZERO
                   MOVE    "K005"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K062-CUR
                   MOVE    1                   TO  SPA-K062-SYORI-KEI
               END-IF
           END-IF
      *    入力チェック処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 410-INPUT-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4501-KANREN-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
           AND    (SPA-KIDCD           =   SPACE )
               MOVE    "0102"          TO  SPA-KIDCD
           END-IF
      *
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴登録処理
      *****************************************************************
       4501-TOUROKU-SYORI-SEC                SECTION.
      *
      *    更新
           IF      SPA-K062-SYORI      =   1
      *        削除処理
               PERFORM 4501-SANTEIPLUS-DEL-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        登録処理
               PERFORM 4502-SANTEIPLUS-ADD-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        一覧初期表示
               PERFORM 420-CLERA-SEC
      *        コメント初期
               PERFORM 3101-PLSU-HENSYU-SEC
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前関連チェック処理
      *****************************************************************
       4501-KANREN-CHK-SEC                SECTION.
      *
           IF      (SPA-K062-COMMENT (1)   =   SPACE )
            AND    (SPA-K062-ENDYMD        =   SPACE )
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-K062-CUR
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHKCOM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   50   )
                          OR  (SPA-ERRCD   NOT =   SPACE )
                          OR  (SPA-K062-COMMENT(IDX)   =   SPACE )
               IF      SPA-K062-TENDYMD (IDX)      =   SPACE
                   MOVE    1                   TO  FLG-CHKCOM
               END-IF
           END-PERFORM
           IF     (SPA-ERRCD           =   SPACE )
           AND    (SPA-K062-COMMENT(1) NOT =   SPACE )
      *        コメント毎の終了日がすべて設定されている時、終了日があること
               IF      (FLG-CHKCOM         =   ZERO  )
                   IF      SPA-K062-ENDYMD     =   SPACE
                       MOVE    "0110"              TO  SPA-KIDCD
                   END-IF
               END-IF
            END-IF
      *
           .
       4501-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       4501-SANTEIPLUS-DEL-SEC             SECTION.
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-K062-SRYCD      TO  SANTEIPLUS-SRYCD
           MOVE    SPA-K062-SRYYM      TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           IF      SPA-K062-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    SPA-K062-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
           MOVE    SPA-K062-SELNUM     TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "K062 DEL SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
      *
           .
       4501-SANTEIPLUS-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4502-SANTEIPLUS-ADD-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-K062-SRYCD      TO  SANTEIPLUS-SRYCD
           MOVE    SPA-K062-SRYYM      TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           IF      SPA-K062-HKNCOMBI   =   SPACE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    SPA-K062-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
           MOVE    SPA-K062-SELNUM     TO  SANTEIPLUS-DAYKEY
      *
      *    開始日の終了日付あり
           IF      SPA-K062-ENDYMD NOT =   SPACE
               PERFORM 4501-SANTEIPLUS-END-INS-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   50   )
                          OR  (SPA-ERRCD   NOT =   SPACE )
                          OR  (SPA-K062-COMMENT(IDX)   =   SPACE )
               PERFORM 4502-SANTEIPLUS-INS-SEC
           END-PERFORM
      *
          .
       4502-SANTEIPLUS-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加情報登録処理
      *****************************************************************
       4501-SANTEIPLUS-END-INS-SEC         SECTION.
      *
           MOVE    ZERO                TO  SANTEIPLUS-RENNUM
           MOVE    SPA-K062-ENDYMD     TO  SANTEIPLUS-ENDYMD
      *
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEIPLUS-UPHMS
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K062 INS SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
          .
       4501-SANTEIPLUS-END-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加情報登録処理
      *****************************************************************
       4502-SANTEIPLUS-INS-SEC         SECTION.
      *
           MOVE    IDX                 TO  SANTEIPLUS-RENNUM
           MOVE    SPA-K062-COMMENT (IDX)
                                       TO  SANTEIPLUS-COMMENT
           MOVE    SPA-K062-TENDYMD (IDX)
                                       TO  SANTEIPLUS-ENDYMD
      *
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEIPLUS-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEIPLUS-UPHMS
      *
           MOVE    SPA-OPID            TO  SANTEIPLUS-OPID
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "K062 INS SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
           END-IF
      *
          .
       4502-SANTEIPLUS-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-BACK-PAGE-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K062-PAGE       =   1
                   CONTINUE
               ELSE
                   COMPUTE SPA-K062-PAGE   =   SPA-K062-PAGE   -   1
                   MOVE    11                  TO  SPA-K062-CUR
               END-IF
           END-IF
           .
       450-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-NEXT-PAGE-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K062-PAGE       =   10
                   MOVE    "0020"          TO  SPA-ERRCD
               ELSE
                   COMPUTE IDX-STR         =   SPA-K062-PAGE   *   5
                   IF      SPA-K062-COMMENT (IDX-STR)  =   SPACE
                       CONTINUE
                   ELSE
      *            次ページ表示
                       ADD     1                   TO  SPA-K062-PAGE
                       MOVE    11                  TO  SPA-K062-CUR
                   END-IF
               END-IF
           END-IF
           .
       460-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日　処理
      *****************************************************************
       490-BACK-DAY-SEC             SECTION.
      *
           IF      SPA-K062-SELNUM     =   ZERO
               MOVE    31              TO  SPA-K062-SELNUM
           END-IF
           MOVE    1               TO  SPA-K062-PAGE
           MOVE    2               TO  FLG-NEXT
           PERFORM 3102-COMMENT-AUTO-SEC
           .
       490-BACK-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日　処理
      *****************************************************************
       4100-NEXT-DAY-SEC             SECTION.
      *
           MOVE    1               TO  SPA-K062-PAGE
           MOVE    1               TO  FLG-NEXT
           PERFORM 3102-COMMENT-AUTO-SEC
           .
       4100-NEXT-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-INTYPE          TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K062"              TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  K062
           INITIALIZE                  K062
      *
           MOVE    SPA-K062-SRYCD     TO  K062-SRYCD
           MOVE    SPA-K062-MEISYO    TO  K062-MEISYO
           MOVE    SPA-K062-SRYYMG    TO  K062-SRYYM
           IF      SPA-K062-HKNCOMBI  =   ZERO
               MOVE    SPACE              TO  K062-HKNCOMBI
           ELSE
               MOVE    SPA-K062-HKNCOMBI  TO  K062-HKNCOMBI
           END-IF
           MOVE    SPA-K062-HKNCOMBIMEI   TO  K062-HKNCOMBIMEI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               MOVE    SPA-K062-DAY (IDX)  TO  K062-DAY (IDX)
               IF      SPA-K062-DAYC (IDX) =   "1"
                   MOVE    "red"           TO  K062-DAY-STYLE (IDX)
               ELSE
                   MOVE    "black"         TO  K062-DAY-STYLE (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K062-SELNUM        TO  K062-SELNUM
      *
           MOVE    SPA-K062-ENDYMDG    TO  K062-ENDYMD
      *
           COMPUTE IDY                 =   (SPA-K062-PAGE  -  1 )
                                       *   5
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               ADD     1                   TO  IDY
               MOVE    SPA-K062-COMMENT (IDY)
                                           TO  K062-COMMENT  (IDX)
               MOVE    SPA-K062-TENDYMDG(IDY)
                                           TO  K062-TENDYMD  (IDX)
      *
               PERFORM 5003-COMMENT-NO-SEC
           END-PERFORM
      *
           IF      SPA-K062-SYORI2     =   1
               MOVE    "【診療行為登録済】"    TO  K062-MIDASI-VALUE
               MOVE    "red"                   TO  K062-MIDASI-STYLE
           END-IF
      *
           MOVE    SPA-K062-MIDASI2     TO  K062-MIDASI2-VALUE
           MOVE    "black"              TO  K062-MIDASI2-STYLE
           MOVE    SPA-K062-MSGENDYMD   TO  K062-MSGENDYMD-VALUE
           MOVE    "black"              TO  K062-MSGENDYMD-STYLE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5003-COMMENT-NO-SEC             SECTION.
      *
           MOVE    IDY                 TO  WRK-Z9
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-Z9              TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           EVALUATE    IDX
               WHEN    1
                   MOVE    KANACHK-OUT-INPUT(1:4)   TO  K062-MUN1
               WHEN    2
                   MOVE    KANACHK-OUT-INPUT(1:4)   TO  K062-MUN2
               WHEN    3
                   MOVE    KANACHK-OUT-INPUT(1:4)   TO  K062-MUN3
               WHEN    4
                   MOVE    KANACHK-OUT-INPUT(1:4)   TO  K062-MUN4
               WHEN    5
                   MOVE    KANACHK-OUT-INPUT(1:4)   TO  K062-MUN5
           END-EVALUATE
           .
       5003-COMMENT-NO-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-K062-CUR        =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-K062-CUR
               WHEN    01
                   MOVE  "SELNUM"            TO  MCP-WIDGET
               WHEN    02
                   MOVE  "ENDYMD"             TO  MCP-WIDGET
               WHEN    11
                   MOVE  "COMMENT01"          TO  MCP-WIDGET
               WHEN    12
                   MOVE  "COMMENT02"          TO  MCP-WIDGET
               WHEN    13
                   MOVE  "COMMENT03"          TO  MCP-WIDGET
               WHEN    14
                   MOVE  "COMMENT04"          TO  MCP-WIDGET
               WHEN    15
                   MOVE  "COMMENT05"          TO  MCP-WIDGET
               WHEN    21
                   MOVE  "ENDYMD01"           TO  MCP-WIDGET
               WHEN    22
                   MOVE  "ENDYMD02"           TO  MCP-WIDGET
               WHEN    23
                   MOVE  "ENDYMD03"           TO  MCP-WIDGET
               WHEN    24
                   MOVE  "ENDYMD04"           TO  MCP-WIDGET
               WHEN    25
                   MOVE  "ENDYMD05"           TO  MCP-WIDGET
               WHEN    98
                   MOVE  "B03"               TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           COMPUTE IDY                 =   (SPA-K062-PAGE  -  1 )
                                       *   5
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    11          TO  SPA-K062-CUR
               WHEN    "ENDYMD"
                   MOVE    11          TO  SPA-K062-CUR
               WHEN    "COMMENT01"
                   MOVE    12          TO  SPA-K062-CUR
                   IF      SPA-K062-INFLG (IDY + 1)  =   "1"
                       MOVE    21          TO  SPA-K062-CUR
                   END-IF
               WHEN    "COMMENT02"
                   MOVE    13          TO  SPA-K062-CUR
                   IF      SPA-K062-INFLG (IDY + 2)  =   "1"
                       MOVE    22          TO  SPA-K062-CUR
                   END-IF
               WHEN    "COMMENT03"
                   MOVE    14          TO  SPA-K062-CUR
                   IF      SPA-K062-INFLG (IDY + 3)  =   "1"
                       MOVE    23          TO  SPA-K062-CUR
                   END-IF
               WHEN    "COMMENT04"
                   MOVE    15          TO  SPA-K062-CUR
                   IF      SPA-K062-INFLG (IDY + 4)  =   "1"
                       MOVE    24          TO  SPA-K062-CUR
                   END-IF
               WHEN    "COMMENT05"
                   MOVE    99          TO  SPA-K062-CUR
                   IF      SPA-K062-INFLG (IDY +  5)  =   "1"
                       MOVE    25          TO  SPA-K062-CUR
                   END-IF
               WHEN    "ENDYMD01"
                   MOVE    12          TO  SPA-K062-CUR
               WHEN    "ENDYMD02"
                   MOVE    13          TO  SPA-K062-CUR
               WHEN    "ENDYMD03"
                   MOVE    14          TO  SPA-K062-CUR
               WHEN    "ENDYMD04"
                   MOVE    15          TO  SPA-K062-CUR
               WHEN    "ENDYMD05"
                   MOVE    99          TO  SPA-K062-CUR
           END-EVALUATE
           .
       50011-INPUT-CUR-EXT.
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
                   MOVE    "算定履歴が登録されていません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療コードを入力して下さい"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "算定されていない日です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0004"
                   STRING  "コメントは全角で入力して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "算定日が入力されていません。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING  "警告！"
                           "診療行為でコメントが登録されています。"
                           "このコメントはレセプト反映しません。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "コメント・終了日の入力がありません。"
                           "削除の時は、削除ボタンを押下して下さい。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
      ****         MOVE    "コメントまたは終了日を入力して下さい。"
      ****                                             TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "終了日付が暦日エラーです。"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "開始日付＞終了日付です。"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING  "コメントが入力されていません。"
                           "終了日付の登録はできません。"
                           DELIMITED   BY  SIZE    INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "リハビリテーション終了日が算定済みです。"
                           "終了日付は算定日より前の日付のみ有効です。"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "終了日付はＨ２２年４月１日以降として下さい"
                                               DELIMITED   BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   MOVE    "最大件数です。これ以上入力できません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "マスタが存在しません" 
                                                   TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "診療科履歴更新エラー" 
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  KERR
           MOVE    SPA-ERRCD            TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  KERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "K062"               TO  SPA-MOTOPG
           MOVE    "KERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "KERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING    "算定日のリハビリコメントを削除します。"
                             "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING    "リハビリコメントを登録します。"
                             "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0110"
                   STRING    "履歴毎の終了日付がありません。"
                          "ＯＫでコメント毎の終了日付を設定をします。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K062"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
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
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           IF      WRK-SRYYM           =   SPACE
               MOVE    SPA-NAI-SRYYM       TO  TNS-YUKOSTYMD (1:6)
           ELSE
               MOVE    WRK-SRYYM           TO  TNS-YUKOSTYMD (1:6)
           END-IF
           MOVE    "01"                TO  TNS-YUKOSTYMD (7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-YUKOSTYMD       TO  WRK-YUKOSTYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60")  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *    附加情報の上限回数設定
           IF      FLG-TENSU           =   ZERO
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスター読込
      *****************************************************************
       900-SANTEI-INV-SEC              SECTION.
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定歴付加マスタ読み込み
      *****************************************************************
       920-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       920-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMBI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
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
      *

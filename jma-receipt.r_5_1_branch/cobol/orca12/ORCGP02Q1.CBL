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
       PROGRAM-ID.             ORCGP02Q1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : ＱＲ患者確認画面（Ｐ０２Ｑ１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/02/13    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNJANUM            PIC X(08).
           03  WRK-HKNJANAME           PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNNUMMEI           PIC X(20).
           03  WRK-KIKAN.
               05  WRK-STYMD           PIC X(09).
               05  WRK-K1              PIC X(02).
               05  WRK-EDYMD           PIC X(09).
      *
           03  WRK-PIDMSG              PIC X(80).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    QRコード(被保険者証)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02Q1"             TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
           INITIALIZE                      SPA-P02Q1-AREA
           INITIALIZE                      SPA-QRDATA-AREA
      *
      *    ＱＲ編集
           PERFORM 3001-QRCDHKN-SYORI-SEC
      *    患者一覧編集
           PERFORM 3002-PTINF-LIST-SEC
           IF      SPA-P02Q1-MAX       =   1
               MOVE    1                   TO  SPA-P02Q1-SELNUM
           END-IF
           IF      SPA-P02Q1-SELNUM    >   ZERO
      *        患者情報編集
               PERFORM 3003-PTINF-HENSYU-SEC
           ELSE
               MOVE    1                   TO  SPA-P02Q1-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ編集処理
      *****************************************************************
       3001-QRCDHKN-SYORI-SEC              SECTION.
      *
           INITIALIZE                      SPA-P02Q1-QRAREA
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-P02Q-SYSYMD     TO  QRCDHKN-REGISTYMD
           MOVE    SPA-P02Q-REGISTID   TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-QRCDHKN         =   ZERO
      *        処理中フラグ設定
               PERFORM 30012-QRCDHKN-MODESET-SEC
      *        ＱＲ情報編集処理
               PERFORM 30011-QRCDHKN-HEN-SEC
           END-IF
      *
           .
       3001-QRCDHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ情報編集処理
      *****************************************************************
       30011-QRCDHKN-HEN-SEC              SECTION.
      *
      *    保険者番号
           MOVE    QRCDHKN-HKNJANUM    TO  SPA-P02Q1-QRHKNJANUM
           MOVE    QRCDHKN-HKNJANUM    TO  WRK-HKNJANUM
           PERFORM 300111-HKNJAINF-HEN-SEC
           MOVE    WRK-HKNJANAME       TO  SPA-P02Q1-QRHKNJANAME
           MOVE    WRK-HKNNUMMEI       TO  SPA-P02Q1-QRHKNNUMMEI
           MOVE    WRK-HKNNUM          TO  SPA-P02Q1-QRHKNNUM
      *    記号番号
           STRING  QRCDHKN-KIGO        DELIMITED  BY  SPACE
                   "・"                DELIMITED  BY  SIZE
                   QRCDHKN-NUM         DELIMITED  BY  SPACE
                                   INTO    SPA-P02Q1-QRKIGONUM
           END-STRING
      *    本人家族区分
           EVALUATE    QRCDHKN-HONKZKKBN
               WHEN    "1"
                   MOVE    "本人"          TO  SPA-P02Q1-QRHONKZKKBN
               WHEN    "2"
                   MOVE    "家族"          TO  SPA-P02Q1-QRHONKZKKBN
           END-EVALUATE
      *    カナ氏名
           MOVE    QRCDHKN-KANANAME    TO  SPA-P02Q1-QRKANANAME
      *    性別
           EVALUATE    QRCDHKN-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P02Q1-QRSEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P02Q1-QRSEX
           END-EVALUATE
      *    生年月日
           IF      QRCDHKN-BIRTHDAYS   =   SPACE
               MOVE    QRCDHKN-BIRTHDAY    TO  SPA-P02Q1-QRBIRTHDAY
           ELSE
               MOVE    QRCDHKN-BIRTHDAYS   TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02Q1-QRBIRTHDAY
           END-IF
      *    氏名
           MOVE    QRCDHKN-NAME        TO  SPA-P02Q1-QRNAME
           .
       30011-QRCDHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       300111-HKNJAINF-HEN-SEC              SECTION.
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM          TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HKNJA-HKNJANAME     TO  WRK-HKNJANAME
           MOVE    SPACE               TO  WRK-HKNNUMMEI
      *
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNNUM        TO  WRK-HKNNUM
      *        保険情報
               MOVE    SPACE               TO  HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    HKNJA-HKNNUM        TO  HKN-HKNNUM
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
                   PERFORM 921-HKNNUM-READ-SEC
               ELSE
                   INITIALIZE              HKNNUM-REC
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
               PERFORM
                      UNTIL   FLG-HKNNUM      =   1
      *            支払区分のないものは対象外とする
                   IF      HKN-PAYKBN        =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-HKNNUM
                       MOVE    HKN-TANSEIDONAME  TO  WRK-HKNNUMMEI
                   END-IF
      *
                   IF      FLG-HKNNUM          =   ZERO
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 921-HKNNUM-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       300111-HKNJAINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中フラグ設定処理
      *****************************************************************
       30012-QRCDHKN-MODESET-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    9                   TO  QRCDHKN-MOD-FLG
           MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
           MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
           END-IF
           .
       30012-QRCDHKN-MODESET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表編集処理
      *****************************************************************
       3002-PTINF-LIST-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Q1-MAX
           INITIALIZE                      SPA-P02Q1-LIST-G
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    QRCDHKN-BIRTHDAYS   TO  PTINF-BIRTHDAY
           MOVE    QRCDHKN-SEX         TO  PTINF-SEX
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-CHK
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )  OR
                            ( IDX          >=  50 )
               PERFORM 30021-PTINF-HEN-SEC
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  SPA-P02Q1-MAX
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3002-PTINF-LIST-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       30021-PTINF-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  SPA-P02Q1-TNUM (IDX)
           MOVE    PTINF-PTID          TO  SPA-P02Q1-TPTID (IDX)
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P02Q1-TPTNUM (IDX)
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-P02Q1-TNAME (IDX)
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P02Q1-TSEX (IDX)
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P02Q1-TSEX (IDX)
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P02Q1-TBIRTHDAY (IDX)
      *
           STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    SPA-P02Q1-TADRS (IDX)
           END-STRING
           MOVE    PTINF-HOME-TEL1     TO  SPA-P02Q1-TTEL (IDX)
      *    ＱＲデータで患者が決定されている
           IF     (QRCDHKN-PTID    NOT =   ZERO )  AND
                  (QRCDHKN-PTID        =   PTINF-PTID )
               MOVE    IDX                 TO  SPA-P02Q1-SELNUM
               MOVE    QRCDHKN-PTID        TO  SPA-P02Q1-QRPTID
           END-IF
           .
       30021-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       3003-PTINF-HENSYU-SEC              SECTION.
      *
           MOVE    SPA-P02Q1-SELNUM    TO  IDX
           INITIALIZE                      SPA-P02Q1-PTINFAREA
           MOVE    SPA-P02Q1-TPTID(IDX)    TO  SPA-P02Q1-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-P02Q1-TPTID(IDX)
                                       TO  PTINF-PTID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P02Q1-PTNUM
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-P02Q1-NAME
           MOVE    PTINF-KANANAME      TO  SPA-P02Q1-KANANAME
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P02Q1-SEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P02Q1-SEX
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P02Q1-BIRTHDAY
      *
           STRING  PTINF-HOME-ADRS     DELIMITED  BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    SPA-P02Q1-ADRS
           END-STRING
           MOVE    PTINF-HOME-TEL1     TO  SPA-P02Q1-TEL
      *
      *    保険情報検索
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL   FLG-PTHKNINF    =   1
               IF     (PTHKN-SAKJOKBN      NOT =   SPACE )  OR
                      (PTHKN-HKNNUM(1:1)       =   "9"   )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-OK
                   MOVE    PTHKN-HKNJANUM      TO  SPA-P02Q1-HKNJANUM
                   MOVE    PTHKN-HKNJANUM      TO  WRK-HKNJANUM
                   PERFORM 300111-HKNJAINF-HEN-SEC
                   MOVE    WRK-HKNJANAME       TO  SPA-P02Q1-HKNJANAME
                   MOVE    WRK-HKNNUMMEI       TO  SPA-P02Q1-HKNNUMMEI
                   MOVE    WRK-HKNNUM          TO  SPA-P02Q1-HKNNUM
      *            記号番号
                   STRING  PTHKN-KIGO          DELIMITED  BY  SPACE
                           "・"                DELIMITED  BY  SIZE
                           PTHKN-NUM           DELIMITED  BY  SPACE
                                           INTO    SPA-P02Q1-KIGONUM
                   END-STRING
      *            本人家族区分
                   EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                       MOVE    "本人"          TO  SPA-P02Q1-HONKZKKBN
                   WHEN    "2"
                       MOVE    "家族"          TO  SPA-P02Q1-HONKZKKBN
                   END-EVALUATE
      *            有効期間
                   MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  WRK-STYMD
                   MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  WRK-EDYMD
                   MOVE    "〜"                TO  WRK-K1
                   MOVE    WRK-KIKAN           TO  SPA-P02Q1-YUKOKIKAN
      *            システム日で有効
                   IF     (PTHKN-TEKSTYMD      <=  SPA-SYSYMD )  AND
                          (PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
                       MOVE    1               TO  SPA-P02Q1-KIKAN-OK
                   ELSE
                       MOVE    ZERO            TO  SPA-P02Q1-KIKAN-OK
                   END-IF
                   IF     (PTHKN-HKNJANUM      =   QRCDHKN-HKNJANUM)
                      AND (PTHKN-KIGO          =   QRCDHKN-KIGO )
                      AND (PTHKN-NUM           =   QRCDHKN-NUM )
                      AND (PTHKN-HONKZKKBN     =   QRCDHKN-HONKZKKBN )
                       MOVE    PTHKN-HKNID         TO  SPA-P02Q1-HKNID
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   1
                   MOVE    1                   TO  FLG-PTHKNINF
               ELSE
                   MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-PTHKNINF-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3003-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0102"
                   IF      SPA-PID1-FLG        =   "OK"
      *            新規患者登録
                       INITIALIZE                      SPA-QRDATA-AREA
                       MOVE    "1"                     TO  SPA-QR-SYORI
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    10                      TO  SPA-P02Q1-CUR
                   END-IF
               WHEN    "0103"
      *            保険変更
                   IF      SPA-PID1-FLG        =   "OK"
                       INITIALIZE                      SPA-QRDATA-AREA
                       MOVE    "4"                     TO  SPA-QR-SYORI
                       MOVE    SPA-P02Q1-PTID          TO  SPA-QR-PTID
                       MOVE    SPA-P02Q1-PTNUM         TO  SPA-QR-PTNUM
                       MOVE    SPA-P02Q1-HKNID         TO  SPA-QR-HKNID
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    12                      TO  SPA-P02Q1-CUR
                   END-IF
               WHEN    "0104"
      *            保険追加
                   IF      SPA-PID1-FLG        =   "OK"
                       INITIALIZE                      SPA-QRDATA-AREA
                       MOVE    "3"                     TO  SPA-QR-SYORI
                       MOVE    SPA-P02Q1-PTID          TO  SPA-QR-PTID
                       MOVE    SPA-P02Q1-PTNUM         TO  SPA-QR-PTNUM
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   ELSE
                       MOVE    10                      TO  SPA-P02Q1-CUR
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02Q1
           INITIALIZE                      P02Q1
           MOVE    SPA-P02Q1-QRHKNJANUM    TO  P02Q1-QR-HKNJANUM
           MOVE    SPA-P02Q1-QRHKNJANAME   TO  P02Q1-QR-HKNJANAME
           MOVE    SPA-P02Q1-QRHKNNUMMEI   TO  P02Q1-QR-HKNNUMMEI
           MOVE    SPA-P02Q1-QRKIGONUM     TO  P02Q1-QR-KIGONUM
           MOVE    SPA-P02Q1-QRHONKZKKBN   TO  P02Q1-QR-HONKZKKBN
           MOVE    SPA-P02Q1-QRBIRTHDAY    TO  P02Q1-QR-BIRTHDAY
           MOVE    SPA-P02Q1-QRSEX         TO  P02Q1-QR-SEX
           MOVE    SPA-P02Q1-QRKANANAME    TO  P02Q1-QR-KANANAME
           MOVE    SPA-P02Q1-QRNAME        TO  P02Q1-QR-NAME
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P02Q1-MAX
               MOVE    SPA-P02Q1-TNUM   (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  P02Q1-TNUM  (IDX)
               MOVE    SPA-P02Q1-TPTNUM (IDX)  TO  P02Q1-TPTNUM(IDX)
               MOVE    SPA-P02Q1-TNAME  (IDX)  TO  P02Q1-TNAME (IDX)
               MOVE    SPA-P02Q1-TSEX   (IDX)  TO  P02Q1-TSEX   (IDX)
               MOVE    SPA-P02Q1-TBIRTHDAY(IDX)
                                               TO  P02Q1-TBIRTHDAY (IDX)
               MOVE    SPA-P02Q1-TADRS  (IDX)  TO  P02Q1-TADRS (IDX)
               MOVE    SPA-P02Q1-TTEL   (IDX)  TO  P02Q1-TTEL  (IDX)
               IF      IDX                 =   SPA-P02Q1-SELNUM
                   MOVE    "T"                 TO  P02Q1-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"                 TO  P02Q1-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P02Q1-MAX        TO  P02Q1-COUNT
      *
           MOVE    SPA-P02Q1-SELNUM     TO  P02Q1-SELNUM
      *
           MOVE    SPA-P02Q1-PTNUM       TO  P02Q1-PTNUM
           MOVE    SPA-P02Q1-ADRS        TO  P02Q1-ADRS
           MOVE    SPA-P02Q1-TEL         TO  P02Q1-TEL
           MOVE    SPA-P02Q1-BIRTHDAY    TO  P02Q1-BIRTHDAY
           MOVE    SPA-P02Q1-SEX         TO  P02Q1-SEX
           MOVE    SPA-P02Q1-NAME        TO  P02Q1-NAME
      *
           MOVE    SPA-P02Q1-HKNJANUM    TO  P02Q1-HKNJANUM
           MOVE    SPA-P02Q1-HKNJANAME   TO  P02Q1-HKNJANAME
           MOVE    SPA-P02Q1-HKNNUMMEI   TO  P02Q1-HKNNUMMEI
           MOVE    SPA-P02Q1-KIGONUM     TO  P02Q1-KIGONUM
           MOVE    SPA-P02Q1-HONKZKKBN   TO  P02Q1-HONKZKKBN
           MOVE    SPA-P02Q1-YUKOKIKAN   TO  P02Q1-KIKAN
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
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-P02Q1-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"       TO  MCP-WIDGET
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
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-QR-SYORI
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    新規患者
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 408-SINKI-SYORI-SEC
      *    保険変更
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-HKNUPD-SYORI-SEC
      *    保険追加
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-HKNADD-SYORI-SEC
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
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P02Q1-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "DUALLIST"
                   PERFORM 4001-LIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-LIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P02Q1-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Q1-MAX
               IF      P02Q1-SELECT (IDX)  =   "T"
                   MOVE    IDX             TO  SPA-P02Q1-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P02Q1-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02Q1-SELNUM        TO  SPA-P02Q1-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           IF     (SPA-P02Q1-SELNUM     =   ZERO )  OR
                  (SPA-P02Q1-SELNUM     >   SPA-P02Q1-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
      *        患者情報編集
               PERFORM 3003-PTINF-HENSYU-SEC
               IF     (SPA-P02Q1-QRHKNNUM      =   SPA-P02Q1-HKNNUM)
                 AND  (SPA-P02Q1-QRHKNJANUM    =   SPA-P02Q1-HKNJANUM)
                   MOVE    10              TO  SPA-P02Q1-CUR
               ELSE
                   MOVE    12              TO  SPA-P02Q1-CUR
               END-IF
           END-IF
           .
      *
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Q1-SELNUM
           MOVE    1                   TO  SPA-P02Q1-CUR
           INITIALIZE                      SPA-P02Q1-PTINFAREA
           .
      *
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *     新規患者処理
      *****************************************************************
        408-SINKI-SYORI-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE )
               IF      SPA-P02Q1-QRPTID    NOT =   ZERO
                   MOVE    "0102"          TO  SPA-PIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
               INITIALIZE                  SPA-QRDATA-AREA
               MOVE    "1"                 TO  SPA-QR-SYORI
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
        408-SINKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険変更処理
      *****************************************************************
       410-HKNUPD-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q1-SELNUM     =   ZERO )  OR
                  (SPA-P02Q1-SELNUM     >   SPA-P02Q1-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-P02Q1-SELNUM    TO  IDX
               IF      SPA-P02Q1-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
      *        保険が違う場合、変更なし
               IF     (SPA-P02Q1-PTID      NOT =   ZERO )
                  AND (SPA-P02Q1-QRHKNNUM      =   SPA-P02Q1-HKNNUM)
                  AND (SPA-P02Q1-QRHKNJANUM    =   SPA-P02Q1-HKNJANUM)
                  AND (SPA-P02Q1-QRKIGONUM     =   SPA-P02Q1-KIGONUM )
                  AND (SPA-P02Q1-QRHONKZKKBN   =   SPA-P02Q1-HONKZKKBN)
                   IF     (SPA-P02Q1-KIKAN-OK      =   ZERO )
                       MOVE    "0103"          TO  SPA-PIDCD
                   END-IF
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        保険変更
               INITIALIZE                      SPA-QRDATA-AREA
               MOVE    "2"                     TO  SPA-QR-SYORI
               MOVE    SPA-P02Q1-PTID          TO  SPA-QR-PTID
               MOVE    SPA-P02Q1-PTNUM         TO  SPA-QR-PTNUM
               MOVE    SPA-P02Q1-HKNID         TO  SPA-QR-HKNID
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
       410-HKNUPD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       412-HKNADD-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q1-SELNUM     =   ZERO )  OR
                  (SPA-P02Q1-SELNUM     >   SPA-P02Q1-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-P02Q1-SELNUM    TO  IDX
               IF      SPA-P02Q1-PTID      =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-P02Q1-PTID  NOT =   ZERO )
                  AND (SPA-P02Q1-QRHKNNUM      =   SPA-P02Q1-HKNNUM)
                  AND (SPA-P02Q1-QRHKNJANUM    =   SPA-P02Q1-HKNJANUM)
                  AND (SPA-P02Q1-QRKIGONUM     =   SPA-P02Q1-KIGONUM )
                  AND (SPA-P02Q1-QRHONKZKKBN   =   SPA-P02Q1-HONKZKKBN)
                   IF     (SPA-P02Q1-KIKAN-OK      =   1 )
                       MOVE    "0005"          TO  SPA-ERRCD
                       MOVE    10              TO  SPA-P02Q1-CUR
                   ELSE
                       MOVE    "0104"          TO  SPA-PIDCD
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-PIDCD           =   SPACE )
      *        患者追加
               INITIALIZE                      SPA-QRDATA-AREA
               MOVE    "3"                     TO  SPA-QR-SYORI
               MOVE    SPA-P02Q1-PTID          TO  SPA-QR-PTID
               MOVE    SPA-P02Q1-PTNUM         TO  SPA-QR-PTNUM
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
       412-HKNADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       490-KAKUTEI-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02Q-SYSYMD     TO  SPA-QR-REGISTYMD
           MOVE    SPA-P02Q-REGISTID   TO  SPA-QR-REGISTID
      *
           PERFORM 210-BACK
           .
      *
       490-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02Q1"             TO  SPA-MOTOPG
           MOVE    "P02Q"              TO  SPA-SAKIPG
      *
      *    MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02Q"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02Q1"             TO  MCP-WINDOW.
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
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0003"
                   STRING  "患者が確定されていません。"
                           "患者確認を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "ＱＲ情報の保険と選択されている患者の保険が"
                           "違います。保険変更はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "ＱＲ情報の保険と同一の保険です。"
                           "保険変更をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02Q1"             TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0102"
                   STRING  "保険情報・性別・生年月日で同一の患者が"
                           "あります。新規患者でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "ＱＲ情報の保険と同一の保険の期間変更を"
                           "行います。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "ＱＲ情報の保険と同一の保険があります。"
                           "保険追加でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02Q1"             TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
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
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       8001-SEIWA-YMDEDIT-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
                                       OR  ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF      STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               ELSE
                   MOVE    SPACE               TO  WRK-HENYMD
               END-IF
           END-IF
           .
       8001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *****************************************************************
      *    QRコード(被保険者証)読込
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-READ-EXT.
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

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
       PROGRAM-ID.         ORAPI032R2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院医療区分・ＡＤＬ点数情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/10/09    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RYOCOM              PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXM                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-ZAISKBKBN           PIC X(01).
           03  WRK-MEDKBN-ID-G.
               05  WRK-MEDKBN-ID       PIC 9(02).
           03  WRK-MEDKBN-MAX          PIC 9(03).
           03  WRK-ADLTEN-G.
               05  WRK-ADLTEN          PIC 9(02).
           03  WRK-MEDKBN              PIC X(1).
           03  WRK-PTEVAL              PIC X(1).
      *
       01  KEY-AREA.
           03  KEY-SRYYM               PIC X(06).
      *
       01  TDAY-AREA.
           03  TDAY-POS                PIC 9(03).
           03  TDAY-DAY-G.
               05  TDAY-DAY            PIC 9(02)
                                       OCCURS      31.
           03  TDAY-CSV                PIC X(256).
      *
       01  TCOMB-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TCOMB//.
      *
       01  CONST-AREA.
           03  CONST-H240401           PIC X(08) VALUE "20120401".
           03  CONST-TMEDKBN-MAX       PIC 9(03) VALUE 50.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    医療区分・ＡＤＬ点数取得サブ
           COPY    "CPORCSNINF04.INC".
           COPY    "CPORCHCN68S01.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPHSPTEVALV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSPTEVALV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院診療会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
           COPY    "MCPDATA.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TCOMB-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
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
           INITIALIZE                  XML-HSPTEVALRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "99"         TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE   "89"         TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-STYUKYMD
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           INITIALIZE                      ORCSNTYMDAREA
           MOVE    SPA-HOSPNUM         TO  SNTYMD-HOSPNUM
           MOVE    SPA-PTID            TO  SNTYMD-PTID
           MOVE    "2"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                           SPA-AREA
           IF  ( SNTYMD-RC         NOT =   ZERO )
               MOVE    "03"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    "5"                 TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NYUINACCT   =   ZERO )
               MOVE    NYUINACCT-REC   TO  TCOMB-AREA
           ELSE
               MOVE    "04"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
           IF    ( FLG-RYOCOM  NOT =   ZERO )
               MOVE    "05"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           INITIALIZE                  HCN68S01-AREA
           MOVE    "1"             TO  HCN68S01-RANGE
           MOVE    SPA-PTID        TO  HCN68S01-PTID
           MOVE    KEY-SRYYM       TO  HCN68S01-SRYYM
           MOVE    SPA-SRYYMD      TO  HCN68S01-SRYYMD
           CALL    "ORCHCN68S01"   USING
                                   SPA-AREA
                                   HCN68S01-AREA
           IF    ( HCN68S01-RYOYO-FLG  =   ZERO )
               MOVE    "06"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           PERFORM 10011-SRYYM-CHECK-SEC
           MOVE    KEY-SRYYM       TO  SPA-SRYYMD
           MOVE    "01"            TO  SPA-SRYYMD (7:2)
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       10011-SRYYM-CHECK-SEC       SECTION.
      *
           IF    ( HSPTEVALREQ-SRYYM   =   SPACE )
               MOVE    SPA-SYSYMD (1:6)
                                   TO  KEY-SRYYM
           ELSE
               MOVE    HSPTEVALREQ-SRYYM
                                   TO  WRK-DATE
               MOVE    "-01"       TO  WRK-DATE (8:3)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "01"    TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               ELSE
                   MOVE    WRK-SYMD (1:6)
                                   TO  KEY-SRYYM
               END-IF
           END-IF
      *
           IF    ( KEY-SRYYM   <   CONST-H240401 (1:6))
               MOVE    "92"    TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       10011-SRYYM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSPTEVALREQ-PTNUM   =   SPACE )
               MOVE    "02"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSPTEVALREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "02"        TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "02"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC          SECTION.
      *
           INITIALIZE                  SNINF04-AREA
                                       HCN68S01-AREA
      *
           MOVE    "1"             TO  HCN68S01-RANGE
           MOVE    SPA-PTID        TO  HCN68S01-PTID
           MOVE    KEY-SRYYM       TO  HCN68S01-SRYYM
           MOVE    SPA-SRYYMD      TO  HCN68S01-SRYYMD
           CALL    "ORCSNINF04"        USING 
                                       SNINF04-AREA
                                       HCN68S01-AREA
                                       SPA-AREA
      *
           MOVE    SNINF04-PTNUM       TO HSPTEVALRES-PTNUM
           MOVE    SNINF04-NAME        TO HSPTEVALRES-NAME
           MOVE    SNINF04-KANANAME    TO HSPTEVALRES-KANANAME
           MOVE    SNINF04-BIRTHDAY    TO HSPTEVALRES-BIRTHDAY
           MOVE    SNINF04-SEX         TO HSPTEVALRES-SEX
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               MOVE    SNINF04-NYUINYMD (IDXA)
                                       TO HSPTEVALRES-NYUINYMD (IDXA)
               MOVE    SNINF04-TAIINYMD (IDXA)
                                       TO HSPTEVALRES-TAIINYMD (IDXA)
      *
           END-PERFORM
      *
           MOVE    SNINF04-SRYYM       TO HSPTEVALRES-SRYYM
      *
           PERFORM VARYING IDXA FROM 1 BY 1
                   UNTIL ( IDXA >    SNINF04-MEDKBN-MAX )
      *
               MOVE    SNINF04-MEDKBN-LV (IDXA)
                                       TO HSPTEVALRES-MEDKBN-LV (IDXA)
               MOVE    SNINF04-MEDKBN-ID (IDXA)
                                       TO HSPTEVALRES-MEDKBN-ID (IDXA)
               MOVE    SNINF04-MEDKBN-NAME (IDXA)
                                       TO HSPTEVALRES-MEDKBN-NAME (IDXA)
               MOVE    SNINF04-MEDKBN-MON (IDXA)
                                       TO HSPTEVALRES-MEDKBN-MON (IDXA)
               MOVE    SNINF04-MEDKBN-DAY (IDXA)
                                       TO HSPTEVALRES-MEDKBN-DAY (IDXA)
           END-PERFORM
      *
           PERFORM VARYING IDXA FROM 1 BY 1  UNTIL IDXA > 4
      *
               MOVE    SNINF04-ADLKBN-ID (IDXA)
                                       TO HSPTEVALRES-ADLKBN-ID (IDXA)
               MOVE    SNINF04-ADLKBN-NAME (IDXA)
                                       TO HSPTEVALRES-ADLKBN-NAME (IDXA)
               MOVE    SNINF04-ADLKBN-DAY (IDXA)
                                       TO HSPTEVALRES-ADLKBN-DAY (IDXA)
           END-PERFORM
      *
           MOVE    SNINF04-MEDKBN-LV-DAY
                                       TO HSPTEVALRES-MEDKBN-LV-DAY
           MOVE    SNINF04-ADLKBN-TTL-DAY
                                       TO HSPTEVALRES-ADLKBN-TTL-DAY
           MOVE    SNINF04-PTEVAL-DAY
                                       TO HSPTEVALRES-PTEVAL-DAY
           MOVE    SNINF04-PTEVAL-DIFF
                                       TO HSPTEVALRES-PTEVAL-DIFF
           MOVE    SNINF04-DESIGNR-DAY
                                       TO HSPTEVALRES-DESIGNR-DAY
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTEVALRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTEVALRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTEVALRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTEVALRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "00"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "01"
               MOVE    "診療年月の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "入退院情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "入院会計情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "医療区分・ＡＤＬ点数情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "06"
               IF    ( SYS-1001-HOSPSBT    =   1 )
                   MOVE    "療養病棟入院基本料が算定されていません"
                                       TO  WRK-ERRMSG
               ELSE
                 MOVE "有床診療所療養病床入院基本料が算定されていません"
                                       TO  WRK-ERRMSG
               END-IF
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
                                               DELIMITED  BY  SIZE
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "92"
               STRING  "診療年月は平成２４年（２０１２年）"
                                       DELIMITED BY SIZE
                       "４月以降"
                                       DELIMITED BY SIZE
                       "を指定してください"
                                       DELIMITED BY SIZE
               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "97"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST16-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptevalv2 object is not low_value"
           END-IF
      *
      * XML情報取り出し
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsptevalv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST16-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "private_objects"   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  APILST16-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  HSPTEVALREQ-PRIVATE-OBJECTS
      *
           IF    ( HSPTEVALREQ-PTNUM   =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTEVALREQ-PTNUM
           END-IF
      *
           IF    ( HSPTEVALREQ-SRYYM   =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTEVALREQ-SRYYM
           END-IF
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST16-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptevalv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    HSPTEVALRES-PRIVATE-OBJECTS
                                           TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsptevalv2res"     TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST16-BODY
               MOVE    "application/xml"   TO  APILST16-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    KEY-SRYYM       TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "99" )
               MOVE    404             TO  APILST16-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTEVALRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTEVALRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTEVALRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTEVALRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.

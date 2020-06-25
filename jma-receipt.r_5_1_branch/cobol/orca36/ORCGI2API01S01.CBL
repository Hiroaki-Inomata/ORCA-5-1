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
       PROGRAM-ID.         ORCGI2API01S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院患者照会
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
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
           COPY    "CPASGNPARA.INC".
      *
           COPY    "I2SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-KIKANOVER           PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-REQUESTED           PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-APIPARA             PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
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
           03  WRK-INSTR               PIC X(500).
           03  WRK-OUTSTR              PIC X(500).
           03  WRK-LEN                 PIC S9(10).
           03  WRK-ERRCD-OCC           OCCURS     3.
               05  WRK-ERRCD1          PIC X(04).
               05  WRK-ERRCD2          PIC X(04).
           03  WRK-NAME                            PIC X(200).
           03  WRK-NAME-G.
               05  WRK-NAME1           PIC X(100).
               05  WRK-NAME2           PIC X(100).
           03  WRK-RANGE-OFFSET        PIC 9(07).
           03  WRK-RANGE-LIMIT         PIC 9(07).
           03  WRK-RANGE-FROM          PIC 9(07).
           03  WRK-RANGE-TO            PIC 9(07).
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-NUMZ                PIC Z(14)9.
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM         PIC X(200).
           03  WRK-BRMNUM-Z            PIC ZZZZZ9.
           03  WRK-LAST-TEIKIYM        PIC X(06).
           03  WRK-LAST-KBT            PIC X(01).
      *
       01  REQ-AREA.
           03  REQ-BASEYMD             PIC X(10).
      *
       01  RES-AREA.
           03  RES-ORCA-UID            PIC X(36).
           03  RES-COUNT               PIC 9(07).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-UTF8              PIC 9(01)   VALUE 2.
           03  CONST-LIMIT             PIC 9(05)   VALUE 200.
      *
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY   //TMPSPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    照会サブ２（検索条件編集）
           COPY    "CPI2SUB02.INC".
      *
      *    照会サブ３（コンボボックス値返却）
           COPY    "CPI2SUB03.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSFINDV3RES.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5013.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA036.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者照会
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    入院履歴ビュー
       01  I001-REC.
           COPY    "CPI001.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01 SCRAREA.
           COPY    "ORCA36SCRAREA.INC".
      *
           COPY    "CPHSFINDV3REQ.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               XML-HSFINDREQ
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  REQ-AREA
           INITIALIZE                  RES-AREA
           INITIALIZE                  I2SUB02-AREA
      *
           MOVE    SPACE           TO  SPA-I2FREE
           INITIALIZE                  SPA-I2FREE
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
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
           INITIALIZE                  XML-HSFINDRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BASEYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        リスト項目取得
               PERFORM 800-ORCGI2SUB03-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SORT-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           IF    ( FLG-REQUESTED   =   ZERO )
               MOVE    "0100"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KIHON-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本項目チェック
      *****************************************************************
       2001-KIHON-CHK-SEC              SECTION.
      *
      *    開始件数
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-RANGE-FROM-CHK-SEC
           END-IF
      *
      *    終了件数
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-RANGE-TO-CHK-SEC
           END-IF
      *
      *    患者番号
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-PTNUM-CHK-SEC
           END-IF
      *    患者氏名
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-NAME-CHK-SEC
           END-IF
      *    開始生年月日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STBIRTHDAY-CHK-SEC
           END-IF
      *    終了生年月日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDBIRTHDAY-CHK-SEC
           END-IF
      *    住所
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-ADRS-CHK-SEC
           END-IF
      *    開始入院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STNYUINYMD-CHK-SEC
           END-IF
      *    終了入院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDNYUINYMD-CHK-SEC
           END-IF
      *    開始退院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STTAIINYMD-CHK-SEC
           END-IF
      *    終了退院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDTAIINYMD-CHK-SEC
           END-IF
      *    診療科
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYKA-CHK-SEC
           END-IF
      *    病棟
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BTUNUM-CHK-SEC
           END-IF
      *    状態
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JOTAI-CHK-SEC
           END-IF
      *    退院請求状態
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JOTAI-CHK-SEC
           END-IF
      *    請求状態
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SEIKYUJOTAI-CHK-SEC
           END-IF
      *    定期請求年月
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TEIKIYM-CHK-SEC
           END-IF
      *    定期請求期別
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KBT-CHK-SEC
           END-IF
      *    退院事由
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TAIINCD-CHK-SEC
           END-IF
      *    死亡区分
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-DEATHKBN-CHK-SEC
           END-IF
      *    テスト患者
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TSTPT-CHK-SEC
           END-IF
      *
           .
       2001-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始件数チェック処理
      *****************************************************************
       2001-RANGE-FROM-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-RANGE-FROM     =   SPACE )
               MOVE    ZERO            TO  WRK-RANGE-OFFSET
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    HSFINDREQ-RANGE-FROM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0107"           TO  SPA-ERRCD
               ELSE
                   IF    ( SNUM-OUTNUM     <= 1 )
                       MOVE    0       TO  WRK-RANGE-OFFSET
                   ELSE
                       COMPUTE WRK-RANGE-OFFSET
                           =   SNUM-OUTNUM
                           -   1
                   END-IF
               END-IF
           END-IF
      *
           . 
       2001-RANGE-FROM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了件数チェック処理
      *****************************************************************
       2001-RANGE-TO-CHK-SEC               SECTION.
      *
           IF    ( HSFINDREQ-RANGE-TO    =   SPACE )
               MOVE    CONST-LIMIT     TO  WRK-RANGE-LIMIT
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    HSFINDREQ-RANGE-TO
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0108"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-RANGE-TO
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( WRK-RANGE-TO 
                                       <   WRK-RANGE-OFFSET + 1 )
                       MOVE    "0110"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   COMPUTE WRK-RANGE-LIMIT
                       =   WRK-RANGE-TO
                       -   WRK-RANGE-OFFSET
                   IF    ( CONST-LIMIT <   WRK-RANGE-LIMIT )
                       MOVE    CONST-LIMIT TO  WRK-RANGE-LIMIT
                   END-IF
               END-IF
           END-IF
      *
           COMPUTE WRK-RANGE-FROM = WRK-RANGE-OFFSET + 1
      *
           . 
       2001-RANGE-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       2001-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-PTNUM     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    HSFINDREQ-PTNUM TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"         USING
                                           ORCSPTNUMAREA
                                           SPA-AREA
               MOVE    SPTNUM-PTNUM    TO  HSFINDREQ-PTNUM
      *
               IF    ( SPTNUM-RC           =   ZERO  )
                   MOVE    SPTNUM-PTNUM    TO  HSFINDREQ-PTNUM
                   MOVE    SPTNUM-PTID     TO  I2SUB02-PTID
                   INITIALIZE                  PTINF-REC
                   MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                   MOVE    I2SUB02-PTID    TO  PTINF-PTID
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   IF    ( FLG-PTINF       NOT =   ZERO )
                       MOVE    "0026"      TO  SPA-ERRCD
                   ELSE
                       MOVE    PTINF-NAME  TO  I2SUB02-NAME
                   END-IF
               ELSE
                   MOVE    "0001"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           . 
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       2001-NAME-CHK-SEC               SECTION.
      *
           IF    ( HSFINDREQ-NAME      =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               IF    ( SPA-ENCODING   =   CONST-UTF8 )
                   INITIALIZE                      ORCSKANACHK2AREA
                   MOVE    "1"                 TO  KANACHK2-SYORI 
                   MOVE    HSFINDREQ-NAME      TO  KANACHK2-MAE-INPUT
                   CALL    "ORCSKANACHK2"      USING
                                               ORCSKANACHK2AREA
                   MOVE    KANACHK2-OUT-INPUT  TO  I2SUB02-NAME
                   IF    ( KANACHK2-RC         =   ZERO )
                    AND  ( KANACHK2-SYUBETU    =   "2" )
                       MOVE    KANACHK2-OUT-INPUT
                                               TO  I2SUB02-NAME
                   ELSE
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI 
                   MOVE    HSFINDREQ-NAME      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-OUT-INPUT   TO  I2SUB02-NAME
                   IF    ( KANACHK-RC          =   ZERO )
                    AND  ( KANACHK-SYUBETU     =   "2" )
                       MOVE    KANACHK-OUT-INPUT   TO  I2SUB02-NAME
                   ELSE
                       MOVE    "0002"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF

           .
       2001-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始生年月日チェック処理
      *****************************************************************
       2001-STBIRTHDAY-CHK-SEC             SECTION.
      *
           IF    ( HSFINDREQ-STBIRTHDAY  =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-STBIRTHDAY
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0016"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-STBIRTHDAY
               END-IF
           END-IF
      *
           .
       2001-STBIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了生年月日チェック処理
      *****************************************************************
       2001-EDBIRTHDAY-CHK-SEC             SECTION.
      *
           IF    ( HSFINDREQ-EDBIRTHDAY    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-EDBIRTHDAY
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0017"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-EDBIRTHDAY
               END-IF
           END-IF
      *
           .
       2001-EDBIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       2001-ADRS-CHK-SEC                   SECTION.
      *
           IF    ( HSFINDREQ-ADRS          =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1                   TO  FLG-REQUESTED
      *
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    HSFINDREQ-ADRS      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  I2SUB02-ADRS
               IF    ( KANACHK-RC          =   ZERO )
                AND  ( KANACHK-SYUBETU     =   "2" )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  I2SUB02-ADRS
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-ADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始入院日チェック処理
      *****************************************************************
       2001-STNYUINYMD-CHK-SEC         SECTION.
      *
           IF   ( HSFINDREQ-STNYUINYMD =   SPACE )
               MOVE    SPACE           TO  I2SUB02-STNYUINYMD
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-STNYUINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0005"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-STNYUINYMD
               END-IF
           END-IF
      *
           .
       2001-STNYUINYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了入院日チェック処理
      *****************************************************************
       2001-EDNYUINYMD-CHK-SEC         SECTION.
      *
           IF   ( HSFINDREQ-EDNYUINYMD =   SPACE )
               MOVE    SPACE           TO  I2SUB02-EDNYUINYMD
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-EDNYUINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0006"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-EDNYUINYMD
               END-IF
           END-IF
      *
           .
       2001-EDNYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始退院日チェック処理
      *****************************************************************
       2001-STTAIINYMD-CHK-SEC         SECTION.
      *
           IF   ( HSFINDREQ-STTAIINYMD =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-STTAIINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0011"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-STTAIINYMD
               END-IF
           END-IF
      *
           .
       2001-STTAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了退院日チェック処理
      *****************************************************************
       2001-EDTAIINYMD-CHK-SEC         SECTION.
      *
           IF   ( HSFINDREQ-EDTAIINYMD   =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-EDTAIINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0012"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  I2SUB02-EDTAIINYMD
               END-IF
           END-IF
      *
           .
       2001-EDTAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       2001-SRYKA-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-SRYKA     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-SRYKA TO  WRK-CMB-CD
               PERFORM 4200-SRYKALST-CHK-SEC
               IF    ( WRK-CMB-ITM     =    SPACE )
                   MOVE    "0007"      TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-SRYKA
                                       TO  I2SUB02-SRYKA
               END-IF
           END-IF
      *
           .
       2001-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟入力処理
      *****************************************************************
       2001-BTUNUM-CHK-SEC             SECTION.
      *
           IF    ( HSFINDREQ-BTUNUM    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-BTUNUM
                                       TO  WRK-CMB-CD
               PERFORM 4200-BTUNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0008"      TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-BTUNUM
                                       TO  I2SUB02-BTUNUM
               END-IF
           END-IF
      *
           .
       2001-BTUNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態入力処理
      *****************************************************************
       2001-JOTAI-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-JOTAI     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-JOTAI TO  WRK-CMB-CD
               PERFORM 4200-JOTAILST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0009"      TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-JOTAI
                                       TO  I2SUB02-JOTAI
               END-IF
           END-IF
      *
           .
       2001-JOTAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求状態入力処理
      *****************************************************************
       2001-SEIKYUJOTAI-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-SEIKYUJOTAI   =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-SEIKYUJOTAI
                                       TO  WRK-CMB-CD
               PERFORM 4200-SEIKYUJOTAILST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0026"      TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-SEIKYUJOTAI
                                       TO  I2SUB02-SEIKYUJOTAI
               END-IF
           END-IF
      *
           .
       2001-SEIKYUJOTAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求年月入力処理
      *****************************************************************
       2001-TEIKIYM-CHK-SEC                SECTION.
      *
           IF    ( HSFINDREQ-TEIKIYM   =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-TEIKIYM
                                       TO  WRK-DATE
               MOVE    "-01"           TO  WRK-DATE(8:)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0027"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                       TO  I2SUB02-TEIKIYM
               END-IF
      *
           END-IF
      *
           .
       2001-TEIKIYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求期別入力処理
      *****************************************************************
       2001-KBT-CHK-SEC                SECTION.
      *
           IF    ( HSFINDREQ-KBT       =   SPACE )
            OR   ( HSFINDREQ-TEIKIYM   =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               IF    ( HSFINDREQ-KBT   =   "1" OR "2" OR "3" )
                   MOVE    HSFINDREQ-KBT
                                       TO  I2SUB02-KBT
               ELSE
                   MOVE    "0028"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-TEIKIKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者入力処理
      *****************************************************************
       2001-TSTPT-CHK-SEC              SECTION.
      *
           IF    ( HSFINDREQ-TSTPT         =   SPACE )
               MOVE    "a"                 TO  I2SUB02-TSTPT
           ELSE
      *
               MOVE    HSFINDREQ-TSTPT     TO  WRK-CMB-CD
               PERFORM 4200-TSTPTLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0018"          TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-TSTPT TO  I2SUB02-TSTPT
               END-IF
      *
           END-IF
      *
           IF    ( I2SUB02-TSTPT   =   "1" )
               MOVE    1               TO  FLG-REQUESTED
           END-IF
      *
           .
       2001-TSTPT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由入力処理
      *****************************************************************
       2001-TAIINCD-CHK-SEC             SECTION.
      *
           IF    ( HSFINDREQ-TAIINCD   =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-TAIINCD   TO  WRK-CMB-CD
               PERFORM 4200-TAIINCDLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0013"          TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-TAIINCD TO  I2SUB02-TAIINCD
               END-IF
      *
           END-IF
      *
           .
       2001-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡区分入力処理
      *****************************************************************
       2001-DEATHKBN-CHK-SEC            SECTION.
      *
           IF    ( HSFINDREQ-DEATHKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    1               TO  FLG-REQUESTED
      *
               MOVE    HSFINDREQ-DEATHKBN
                                       TO  WRK-CMB-CD
               PERFORM 4200-DEATHKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0014"          TO  SPA-ERRCD
               ELSE
                   MOVE    HSFINDREQ-DEATHKBN TO  I2SUB02-DEATHKBN
               END-IF
      *
           END-IF
           .
       2001-DEATHKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順チェック
      *****************************************************************
       2001-SORT-CHK-SEC               SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   3 )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( HSFINDREQ-SORT-KEY (IDX1)
                                           NOT =   SPACE )
                       PERFORM 20011-SORT-CHK-SEC
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       2001-SORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック
      *****************************************************************
       2001-BASEYMD-CHK-SEC            SECTION.
      *
           IF    ( HSFINDREQ-BASEYMD   =   SPACE )
               MOVE    SPA-SYSYMD      TO  REQ-BASEYMD
           ELSE
      *
               MOVE    HSFINDREQ-BASEYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0025"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  REQ-BASEYMD
               END-IF
           END-IF
      *
           .
       2001-BASEYMD-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並べ替え項目チェック
      *****************************************************************
       20011-SORT-CHK-SEC              SECTION.
      *
           MOVE    "0019"              TO  WRK-ERRCD1 (1)
           MOVE    "0020"              TO  WRK-ERRCD2 (1)
      *
           MOVE    "0021"              TO  WRK-ERRCD1 (2)
           MOVE    "0022"              TO  WRK-ERRCD2 (2)
      *
           MOVE    "0023"              TO  WRK-ERRCD1 (3)
           MOVE    "0024"              TO  WRK-ERRCD2 (3)
      *
           MOVE    HSFINDREQ-SORT-KEY (IDX1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-SORTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    WRK-ERRCD1 (IDX1)   TO  SPA-ERRCD
           ELSE
               MOVE    HSFINDREQ-SORT-KEY (IDX1)
                                       TO  I2SUB02-SORT-CD (IDX1)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    HSFINDREQ-SORT-ORDER (IDX1)
               WHEN    SPACE
                   MOVE    "1"         TO  I2SUB02-SORT-ORDER (IDX1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    HSFINDREQ-SORT-ORDER (IDX1)
                                       TO  I2SUB02-SORT-ORDER (IDX1)
               WHEN    OTHER
                   MOVE    WRK-ERRCD2 (IDX1)
                                       TO  SPA-ERRCD
               END-EVALUATE
      *
           END-IF
      *
           .
       20011-SORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本情報関連チェック
      *****************************************************************
       2002-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( I2SUB02-STBIRTHDAY  NOT =   SPACE )
                AND  ( I2SUB02-EDBIRTHDAY  NOT =   SPACE )
                AND  ( I2SUB02-STBIRTHDAY  >   I2SUB02-EDBIRTHDAY )
                   MOVE    "0103"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( I2SUB02-STNYUINYMD  NOT =   SPACE )
                AND  ( I2SUB02-EDNYUINYMD  NOT =   SPACE )
                AND  ( I2SUB02-STNYUINYMD  >   I2SUB02-EDNYUINYMD )
                   MOVE    "0101"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( I2SUB02-STTAIINYMD  NOT =   SPACE )
                AND  ( I2SUB02-EDTAIINYMD  NOT =   SPACE )
                AND  ( I2SUB02-STTAIINYMD  >   I2SUB02-EDTAIINYMD )
                   MOVE    "0102"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2002-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-SANITIZE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-DB-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-RESPONSE-SEC
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    サニタイズ処理
      *****************************************************************
       20031-SANITIZE-SEC                  SECTION.
      *
           MOVE    I2SUB02-NAME        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-NAME
      *
           MOVE    I2SUB02-ADRS        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-ADRS
      *
           MOVE    I2SUB02-STNYUINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-STNYUINYMD
      *
           MOVE    I2SUB02-EDNYUINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-EDNYUINYMD
      *
           MOVE    I2SUB02-STTAIINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-STTAIINYMD
      *
           MOVE    I2SUB02-EDTAIINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-EDTAIINYMD
      *
           MOVE    I2SUB02-SRYKA       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-SRYKA
      *
           MOVE    I2SUB02-STBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-STBIRTHDAY
      *
           MOVE    I2SUB02-EDBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-EDBIRTHDAY
      *
           MOVE    I2SUB02-BTUNUM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-BTUNUM
      *
           MOVE    I2SUB02-JOTAI       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-JOTAI
      *
           MOVE    I2SUB02-SEIKYUJOTAI TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-SEIKYUJOTAI
      *
           MOVE    I2SUB02-TEIKIYM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-TEIKIYM
      *
           MOVE    I2SUB02-KBT         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-KBT
      *
           MOVE    I2SUB02-TSTPT   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-TSTPT
      *
           MOVE    I2SUB02-TAIINCD     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-TAIINCD
      *
           MOVE    I2SUB02-DEATHKBN    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-DEATHKBN
      *
           .
       20031-SANITIZE-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理
      *****************************************************************
       20031-DB-SEC                    SECTION.
      *
           MOVE    REQ-BASEYMD         TO  I2SUB02-BASEYMD
           MOVE    1                   TO  I2SUB02-API-FLG
           CALL    "ORCGI2SUB02"       USING
                                       I2SUB02-AREA
                                       I001-REC
                                       SPA-AREA
           IF    ( I2SUB02-RC      NOT =   ZERO ) 
               MOVE    "0105"          TO  SPA-ERRCD
           ELSE
               MOVE    I2SUB02-LAST-TEIKIYM
                                       TO  WRK-LAST-TEIKIYM
               MOVE    I2SUB02-LAST-KBT
                                       TO  WRK-LAST-KBT
           END-IF
      *
           .
       20031-DB-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
           PERFORM 900-PTSRH-CNT-SEL-SEC
           MOVE    PTSRH-PTID      TO  RES-COUNT
           MOVE    RES-COUNT       TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  HSFINDRES-COUNT
      *
           IF    ( HSFINDREQ-SEIKYUJOTAI   =   "2" )
               MOVE    WRK-LAST-TEIKIYM
                                       TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:2)
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE(1:7)   TO  HSFINDRES-LAST-TEIKIYM
               MOVE    WRK-LAST-KBT    TO  HSFINDRES-LAST-KBT
           END-IF
      *
           IF    ( PTSRH-PTID      =   ZERO )
               MOVE    "0106"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-RANGE-FROM  >   RES-COUNT )
                   MOVE    "0109"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               COMPUTE WRK-RANGE-OFFSET =  WRK-RANGE-FROM - 1
      *
               MOVE    WRK-RANGE-FROM  TO  WRK-NUMZ
               MOVE    FUNCTION TRIM (WRK-NUMZ)
                                       TO  HSFINDRES-RANGE-FROM
      *
               PERFORM 800-ORCGI2SUB03-SEC
      *
               PERFORM 900-PTSRH-ODR-SEL-SEC
      *
               MOVE    ZERO                TO  IDXL
               PERFORM UNTIL ( FLG-PTSRH       NOT =   ZERO )
                        OR   ( IDXL        >=  CONST-LIMIT )
      *
                   PERFORM 200311-RESPONSE-SEC
      *
                   PERFORM 900-PTSRH-ODR-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_ptsrh"     TO  MCP-TABLE
               MOVE    "odr"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               INITIALIZE                  PTSRH-REC
               MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
               MOVE    SPA-TERMID      TO  PTSRH-TERMID
               MOVE    PTSRH-REC       TO  MCPDATA-REC
               MOVE    "tbl_ptsrh"     TO  MCP-TABLE
               MOVE    "del3"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       200311-RESPONSE-SEC             SECTION.
      *
           COMPUTE IDXL  =   IDXL  +   1
      *
           INITIALIZE                  I2SUB02-AREA
           MOVE    "I"             TO  I2SUB02-MODE
           MOVE    PTSRH-MEMO      TO  I2SUB02-MEMO
           CALL    "ORCGI2SUB02"       USING
                                       I2SUB02-AREA
                                       I001-REC
                                       SPA-AREA
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
      *
           MOVE    PTSRH-SEQNUM    TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  HSFINDRES-SEQNUM (IDXL)
                                       HSFINDRES-RANGE-TO
      *
      *
           MOVE    I001-PTNUM      TO  HSFINDRES-PTNUM (IDXL)
           MOVE    I001-KANANAME   TO  HSFINDRES-KANANAME (IDXL)
           MOVE    I001-NAME       TO  HSFINDRES-NAME (IDXL)
           MOVE    I001-SEX        TO  HSFINDRES-SEX (IDXL)
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSFINDRES-BIRTHDAY (IDXL)
      *
           MOVE    I001-BTUNUM     TO  HSFINDRES-BTUNUM (IDXL)
      *
           MOVE    I001-BTUNUM     TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  HSFINDRES-BTUNAME (IDXL)
      *
           IF    ( I001-BRMNUM(3:) IS  NUMERIC )
               MOVE    FUNCTION NUMVAL(I001-BRMNUM (3:))
                                   TO  WRK-BRMNUM-Z
               MOVE    FUNCTION TRIM(WRK-BRMNUM-Z)
                                   TO  HSFINDRES-BRMNUM (IDXL)
           ELSE
               MOVE    I001-BRMNUM(3:)
                                  TO   HSFINDRES-BRMNUM (IDXL)
           END-IF
      *
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  WRK-HKNCOMBINUM
           MOVE    PTINF-HOME-TEL1 TO  HSFINDRES-TEL (IDXL)
           MOVE    PTINF-HOME-POST TO  HSFINDRES-ZIPCODE (IDXL)
           MOVE    PTINF-HOME-ADRS TO  HSFINDRES-ADD1 (IDXL)
           MOVE    PTINF-HOME-BANTI TO HSFINDRES-ADD2 (IDXL)
      *
           PERFORM 200311-HOKEN-EDIT-SEC
      *
           MOVE    I001-NYUINYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSFINDRES-NYUINYMD (IDXL)
      *
           IF    ( I001-TAIINYMD   =   "99999999" )
               CONTINUE
           ELSE
               MOVE    I001-TAIINYMD   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  HSFINDRES-TAIINYMD (IDXL)
           END-IF
      *
           MOVE    I001-TAIINCD    TO  HSFINDRES-TAIINCD (IDXL)
      *
           MOVE    I001-TAIINCD    TO  WRK-CMB-CD
           PERFORM 4200-TAIINCDLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  HSFINDRES-TAIINNAME (IDXL)
      *
           MOVE    I001-NYUINKA    TO  HSFINDRES-SRYKA (IDXL)
      *
           MOVE    I001-NYUINKA    TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  HSFINDRES-SRYKANAME (IDXL)
      *
           .
       200311-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       200311-HOKEN-EDIT-SEC           SECTION.
      *
           MOVE    WRK-HKNCOMBINUM TO  HSFINDRES-HKNCOMBI (IDXL)
      *
           INITIALIZE                  ORCSAPIHKNAREA
           MOVE    PTSRH-PTID      TO  SPA-PTID
           MOVE    WRK-HKNCOMBINUM TO  ORCSAPIHKN-IN-HKNCOMBI
           MOVE    "2"             TO  ORCSAPIHKN-KBN
           CALL    "ORCSAPIHKN"    USING
                                   SPA-AREA
                                   ORCSAPIHKNAREA
           MOVE    ZERO            TO  SPA-PTID
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   20 )
                     OR  ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                   =   SPACE  )
               IF    ( WRK-HKNCOMBINUM
                                   =   ORCSAPIHKN-THKNCOMBI (IDX1))
      *
                   MOVE    ORCSAPIHKN-HKNNUM (IDX1)
                                   TO  HSFINDRES-HKNNUM (IDXL)
                   MOVE    ORCSAPIHKN-HKNJANUM (IDX1)
                                   TO  HSFINDRES-HKNJANUM (IDXL)
                   MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX1)
                                   TO  HSFINDRES-HKNNAME (IDXL)
                   MOVE    ORCSAPIHKN-CONTKBN  (IDX1)
                                   TO  HSFINDRES-CONTKBN (IDXL)
                   MOVE    ORCSAPIHKN-HOJOKBN  (IDX1)
                                   TO  HSFINDRES-HOJOKBN (IDXL)
                   MOVE    ORCSAPIHKN-HONKZKKBN  (IDX1)
                                   TO  HSFINDRES-HONKZKKBN (IDXL)
                   MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX1)
                                   TO  HSFINDRES-HIHKNJANAME (IDXL)
                   MOVE    ORCSAPIHKN-TEKSTYMD  (IDX1)
                                   TO  HSFINDRES-TEKSTYMD (IDXL)
                   MOVE    ORCSAPIHKN-TEKEDYMD  (IDX1)
                                   TO  HSFINDRES-TEKEDYMD (IDXL)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL     ( IDX2    >   4 )
                       MOVE    ORCSAPIHKN-KOHNUM (IDX1 IDX2)
                                   TO  HSFINDRES-KOHNUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX1 IDX2)
                                   TO  HSFINDRES-KOHNAME (IDXL IDX2)
                       MOVE    ORCSAPIHKN-FTNJANUM (IDX1 IDX2)
                                   TO  HSFINDRES-FTNJANUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-JKYSNUM (IDX1 IDX2)
                                   TO  HSFINDRES-JKYSNUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX1 IDX2)
                                   TO  HSFINDRES-KOH-TEKSTYMD(IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX1 IDX2)
                                   TO  HSFINDRES-KOH-TEKEDYMD(IDXL IDX2)
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200311-HOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科リストチェック処理
      *****************************************************************
       4200-SRYKALST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-SRYKALST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-SRYKAL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-SRYKAMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号リストチェック処理
      *****************************************************************
       4200-BTUNUMLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-BTUNUMLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-BTUNUML (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-BTUNUMMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BTUNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態リストチェック処理
      *****************************************************************
       4200-JOTAILST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-JOTAILST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-JOTAIL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-JOTAIMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-JOTAILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求状態リストチェック処理
      *****************************************************************
       4200-SEIKYUJOTAILST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-SEIKYUJOTAILST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-SEIKYUJOTAIL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-SEIKYUJOTAIMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SEIKYUJOTAILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者リストチェック処理
      *****************************************************************
       4200-TSTPTLST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-TSTPTLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-TSTPTL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-TSTPTMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TSTPTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由リストチェック処理
      *****************************************************************
       4200-TAIINCDLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-TAIINCDLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-TAIINCDL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-TAIINCDMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TAIINCDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡区分リストチェック処理
      *****************************************************************
       4200-DEATHKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-DEATHKBNLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-DEATHKBNL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-DEATHKBNMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-DEATHKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順リストチェック処理
      *****************************************************************
       4200-SORTLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDXC     FROM    1   BY  1
                   UNTIL ( IDXC     >   I2SUB03-SORTLST-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( I2SUB03-SORTL (IDXC)
                                       =   WRK-CMB-CD )
                   MOVE    I2SUB03-SORTMEIL (IDXC)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SORTLST-CHK-EXT.
           EXIT.
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
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
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
      *    リスト項目取得処理
      *****************************************************************
       800-ORCGI2SUB03-SEC              SECTION.
      *
           INITIALIZE                      I2SUB03-AREA
           MOVE    "API"               TO  I2SUB03-FROM
           MOVE    REQ-BASEYMD         TO  I2SUB03-BASEYMD
           CALL    "ORCGI2SUB03"   USING   I2SUB03-AREA
                                           SPA-AREA
      *
           .
      *
       800-ORCGI2SUB03-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGI2ERRMSG"  USING
                                   SPA-AREA
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSFINDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  HSFINDRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  HSFINDRES-API-RESULT
           ELSE
               MOVE    "E"         TO  HSFINDRES-API-RESULT
               MOVE    SPA-ERRCD   TO  HSFINDRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG      TO  HSFINDRES-API-RESULT-MSG
      *
           MOVE    WRK-TIME        TO  HSFINDRES-INFORMATION-TIME
      *
           MOVE    "xml_hsfindv3res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  HSFINDRES-MODE
           MOVE    "hsfindv3res"       TO  HSFINDRES-RECORDNAME
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           XML-HSFINDRES
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    HSFINDRES-OBJECT    TO  I2API01-BODY
               MOVE    "application/xml"   TO  I2API01-CONTENT-TYPE
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
      *    検索文字エスケープ処理
      *****************************************************************
       900-PTSRH-DBESCAPE-SEC          SECTION.
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBESCAPE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    MCPDATA-REC     TO  PTSRH-REC
      *
           .
      *
       900-PTSRH-DBESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    "%"             TO  PTSRH-UPYMD
           MOVE    "%"             TO  PTSRH-UPHMS
           STRING  " ORDER BY SEQNUM " DELIMITED BY SIZE
                   " LIMIT "           DELIMITED BY SIZE
                   WRK-RANGE-LIMIT     DELIMITED BY SIZE
                   " OFFSET "          DELIMITED BY SIZE
                   WRK-RANGE-OFFSET    DELIMITED BY SIZE
           INTO    PTSRH-SQL
           END-STRING
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
           ELSE
               MOVE    1           TO  FLG-PTSRH
               INITIALIZE              PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       900-PTSRH-ODR-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-CNT-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTSRH
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    "%"             TO  PTSRH-UPYMD
           MOVE    "%"             TO  PTSRH-UPHMS
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "cnt"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
           ELSE
               MOVE    1           TO  FLG-PTSRH
               INITIALIZE              PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "cnt"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTSRH-CNT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴情報検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO   FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM        TO   PTNYUINRRK-HOSPNUM
           MOVE    I001-PTID          TO   PTNYUINRRK-PTID
           MOVE    I001-RRKNUM        TO   PTNYUINRRK-RRKNUM
           MOVE    I001-RRKEDANUM-MAX TO   PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC     TO   MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"   TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.

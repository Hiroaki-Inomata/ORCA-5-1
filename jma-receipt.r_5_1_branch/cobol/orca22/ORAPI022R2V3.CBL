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
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI022R2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    :  API連携用モジュール
      *  コンポーネント名  : 患者病名登録(xml2)
      *                      補足コメントをdiseasegevt2の並びと名称にあわせる
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/08/29    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/05/01  急性フラグの追加
      *                                     傷病名コードを２１に変更
      *  05.00.02    NACL-藤原    18/08/21  病名からの疑いフラグの設定
      *  05.00.03    NACL-藤原    18/10/05  中止の追加
      *                                     削除時の条件変更
      *  05.00.04    NACL-藤原    19/09/10  更新時のエラー追加
      *  05.00.05    NACL-藤原    20/02/14  第三者行為の同一病名チェック
      *                                     の条件追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-PTBYOMEI-UMU    PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-HOSOKU-CHK      PIC 9(01).
      *
      *    0:正常 1:エラー  2:単独病名エラー
      *    3:コード順エラー 4:修飾語のみ
      *    5:未コード化傷病名コード
      *    6:単独病名エラー（病名）
      *    9:同一病名重複
           03  FLG-BYOMEI-CHK      PIC 9(01).
           03  FLG-BYOMEI-DELETE   PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-DABURI-CHK      PIC 9(01).
           03  FLG-DIAG            PIC 9(01).
           03  FLG-PTBYOMEI-OK     PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-BYOMEI-HEN      PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDXX                PIC 9(04).
      *
           03  PTBYO-CNT           PIC 9(02).
           03  PTBYOMEI-SRYKA-CNT  PIC 9(02).
           03  PTBYOMEI-HKNCOMBI-CNT
                                   PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(04).
           03  CNT-DIAG            PIC 9(04).
           03  CNT-DELETE          PIC 9(04).
           03  CNT-PTBYOMEI-KOHID  PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SMCNDATE-DATE   PIC X(10).
           03  WRK-SMCNDATE-TIME   PIC X(08).
      *
           03  WRK-PTID            PIC 9(10).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-BASE-MONTH      PIC X(07).
      *
           03  WRK-DISEASEQ-AREA.
               05  WRK-NYUGAIKBN   PIC X(01).
      *
               05  WRK-BYOYMD.
                   07  WRK-BYOYY   PIC X(04).
                   07  WRK-BYOMM   PIC X(02).
                   07  WRK-BYODD   PIC X(02).
               05  WRK-TENKIYMD.
                   07  WRK-TENKIYY PIC X(04).
                   07  WRK-TENKIMM PIC X(02).
                   07  WRK-TENKIDD PIC X(02).
               05  WRK-TENKI       PIC X(01).
               05  WRK-SYUBYOFLG   PIC X(01).
               05  WRK-UTAGAIFLG   PIC X(01).
               05  WRK-UTAGAIFLG-KETASU
                                   PIC 9(02).
      *
               05  WRK-KARTE-NAME  PIC X(80).
               05  WRK-MANSEIKBN   PIC 9(2).
               05  WRK-HKNCOMBI    PIC 9(4).
               05  WRK-HKNCOMBI-DELKBN
                                   PIC X(01).
               05  WRK-REZEFLG     PIC X(1).
               05  WRK-REZEMM      PIC 9(2).
               05  WRK-HKNBYOMEIFLG
                                   PIC X(1).
               05  WRK-DISCHARGEFLG
                                   PIC X(1).
               05  WRK-CLASS1      PIC 9(2).
               05  WRK-CLASS2      PIC 9(2).
               05  WRK-KOHID       PIC 9(10).
               05  WRK-KOHID-DELKBN
                                   PIC X(01).
      *
               05  WRK-HOSOKU-TBL.
                   07  WRK-HOSOKU-G    OCCURS  3.
                       09  WRK-THOSOKUCD
                                   PIC X(10).
                       09  WRK-THOSOKU-COMMENT
                                   PIC X(40).
               05  WRK-HOSOKU-COMMENT
                                   PIC X(40).
               05  WRK-HOSOKU-COMMENT1
                                   PIC X(40).
      *
               05  WRK-MANSEIKBN-X PIC X(4).
               05  WRK-DISEASEQ-AREA-X.
                   07  WRK-KARTE-NAME-X
                                       PIC X(4).
                   07  WRK-HKNCOMBI-X  PIC X(4).
                   07  WRK-REZEFLG-X   PIC X(4).
                   07  WRK-REZEMM-X    PIC X(4).
                   07  WRK-HKNBYOMEIFLG-X
                                       PIC X(4).
                   07  WRK-DISCHARGEFLG-X
                                       PIC X(4).
                   07  WRK-CLASS1-X    PIC X(4).
                   07  WRK-CLASS2-X    PIC X(4).
      *
           03  WRK-BYOMEI-TBL. 
               05  WRK-BYOMEICD-G.
                   07  WRK-TBYOMEICD   PIC X(10)   OCCURS  22.
               05  WRK-SPICOD-G.
                   07  WRK-TSPICOD     PIC X(10)   OCCURS  22.
               05  WRK-BYOMEI-G    OCCURS  22.
                   07  WRK-TBYOMEI     PIC X(100).
                   07  WRK-TTANDOKUKBN PIC 9(02).
                   07  WRK-TUTAGAIFLG  PIC X(01).
           03  WRK-BYOMEICD        PIC X(07).
           03  WRK-BYOMEI.
               05  WRK-BYOMEI1     PIC X(80).
               05  WRK-BYOMEI2     PIC X(20). 
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-TANDOKUKBN      PIC 9(01).
           03  WRK-TOKSKNCD        PIC 9(02).
           03  WRK-RECECD-TBL.
               05  WRK-RECECD      PIC X(01)   OCCURS  3.
           03  WRK-MOJISU          PIC 9(03).
           03  WRK-POSITION        PIC 9(02).
           03  WRK-POSITION-X      REDEFINES   WRK-POSITION
                                   PIC X(02).
           03 WRK-DISEASES-SPICOD1 PIC X(155).
      *
           03  WRK-DIAG-AREA.
               05  WRK-DIAG-POSITION
                                   PIC 9(02).
               05  WRK-DIAG-AREA-X.
                   07  WRK-DIAG-WARNING
                                   PIC X(03).
                   07  WRK-DIAG-MSG
                                   PIC X(100).
                   07  WRK-DIAG-CHANGE
                                   PIC X(02).
                   07  WRK-DIAG-CODE
                                   PIC X(155).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
           03  WRK-ERR-SRYKA       PIC X(20).
      *
           03  WRK-COL             PIC 9(03).
      *
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *
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
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WARNING             PIC X(03).
           03  WRK-WARNING-MSG         PIC X(100).
      *
           03  WRK-PTBYO-MCP-PATHNAME  PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
       01  WRK-DISEASEQ-INFORMATION-AREA.
           03  WRK-DISEASEQ-MAX        PIC 9(02).
           03  WRK-DISEASEQ-INFORMATION-OCC    OCCURS  50.
               05  WRK-DISEASEQ-SRYYMD     PIC X(08).
               05  WRK-DISEASEQ-NYUGAIKBN  PIC X(01).
               05  WRK-DISEASEQ-BYOMEI     PIC X(80).
               05  WRK-DISEASEQ-HOSOKU-COMMENT
                                           PIC X(40).
               05  WRK-DISEASEQ-HKNCOMBI   PIC 9(04).
               05  WRK-DISEASEQ-HKNCOMBI-X PIC X(04).
               05  WRK-DISEASEQ-KOHID      PIC 9(04).
               05  WRK-DISEASEQ-TENKIYMD   PIC X(08).
               05  WRK-DISEASEQ-TENKIKBN   PIC X(01).
               05  WRK-DISEASEQ-SYUBYOFLG  PIC X(01).
               05  WRK-DISEASEQ-UTAGAIFLG  PIC X(01).
               05  WRK-DISEASEQ-KYUSEIFLG  PIC X(01).
               05  WRK-DISEASEQ-KARTE-NAME PIC X(80).
               05  WRK-DISEASEQ-MANSEIKBN  PIC 9(2).
               05 WRK-DISEASEQ-INSURANCE-CLASS
                                           PIC X(01).
      *
      *    保険組合せ、第三者行為重複チェック用エリア
       01  WRK-CHK-HKNCOMBI-AREA.
           02  WRK-CHK-HKNCOMBI1   PIC 9(04).
           02  WRK-CHK-HKNCOMBI2   PIC 9(04).
      *
           02  WRK-CHK-KOHID1      PIC 9(10).
           02  WRK-CHK-KOHID2      PIC 9(10).
      *
           02  FLG-HKNCOMBI-DABURI PIC 9(01).
           02  WRK-CHK-HKNCOMBI-REC    OCCURS  2.
               COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                           BY         //WRK-CHK-COMB-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    最小診療科病名情報
       01  WRK-PTBYOMEI-SRYKA-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO-SRYKA//.
      *
      *    病名情報チェック
       01  CHK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //CHK-PTBYO//.
      *    同一病名情報（診療科、入外区分、保険組合せ）
       01  OK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //OK-PTBYO//.
      *
      *    更新対象病名情報
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"  REPLACING  //PTBYO//
                                     BY         //WRK-PTBYO//.
      *
       01  WRK-CONS-AREA.
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *
      *    未コード化傷病名コード
           03  WRK-CONS-BYOMEICD   PIC X(07)   VALUE   "0000999".
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    自院病名
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    同義語
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    スパ領域
           COPY    "COMMON-SPA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPDISEASEV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPDISEASEV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
           COPY    "MCPDATA2.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
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
           DISPLAY   "**********************"
           DISPLAY   "*  Diseasev3  start  *"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
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
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST03-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*  Diseasev3   end   *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-SMCNDATE-DATE
           MOVE    WRK-HEN-TIME        TO  WRK-SMCNDATE-TIME
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
           INITIALIZE                      XML-DISEASERES
      *
           MOVE   "Acceptance_Info"    TO  DISEASES-RESKEY
      *
           MOVE    WRK-SMCNDATE-DATE   TO  DISEASES-INFORMATION-DATE
           MOVE    WRK-SMCNDATE-TIME   TO  DISEASES-INFORMATION-TIME
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
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
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"            TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    システム管理取得（患者番号構成管理情報）
           INITIALIZE                  SYS-1009-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1009-REC
               INITIALIZE                      SYS-1009-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-JIYKSIKBN
                                           TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                           TO  SPA-1009-JIYKSIKETA
      *        拡張構成区分
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           ELSE
               MOVE   "E38"            TO  WRK-ERRCD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           IF      DISEASEQ-PERFORM-DATE
                                   NOT =   SPACE
               MOVE    DISEASEQ-PERFORM-DATE
                                           TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-SYMD            TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-SYSYMD
                   MOVE    DISEASEQ-PERFORM-DATE
                                               TO  DISEASES-PERFORM-DATE
               END-IF
           END-IF
           IF      DISEASES-PERFORM-DATE
                                       =   SPACE
               MOVE    WRK-SMCNDATE-DATE   TO  DISEASES-PERFORM-DATE
           END-IF
      *
           IF      DISEASEQ-PERFORM-TIME
                                       =   SPACE
               MOVE    WRK-SMCNDATE-TIME   TO  DISEASES-PERFORM-TIME
           ELSE
               MOVE    DISEASEQ-PERFORM-TIME
                                           TO  DISEASES-PERFORM-TIME
           END-IF
      *
           MOVE    ZERO                TO  DISEASES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
                                           WRK-WARNING
      *??
           DISPLAY "SPA-SYSYMD=" SPA-SYSYMD "#" 
      *??
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
      *
      *??
           DISPLAY "2001=" WRK-ERRCD "#" 
      *??
           IF      WRK-ERRCD           =   SPACE
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "E90"               TO  WRK-ERRCD
                   MOVE    SPACE               TO  WRK-WARNING
                   GO  TO  200-MAIN-EXT
               END-IF
      *
      *        病名更新処理
               PERFORM 2002-DISEASE-SEC
      *        不一致病名編集処理
               PERFORM 2003-DISEASES-UNMATCH-SEC
      *
               PERFORM 990-LOCK-OUT-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   DISPLAY "ロック解除失敗"
               END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-DISEASEREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_diseasev3req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "diseasereq"        TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-DISEASEREQ
      ****     PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
                MOVE   "処理実施終了"  TO  DISEASES-API-RESULT-MSG
           ELSE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  DISEASES-API-RESULT
               MOVE    WRK-ERRMSG      TO  DISEASES-API-RESULT-MSG
           END-IF
      *
           IF      APILST03-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_diseasev3res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  DISEASES-MODE
           MOVE    "diseaseres"        TO  DISEASES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DISEASERES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  DISEASES-OBJECT      TO  APILST03-BODY
               MOVE  "application/xml"    TO  APILST03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    基準月
           IF      DISEASEQ-BASE-MONTH =   SPACE
               MOVE    WRK-SMCNDATE-DATE (1:7)
                                           TO  DISEASEQ-BASE-MONTH
               MOVE    SMCNDATE-YMD (1:6)  TO  WRK-BASE-MONTH
           ELSE
               MOVE    DISEASEQ-BASE-MONTH TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD (1:6)      TO  WRK-BASE-MONTH
      *        暦日チェック
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-BASE-MONTH      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                    MOVE    SGDAY-SDAY (1:6)    TO  WRK-BASE-MONTH
               ELSE
                   MOVE    WRK-SMCNDATE-DATE (1:7)
                                               TO  DISEASEQ-BASE-MONTH
                   MOVE    SMCNDATE-YMD (1:6)  TO  WRK-BASE-MONTH
               END-IF
           END-IF
           MOVE    DISEASEQ-BASE-MONTH TO  DISEASES-BASE-MONTH
      *
      *    患者番号
           IF      DISEASEQ-PATIENT-ID =   SPACE
               MOVE    "E01"              TO  WRK-ERRCD
           ELSE
               DISPLAY "患者番号 = ["    DISEASEQ-PATIENT-ID "]"
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    DISEASEQ-PATIENT-ID TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC        NOT =   ZERO
                   MOVE    "E10"              TO  WRK-ERRCD
                   DISPLAY  "PTNUM ERR SPA-HOSPNUM = ["
                                                SPA-HOSPNUM "]"
                   DISPLAY  "PTNUM ERR PTID        = ["
                                                DISEASEQ-PATIENT-ID "]"
               ELSE
                   MOVE    SPTID-PTID          TO  WRK-PTID
               END-IF
           END-IF
           MOVE    DISEASEQ-PATIENT-ID TO  DISEASES-PATIENT-ID
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    診療科
          IF      DISEASEQ-DEPARTMENT-CODE
                                       =   SPACE
               MOVE    "01"                TO  WRK-SRYKA
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    DISEASEQ-DEPARTMENT-CODE
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC         NOT =   ZERO )
               OR    ( SNUM-SEISUU         >   2    )
               OR    ( SNUM-SYOSUU         >   0    )
               OR    ( SNUM-MINKBN         =   "1"  )
                   MOVE    "01"                TO  WRK-SRYKA
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  WRK-SRYKA
               END-IF
           END-IF
      *
           DISPLAY "診療科   = ["    WRK-SRYKA "]"
      *
           MOVE    WRK-SRYKA           TO  DISEASES-DPERTMENT-CODE
           PERFORM 5001-SRYKA-HENSYU-SEC
           IF      FLG-SYSKANRI        =   1
               MOVE    "E13"              TO  WRK-ERRCD
               DISPLAY  "SRYKA ERR SRYKA           = ["
                                      DISEASEQ-DEPARTMENT-CODE "]"
           ELSE
               MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
               MOVE    SYS-1005-SRYKANAME  TO  DISEASES-DPERTMENT-NAME
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名更新処理
      *****************************************************************
        2002-DISEASE-SEC                SECTION.
      *
      *****DISPLAY "病名情報 ================== "
      *
           MOVE    ZERO            TO  FLG-DIAG
           INITIALIZE                  WRK-DISEASEQ-INFORMATION-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       50
      *
               INITIALIZE                  WRK-BYOMEI-TBL
                                           WRK-HOSOKU-TBL
                                           FLG-BYOMEI-CHK
                                           WRK-TOKSKNCD
                                           WRK-BYOMEI
                                           WRK-HOSOKU-COMMENT
                                           WRK-DIAG-AREA
      *
               INITIALIZE                  WRK-DISEASEQ-AREA
      *
               IF    ( DISEASEQ-SNDSPITBL(IDX)   =   SPACE )
                   IF    ( DISEASEQ-SPICOD1 (IDX)  =   SPACE )
                   AND   ( DISEASEQ-SPINAME1(IDX)  =   SPACE )
                       MOVE    50                  TO  IDX
                   ELSE
      *                連結式病名編集処理
                       PERFORM 20021-SPICOD1-HENSYU-SEC
                   END-IF
               ELSE
      *            単独式病名編集処理
                   PERFORM 20022-SPICOD2-HENSYU-SEC
                END-IF
           END-PERFORM
      *
      *    コード、病名が設定されていないとき
           IF      FLG-BYOMEI-HEN      =   ZERO
               MOVE    2                   TO  FLG-DIAG
           END-IF
      *
           EVALUATE    FLG-DIAG
               WHEN    1
                   MOVE    "E40"           TO  WRK-ERRCD
               WHEN    2
                   MOVE    "E41"           TO  WRK-ERRCD
               WHEN    9
                   MOVE    "E42"           TO  WRK-ERRCD
           END-EVALUATE
           .
        2002-DISEASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    連結式病名編集処理
      *****************************************************************
       20021-SPICOD1-HENSYU-SEC                 SECTION.
      *
           DISPLAY "--------------------------- "
           DISPLAY "IDX=" IDX
      *
           DISPLAY " diagnosis code =" DISEASEQ-SPICOD1 (IDX)
           DISPLAY " diagnosis name =" DISEASEQ-SPINAME1(IDX)
      *
      *    疾患コードが設定されていないとき
           IF         DISEASEQ-SPICOD1 (IDX)  =   SPACE
               MOVE    1                      TO  FLG-BYOMEI-CHK
           ELSE  
      *        疾患コードを分割する
               UNSTRING   DISEASEQ-SPICOD1 (IDX)  DELIMITED  BY  "."
                                           INTO   WRK-TBYOMEICD (1)
                                                  WRK-TBYOMEICD (2)
                                                  WRK-TBYOMEICD (3)
                                                  WRK-TBYOMEICD (4) 
                                                  WRK-TBYOMEICD (5)
                                                  WRK-TBYOMEICD (6)
                                                  WRK-TBYOMEICD (7)
                                                  WRK-TBYOMEICD (8)
                                                  WRK-TBYOMEICD (9)
                                                  WRK-TBYOMEICD (10)
                                                  WRK-TBYOMEICD (11)
                                                  WRK-TBYOMEICD (12)
                                                  WRK-TBYOMEICD (13)
                                                  WRK-TBYOMEICD (14) 
                                                  WRK-TBYOMEICD (15)
                                                  WRK-TBYOMEICD (16)
                                                  WRK-TBYOMEICD (17)
                                                  WRK-TBYOMEICD (18)
                                                  WRK-TBYOMEICD (19)
                                                  WRK-TBYOMEICD (20)
                                                  WRK-TBYOMEICD (21)
                                                  WRK-TBYOMEICD (22)
               END-UNSTRING
           END-IF
      *
      *    ２２個以上のコードが設定されているとき
           IF      WRK-TBYOMEICD (22)  NOT =   SPACE
               MOVE    1                   TO  FLG-BYOMEI-CHK
           END-IF
      *
      *    病名コードチェック
           PERFORM 200211-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               MOVE    DISEASEQ-SPINAME1 (IDX) TO  WRK-BYOMEI
           END-IF
      *
      *    患者病名マスタ作成
           PERFORM 20023-BYOMEI-HEN-SEC
           .
       20021-SPICOD1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独式病名編集処理
      *****************************************************************
       20022-SPICOD2-HENSYU-SEC                 SECTION.
      *
           DISPLAY "--------------------------- " 
           DISPLAY "IDX=" IDX 
      *
      *    病名のみ設定のときコードチェック
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       21
                   OR (    DISEASEQ-SPINAME2(IDX IDY) 
                                               =   SPACE
                       AND DISEASEQ-SPICOD2 (IDX IDY)
                                               =   SPACE )
                   OR (    FLG-BYOMEI-CHK      =   1     )
      *
               DISPLAY " IDY="                     IDY
               DISPLAY " diagnosis dxItem code ="
                                 DISEASEQ-SPICOD2 (IDX IDY)
               DISPLAY " diagnosis dxItem name ="
                                 DISEASEQ-SPINAME2(IDX IDY)
      *
               IF      DISEASEQ-SPICOD2 (IDX IDY)
                                           =   SPACE
                   MOVE    1                   TO  FLG-BYOMEI-CHK 
               ELSE
                   MOVE    DISEASEQ-SPICOD2 (IDX IDY)
                                               TO  WRK-TBYOMEICD (IDY)
               END-IF
           END-PERFORM
      *     
      *    病名コードチェック
           PERFORM 200211-BYOMEICD-CHK-SEC
      *
      *    病名コードがエラー又は未コード化傷病名コードのときは疾患名を編集
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               STRING  DISEASEQ-SPINAME2 (IDX 1)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 2)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 3)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 4)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 5)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 6)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 7)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 8)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 9)   DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 10)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 11)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 12)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 13)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 14)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 15)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 16)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 17)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 18)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 19)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 20)  DELIMITED BY SPACE
                       DISEASEQ-SPINAME2 (IDX 21)  DELIMITED BY SPACE
                                                   INTO    WRK-BYOMEI
               END-STRING
           END-IF
      *
      *    患者病名マスタ作成
           PERFORM 20023-BYOMEI-HEN-SEC
           .
       20022-SPICOD2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       200211-BYOMEICD-CHK-SEC         SECTION.
      *
           MOVE    WRK-BYOMEICD-G  TO  WRK-SPICOD-G
      *
      *    補足コメントチェック
              PERFORM 2002111-HOSOKU-COMMENT-CHK-SEC
      *
           IF      FLG-BYOMEI-CHK      =   1
               GO  TO  200211-BYOMEICD-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       21
                   OR      WRK-TBYOMEICD (IDY) =   SPACE
                   OR      FLG-BYOMEI-CHK      =   1
      *
      *        病名コードの半角チェック、長さチェック
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-TBYOMEICD (IDY) TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-RC2         =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "1"  )
                   EVALUATE    KANACHK-MAX
                       WHEN    4
                           MOVE    SPACE       TO  WRK-BYOMEICD
                           MOVE    "ZZZ"       TO  WRK-BYOMEICD (1:3)
                           MOVE    WRK-TBYOMEICD (IDY)(1:4)
                                               TO  WRK-BYOMEICD (4:4)
                          MOVE    WRK-BYOMEICD
                                               TO  WRK-TBYOMEICD (IDY)
                       WHEN    7
                           CONTINUE
                       WHEN    OTHER
                           MOVE    1           TO  FLG-BYOMEI-CHK
                   END-EVALUATE
               ELSE
                   MOVE    1               TO  FLG-BYOMEI-CHK
               END-IF
      *
      *        病名マスタ存在チェック
               IF      FLG-BYOMEI-CHK          =   ZERO
                   IF      WRK-TBYOMEICD (IDY)     =   WRK-CONS-BYOMEICD
                       MOVE    5               TO  FLG-BYOMEI-CHK
                   ELSE
                       IF  (   WRK-TBYOMEICD (IDY) (1:3)   =   "ZZZ"   )
                       AND ( ( WRK-TBYOMEICD (IDY) (4:1)   <   "1" ) OR 
                             ( WRK-TBYOMEICD (IDY) (4:1)   =   "C" )   )
      *                    予約病名コード検索
                           SET     TBL-BYOMEICD-IDX    TO  1
                           SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                               AT  END
                                   MOVE    1       TO  FLG-BYOMEI
                                   MOVE    1       TO  FLG-BYOMEI-CHK
                               WHEN    WRK-TBYOMEICD (IDY) =
                                                   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                   MOVE    ZERO        TO  FLG-BYOMEI
                                   MOVE    TBL-YYK-BYOMEI
                                                      (TBL-BYOMEICD-IDX)
                                                       TO
                                                       WRK-TBYOMEI (IDY)
                           END-SEARCH
                       ELSE
                           INITIALIZE                      BYOMEI-REC
                           MOVE    WRK-TBYOMEICD (IDY) TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI          =   1
                               MOVE    1               TO
                                                       FLG-BYOMEI-CHK
                           ELSE
                               MOVE    BYO-BYOMEI      TO
                                                   WRK-TBYOMEI    (IDY)
                               MOVE    BYO-TANDOKUKBN  TO
                                                   WRK-TTANDOKUKBN(IDY)
                               MOVE    BYO-UTAGAIFLG   TO
                                                   WRK-TUTAGAIFLG (IDY)
                               IF      WRK-TBYOMEICD (IDY) (1:1)
                                                   NOT =   "Z"
                                   IF      BYO-NANBYOCD
                                                       NOT =   ZERO
                                       MOVE    BYO-NANBYOCD
                                                       TO  WRK-TOKSKNCD
                                   ELSE 
                                       MOVE    BYO-TOKSKNCD
                                                       TO  WRK-TOKSKNCD
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    接尾語・接頭語チェック
           IF      FLG-BYOMEI-CHK       =   ZERO
               MOVE    SPACE                TO  WRK-RECECD-TBL
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       21
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
                       OR      FLG-BYOMEI-CHK      =   3
                   EVALUATE    WRK-TBYOMEICD (IDY)(1:3)
                                     ALSO    WRK-TBYOMEICD (IDY)(4:1)
                       WHEN    "ZZZ"         ALSO    "1"  THRU    "7"
                           IF    ( WRK-RECECD (2)    NOT =   SPACE )
                           OR    ( WRK-RECECD (3)    NOT =   SPACE )
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (1)
                           END-IF            
                       WHEN    "ZZZ"       ALSO    "8"  
                           IF      WRK-RECECD (2)    =   SPACE 
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (3)
                           END-IF
                       WHEN    OTHER
                           IF    ( WRK-RECECD (3)    NOT =   SPACE )
                           OR    ( WRK-RECECD (2)    NOT =   SPACE )
                               MOVE    3             TO  FLG-BYOMEI-CHK
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (2)
                           END-IF    
                   END-EVALUATE
               END-PERFORM
               IF    ( FLG-BYOMEI-CHK      =   ZERO  )
               AND   ( WRK-RECECD (2)      =   SPACE )
                   MOVE    4                   TO  FLG-BYOMEI-CHK
                END-IF
           END-IF 
      *
      *    単独使用禁止の病名チェック（補足コメントエラー以外）
           IF    ( FLG-HOSOKU-CHK      =   2     )
           AND   ( WRK-HOSOKU-COMMENT  =   SPACE )
               CONTINUE
           ELSE
               IF      FLG-BYOMEI-CHK      =   ZERO
                   MOVE    ZERO                TO  WRK-TANDOKUKBN
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       21
                           OR      WRK-TBYOMEICD (IDY) =   SPACE
                       IF      WRK-TTANDOKUKBN (IDY)   =   ZERO
                           MOVE    1               TO  WRK-TANDOKUKBN
                       END-IF
                   END-PERFORM
      *
                   IF      WRK-TANDOKUKBN      =   ZERO
                       IF      WRK-HOSOKU-COMMENT  =   SPACE
                           MOVE    2           TO  FLG-BYOMEI-CHK
                       END-IF
                   END-IF
      *
                   IF      FLG-BYOMEI-CHK      =   2
                       MOVE    "W02"               TO  WRK-DIAG-WARNING
                       PERFORM 500-WARNING-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *           
      *    マスタから病名を組み立てる
           MOVE    SPACE                   TO  WRK-BYOMEI
           IF      FLG-BYOMEI-CHK          =   2
               STRING  "＊"                DELIMITED   BY  SIZE
                       WRK-TBYOMEI (1)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (7)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (8)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (9)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (10)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (11)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (12)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (13)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (14)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (15)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (16)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (17)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (18)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (19)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (20)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (21)    DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
               INITIALIZE                      WRK-BYOMEICD-G
               MOVE    WRK-CONS-BYOMEICD   TO  WRK-TBYOMEICD (1) 
           ELSE
               STRING  WRK-TBYOMEI (1)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (2)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (3)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (4)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (5)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (6)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (7)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (8)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (9)     DELIMITED   BY  SPACE
                       WRK-TBYOMEI (10)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (11)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (12)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (13)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (14)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (15)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (16)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (17)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (18)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (19)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (20)    DELIMITED   BY  SPACE
                       WRK-TBYOMEI (21)    DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
               END-STRING
           END-IF
           .
       200211-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       2002111-HOSOKU-COMMENT-CHK-SEC  SECTION.
      *
           DISPLAY " disease_sname ="  DISEASEQ-HOSOKU-COMMENT (IDX)
           DISPLAY " disease_single ="  DISEASEQ-HOSOKUCD-G (IDX)
      *
           MOVE    SPACE               TO  WRK-HOSOKU-COMMENT
           MOVE    ZERO                TO  FLG-HOSOKU-CHK
      *
           IF      DISEASEQ-HOSOKUCD-G (IDX) =   SPACE
               MOVE    DISEASEQ-HOSOKU-COMMENT (IDX)
                                             TO  WRK-HOSOKU-COMMENT
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                       OR      DISEASEQ-HOSOKUCD (IDX IDY) 
                                   =       SPACE
      *
                   DISPLAY " IDY="   IDY
                   DISPLAY " disease_scode ="
                                     DISEASEQ-HOSOKUCD (IDX IDY)
      *
                   MOVE    DISEASEQ-HOSOKUCD (IDX IDY)
                                           TO  WRK-THOSOKUCD (IDY)
               END-PERFORM
      *
      *        補足コメントコードチェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                       OR      WRK-THOSOKUCD (IDY) =   SPACE
                       OR      FLG-HOSOKU-CHK      =   2
                   MOVE    ZERO                TO  FLG-HOSOKU-CHK
      *
      *            病名コードの長さチェック
                   MOVE    ZERO                TO  WRK-LENGTH 
                   PERFORM VARYING IDZ FROM    10  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-THOSOKUCD (IDY)(IDZ:1)
                                               NOT =   SPACE
                           MOVE    IDZ             TO  WRK-LENGTH
                           MOVE    1               TO  IDZ
                       END-IF
                   END-PERFORM
                   EVALUATE    WRK-LENGTH
                       WHEN    4
                       WHEN    7
                           CONTINUE
                       WHEN    OTHER
                           MOVE    1           TO  FLG-HOSOKU-CHK
                   END-EVALUATE
      *
      *            病名マスタチェック
                   IF      FLG-HOSOKU-CHK      =   ZERO
                       INITIALIZE                      BYOMEI-REC
                       EVALUATE    WRK-LENGTH
                           WHEN    4
                               MOVE    SPACE       TO  WRK-BYOMEICD
                               MOVE    "ZZZ"       TO  WRK-BYOMEICD(1:3)
                               MOVE    WRK-THOSOKUCD (IDY)(1:4)
                                                   TO  WRK-BYOMEICD(4:4)
                               MOVE    WRK-BYOMEICD
                                                   TO  BYO-BYOMEICD
                           WHEN    7
                               MOVE    WRK-THOSOKUCD (IDY)
                                                   TO  BYO-BYOMEICD
                       END-EVALUATE
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      FLG-BYOMEI          =   ZERO  
                           MOVE    BYO-BYOMEI      TO
                                               WRK-THOSOKU-COMMENT (IDY)
                           MOVE    BYO-BYOMEICD
                                                   TO
                                               WRK-THOSOKUCD (IDY)
                       ELSE
                           MOVE    1               TO  FLG-HOSOKU-CHK
                       END-IF
                   END-IF
                   IF      FLG-HOSOKU-CHK      =   1
      *                自院病名チェック
                       INITIALIZE              USERBYOMEI-REC
                       MOVE    WRK-THOSOKUCD (IDY)
                                               TO  USBYO-BYOMEIINPUTCD
                       PERFORM 900-USBYO-INV-SEC
                       IF      FLG-USERBYOMEI  =   ZERO
                           MOVE    USBYO-BYOMEI    TO
                                               WRK-THOSOKU-COMMENT (IDY)
                           MOVE    ZERO            TO  FLG-HOSOKU-CHK
                       ELSE
                           MOVE    2               TO  FLG-HOSOKU-CHK
                           INITIALIZE                  WRK-HOSOKU-TBL
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        補足コメントを組み立てる
      *??
                   DISPLAY "FLG-HOSOKU-CHK=" FLG-HOSOKU-CHK
      *??
               MOVE    SPACE                   TO  WRK-HOSOKU-COMMENT
               IF      FLG-HOSOKU-CHK      =   ZERO
                   STRING  WRK-THOSOKU-COMMENT (1) DELIMITED   BY  SPACE
                           WRK-THOSOKU-COMMENT (2) DELIMITED   BY  SPACE
                           WRK-THOSOKU-COMMENT (3) DELIMITED   BY  SPACE
                                           INTO    WRK-HOSOKU-COMMENT
                   END-STRING
               ELSE
                   MOVE    DISEASEQ-HOSOKU-COMMENT (IDX)
                                               TO  WRK-HOSOKU-COMMENT
               END-IF
           END-IF
      *
      *    全角チェック
           IF      WRK-HOSOKU-COMMENT  NOT =   SPACE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    WRK-HOSOKU-COMMENT  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-HOSOKU-COMMENT
               ELSE
                   MOVE    "W05"                TO  WRK-DIAG-WARNING
               END-IF
      *
               IF  WRK-DIAG-WARNING        =   SPACE
      *            改行コードチェック
                   MOVE    ZERO                TO  WRK-COL
                   INSPECT WRK-HOSOKU-COMMENT  TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL             >   ZERO
                       MOVE    "W06"               TO  WRK-DIAG-WARNING
                   END-IF
               END-IF
      *
               IF  WRK-DIAG-WARNING    NOT =   SPACE
                   MOVE    "？？？？？"        TO  WRK-HOSOKU-COMMENT
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
           END-IF
      *
      *    前後の（）をはずす
           IF      WRK-HOSOKU-COMMENT  NOT =   SPACE    
               MOVE    SPACE               TO  WRK-HOSOKU-COMMENT1
               COMPUTE IDY     =   KANACHK-MAX     -   1
               IF      WRK-HOSOKU-COMMENT (IDY:2)
                                               =   "）"
                   COMPUTE IDZ     =   KANACHK-MAX     -   2
                   MOVE    WRK-HOSOKU-COMMENT (1:IDZ)
                                               TO  WRK-HOSOKU-COMMENT1
                   MOVE    WRK-HOSOKU-COMMENT1 TO  WRK-HOSOKU-COMMENT
               END-IF
               MOVE    SPACE               TO  WRK-HOSOKU-COMMENT1
               IF      WRK-HOSOKU-COMMENT (1:2)
                                               =   "（"
                   MOVE    WRK-HOSOKU-COMMENT (3:)
                                               TO  WRK-HOSOKU-COMMENT1
                   MOVE    WRK-HOSOKU-COMMENT1 TO  WRK-HOSOKU-COMMENT
               END-IF
           END-IF 
      *
           DISPLAY " diagnosis supplement ="  WRK-HOSOKU-COMMENT
      *
           .
       2002111-HOSOKU-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ作成処理
      *****************************************************************
       20023-BYOMEI-HEN-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-BYOMEI-HEN
      *
           DISPLAY " diagnosis connectname ="  WRK-BYOMEI
           DISPLAY " diagnosis check flag  ="  FLG-BYOMEI-CHK
      *
      *    病名が空白のとき
           IF      WRK-BYOMEI          =   SPACE
               MOVE    "E33"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  20023-BYOMEI-HEN-EXT
           END-IF
      *
      *    補足コメントコードがエラーのとき
           IF    ( FLG-HOSOKU-CHK      =   2     )
           AND   ( WRK-HOSOKU-COMMENT  =   SPACE )
               MOVE    "E34"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  20023-BYOMEI-HEN-EXT
            END-IF
      *
      *    疾患開始日
           DISPLAY "startDate ="      DISEASEQ-STARTDATE(IDX)
      *
           MOVE    DISEASEQ-STARTDATE(IDX)
                                       TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    WRK-SYMD            TO  WRK-BYOYMD
           ELSE
               MOVE    "E16"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  20023-BYOMEI-HEN-EXT
           END-IF
      *
      *    病名コードがエラー又は未コード化傷病名コードのとき
      *    疾患名から単独使用禁止の病名チェック
           IF      FLG-BYOMEI-CHK      =   1   OR  5
               INITIALIZE                  ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    WRK-BYOYMD          TO  LNK-BYO-SRYYMD-IN
               MOVE    WRK-BYOMEI          TO  LNK-BYO-BYOMEI
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
               IF      LNK-BYO-TANDOKUKBN-ERR  =   1
                       IF      WRK-HOSOKU-COMMENT  =   SPACE
                           MOVE    6               TO  FLG-BYOMEI-CHK
                       END-IF
               END-IF
      *
               IF      FLG-BYOMEI-CHK          =   6
                   MOVE    "W02"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
      *
                   MOVE    SPACE               TO  WRK-BYOMEI
                   STRING  "＊"                DELIMITED   BY  SIZE
                           LNK-BYO-BYOMEI      DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI
                   END-STRING
      *
                   DISPLAY " diagnosis connectname ="  WRK-BYOMEI
                   DISPLAY " diagnosis check flag  ="  FLG-BYOMEI-CHK
               END-IF
           END-IF
      *
      *    病名
      *    全角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI 
           MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-SYUBETU     =   "2"  )
               MOVE    SPACE               TO  WRK-BYOMEI
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-BYOMEI
           ELSE
               MOVE    "W03"               TO  WRK-DIAG-WARNING
           END-IF
      *
           IF  WRK-DIAG-WARNING        =   SPACE
      *        改行コードチェック
               MOVE    ZERO                TO  WRK-COL
               INSPECT WRK-BYOMEI          TALLYING    WRK-COL
                                           FOR ALL WRK-CONS-RETURN
               IF      WRK-COL             >   ZERO
                   MOVE    "W04"               TO  WRK-DIAG-WARNING
               END-IF
           END-IF
      *
           IF      WRK-DIAG-WARNING
                                   NOT =   SPACE
               MOVE    "？？？？？"        TO  WRK-BYOMEI
               PERFORM 500-WARNING-HENSYU-SEC
           END-IF
      *
      *    疾患終了日
           DISPLAY "endDate   ="       DISEASEQ-ENDDATE(IDX)
      *
           IF      DISEASEQ-ENDDATE (IDX)
                                   =   SPACE
               MOVE    SPACE           TO  WRK-TENKIYMD
           ELSE
               MOVE    DISEASEQ-ENDDATE (IDX)
                                       TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-SYMD            TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD            TO  WRK-TENKIYMD
               ELSE
                   MOVE    "E17"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  20023-BYOMEI-HEN-EXT
               END-IF
           END-IF
      *
      *    転帰
           DISPLAY "outcome ="        DISEASEQ-TENKIKBN (IDX)
      *
           MOVE    SPACE               TO  WRK-TENKI
           IF      DISEASEQ-TENKIKBN (IDX)  =   "O"
               MOVE    1                   TO  FLG-BYOMEI-DELETE
           ELSE
               MOVE    0                   TO  FLG-BYOMEI-DELETE
               IF      DISEASEQ-ENDDATE(IDX)    NOT =   SPACE
      *            転帰が空白の時、治癒
                   EVALUATE    DISEASEQ-TENKIKBN (IDX)
                       WHEN    SPACE
                       WHEN    "F"
                           MOVE    "1"              TO  WRK-TENKI
                       WHEN    "D"
                           MOVE    "2"              TO  WRK-TENKI
                       WHEN    "N"
                       WHEN    "R"
                       WHEN    "S"
                       WHEN    "U"
                       WHEN    "W"
                       WHEN    "P"
                           MOVE    "3"              TO  WRK-TENKI
                       WHEN    OTHER
                           MOVE    "1"              TO  WRK-TENKI
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    主病名フラグ
           IF      DISEASEQ-SYUBYOFLG (IDX)
                                   =   "PD"
               MOVE    "1"             TO  WRK-SYUBYOFLG
           END-IF
      *
      *    疑いフラグ
           DISPLAY " SuspectedFlag ="   DISEASEQ-UTAGAIFLG (IDX)
           DISPLAY " AcuteFlag     ="   DISEASEQ-KYUSEIFLG (IDX)
           EVALUATE    DISEASEQ-UTAGAIFLG (IDX)
                                   ALSO    DISEASEQ-KYUSEIFLG (IDX)
               WHEN    "S"         ALSO    SPACE
                   MOVE    "1"             TO  WRK-UTAGAIFLG
               WHEN    SPACE       ALSO    "A"
                   MOVE    "2"             TO  WRK-UTAGAIFLG
               WHEN    "S"         ALSO    "A"
                   MOVE    "3"             TO  WRK-UTAGAIFLG
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-UTAGAIFLG
           END-EVALUATE
      *
      *    入外区分
           DISPLAY " "
           DISPLAY " InOutPatient ="   DISEASEQ-NYUGAIKBN (IDX)
      *
           EVALUATE    DISEASEQ-NYUGAIKBN (IDX)
               WHEN    "I"
                   MOVE    "1"         TO  WRK-NYUGAIKBN
               WHEN    "O"
                   MOVE    "2"         TO  WRK-NYUGAIKBN
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    カルテ病名
           IF      DISEASEQ-KARTE-NAME (IDX)
                                       =   "None"
               MOVE    DISEASEQ-KARTE-NAME (IDX)
                                           TO  WRK-KARTE-NAME-X
           ELSE
               IF      DISEASEQ-KARTE-NAME (IDX)
                                           =   SPACE
                   CONTINUE
               ELSE
      *            全角チェック
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"                 TO  KANACHK-SYORI 
                   MOVE    DISEASEQ-KARTE-NAME (IDX)
                                               TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   ZERO )
                   AND   ( KANACHK-SYUBETU     =   "2"  )
                        MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-KARTE-NAME
                   ELSE
                       MOVE    "W07"               TO  WRK-DIAG-WARNING
                   END-IF
      *
                   IF  WRK-DIAG-WARNING        =   SPACE
      *                改行コードチェック
                       MOVE    ZERO            TO  WRK-COL
                       INSPECT WRK-KARTE-NAME  TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                       IF      WRK-COL         >   ZERO
                           MOVE    "W08"           TO  WRK-DIAG-WARNING
                       END-IF
                   END-IF
      *
                   IF  WRK-DIAG-WARNING    NOT =   SPACE
                       MOVE    "？？？？？"        TO  WRK-KARTE-NAME
                       PERFORM 500-WARNING-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    疾患区分
          IF      DISEASEQ-MANSEIKBN (IDX)
                                       =   "None"  OR  "Auto"
               MOVE    DISEASEQ-MANSEIKBN (IDX)
                                               TO  WRK-MANSEIKBN-X
           ELSE
               EVALUATE    DISEASEQ-MANSEIKBN (IDX)
                   WHEN    "03"
                   WHEN    "04"
                   WHEN    "05"
                   WHEN    "07"
                   WHEN    "08"
                   WHEN    "09"
                       MOVE    DISEASEQ-MANSEIKBN (IDX)
                                                   TO  WRK-MANSEIKBN
                   WHEN    OTHER
                       MOVE    ZERO                TO  WRK-MANSEIKBN
               END-EVALUATE
           END-IF
           DISPLAY " WRK-MANSEIKBN=" WRK-MANSEIKBN-X " " WRK-MANSEIKBN
      *
      *    保険適用（第三者行為）
           IF      DISEASEQ-HKNCOMBI (IDX) =   "None"  OR  SPACE
               IF      DISEASEQ-INSURANCE-CLASS (IDX)
                                           =   "1"
                   MOVE    "E18"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  20023-BYOMEI-HEN-EXT
               ELSE
                   MOVE    DISEASEQ-HKNCOMBI (IDX)
                                               TO  WRK-HKNCOMBI-X
               END-IF
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    DISEASEQ-HKNCOMBI (IDX)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING   ORCSNUMAREA
               IF    ( SNUM-RC         =   ZERO )
               AND   ( SNUM-SEISUU     <   5    )
               AND   ( SNUM-SYOSUU     =   0    )
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                   MOVE    WRK-PTID        TO  COMB-PTID
                   MOVE    SNUM-OUTNUM     TO  COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 900-COMB-READ-SEC
                   IF    ( FLG-HKNCOMBI        =   1  )
      *************OR    ( COMB-DELKBN         =  "1" )
                       MOVE    "E19"           TO  WRK-DIAG-WARNING
                       PERFORM 500-WARNING-HENSYU-SEC
                       GO  TO  20023-BYOMEI-HEN-EXT
                   ELSE
                       MOVE    COMB-DELKBN     TO  WRK-HKNCOMBI-DELKBN
                       IF      DISEASEQ-INSURANCE-CLASS (IDX)
                                               =   "1"
                           IF    ( COMB-HKNNUM     =   "971"   OR
                                                       "973"   OR
                                                       "975" )
                           OR    ( COMB-KOHHKNNUM (1)
                                                   =   "970" )
                               CONTINUE
                           ELSE
                               MOVE    "E20"       TO  WRK-DIAG-WARNING
                               PERFORM 500-WARNING-HENSYU-SEC
                               GO  TO  20023-BYOMEI-HEN-EXT
                           END-IF
                       ELSE
                          IF    ( COMB-HKNNUM      =   "971"   OR
                                                       "973"   OR
                                                       "975" )
                           OR    ( COMB-KOHHKNNUM (1)
                                                   =   "970" )
                               MOVE    "E21"       TO  WRK-DIAG-WARNING
                               PERFORM 500-WARNING-HENSYU-SEC
                               GO  TO  20023-BYOMEI-HEN-EXT
                           END-IF
                       END-IF
      *                開始日と保険組合せ（第三者行為）のチェック
                       IF      COMB-KOHHKNNUM (1)  =   "970"
                           INITIALIZE                      PTRSIINF-REC
                           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
                           MOVE    WRK-PTID            TO  PTRSI-PTID
                           MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
                           MOVE    ZERO                TO  PTRSI-HKNID
                           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                           PERFORM 900-PTRSIINF-READ-SEC
                           IF    ( FLG-PTRSIINF        =   1   )
      *********************OR    ( PTRSI-SAKUJOKBN     =   "1" )
                               MOVE    "E25"       TO  WRK-DIAG-WARNING
                               PERFORM 500-WARNING-HENSYU-SEC
                               GO  TO  20023-BYOMEI-HEN-EXT
                           ELSE
                               MOVE    PTRSI-SAKUJOKBN
                                                   TO  WRK-KOHID-DELKBN
                               IF      WRK-BYOYMD  <=  PTRSI-RYOEDYMD
                                   CONTINUE
                               ELSE
                                   MOVE    "E26"   TO  WRK-DIAG-WARNING
                                   PERFORM 500-WARNING-HENSYU-SEC
                                   GO  TO  20023-BYOMEI-HEN-EXT
                               END-IF
                           END-IF
                       ELSE
                           IF      WRK-BYOYMD  <=  COMB-TEKEDYMD
                               CONTINUE
                           ELSE    
                               MOVE    "E27"       TO  WRK-DIAG-WARNING
                               PERFORM 500-WARNING-HENSYU-SEC
                               GO  TO  20023-BYOMEI-HEN-EXT
                           END-IF
                       END-IF
      *
                       IF      COMB-KOHHKNNUM (1)  =   "970"
                           MOVE    COMB-KOHID (1)      TO  WRK-KOHID
                       ELSE
                           MOVE    COMB-HKNCOMBINUM    TO  WRK-HKNCOMBI
                       END-IF
                   END-IF
               ELSE
                   MOVE    "E22"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  20023-BYOMEI-HEN-EXT
               END-IF
           END-IF
      *
      *    レセプト表示
           IF      DISEASEQ-REZEFLG (IDX)   =   "None"
               MOVE    DISEASEQ-REZEFLG (IDX)   TO  WRK-REZEFLG-X
           ELSE
               IF      DISEASEQ-REZEFLG (IDX)    =   "1"
                   MOVE    DISEASEQ-REZEFLG (IDX)    TO  WRK-REZEFLG
               ELSE
                   MOVE    SPACE               TO  WRK-REZEFLG
               END-IF
           END-IF
      *
      *    表示期間
           IF      DISEASEQ-REZEMM (IDX)   =   "None"
               MOVE    DISEASEQ-REZEMM (IDX)   TO  WRK-REZEMM-X
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    DISEASEQ-REZEMM (IDX)   TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC         =   ZERO )
               AND   ( SNUM-SEISUU     <   3    )
               AND   ( SNUM-SYOSUU     =   0    )
                   MOVE    SNUM-OUTNUM     TO  WRK-REZEMM
               ELSE
                   MOVE    ZERO            TO  WRK-REZEMM
               END-IF
           END-IF
      *
      *    保険病名
           IF      DISEASEQ-HKNBYOMEIFLG (IDX)
                                       =   "None"
               MOVE    DISEASEQ-HKNBYOMEIFLG (IDX)
                                           TO  WRK-HKNBYOMEIFLG-X
           ELSE
               IF      DISEASEQ-HKNBYOMEIFLG (IDX)
                                           =   "1"
                   MOVE    DISEASEQ-HKNBYOMEIFLG (IDX)
                                               TO  WRK-HKNBYOMEIFLG
               ELSE
                   MOVE    SPACE               TO  WRK-HKNBYOMEIFLG
                END-IF
           END-IF
      *
      *    退院証明書
           IF      DISEASEQ-DISCHARGEFLG (IDX)
                                       =   "None"
               MOVE    DISEASEQ-DISCHARGEFLG (IDX)
                                           TO  WRK-DISCHARGEFLG-X
           ELSE
               IF      DISEASEQ-DISCHARGEFLG (IDX)
                                           =   "1" OR  "0"
                   MOVE    DISEASEQ-DISCHARGEFLG (IDX)
                                               TO  WRK-DISCHARGEFLG
               ELSE
                   MOVE    SPACE               TO  WRK-DISCHARGEFLG
               END-IF
           END-IF
      *
      *    原疾患区分
           IF      DISEASEQ-CLASS1 (IDX)
                                       =   "None"
               MOVE    DISEASEQ-CLASS1 (IDX)
                                           TO  WRK-CLASS1-X
           ELSE
               EVALUATE    DISEASEQ-CLASS1 (IDX)
                   WHEN    "01"
                   WHEN    "02"
                   WHEN    "03"
                   WHEN    "04"
                   WHEN    "05"
                       MOVE    DISEASEQ-CLASS1 (IDX)
                                                   TO  WRK-CLASS1
                   WHEN    OTHER
                       MOVE    ZERO                TO  WRK-CLASS1
               END-EVALUATE
           END-IF
      *    合併症区分
           IF      DISEASEQ-CLASS2 (IDX)
                                       =   "None"
               MOVE    DISEASEQ-CLASS2 (IDX)
                                           TO  WRK-CLASS2-X
           ELSE
               EVALUATE    DISEASEQ-CLASS2 (IDX)
                   WHEN    "01"
                   WHEN    "02"
                   WHEN    "03"
                   WHEN    "04"
                   WHEN    "05"
                       MOVE    DISEASEQ-CLASS2 (IDX)
                                                   TO  WRK-CLASS2
                   WHEN    OTHER
                       MOVE    ZERO                TO  WRK-CLASS2
               END-EVALUATE
           END-IF
      *
      *    更新処理
           INITIALIZE                  FLG-PTBYOMEI-UMU
                                       WRK-PTBYOMEI-SRYKA-REC
                                       WRK-UTAGAIFLG-KETASU
                                       PTBYOMEI-SRYKA-CNT
      *
      *    「の疑い」を検索
           PERFORM VARYING IDY FROM    1  BY  2
                   UNTIL   IDY >       75
               IF      WRK-BYOMEI1 (IDY:6)  =   "の疑い"
                   MOVE    IDY              TO  WRK-UTAGAIFLG-KETASU
                   MOVE    80               TO  IDY
               END-IF
           END-PERFORM
      *
      *    患者病名削除処理
           IF      FLG-BYOMEI-DELETE   =   1
               PERFORM 200231-PTBYO-DELETE-SEC
               GO  TO  20023-BYOMEI-HEN-EXT
           END-IF
      *
      *    患者病名検索処理（完全一致）
           PERFORM 200232-BYOMEI-CHK-SEC
      *
      *    医保分に対して第三者行為が複数存在するとき
           IF      CNT-PTBYOMEI-KOHID  >   1
               MOVE    "E37"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  20023-BYOMEI-HEN-EXT
           END-IF
      *
           DISPLAY "FLG-PTBYOMEI-OK=" FLG-PTBYOMEI-OK "#" 
      *
      *    完全一致する病名があるとき
           IF      FLG-PTBYOMEI-OK     =   1
      *        重複チェック
               MOVE    OK-PTBYOMEI-REC     TO  CHK-PTBYOMEI-REC
               PERFORM 200233-BYOMEI-DABURI-CHK-SEC
               IF      FLG-BYOMEI-CHK      =   9
                   CONTINUE
               ELSE
      *            患者病名更新処理
                   MOVE    OK-PTBYOMEI-REC     TO  PTBYOMEI-REC
                   PERFORM 200234-KOUSIN-02-SEC
               END-IF
               GO  TO  20023-BYOMEI-HEN-EXT
           END-IF
      *
      *    患者病名マスタ検索（同一開始日・病名）
           INITIALIZE                  WRK-PTBYOMEI-SRYKA-REC
                                       PTBYOMEI-SRYKA-CNT
                                       PTBYOMEI-HKNCOMBI-CNT
                                       FLG-PTBYOMEI-OK
                                       OK-PTBYOMEI-REC
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           IF      WRK-UTAGAIFLG-KETASU    =   ZERO
               MOVE    WRK-BYOMEI1             TO  PTBYO-BYOMEI
               STRING  WRK-BYOMEI1             DELIMITED BY  SPACE
                       "の疑い"                DELIMITED BY  SIZE  
                                               INTO    PTBYO-XXBYOMEI
               END-STRING
           ELSE
               IF      WRK-UTAGAIFLG-KETASU    =   1
                   MOVE    WRK-BYOMEI1             TO  PTBYO-BYOMEI
                                                       PTBYO-XXBYOMEI
               ELSE
                   MOVE    WRK-BYOMEI1             TO  PTBYO-BYOMEI
                   STRING  WRK-BYOMEI1 (1:WRK-UTAGAIFLG-KETASU - 1)
                                                   DELIMITED BY  SIZE
                           WRK-BYOMEI1 (WRK-UTAGAIFLG-KETASU + 6:)
                                                   DELIMITED BY  SPACE
                                                   INTO   PTBYO-XXBYOMEI
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM             UNTIL   FLG-PTBYOMEI        =   1
               MOVE    ZERO            TO  FLG-PTBYOMEI-OK
      *        同一診療科の病名情報を取得
               IF      PTBYO-SRYKA         =   WRK-SRYKA
                   ADD     1               TO  PTBYOMEI-SRYKA-CNT
                   MOVE    PTBYOMEI-REC    TO  WRK-PTBYOMEI-SRYKA-REC
      *            同一診療科で保険組合せが同じ時
                   IF    ( WRK-HKNCOMBI    =   PTBYO-HKNCOMBI  )
                   AND   ( WRK-KOHID       =   PTBYO-KOHID     )
                       ADD     1           TO  PTBYOMEI-HKNCOMBI-CNT
                       MOVE    1           TO  FLG-PTBYOMEI-OK
                       MOVE    PTBYOMEI-REC
                                           TO  OK-PTBYOMEI-REC
                   ELSE
      *               医保分のとき
                       IF      DISEASEQ-INSURANCE-CLASS (IDX)
                                           NOT =   "1"
                           IF      PTBYO-HKNCOMBI  =   ZERO
                               ADD     1       TO  PTBYOMEI-HKNCOMBI-CNT
                               MOVE    1       TO  FLG-PTBYOMEI-OK
                               MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                           ELSE
                               MOVE    SPA-HOSPNUM
                                                   TO  COMB-HOSPNUM
                               MOVE    WRK-PTID    TO  COMB-PTID
                               MOVE    PTBYO-HKNCOMBI
                                                   TO  COMB-HKNCOMBINUM
                               MOVE    HKNCOMBI-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-COMB-READ-SEC
                               IF    ( COMB-HKNNUM =   "971"   OR
                                                       "973"   OR
                                                       "975" )
                               OR    ( COMB-KOHHKNNUM (1)
                                                   =   "970" )
                                   CONTINUE
                               ELSE
                                   IF    ( PTBYOMEI-HKNCOMBI-CNT
                                                   =   ZERO            )
                                   OR    ( WRK-NYUGAIKBN
                                                   =   PTBYO-NYUGAIKBN )
                                       MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                                   END-IF
                                   ADD     1   TO  PTBYOMEI-HKNCOMBI-CNT
                                   MOVE    1   TO  FLG-PTBYOMEI-OK
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *??
           DISPLAY "PTBYOMEI-HKNCOMBI-CNT=" PTBYOMEI-HKNCOMBI-CNT "#" 
           DISPLAY "PTBYOMEI-SRYKA-CNT=" PTBYOMEI-SRYKA-CNT "#"
      *??
      *
           EVALUATE    PTBYOMEI-HKNCOMBI-CNT
               WHEN    ZERO
      *            同一保険組合せが存在しないとき
                   INITIALIZE           CHK-PTBYOMEI-REC
                   PERFORM 200233-BYOMEI-DABURI-CHK-SEC
                   IF      FLG-BYOMEI-CHK      =   9
                       CONTINUE
                   ELSE
                       PERFORM 200235-KOUSIN-01-SEC
                   END-IF
               WHEN    1
                   IF    ( WRK-NYUGAIKBN       =   OK-PTBYO-NYUGAIKBN )
                   OR    ( WRK-NYUGAIKBN       =   SPACE              )
                   OR    ( OK-PTBYO-NYUGAIKBN  =   SPACE              )
      *                重複チェック
                       MOVE    OK-PTBYOMEI-REC     TO  CHK-PTBYOMEI-REC
                       PERFORM 200233-BYOMEI-DABURI-CHK-SEC
                       IF      FLG-BYOMEI-CHK      =   9
                           CONTINUE
                       ELSE
      *                    患者病名更新処理
                           MOVE    OK-PTBYOMEI-REC     TO  PTBYOMEI-REC
                           PERFORM 200234-KOUSIN-02-SEC
                       END-IF
                   ELSE
      *                同一保険組合せで入外区分が違うとき
                       INITIALIZE           CHK-PTBYOMEI-REC
                       PERFORM 200233-BYOMEI-DABURI-CHK-SEC
                       IF      FLG-BYOMEI-CHK      =   9
                           CONTINUE
                       ELSE
      *                    患者病名追加処理
                           PERFORM 200235-KOUSIN-01-SEC
                       END-IF
                   END-IF
               WHEN    2
                   IF      WRK-NYUGAIKBN       =   SPACE
      *                入外で存在するときに空白での更新はなし
                       MOVE    "E23"               TO  WRK-DIAG-WARNING
                       PERFORM 5001-SRYKA-HENSYU-SEC
                       MOVE    SYS-1005-SRYKANAME1 TO  WRK-ERR-SRYKA
                       PERFORM 500-WARNING-HENSYU-SEC
                   ELSE
                       IF      WRK-NYUGAIKBN   =   OK-PTBYO-NYUGAIKBN
      *                    重複チェック
                           MOVE    OK-PTBYOMEI-REC TO  CHK-PTBYOMEI-REC
                           PERFORM 200233-BYOMEI-DABURI-CHK-SEC
                           IF      FLG-BYOMEI-CHK  =   9
                               CONTINUE
                           ELSE
      *                        患者病名更新処理
                               MOVE    OK-PTBYOMEI-REC TO  PTBYOMEI-REC
                               PERFORM 200234-KOUSIN-02-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    OTHER
      *            同じ保険組合せで３件以上の入力があるとき
                   MOVE    "E24"               TO  WRK-DIAG-WARNING
                   PERFORM 5001-SRYKA-HENSYU-SEC
                   MOVE    SYS-1005-SRYKANAME1 TO  WRK-ERR-SRYKA
                   PERFORM 500-WARNING-HENSYU-SEC
           END-EVALUATE
      *
           .
       20023-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名削除処理
      *****************************************************************
       200231-PTBYO-DELETE-SEC             SECTION.
      *
           INITIALIZE              WRK-PTBYOMEI-SRYKA-REC
                                   FLG-PTBYOMEI-UMU
      *
      *    「の疑い」を検索したとき
           IF      WRK-UTAGAIFLG-KETASU    >   ZERO
               EVALUATE    WRK-UTAGAIFLG
                   WHEN    SPACE
                   WHEN    "1"
                       MOVE    "1"         TO  WRK-UTAGAIFLG
                   WHEN    "2"
                   WHEN    "3"
                       MOVE    "3"         TO  WRK-UTAGAIFLG
               END-EVALUATE
           END-IF
      *
           IF      WRK-UTAGAIFLG       =   "3" OR  "2"
               CONTINUE
           ELSE
      *        「急性」を検索
               PERFORM VARYING IDY FROM    1   BY  2
                       UNTIL   IDY >       77
                   IF      WRK-BYOMEI1 (IDY:4) =   "急性"
                       EVALUATE    WRK-UTAGAIFLG
                           WHEN    SPACE
                               MOVE    "2"         TO  WRK-UTAGAIFLG
                           WHEN    "1"
                               MOVE    "3"         TO  WRK-UTAGAIFLG
                       END-EVALUATE
                       MOVE    80              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                           PTBYO-XXBYOMEI
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM             UNTIL   FLG-PTBYOMEI        =   1
               IF    ( WRK-SRYKA       =   PTBYO-SRYKA     )
               AND   ( WRK-NYUGAIKBN   =   PTBYO-NYUGAIKBN )
               AND   ( WRK-KOHID       =   PTBYO-KOHID     )
               AND   ( WRK-TENKIYMD    =   PTBYO-TENKIYMD  )
      *********AND   ( WRK-UTAGAIFLG   =   PTBYO-UTAGAIFLG )
                   IF      WRK-HKNCOMBI-X  =   "None"
                       INITIALIZE                  HKNCOMBI-REC
                       MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                       MOVE    WRK-PTID        TO  COMB-PTID
                       MOVE    PTBYO-HKNCOMBI  TO  COMB-HKNCOMBINUM
                       MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                       PERFORM 900-COMB-READ-SEC
      *                医保分以外は対象としない
                       IF    ( COMB-HKNNUM     =   "971"   OR
                                                   "973"   OR
                                                   "975" )
                       OR    ( COMB-KOHHKNNUM (1)
                                               =   "970" )
                           CONTINUE
                       ELSE
                           MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-SRYKA-REC
                           MOVE    1               TO  FLG-PTBYOMEI-UMU
                       END-IF
                   ELSE
                       IF      WRK-HKNCOMBI    =   PTBYO-HKNCOMBI
                           MOVE    PTBYOMEI-REC    TO
                                               WRK-PTBYOMEI-SRYKA-REC
                           MOVE    1               TO  FLG-PTBYOMEI-UMU
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTBYOMEI-UMU    =   1
               MOVE    WRK-PTBYOMEI-SRYKA-REC
                                           TO  PTBYOMEI-REC
               MOVE    "1"                 TO  PTBYO-DLTFLG
               MOVE    "api2"              TO  PTBYO-DLT-OPID
               MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
               MOVE    "api2"              TO  PTBYO-OPID
      *
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO 
                   DISPLAY "UPD PTBYOMEI-KEY =>" PTBYO-KEY  "."
                   MOVE    "E53"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200231-PTBYO-DELETE-EXT
               ELSE
                   ADD     1                   TO  CNT-OUT
                   ADD     1                   TO  CNT-DELETE
               END-IF
           END-IF
      *         
           IF      CNT-DELETE          =   0
               MOVE    "E36"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
           END-IF
      *
           .
       200231-PTBYO-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名チェック処理
      *****************************************************************
       200232-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  CNT-PTBYOMEI-KOHID
           MOVE    ZERO                TO  FLG-PTBYOMEI-OK
           INITIALIZE                      OK-PTBYOMEI-REC
      *
      *    医保分のときの第三者行為のチェック
           IF      DISEASEQ-INSURANCE-CLASS (IDX)
                                   NOT =   "1"
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    WRK-PTID            TO  PTBYO-PTID
               MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
               MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                               PTBYO-XXBYOMEI
               MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
      *
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                   MOVE    "key47"             TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTBYOMEI
               END-IF
      *
               PERFORM             UNTIL   FLG-PTBYOMEI    =   1
                   IF      WRK-SRYKA       =   PTBYO-SRYKA
                       EVALUATE    WRK-NYUGAIKBN   ALSO
                                                       PTBYO-NYUGAIKBN
                           WHEN    SPACE           ALSO    ANY
                           WHEN    "1"             ALSO    SPACE
                           WHEN    "1"             ALSO    "1"
                           WHEN    "2"             ALSO    SPACE
                           WHEN    "2"             ALSO    "2"
                               IF      PTBYO-KOHID     >   ZERO
                                   ADD     1   TO  CNT-PTBYOMEI-KOHID
                               END-IF
                       END-EVALUATE
                   END-IF
                   MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                   MOVE    "key47"             TO  MCP-PATHNAME
                   PERFORM 900-PTBYO-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      CNT-PTBYOMEI-KOHID  >   1
                   GO  TO  200232-BYOMEI-CHK-EXT
               END-IF
           END-IF
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
                                           PTBYO-XXBYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM             UNTIL   FLG-PTBYOMEI    =   1
                               OR      FLG-PTBYOMEI-OK =   1
               IF      WRK-SRYKA       =   PTBYO-SRYKA
                   ADD     1               TO  PTBYOMEI-SRYKA-CNT
                   IF    ( WRK-NYUGAIKBN   =   PTBYO-NYUGAIKBN )
                   AND   ( WRK-SRYKA       =   PTBYO-SRYKA     )
                       IF    ( WRK-HKNCOMBI    =   PTBYO-HKNCOMBI  )
                       AND   ( WRK-KOHID       =   PTBYO-KOHID     )
                           MOVE    1           TO  FLG-PTBYOMEI-OK
                           MOVE    PTBYOMEI-REC
                                               TO  OK-PTBYOMEI-REC
                       ELSE
      *                    医保分のとき
                           IF      DISEASEQ-INSURANCE-CLASS (IDX)
                                               NOT =   "1"
                               IF      PTBYO-HKNCOMBI  =   ZERO
                                   MOVE    1       TO  FLG-PTBYOMEI-OK
                                   MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                               ELSE
                                   MOVE    SPA-HOSPNUM
                                                   TO  COMB-HOSPNUM
                                   MOVE    WRK-PTID
                                                   TO  COMB-PTID
                                   MOVE    PTBYO-HKNCOMBI
                                                   TO  COMB-HKNCOMBINUM
                                   MOVE    HKNCOMBI-REC
                                                   TO  MCPDATA-REC
                                   PERFORM 900-COMB-READ-SEC
                                   IF    ( COMB-HKNNUM
                                                   =   "971"   OR
                                                       "973"   OR
                                                       "975" )
                                   OR    ( COMB-KOHHKNNUM (1)
                                                   =   "970" )
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-PTBYOMEI-OK
                                       MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                                   END-IF
                               END-IF
                           ELSE
      *                        第三者行為のとき
                               IF      WRK-KOHID       >   ZERO
                                   IF      PTBYO-KOHID     >   ZERO
                                       CONTINUE
                                   ELSE
                                       IF      PTBYO-HKNCOMBI  =   ZERO
                                           MOVE    1
                                                   TO  FLG-PTBYOMEI-OK
                                           MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                                       ELSE
                                           MOVE    SPA-HOSPNUM
                                                   TO  COMB-HOSPNUM
                                           MOVE    WRK-PTID
                                                   TO  COMB-PTID
                                           MOVE    PTBYO-HKNCOMBI
                                                   TO  COMB-HKNCOMBINUM
                                           MOVE    HKNCOMBI-REC
                                                   TO  MCPDATA-REC
                                           PERFORM 900-COMB-READ-SEC
                                           IF    ( COMB-HKNNUM
                                                       =   "971"   OR
                                                           "973"   OR
                                                           "975" )
                                               CONTINUE
                                           ELSE
                                               MOVE    1
                                                   TO  FLG-PTBYOMEI-OK
                                               MOVE    PTBYOMEI-REC
                                                   TO  OK-PTBYOMEI-REC
                                           END-IF
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200232-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタとの重複チェック処理（転帰等）
      *****************************************************************
       200233-BYOMEI-DABURI-CHK-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-ERR-SRYYMD
                                       WRK-ERR-SRYKA
           INITIALIZE                  CHK-PTBYOMEI-REC
      *
           MOVE    ZERO                TO  FLG-DABURI-CHK
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-BYOMEI1 (1:80)  TO  PTBYO-BYOMEI
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
           MOVE    WRK-NYUGAIKBN       TO  PTBYO-NYUGAIKBN
           IF      WRK-NYUGAIKBN       =   SPACE
               MOVE    "key40"             TO  WRK-PTBYO-MCP-PATHNAME
           ELSE
               MOVE    "key41"             TO  WRK-PTBYO-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM               UNTIL   FLG-PTBYOMEI    =   1
                                 OR      FLG-DABURI-CHK  >   0
               IF      CHK-PTBYO-SRYYMD    =   PTBYO-SRYYMD
               AND     CHK-PTBYO-SRYKA     =   PTBYO-SRYKA
               AND     CHK-PTBYO-RENNUM    =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
      *****           WHEN    WRK-BYOYMD  =   PTBYO-SRYYMD
      *****                MOVE    2               TO  FLG-DABURI-CHK
                       WHEN    WRK-BYOYMD  <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF     WRK-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                       WHEN    WRK-BYOYMD  >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    1       TO  FLG-DABURI-CHK
                           ELSE
                               IF      WRK-BYOYMD  <   PTBYO-TENKIYMD
                                   MOVE    1       TO  FLG-DABURI-CHK
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      FLG-DABURI-CHK      >   0
      *                同一病名における保険限定の重複チェック処理
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       MOVE    WRK-HKNCOMBI    TO  WRK-CHK-HKNCOMBI1
                       MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                       MOVE    WRK-KOHID       TO  WRK-CHK-KOHID1
                       MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                       PERFORM 2002331-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE    ZERO        TO  FLG-DABURI-CHK
                       END-IF
                       IF      FLG-DABURI-CHK      >   0
                           MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-PTBYO-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-PTBYO-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-DABURI-CHK      >   0
               MOVE    9                   TO  FLG-BYOMEI-CHK
      *
               IF      FLG-DABURI-CHK      =   1
                   MOVE    "E31"               TO  WRK-DIAG-WARNING
      *********ELSE
      *********    MOVE    "E32"               TO  WRK-DIAG-WARNING
               END-IF 
               PERFORM 500-WARNING-HENSYU-SEC
           END-IF
           .
       200233-BYOMEI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       2002331-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI-DABURI
      *
      **    医保以外の保険がない場合はチェックしない
      **     IF    ( SPA-NAI-C02-HKNCOMBI-ROUSAI
      **                                 =   ZERO )
      **     AND   ( SPA-NAI-C02-HKNCOMBI-JIHI
      **                                 =   ZERO )
      **     AND   ( SPA-NAI-C02-HKNCOMBI-KOUGAI
      **                                 =   ZERO )
      **     AND   ( SPA-NAI-C02-KOHID-DAISANSYA
      **                                 =   ZERO )
      **         GO  TO   2002331-HKNCOMBI-DABURI-CHK-EXT
      **     END-IF
      *
      *    保険限定の設定がない場合はチェックしない
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
           AND   ( WRK-CHK-KOHID1      =   ZERO )
           AND   ( WRK-CHK-KOHID2      =   ZERO )
               GO  TO   2002331-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの重複チェック
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
             CONTINUE
           ELSE
             IF      WRK-CHK-HKNCOMBI1
                                   NOT =   ZERO
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    WRK-PTID            TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI1   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-READ-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (1)
             END-IF    
             IF      WRK-CHK-HKNCOMBI2
                                   NOT =   ZERO
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    WRK-PTID            TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI2   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-READ-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (2)
             END-IF
      *
      *      片方のみ労災または自賠責または公害の場合は登録可能とする
             EVALUATE    WRK-CHK-COMB-HKNNUM (1)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2)
               WHEN    "971"       ALSO    "971"
               WHEN    "973"       ALSO    "973"
               WHEN    "975"       ALSO    "975"
                   IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                       MOVE    1                TO  FLG-HKNCOMBI-DABURI
                   END-IF
               WHEN    "971"       ALSO    ANY
               WHEN    "973"       ALSO    ANY
               WHEN    "975"       ALSO    ANY
               WHEN    ANY         ALSO    "971"
               WHEN    ANY         ALSO    "973"
               WHEN    ANY         ALSO    "975"
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
             END-EVALUATE
      *
      *      自費と保険限定なしまたは同じ自費の場合は登録不可とする
             IF    ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   =   "98" ) 
             OR    ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                   =   "98" ) 
               EVALUATE    WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2) (1:2)
                   WHEN    "98"       ALSO    "98"
                       IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                           MOVE    1            TO  FLG-HKNCOMBI-DABURI
                       END-IF
                   WHEN    "98"       ALSO    SPACE
                   WHEN    SPACE      ALSO    "98"
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
             END-IF
      *
      *      保険組合せで登録不可のとき
             IF      FLG-HKNCOMBI-DABURI =   ZERO
               GO  TO   2002331-HKNCOMBI-DABURI-CHK-EXT
             END-IF
           END-IF
      *
      *    第三者行為の設定がない場合はチェックしない
           IF    ( WRK-CHK-KOHID1   =   ZERO )
           AND   ( WRK-CHK-KOHID2   =   ZERO )
               GO  TO   2002331-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    第三者行為の設定が両方の場合
           IF    ( WRK-CHK-KOHID1
                               NOT =   ZERO )
           AND   ( WRK-CHK-KOHID2
                               NOT =   ZERO )
      *        同じ第三者行為の場合は登録不可とする
               IF      WRK-CHK-KOHID1  =   WRK-CHK-KOHID2
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-IF
               GO  TO  2002331-HKNCOMBI-DABURI-CHK-EXT
           ELSE
      *        片方が医保以外のときチェックしない
               IF    ( WRK-CHK-KOHID1
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  2002331-HKNCOMBI-DABURI-CHK-EXT
               END-IF
               IF    ( WRK-CHK-KOHID2
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  2002331-HKNCOMBI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    医保と第三者行為のため登録不可とする
           MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
           .
      *
       2002331-HKNCOMBI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ更新処理
      *****************************************************************
       200234-KOUSIN-02-SEC           SECTION.
      *
      *    保険組合せが削除のとき
           IF    ( WRK-HKNCOMBI        >   ZERO           )
           AND   ( WRK-HKNCOMBI    NOT =   PTBYO-HKNCOMBI )
               IF      WRK-HKNCOMBI-DELKBN =  "1"
                   MOVE    "E28"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200234-KOUSIN-02-EXT
               END-IF
           END-IF
      *    第三者行為が削除のとき
           IF    ( WRK-KOHID           >   ZERO        )
           AND   ( WRK-KOHID       NOT =   PTBYO-KOHID )
               IF     WRK-KOHID-DELKBN     =   "1"
                   MOVE    "E29"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200234-KOUSIN-02-EXT
               END-IF
           END-IF
      *
      *    転帰日
           MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *    転帰区分
           MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
      *    疑いフラグ
           MOVE    WRK-UTAGAIFLG       TO  PTBYO-UTAGAIFLG
      *
      *    病名編集処理
           MOVE    2                   TO  FLG-KOUSIN
           PERFORM 2002341-PTBYOMEI-HENSYU-SEC
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
      *
           MOVE    "api02"             TO  PTBYO-OPID
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO 
               DISPLAY "UPD PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    "E52"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
      ***         MOVE    "12"                TO  LNK-ERR2-CODE
      ***         MOVE
      ***             "全ての病名の更新に失敗しました（更新時にエラー）"
      ***                                     TO  LNK-ERR2-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT
      *
               PERFORM 20029-DISEASEQ-INFORMATION-SEC
           END-IF
           .
       200234-KOUSIN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       2002341-PTBYOMEI-HENSYU-SEC       SECTION.
      *
      *    入外区分
           MOVE    WRK-NYUGAIKBN       TO  PTBYO-NYUGAIKBN
      *
      *    主病名フラグ
           MOVE    WRK-SYUBYOFLG       TO  PTBYO-SYUBYOFLG
      *
      *    病名
           MOVE    WRK-BYOMEI1         TO  PTBYO-BYOMEI
      *
           INITIALIZE                      PTBYO-BYOMEICD-G
                                           PTBYO-KHNBUICD-G
           MOVE    ZERO                TO  PTBYO-BYOMEICDSU
      *
      *    病名コードがエラー又は未コード化傷病名コード以外のとき
           IF      FLG-BYOMEI-CHK      NOT =   1   AND 5 AND 6
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       21
                       OR      WRK-TBYOMEICD (IDY) =   SPACE
      *            病名コード
                   MOVE    WRK-TBYOMEICD (IDY) 
                                           TO  PTBYO-BYOMEICD (IDY)
      *            疑いフラグ（病名コードより）
                   IF      PTBYO-UTAGAIFLG     =   "3"
                       CONTINUE
                   ELSE
                       EVALUATE    PTBYO-UTAGAIFLG ALSO
                                               WRK-TUTAGAIFLG (IDY)
                           WHEN    SPACE           ALSO    "1"
                           WHEN    SPACE           ALSO    "2"
                               MOVE    WRK-TUTAGAIFLG (IDY)
                                                   TO  PTBYO-UTAGAIFLG
                           WHEN    "2"             ALSO    "1"
                           WHEN    "1"             ALSO    "2"
                               MOVE    "3"     TO  PTBYO-UTAGAIFLG
                       END-EVALUATE
                   END-IF
      *            病名コード数
                   ADD     1                   TO  PTBYO-BYOMEICDSU
      *            基本病名コード・基本部位コード
                   IF      PTBYO-BYOMEICD (IDY)(1:1)
                                               NOT =   "Z" 
                       MOVE    PTBYO-BYOMEICD (IDY)
                                           TO  PTBYO-KHNBYOMEICD
                   ELSE
                       IF      PTBYO-BYOMEICD (IDY) (1:4)
                                               =   "ZZZ1"
                           ADD     1       TO  IDZ
                           IF      IDZ         <   4
                               MOVE    PTBYO-BYOMEICD (IDY)
                                               TO  PTBYO-KHNBUICD(IDZ)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        病名文字数
               PERFORM VARYING     IDY     FROM    80 BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-BYOMEI(IDY:1)     NOT =   SPACE
                       COMPUTE PTBYO-BYOMEIMOJI    =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
               END-PERFORM
      *
      *        病名編集フラグ（コードがエラーのとき、病名が長いとき）
               IF    ( FLG-BYOMEI-CHK  NOT =   ZERO  )
               OR    ( WRK-BYOMEI2     NOT =   SPACE )    
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               END-IF
      *
      *        レセ電エラーフラグ（編集病名のとき）
               IF    ( PTBYO-BYOMEIHENFLG  =   "1" )
               AND   ( PTBYO-BYOMEIMOJI    >   20  )
                   MOVE    "1"                 TO  PTBYO-RECEDENFLG
               END-IF
      *
      *        レセ電エラーフラグ（疑いフラグが設定されているとき）
               IF    ( PTBYO-RECEDENFLG    =   SPACE ) 
               AND   ( PTBYO-UTAGAIFLG NOT =   SPACE )
                   INITIALIZE                      ORCSMKBYOMEIAREA
                   MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
                   MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
                   MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
                   MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
                   MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
                   COMPUTE WRK-MOJISU      =   SMKBYO-LENGTH  /   2
                   IF      PTBYO-BYOMEIHENFLG  =   "1"
                       IF      WRK-MOJISU          >   20
                           MOVE    "1"             TO  PTBYO-RECEDENFLG
                       END-IF
                   END-IF
               END-IF
      *       慢性区分
               IF      WRK-MANSEIKBN-X =   "None"
                   CONTINUE
               ELSE
                   IF      WRK-MANSEIKBN-X =   "Auto"
                       MOVE    WRK-TOKSKNCD    TO  PTBYO-MANSEIKBN
                   ELSE
                       MOVE    WRK-MANSEIKBN   TO  PTBYO-MANSEIKBN
                   END-IF
               END-IF
           ELSE
      *        疾患コードがエラー又は未コード化傷病名コードのとき
      *        病名からコード検索
               INITIALIZE                      ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    PTBYO-SRYYMD        TO  LNK-BYO-SRYYMD-IN
               MOVE    PTBYO-UTAGAIFLG     TO  LNK-BYO-UTAGAIFLG-IN
               MOVE    PTBYO-BYOMEI        TO  LNK-BYO-BYOMEI
               MOVE    PTBYO-SYUBYOFLG     TO  LNK-BYO-SYUBYOFLG 
               CALL    "ORCSBYOMEI"        USING    ORCSBYOMEIAREA
                                                    SPA-AREA
      *        病名
      ****     MOVE    LNK-BYO-BYOMEI      TO  PTBYO-BYOMEI
               IF      FLG-KOUSIN          =   1
                   IF      LNK-BYO-TANDOKUKBN-ERR  =   1
      *                単独病名のとき
                       IF      PTBYO-HOSOKU-COMMENT
                                               NOT =   SPACE
                           CONTINUE
                       ELSE
                           STRING  "＊"            DELIMITED   BY  SIZE
                                   LNK-BYO-BYOMEI  DELIMITED   BY  SPACE
                                                   INTO    PTBYO-BYOMEI
                           END-STRING
                           COMPUTE PTBYO-BYOMEIMOJI    =
                                               PTBYO-BYOMEIMOJI    +   2
                       END-IF
                   END-IF
               END-IF
      *        疑いフラグ
               IF      PTBYO-UTAGAIFLG     =   "3"
                   CONTINUE
               ELSE
                   EVALUATE    PTBYO-UTAGAIFLG ALSO
                                           LNK-BYO-UTAGAIFLG
                       WHEN    ANY             ALSO    "3"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                       WHEN    SPACE           ALSO    "1"
                       WHEN    SPACE           ALSO    "2"
                           MOVE    LNK-BYO-UTAGAIFLG
                                                   TO  PTBYO-UTAGAIFLG
                       WHEN    "2"             ALSO    "1"
                       WHEN    "1"             ALSO    "2"
                           MOVE    "3"         TO  PTBYO-UTAGAIFLG
                   END-EVALUATE
               END-IF      
      *        病名コード
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDX4)
                                       TO  PTBYO-BYOMEICD  (IDX4)
               END-PERFORM
      *        病名コード数
               MOVE    LNK-BYO-BYOMEICDSU  TO  PTBYO-BYOMEICDSU
      *        基本病名コード
               MOVE    LNK-BYO-KHNBYOMEICD TO  PTBYO-KHNBYOMEICD
      *        基本部位コード
               MOVE    LNK-BYO-KHNBUICD (1)
                                           TO  PTBYO-KHNBUICD (1)
               MOVE    LNK-BYO-KHNBUICD (2)
                                           TO  PTBYO-KHNBUICD (2)
               MOVE    LNK-BYO-KHNBUICD (3)
                                           TO  PTBYO-KHNBUICD (3)
      *        レセ電エラーフラグ
               MOVE    LNK-BYO-RECEDENFLG  TO  PTBYO-RECEDENFLG
      *        病名文字数
               MOVE    LNK-BYO-BYOMEIMOJI  TO  PTBYO-BYOMEIMOJI
      *        病名編集フラグ
               IF    ( LNK-BYO-BYOMEICD-ERR    =   1  )
               OR    ( LNK-BYO-TANDOKUKBN-ERR  =   1  )
               OR    ( LNK-BYO-BYOMEIMOJI      >   80 )
      *            単独病名・修飾語コードのみ・病名が長いとき
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                               TO  PTBYO-BYOMEIHENFLG
               END-IF
      *       慢性区分
               IF      WRK-MANSEIKBN-X =   "None"
                   CONTINUE
               ELSE
                   IF      WRK-MANSEIKBN-X =   "Auto"
                       IF      LNK-BYO-NANBYOCD
                                           NOT =   ZERO
                           MOVE    LNK-BYO-NANBYOCD
                                                   TO  PTBYO-MANSEIKBN
                       ELSE
                           IF      LNK-BYO-MANSEIKBN   NOT =   ZERO
                               MOVE    LNK-BYO-MANSEIKBN
                                                   TO  PTBYO-MANSEIKBN
                           END-IF
                       END-IF
                   ELSE
                       MOVE    WRK-MANSEIKBN   TO  PTBYO-MANSEIKBN
                   END-IF
               END-IF
           END-IF
      *
      *    カルテ病名
           IF      WRK-KARTE-NAME-X    =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-KARTE-NAME      TO  PTBYO-CHARTBYOMEI
      *        カルテ病名文字数
               PERFORM VARYING     IDY     FROM    80  BY  -1
                       UNTIL       IDY     <   1
                   IF      PTBYO-CHARTBYOMEI (IDY:1)
                                           NOT =   SPACE
                       COMPUTE PTBYO-CHARTBYOMEIMOJI   =   IDY /   2
                       MOVE    1                   TO  IDY
                   END-IF
                END-PERFORM
           END-IF
      *    保険適用
           IF      WRK-HKNCOMBI-X      =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-HKNCOMBI        TO  PTBYO-HKNCOMBI
               MOVE    WRK-KOHID           TO  PTBYO-KOHID
           END-IF
      *    レセプト表示
           IF      WRK-REZEFLG-X       =   "None"
               CONTINUE
           ELSE
              MOVE    WRK-REZEFLG         TO  PTBYO-REZEFLG
           END-IF
      *    表示期間
           IF      WRK-REZEMM-X        =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-REZEMM          TO  PTBYO-REZEMM
           END-IF
      *    保険病名
           IF      WRK-HKNBYOMEIFLG-X  =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-HKNBYOMEIFLG    TO  PTBYO-HKNBYOMEIFLG
            END-IF
      *    退院証明書
           IF      WRK-DISCHARGEFLG-X  =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-DISCHARGEFLG    TO  PTBYO-DISCHARGEFLG
           END-IF
      *    原疾患区分
           IF      WRK-CLASS1-X        =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-CLASS1          TO  PTBYO-CLASS1
           END-IF
      *    合併症区分
           IF      WRK-CLASS2-X        =   "None"
               CONTINUE
           ELSE
               MOVE    WRK-CLASS2          TO  PTBYO-CLASS2
           END-IF
           .
       2002341-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名追加処理
      *****************************************************************
       200235-KOUSIN-01-SEC           SECTION.
      *
      *    保険組合せが削除のとき
           IF      WRK-HKNCOMBI        >   ZERO
               IF      WRK-HKNCOMBI-DELKBN =  "1"
                   MOVE    "E28"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200235-KOUSIN-01-EXT
               END-IF
           END-IF
      *    第三者行為が削除のとき
           IF      WRK-KOHID           >   ZERO
               IF     WRK-KOHID-DELKBN     =   "1"
                   MOVE    "E29"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200235-KOUSIN-01-EXT
               END-IF
           END-IF
      *
      *    連番取得
           MOVE    ZERO                TO  WRK-PTBYO-RENNUM
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           PERFORM 900-PTBYO-INV-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               IF      PTBYO-RENNUM        =   99
                   MOVE    "E50"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
                   GO  TO  200235-KOUSIN-01-EXT
               ELSE
                   MOVE    PTBYO-RENNUM        TO  WRK-PTBYO-RENNUM
               END-IF
           END-IF
           ADD     1                       TO  WRK-PTBYO-RENNUM
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BYOYMD          TO  PTBYO-SRYYMD
           MOVE    WRK-PTBYO-RENNUM    TO  PTBYO-RENNUM
           MOVE    9999                TO  PTBYO-SEQNUM
      *    転帰
           IF      WRK-TENKIYMD    NOT =   SPACE
      *        転帰日
               MOVE    WRK-TENKIYMD        TO  PTBYO-TENKIYMD
      *        転帰区分
               MOVE    WRK-TENKI           TO  PTBYO-TENKIKBN
           END-IF
      *    補足コメントコード
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       3
                   OR      WRK-THOSOKUCD (IDY) =   SPACE
                MOVE    WRK-THOSOKUCD (IDY) 
                                           TO  PTBYO-HOSOKUCD (IDY)
           END-PERFORM
      *    補足コメント
           MOVE    WRK-HOSOKU-COMMENT  TO  PTBYO-HOSOKU-COMMENT
      *    疑いフラグ
           MOVE    WRK-UTAGAIFLG       TO  PTBYO-UTAGAIFLG
      *
      *    病名編集処理
           MOVE    SPACE               TO  WRK-DISEASEQ-AREA-X
           IF      WRK-MANSEIKBN-X     =   "None"
               MOVE    SPACE               TO  WRK-MANSEIKBN-X
           END-IF
           MOVE    1                   TO  FLG-KOUSIN
           PERFORM 2002341-PTBYOMEI-HENSYU-SEC
      *
           MOVE    SMCNDATE-YMD        TO  PTBYO-CREYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-CREHMS
           MOVE    SMCNDATE-YMD        TO  PTBYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTBYO-UPHMS
           MOVE    "api02"             TO  PTBYO-OPID
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               DISPLAY "INS PTBYOMEI-KEY =>" PTBYO-KEY  "."
               MOVE    "E51"               TO  WRK-DIAG-WARNING
               PERFORM 500-WARNING-HENSYU-SEC
               GO  TO  200235-KOUSIN-01-EXT
      *??      MOVE    "11"                TO  LNK-ERR2-CODE
      *??      MOVE
      *??           "全ての病名の更新に失敗しました（追加時にエラー）"
      *??                                  TO  LNK-ERR2-MSG 
           ELSE
               ADD     1                   TO  CNT-OUT
      *
               PERFORM 20029-DISEASEQ-INFORMATION-SEC
      *
               MOVE    SPACE               TO  WRK-DIAG-CHANGE
               IF      PTBYO-KHNBYOMEICD   =   WRK-CONS-BYOMEICD
                   INITIALIZE                  SYNONYM-REC
                   MOVE    PTBYO-BYOMEI        TO  SYNONYM-BYOMEI
                   PERFORM 5001-SYNONYM-CHK-SEC
                   IF      FLG-SYNONYM         =   ZERO
                       MOVE    "03"                TO  WRK-DIAG-CHANGE
                   END-IF
               ELSE
                   IF      PTBYO-BYOMEIHENFLG  =   SPACE
                       INITIALIZE                  BYOMEI-REC
                       MOVE    PTBYO-KHNBYOMEICD   TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                       IF      BYO-IKOSAKICD   NOT =   SPACE
                           MOVE    "01"            TO  WRK-DIAG-CHANGE
                       ELSE    
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                              CONTINUE
                           ELSE
                               MOVE    "02"        TO  WRK-DIAG-CHANGE
                               INITIALIZE              SYNONYM-REC
                               MOVE    PTBYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    "03"    TO  WRK-DIAG-CHANGE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF      
               IF     WRK-DIAG-CHANGE  NOT =   SPACE
                   MOVE    "W01"               TO  WRK-DIAG-WARNING
                   PERFORM 500-WARNING-HENSYU-SEC
               END-IF
           END-IF
           .
       200235-KOUSIN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    不一致病名編集処理
      *****************************************************************
       2003-DISEASES-UNMATCH-SEC       SECTION.
      *
           MOVE    "False"             TO  DISEASES-INFORMATION-OVERFLOW
           MOVE    ZERO                TO  IDY
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WRK-PTID            TO  PTBYO-PTID
           MOVE    WRK-SRYKA           TO  PTBYO-SRYKA
           MOVE    WRK-BASE-MONTH      TO  PTBYO-SRYYMD (1:6)
           MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL   FLG-PTBYOMEI    =   1
               PERFORM 20031-DISEASES-UNMATCH-CHK-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-DISEASES-UNMATCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    不一致病名チェック処理
      *****************************************************************
       20031-DISEASES-UNMATCH-CHK-SEC SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       WRK-DISEASEQ-MAX
                   OR          FLG-OK  =       1
               IF    ( PTBYO-SRYYMD    =   WRK-DISEASEQ-SRYYMD   (IDX) )
               AND   ( PTBYO-NYUGAIKBN =   WRK-DISEASEQ-NYUGAIKBN(IDX) )
               AND   ( PTBYO-BYOMEI    =   WRK-DISEASEQ-BYOMEI   (IDX) )
               AND   ( PTBYO-HOSOKU-COMMENT
                                       =   WRK-DISEASEQ-HOSOKU-COMMENT
                                                                 (IDX) )
               AND   ( PTBYO-TENKIYMD  =   WRK-DISEASEQ-TENKIYMD (IDX) )
      ******** AND   ( PTBYO-TENKIKBN  =   WRK-DISEASEQ-TENKIKBN (IDX) )
      ******** AND   ( PTBYO-SYUBYOFLG =   WRK-DISEASEQ-SYUBYOFLG(IDX) )
      ******** AND   ( PTBYO-UTAGAIFLG =   WRK-DISEASEQ-UTAGAIFLG(IDX) )
                   IF      WRK-DISEASEQ-HKNCOMBI-X (IDX)
                                           =   "None"
                       IF      PTBYO-HKNCOMBI  =   ZERO
                           MOVE    1               TO  FLG-OK
                       ELSE
                           INITIALIZE                  HKNCOMBI-REC
                           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                           MOVE    WRK-PTID        TO  COMB-PTID
                           MOVE    PTBYO-HKNCOMBI  TO  COMB-HKNCOMBINUM
                           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                           PERFORM 900-COMB-READ-SEC
                           IF    ( COMB-HKNNUM     =   "971"   OR
                                                       "973"   OR
                                                       "975" )
                           OR    ( COMB-KOHHKNNUM (1)
                                                   =   "970" )
                               CONTINUE
                           ELSE
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-DISEASEQ-INSURANCE-CLASS (IDX)
                                               =   "1"
                           IF      PTBYO-HKNCOMBI  =
                                           WRK-DISEASEQ-HKNCOMBI (IDX)
                               MOVE    1               TO  FLG-OK
                           END-IF
                           IF      PTBYO-KOHID     =
                                           WRK-DISEASEQ-KOHID    (IDX)
                               MOVE    1               TO  FLG-OK
                           END-IF
                       ELSE
                           IF      PTBYO-HKNCOMBI  =
                                           WRK-DISEASEQ-HKNCOMBI (IDX)
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    対象の病名がないとき不一致リストに返却
           IF      FLG-OK              =   ZERO
      *        ５０件以上のときエラーとする
               IF      IDY                 =   50
                   MOVE    "True"              TO
                                          DISEASES-INFORMATION-OVERFLOW
                   MOVE    1                   TO  FLG-PTBYOMEI
                   GO  TO  20031-DISEASES-UNMATCH-CHK-EXT
               END-IF
      *
               ADD     1                   TO  IDY
      *
      *        入る病名コードまで返却
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       21
                       OR      PTBYO-BYOMEICD (IDXX)   =   SPACE
                   MOVE    SPACE           TO  WRK-BYOMEICD
                                               WRK-DISEASES-SPICOD1
                   MOVE    DISEASES-SPICOD1 (IDY)
                                           TO  WRK-DISEASES-SPICOD1
                   IF      PTBYO-BYOMEICD (IDXX) (1:3)
                                           =   "ZZZ"
                       IF      WRK-DISEASES-SPICOD1 (151:)
                                              =   SPACE
                           MOVE    PTBYO-BYOMEICD (IDXX) (4:)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   ELSE
                       IF      WRK-DISEASES-SPICOD1 (148:)
                                              =   SPACE
                           MOVE    PTBYO-BYOMEICD (IDXX)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   END-IF
                   IF      WRK-BYOMEICD    =   SPACE
                       MOVE    21              TO  IDXX
                   ELSE
                       MOVE    SPACE           TO  DISEASES-SPICOD1(IDY)
                       IF      IDXX                =   1
                           MOVE    WRK-BYOMEICD        TO
                                                   DISEASES-SPICOD1(IDY)
                       ELSE
                           STRING  WRK-DISEASES-SPICOD1
                                               DELIMITED  BY  SPACE
                               "."             DELIMITED  BY  SIZE
                               WRK-BYOMEICD    DELIMITED  BY  SPACE
                                       INTO    DISEASES-SPICOD1 (IDY)
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    PTBYO-BYOMEI        TO  DISEASES-SPINAME1   (IDY)
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       3
                   MOVE    PTBYO-HOSOKUCD (IDXX)   TO
                                       DISEASES-HOSOKUCD-O (IDY IDXX)
      *            補足コメントコードからコメント内容を編集
                   PERFORM 200311-HOSOKUCD-COMMENT-SEC
               END-PERFORM
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  DISEASES-HOSOKU-COMMENT
                                                                   (IDY)
               EVALUATE    PTBYO-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "I"         TO  DISEASES-NYUGAIKBN  (IDY)
                   WHEN    "2"
                       MOVE    "O"         TO  DISEASES-NYUGAIKBN  (IDY)
               END-EVALUATE
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    "PD"                TO
                                               DISEASES-SYUOBYOFLG (IDY)
               END-IF
               MOVE    PTBYO-UTAGAIFLG     TO  DISEASES-UTAGAIFLG  (IDY)
               IF      PTBYO-UTAGAIFLG     =   "2" OR "3"
                   MOVE    "A"                 TO
                                               DISEASES-KYUSEIFLG  (IDY)
               END-IF
               MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  DISEASES-STARTDATE  (IDY)
               IF      PTBYO-TENKIYMD  NOT =   SPACE
                   MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO
                                           DISEASES-ENDDATE    (IDY)
               END-IF
               MOVE    PTBYO-TENKIKBN      TO  DISEASES-TENKIKBN   (IDY)
               MOVE    PTBYO-CHARTBYOMEI   TO  DISEASES-KARTE-NAME (IDY)
               IF      PTBYO-MANSEIKBN     >   ZERO
                   MOVE    PTBYO-MANSEIKBN     TO
                                               DISEASES-MANSEIKBN  (IDY)
               END-IF
               IF      PTBYO-HKNCOMBI      >   ZERO
                   MOVE    PTBYO-HKNCOMBI      TO
                                               DISEASES-HKNCOMBI   (IDY)
               END-IF
               IF      PTBYO-KOHID         >   ZERO
                   INITIALIZE                      HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    PTBYO-PTID          TO  COMB-PTID
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-COMB-READ-N-SEC
                   ELSE
                       INITIALIZE                      HKNCOMBI-REC
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
                   PERFORM            UNTIL    FLG-HKNCOMBI =   1
                       IF   ( PTBYO-KOHID      =   COMB-KOHID (1) )
                       AND  ( PTBYO-SRYYMD     <=  COMB-TEKEDYMD  )
                           MOVE    PTBYO-HKNCOMBI      TO
                                               DISEASES-HKNCOMBI (IDY)
                           MOVE    1                   TO  FLG-HKNCOMBI
                       ELSE
                           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-COMB-READ-N-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
               MOVE    PTBYO-REZEFLG       TO  DISEASES-REZEFLG    (IDY)
               IF      PTBYO-REZEMM        >   ZERO
                   MOVE    PTBYO-REZEMM        TO  DISEASES-REZEMM (IDY)
               END-IF
               MOVE    PTBYO-HKNBYOMEIFLG  TO  DISEASES-HKNBYOMEIFLG
                                                                   (IDY)
               MOVE    PTBYO-DISCHARGEFLG  TO  DISEASES-DISCHARGEFLG
                                                                   (IDY)
               IF      PTBYO-CLASS1        >   ZERO
                   MOVE    PTBYO-CLASS1        TO  DISEASES-CLASS1 (IDY)
               END-IF
               IF      PTBYO-CLASS1        >   ZERO
                   MOVE    PTBYO-CLASS2        TO  DISEASES-CLASS2 (IDY)
               END-IF
           END-IF
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 900-PTBYO-READ-SEC
           .
       20031-DISEASES-UNMATCH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコードからコメント内容を編集
      *****************************************************************
       200311-HOSOKUCD-COMMENT-SEC   SECTION.
      *
           IF      PTBYO-HOSOKUCD (IDXX)
                                       =   SPACE
               MOVE    SPACE               TO
                                   DISEASES-HOSOKUCD-COMMENT-O(IDY IDXX)
           ELSE
               MOVE    ZERO                TO  WRK-LENGTH
               PERFORM VARYING IDX2    FROM    10  BY  -1
                       UNTIL   IDX2    <       1
                   IF      PTBYO-HOSOKUCD (IDXX) (IDX2:1)
                                       NOT =   SPACE
                       MOVE    IDX2            TO  WRK-LENGTH
                       MOVE    1               TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      WRK-LENGTH          =   7
                   MOVE    ZERO                  TO  FLG-BYOMEI
      *
                   INITIALIZE                  BYOMEI-REC
                   MOVE    PTBYO-HOSOKUCD (IDXX)
                                               TO  BYO-BYOMEICD
      *
                   IF  (   BYO-BYOMEICD (1:3)  =   "ZZZ"    )
                   AND ( ( BYO-BYOMEICD (4:1)  <   "1" ) OR 
                         ( BYO-BYOMEICD (4:1)  =   "C" )    )
      *                予約病名コード検索
                       SET     TBL-BYOMEICD-IDX    TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    1           TO  FLG-BYOMEI
                           WHEN    BYO-BYOMEICD    =
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                               MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                                       TO
                                   DISEASES-HOSOKUCD-COMMENT-O(IDY IDXX)
                       END-SEARCH
                   ELSE
      *                病名マスタ読込
                       IF      BYO-BYOMEICD    =   "0000999"
                           CONTINUE
                       ELSE
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                   DISEASES-HOSOKUCD-COMMENT-O(IDY IDXX)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-BYOMEI          =   1 )
               OR    ( WRK-LENGTH      NOT =   7 )
      *            自院病名読込
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    PTBYO-HOSOKUCD (IDXX)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-INV-SEC
                   IF      FLG-USERBYOMEI      =   ZERO
                       MOVE    USBYO-BYOMEI        TO
                                   DISEASES-HOSOKUCD-COMMENT-O(IDY IDXX)
                   END-IF
               END-IF
           END-IF
           .
       200311-HOSOKUCD-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新対象一覧処理
      *****************************************************************
       20029-DISEASEQ-INFORMATION-SEC          SECTION.
      *
           ADD     1                   TO  WRK-DISEASEQ-MAX
      *
           MOVE    DISEASEQ-INSURANCE-CLASS (IDX)
                                       TO  WRK-DISEASEQ-INSURANCE-CLASS
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-BYOYMD          TO  WRK-DISEASEQ-SRYYMD
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-NYUGAIKBN       TO  WRK-DISEASEQ-NYUGAIKBN
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-BYOMEI1         TO  WRK-DISEASEQ-BYOMEI
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-HOSOKU-COMMENT  TO  WRK-DISEASEQ-HOSOKU-COMMENT
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-TENKIYMD        TO  WRK-DISEASEQ-TENKIYMD
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-TENKI           TO  WRK-DISEASEQ-TENKIKBN
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-HKNCOMBI        TO  WRK-DISEASEQ-HKNCOMBI
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-HKNCOMBI-X      TO  WRK-DISEASEQ-HKNCOMBI-X
                                                   (WRK-DISEASEQ-MAX)
           MOVE    WRK-KOHID           TO  WRK-DISEASEQ-KOHID
                                                   (WRK-DISEASEQ-MAX)
      *    MOVE                        TO  WRK-DISEASEQ-SYUBYOFLG
      *    MOVE                        TO  WRK-DISEASEQ-UTAGAIFLG
      *    MOVE                        TO  WRK-DISEASEQ-MANSEIKBN
           .
       20029-DISEASEQ-INFORMATION-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告編集処理
      *****************************************************************
       500-WARNING-HENSYU-SEC          SECTION.
      *
           IF      WRK-DIAG-POSITION   >   ZERO
               MOVE    SPACE               TO  WRK-DIAG-AREA-X
               INITIALIZE                      WRK-DIAG-AREA-X
               GO  TO  500-WARNING-HENSYU-EXT
           ELSE
               MOVE    IDX                 TO  WRK-DIAG-POSITION
           END-IF
      *
           ADD     1                   TO  CNT-DIAG
      *
           MOVE    IDX                 TO  WRK-POSITION
           MOVE    WRK-POSITION-X      TO  DISEASES-DIAG-POSITION
                                                           (CNT-DIAG)
           MOVE    WRK-BYOMEI1         TO  DISEASES-DIAG-NAME
                                                           (CNT-DIAG)
           MOVE    DISEASEQ-STARTDATE (IDX)
                                       TO  DISEASES-DIAG-STARTDATE
                                                           (CNT-DIAG)
      *
           MOVE    SPACE               TO  WRK-DIAG-MSG
           EVALUATE    WRK-DIAG-WARNING
               WHEN    "E16"
                   MOVE    "開始日が暦日ではありません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E17"
                   MOVE    "転帰日が暦日ではありません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E18"
                   MOVE    "医保分でしか設定できない値です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E19"
                   MOVE    "保険組合せ番号が存在しません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E20"
               WHEN    "E21"
                   MOVE    "保険区分の設定と異なる保険組合せ番号です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E22"
                   MOVE    "保険組合せ番号の設定に誤りがあります。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E23"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYKA       DELIMITED   BY  SPACE
                           "に"                DELIMITED   BY  SIZE 
                           "複数存在します。"  DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
                WHEN    "E24"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYKA       DELIMITED   BY  SPACE
                           "に３件以上存在します。"
                                               DELIMITED   BY  SIZE 
                           "画面で確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "E25"
                   MOVE    "第三者行為が存在しません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E26"
                   MOVE    "開始日が第三者行為の適用日の範囲外です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E27"
                   MOVE    "開始日が保険組合せの適用日の範囲外です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E28"
                   MOVE    "保険組合せ番号が存在しません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E29"
                   MOVE    "第三者行為が存在しません。"
                                           TO  WRK-DIAG-MSG
      *
               WHEN    "E31"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYYMD      DELIMITED   BY  SPACE
                           "に存在します（転帰日等を確認して下さい）。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "E32"
                   STRING  "同名の病名が"      DELIMITED   BY  SIZE 
                           WRK-ERR-SRYYMD      DELIMITED   BY  SPACE
                           "に存在します。"    DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "E33"
                   MOVE    "病名コードが不正です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E34"
                   MOVE    "補足コメントコードが不正です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E36"
                   MOVE    "削除対象の病名がありません。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E37"
                   MOVE    "複数の第三者行為が存在します。"
                                           TO  WRK-DIAG-MSG
               WHEN    "E50"
                   MOVE    "登録エラー（上限超え）"
                                           TO  WRK-DIAG-MSG
               WHEN    "E51"
                   MOVE    "登録エラー"
                                           TO  WRK-DIAG-MSG
               WHEN    "E52"
                   MOVE    "更新エラー"
                                           TO  WRK-DIAG-MSG
               WHEN    "E53"
                   MOVE    "削除エラー（削除）"
                                           TO  WRK-DIAG-MSG
      *
               WHEN    "W01"
                   MOVE    "廃止・移行先・推奨のある病名が存在します。"
                                           TO  WRK-DIAG-MSG
                   MOVE    WRK-DIAG-CHANGE
                                       TO  DISEASES-DIAG-CHANGE
                                                           (CNT-DIAG)
               WHEN    "W02"
                   MOVE    "単独使用禁止病名です。"
                                           TO  WRK-DIAG-MSG
               WHEN    "W03"
                   STRING  "全角チェックでエラーとなる文字が"
                                               DELIMITED   BY  SIZE
                           "病名に存在します。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "W04"
                   MOVE    "病名に改行コードが存在します。"
                                           TO  WRK-DIAG-MSG
               WHEN    "W05"
                   STRING  "全角チェックでエラーとなる文字が"
                                               DELIMITED   BY  SIZE
                           "補足コメントに存在します。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "W06"
                   MOVE    "補足コメントに改行コードが存在します。"
                                           TO  WRK-DIAG-MSG
               WHEN    "W07"
                   STRING  "全角チェックでエラーとなる文字が"
                                               DELIMITED   BY  SIZE
                           "カルテ病名に存在します。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-DIAG-MSG
                   END-STRING
               WHEN    "W08"
                   MOVE    "カルテ病名に改行コードが存在します。"
                                           TO  WRK-DIAG-MSG
           END-EVALUATE
      *
           EVALUATE    WRK-DIAG-WARNING (1:1)
               WHEN    "E"
                   MOVE    WRK-DIAG-WARNING    TO  DISEASES-RESULT
                                                           (CNT-DIAG)
                   MOVE    WRK-DIAG-MSG        TO  DISEASES-RESULT-MSG
                                                           (CNT-DIAG)
               WHEN    "W"
                   MOVE    WRK-DIAG-WARNING    TO  DISEASES-DIAG-WARNING
                                                           (CNT-DIAG)
                   MOVE    WRK-DIAG-MSG        TO  DISEASES-DIAG-MSG
                                                           (CNT-DIAG)
           END-EVALUATE
      *
           EVALUATE    WRK-DIAG-WARNING
               WHEN    "W01"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       21
                           OR      PTBYO-BYOMEICD (IDXX)   =   SPACE
                       MOVE    DISEASES-DIAG-CODE (CNT-DIAG)
                                                   TO  WRK-DIAG-CODE
                       MOVE    SPACE               TO
                                           DISEASES-DIAG-CODE (CNT-DIAG)
                       MOVE    PTBYO-BYOMEICD (IDXX)
                                                   TO  WRK-BYOMEICD
                       IF       PTBYO-BYOMEICD (IDXX) (1:3)
                                               =   "ZZZ"
                           MOVE    PTBYO-BYOMEICD (IDXX) (4:)
                                                   TO  WRK-BYOMEICD
                       END-IF
                       IF      IDXX                =   1
                           MOVE    WRK-BYOMEICD        TO
                                           DISEASES-DIAG-CODE (CNT-DIAG)
                       ELSE
                           STRING  WRK-DIAG-CODE   DELIMITED  BY  SPACE
                                   "."
                                   WRK-BYOMEICD    DELIMITED  BY  SPACE
                                       INTO    DISEASES-DIAG-CODE
                                                           (CNT-DIAG)
                           END-STRING
                       END-IF
                   END-PERFORM
               WHEN    OTHER
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       22
                           OR      WRK-TSPICOD (IDXX)   =   SPACE
                       MOVE    DISEASES-DIAG-CODE (CNT-DIAG)
                                                   TO  WRK-DIAG-CODE
                       MOVE    SPACE               TO
                                           DISEASES-DIAG-CODE (CNT-DIAG)
                       IF      IDXX                =   1
                           MOVE    WRK-TSPICOD (IDXX)  TO
                                           DISEASES-DIAG-CODE (CNT-DIAG)
                       ELSE
                           STRING  WRK-DIAG-CODE   DELIMITED  BY  SPACE
                                   "."
                                   WRK-TSPICOD (IDXX)
                                                   DELIMITED  BY  SPACE
                                       INTO    DISEASES-DIAG-CODE
                                                           (CNT-DIAG)
                           END-STRING
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
           IF      FLG-DIAG            =   9
               CONTINUE
           ELSE
               EVALUATE    WRK-DIAG-WARNING (1:1)
                   WHEN    "W"
                       MOVE    1                   TO  FLG-DIAG
                   WHEN    "E"
                       MOVE    9                   TO  FLG-DIAG
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  WRK-DIAG-AREA-X
           INITIALIZE                      WRK-DIAG-AREA-X
           .
       500-WARNING-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      *
           PERFORM 900-SYNONYM-READ-SEC
      *
           IF      FLG-SYNONYM         =   ZERO
               MOVE    SYNONYM-BYOMEICD    TO  BYO-BYOMEICD
               MOVE    ZERO                TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   1
                   MOVE    1                   TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
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
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
            MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E01"
                   MOVE    "患者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E04"
                   MOVE    "電子カルテＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "オルカＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E10"
                   MOVE    "患者番号に該当する患者が存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "診療科が存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E38"
                   STRING  "システム管理（患者番号構成管理情報）が"
                                                   DELIMITED   BY  SIZE
                           "取得できませんでした。"
                                                   DELIMITED   BY  SIZE
                                                   INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "E40"
                   MOVE    "警告がある病名が存在します。"
                                                   TO  WRK-ERRMSG
               WHEN    "E41"
                   MOVE    "病名の設定がありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E42"
                   MOVE    "登録出来ない病名が存在します。"
                                                   TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
           PERFORM  920-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込（主キー）
      *****************************************************************
       900-USBYO-INV-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
                   MOVE    ZERO            TO  FLG-USERBYOMEI
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    1               TO  FLG-USERBYOMEI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-USBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタ読込
      *****************************************************************
       900-SYNONYM-READ-SEC         SECTION.
      *
           MOVE    SYNONYM-REC         TO  MCPDATA-REC
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_synonym"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYNONYM-REC
                   MOVE    ZERO            TO  FLG-SYNONYM
               ELSE
                   INITIALIZE                  SYNONYM-REC
                   MOVE    1               TO  FLG-SYNONYM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYNONYM
               INITIALIZE                      SYNONYM-REC
           END-IF
      *
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYNONYM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       900-COMB-READ-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-N-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       900-COMB-READ-N-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-COMB-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読込
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   INITIALIZE                  PTRSIINF-REC
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "C02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    WRK-PTID            TO  SLOCK-PTID
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
           MOVE    "C02"               TO  SPA-MOTOPG
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *

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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGO03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 定期請求作成−印刷指示（Ｏ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/21    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.01    NACL-太田    03.07.10  印刷処理時処理確認画面へ遷移
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
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
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    定期請求作成指示共通パラメタ
           COPY    "O01COMMON-SPA".
      *
      *    定期請求共通ＳＰＡ
           COPY    "O01SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TEIKIKANRI      PIC 9(01).
           03  FLG-TEIKIPRT        PIC 9(01).
           03  FLG-QTEIKI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SELTAIIN        PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR        PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-SELKBT          PIC 9(06).
           03  CNT-TAIIN           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDXA                PIC 9(05).
           03  IDXB                PIC 9(05).
           03  IDXC                PIC 9(05).
           03  IDX-CMB             PIC 9(05).
           03  IDX-SELKBT          PIC 9(05).
           03  IDX-PMAX            PIC 9(05).
           03  IDX-LMAX            PIC 9(05).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-KBT-G.
               05  WRK-KBT9            PIC 9(01).
               05  WRK-KBTR   REDEFINES    WRK-KBT9.
                   07  WRK-KBTX        PIC X(01).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).
           03  WRK-OIDMSG          PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-TEIKIERR        PIC X(200).
           03  WRK-CNT             PIC 9(05).
           03  WRK-CMB-CD          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM     PIC X(200).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-PNT             PIC 9(05).
           03  WRK-KJNYMD          PIC X(08).
           03  WRK-CURSOR          PIC 9(05).
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
           03  WRK-STIDX           PIC 9(05).
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
      *
       01  SRTSYU-AREA.
           03  SRTSYU-MAX                          PIC 9(05).
           03  SRTSYU-OCC                          OCCURS  3001.
               05  SRTSYU-KEY.
                   07  SRTSYU-TAIINFLG             PIC 9(01).
                   07  SRTSYU-FIL                  PIC X(08).
               05  SRTSYU-KEY-R1 REDEFINES SRTSYU-KEY.
                   07  SRTSYU-R1-FIL               PIC X(01).
                   07  SRTSYU-R1-BRMNUM            PIC X(08).
               05  SRTSYU-KEY-R2 REDEFINES SRTSYU-KEY.
                   07  SRTSYU-R2-FIL               PIC X(01).
                   07  SRTSYU-R2-BRMNUM            PIC X(06).
                   07  SRTSYU-R2-BTUNUM            PIC X(02).
               05  SRTSYU-PTID                     PIC 9(10).
               05  SRTSYU-DENPNUM                  PIC 9(07).
               05  SRTSYU-SRYYM                    PIC X(08).
               05  SRTSYU-KBT                      PIC X(01).
               05  SRTSYU-HAKHOUFLG                PIC 9(01).
               05  SRTSYU-HKNNUM                   PIC X(03).
               05  SRTSYU-BTUNUM                   PIC X(02).
               05  SRTSYU-BRMNUM                   PIC X(08).
               05  SRTSYU-TENNYUYMD                PIC X(08).
               05  SRTSYU-TAIINYMD                 PIC X(08).
               05  SRTSYU-KENSU                    PIC 9(05).
               05  SRTSYU-PAGE                     PIC 9(05).
      *
       01  CONST-AREA.
           03  CONST-SRTSYU-SRT            PIC 9(05)   VALUE   3001.
           03  CONST-SRTSYU-MAX            PIC 9(05)   VALUE   3000.
           03  CONST-KBT-G.
               05  CONST-KBT-VAL.
                   07                      PIC X(01)   VALUE   "1".
                   07                      PIC X(04)   VALUE   "１期".
                   07                      PIC X(01)   VALUE   "2".
                   07                      PIC X(04)   VALUE   "２期".
                   07                      PIC X(01)   VALUE   "3".
                   07                      PIC X(04)   VALUE   "３期".
               05  CONST-KBT-R REDEFINES   CONST-KBT-VAL.
                   07  CONST-KBT-OCC       OCCURS  3.
                       09  CONST-KBT       PIC X(01).
                       09  CONST-KBTMSG    PIC X(04).
           03  CONST-LINE-MAX              PIC 9(02)   VALUE   30.
           03  CONST-PAGE-MAX              PIC 9(02)   VALUE   50.
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID
                                           PIC X(08)   VALUE   "TEIKI2".
           03  CONST-JOBID                 PIC 9(07)   VALUE   1.
           03  CONST-PRT-SHELLID           PIC X(08)   VALUE "TEIKIPRT".
           03  CONST-PRT-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-PRTKANRI-TBL-KEY      PIC X(08)   VALUE   "TEIKI2".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPTEIKI2BTPARA.INC".
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
           COPY    "CPSK5010.INC".
           COPY    "CPSK3103.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    定期請求管理
       01  TEIKIKANRI-REC.
           COPY    "CPTEIKIKANRI.INC".
      *
      *    定期請求クエリ
       01  QTEIKI-REC.
           COPY    "CPQTEIKI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
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
           COPY    "O01.INC".
           COPY    "O02.INC".
           COPY    "O03.INC".
           COPY    "O04.INC".
           COPY    "O05.INC".
           COPY    "O99.INC".
           COPY    "OERR.INC".
           COPY    "OID1.INC".
           COPY    "OID2.INC".
           COPY    "OID3.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-O01FREE
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-O01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "OERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "O03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-O03-CUR
      *    他のＬＤより
           IF    ( LINKAREA    NOT =   SPACE )
      *
               MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01" OR "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-O01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-O01FREE
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG   TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "O04"
               GO  TO      300-SCREEN-EXT
           WHEN    "O99"
               PERFORM 340-O99-SET-SEC
               GO  TO      300-SCREEN-EXT
           WHEN    "OID1"
               PERFORM 330-OID1-SET-SEC
               GO  TO      300-SCREEN-EXT
           WHEN    "OID2"
               PERFORM 340-OID2-SET-SEC
               GO  TO      300-SCREEN-EXT
           WHEN    "OID3"
               PERFORM 350-OID3-SET-SEC
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
      *****MOVE    1               TO  SPA-GMN-O03-PAGE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-O03-AREA
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               INITIALIZE                          SBC-AREA
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-NAI-O03-CUSTOM-ENABLED
               END-IF                                  
           ELSE                                        
               MOVE    "1"             TO  SPA-NAI-O03-CUSTOM-ENABLED
           END-IF
      *
      *    処理区分
           MOVE    "T"             TO  SPA-GMN-O03-SYOKBN
           MOVE    "F"             TO  SPA-GMN-O03-CHKTAIIN
      *
           PERFORM 3101-EDIT-FILEKBN-SEC
      *
           PERFORM 3101-EDIT-COMBO-SEC
      *
           INITIALIZE                  SYS-5000-REC
           MOVE    "5000"          TO  SYS-5000-KANRICD
           MOVE    "*"             TO  SYS-5000-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-5000-STYUKYMD
                                       SYS-5000-EDYUKYMD
           MOVE    SYS-5000-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           INITIALIZE                  SYS-5010-REC
           MOVE    "5010"          TO  SYS-5010-KANRICD
           MOVE    "*"             TO  SYS-5010-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-5010-STYUKYMD
                                       SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5010-REC
      *
           INITIALIZE                  SYS-3103-REC
           MOVE    "3103"          TO  SYS-3103-KANRICD
           MOVE    "5010001"       TO  SYS-3103-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3103-STYUKYMD
                                       SYS-3103-EDYUKYMD
           MOVE    SYS-3103-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-3103-REC
      *
           MOVE    SYS-5010-SKYPRTKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO   SPA-GMN-O03-SKYPRTKBN-G
           ELSE
               MOVE    SYS-5000-SKYPRTKBN  TO  WRK-CMB-CD
               PERFORM 4200-SKYPRTKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO   SPA-GMN-O03-SKYPRTKBN-G
               ELSE
                   MOVE    SPA-GMN-O03-SKYPRTKBNLST (1)
                                       TO  SPA-GMN-O03-SKYPRTKBN-G
               END-IF
           END-IF
      *
           MOVE    SYS-5010-SRYPRTKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-SRYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO   SPA-GMN-O03-SRYPRTKBN-G
           ELSE
               MOVE    SYS-5000-SRYPRTKBN  TO  WRK-CMB-CD
               PERFORM 4200-SRYPRTKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM TO   SPA-GMN-O03-SRYPRTKBN-G
               ELSE
                   MOVE    SPA-GMN-O03-SRYPRTKBNLST (1)
                                       TO  SPA-GMN-O03-SRYPRTKBN-G
               END-IF
           END-IF
      *
           MOVE    SYS-5010-PTINFREFKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-PTINFREFKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-O03-PTINFREFKBN-G
           ELSE
               MOVE    "1"         TO  WRK-CMB-CD
               PERFORM 4200-PTINFREFKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-O03-PTINFREFKBN-G
           END-IF
      *
           MOVE    SYS-5010-PRTJYUN
                                   TO  WRK-CMB-CD
           PERFORM 4200-PRTJYUNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-O03-PRTJYUN-G
           ELSE
               MOVE    SPA-GMN-O03-PRTJYUNLST(01)
                                   TO  SPA-GMN-O03-PRTJYUN-G
           END-IF
      *
           IF    ( SYS-5010-TAIINGRPKBN    =   "1" )
               MOVE    "T"         TO  SPA-GMN-O03-CHKTAIIN
           END-IF
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
            AND  ( SPA-NAI-O03-CUSTOM-ENABLED
                                   =   "1" )
               MOVE    SYS-3103-REPOS-NAME
                                   TO  SPA-NAI-O03-REPOS-NAME
           END-IF
      *
           PERFORM 3101-EDIT-PAGE-SEC
      *
           MOVE    212             TO  SPA-GMN-O03-CUR
      *
           MOVE    1               TO  SPA-GMN-O03-PAGE
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル作成区分編集処理
      *****************************************************************
       3101-EDIT-FILEKBN-SEC           SECTION.
      *
           IF    ( SPA-O01-PRTKBN  =   "1" OR "2" )
               PERFORM 900-TEIKIKANRI-ALL-SEL-SEC
               IF    ( TEIKIKANRI-SYOKBN   =   "2" )
                   MOVE    "個別作成分"    TO  SPA-GMN-O03-FILEKBN
               ELSE
      *
                   MOVE    TEIKIKANRI-SRYYM    TO  SPA-NAI-O03-SRYYM
      *
                   MOVE    SPACE               TO  WRK-SYMD
                   MOVE    TEIKIKANRI-SRYYM    TO  WRK-SYMD (1 : 6)
                   MOVE    "01"                TO  WRK-SYMD (7 : 2)
                   PERFORM 800-HIZUKE-HEN-SEC
                   STRING  WRK-HENYMDG3(1:16) DELIMITED  BY  SIZE
                           "一括作成分"       DELIMITED  BY  SIZE
                   INTO    SPA-GMN-O03-FILEKBN
                   END-STRING
               END-IF
      *
               MOVE    TEIKIKANRI-CREYMD   TO  WRK-SYMD
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3        TO  SPA-GMN-O03-CREYMD
      *
           ELSE
      *
               MOVE    SPA-O01-SRYYM       TO  SPA-NAI-O03-SRYYM
      *
               MOVE    SPA-O01-SRYYM       TO  WRK-SYMD (1 : 6)
               MOVE    "01"                TO  WRK-SYMD (7 : 2)
               PERFORM 800-HIZUKE-HEN-SEC
               STRING  WRK-HENYMDG3(1:16) DELIMITED  BY  SIZE
                       "分再印刷"         DELIMITED  BY  SIZE
               INTO    SPA-GMN-O03-FILEKBN
               END-STRING
      *
               MOVE    ZERO                TO  CNT-SELKBT
               MOVE    1                   TO  WRK-PNT
               PERFORM VARYING IDX-SELKBT  FROM    1   BY  1
                       UNTIL ( IDX-SELKBT  >   3 )
                   IF    ( SPA-O01-KBT (IDX-SELKBT)    =   "1" )
                       COMPUTE CNT-SELKBT  =   CNT-SELKBT  +  1
                       IF    ( CNT-SELKBT  =   1 )
                           STRING  CONST-KBTMSG (IDX-SELKBT)
                                               DELIMITED   BY  SIZE
                           INTO    SPA-GMN-O03-CREYMD
                           WITH    POINTER     WRK-PNT
                           END-STRING
                       ELSE
                           STRING  "、"        DELIMITED   BY  SIZE
                                   CONST-KBTMSG (IDX-SELKBT)
                                               DELIMITED   BY  SIZE
                           INTO    SPA-GMN-O03-CREYMD
                           WITH    POINTER     WRK-PNT
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       3101-EDIT-FILEKBN-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       3101-EDIT-COMBO-SEC             SECTION.
      *
           PERFORM 31011-SKYPRTKBNLST-SEC
      *
           PERFORM 31011-SRYPRTKBNLST-SEC
      *
           PERFORM 31011-PTINFREFKBNLST-SEC
      *
           PERFORM 31011-PRTJYUNMEILST-SEC
      *
           .
       3101-EDIT-COMBO-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分リスト編集処理
      *****************************************************************
       31011-SKYPRTKBNLST-SEC          SECTION.
      *
      *
           MOVE    "0"               TO  SPA-GMN-O03-SKYPRTKBNL   (1)
           MOVE    "発行しない"
                                     TO  SPA-GMN-O03-SKYPRTKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-O03-SKYPRTKBNL   (2)
           MOVE    "発行する"        TO  SPA-GMN-O03-SKYPRTKBNMEIL(2)
      *
           MOVE    "2"               TO  SPA-GMN-O03-SKYPRTKBNL   (3)
           MOVE    "発行する（請求あり）"
                                     TO  SPA-GMN-O03-SKYPRTKBNMEIL(3)
      *
           MOVE    3                 TO  SPA-GMN-O03-SKYPRTKBN-MAX
      *
           .
       31011-SKYPRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    印刷帳票区分リスト編集処理
      *****************************************************************
       31011-SRYPRTKBNLST-SEC          SECTION.
      *
           MOVE    "0"               TO  SPA-GMN-O03-SRYPRTKBNL   (1)
           MOVE    "発行しない"
                                     TO  SPA-GMN-O03-SRYPRTKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-O03-SRYPRTKBNL   (2)
           MOVE    "発行する"        TO  SPA-GMN-O03-SRYPRTKBNMEIL(2)
      *
           MOVE    "2"               TO  SPA-GMN-O03-SRYPRTKBNL   (3)
           MOVE    "発行する（請求あり）"
                                     TO  SPA-GMN-O03-SRYPRTKBNMEIL(3)
      *
           MOVE    3                 TO  SPA-GMN-O03-SRYPRTKBN-MAX
      *
           .
       31011-SRYPRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者情報参照リスト編集処理
      *****************************************************************
       31011-PTINFREFKBNLST-SEC          SECTION.
      *
           MOVE    "0"               TO  SPA-GMN-O03-PTINFREFKBNL   (1)
           MOVE    "参照しない"
                                     TO  SPA-GMN-O03-PTINFREFKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-O03-PTINFREFKBNL   (2)
           MOVE    "参照する"        TO  SPA-GMN-O03-PTINFREFKBNMEIL(2)
      *
           MOVE    2                 TO  SPA-GMN-O03-PTINFREFKBN-MAX
      *
           .
       31011-PTINFREFKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分リスト編集処理
      *****************************************************************
       31011-PRTJYUNMEILST-SEC         SECTION.
      *
           MOVE    "01"                  TO  SPA-GMN-O03-PRTJYUNL    (1)
           MOVE    "病棟番号・カナ氏名順"
                                         TO  SPA-GMN-O03-PRTJYUNMEIL (1)
           MOVE    "02"                  TO  SPA-GMN-O03-PRTJYUNL    (2)
           MOVE    "病棟番号・患者番号順"
                                         TO  SPA-GMN-O03-PRTJYUNMEIL (2)
           MOVE    "03"                  TO  SPA-GMN-O03-PRTJYUNL    (3)
           MOVE    "カナ氏名順"          TO  SPA-GMN-O03-PRTJYUNMEIL (3)
           MOVE    "04"                  TO  SPA-GMN-O03-PRTJYUNL    (4)
           MOVE    "患者番号順"          TO  SPA-GMN-O03-PRTJYUNMEIL (4)
           MOVE    "05"                  TO  SPA-GMN-O03-PRTJYUNL    (5)
           MOVE    "病棟番号・病室番号順"
                                         TO  SPA-GMN-O03-PRTJYUNMEIL (5)
           MOVE    "06"                  TO  SPA-GMN-O03-PRTJYUNL    (6)
           MOVE    "病室番号順"          TO  SPA-GMN-O03-PRTJYUNMEIL (6)
      *
           MOVE    6                     TO  SPA-GMN-O03-PRTJYUN-MAX
      *
           MOVE    SPA-GMN-O03-PRTJYUNLST(01)
                                         TO  SPA-GMN-O03-PRTJYUN-G
      *
           .
       31011-PRTJYUNMEILST-EXT.
           EXIT.
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       3101-EDIT-PAGE-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PATHNAME
      *
      *    処理区分
           MOVE    "T"                 TO  SPA-GMN-O03-SYOKBN
      *
           INITIALIZE                      SPA-GMN-O03-TBL
                                           SPA-NAI-O03-TBL
           MOVE    ZERO                TO  SPA-GMN-O03-PAGEH
                                           SPA-GMN-O03-KENSUH
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-PAGE-MAX )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-LINE-MAX )
                   MOVE    "F"         TO  SPA-GMN-O03-TCHK (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-SELTAIIN
           PERFORM 31011-EDIT-SRTSYU-SEC
      *
           MOVE    ZERO                TO  IDX-PMAX
           MOVE    ZERO                TO  IDX-LMAX
      *
           MOVE    ZERO                TO  IDXA
           PERFORM UNTIL ( IDXA        >=  SRTSYU-MAX )
                    OR   ( IDX-PMAX    >=  CONST-PAGE-MAX )
      *
               COMPUTE IDX-PMAX    =   IDX-PMAX    +   1
               MOVE    ZERO            TO  IDX-LMAX
      *
               PERFORM UNTIL ( IDXA       >=  SRTSYU-MAX )
                    OR   ( IDX-PMAX       >=   CONST-PAGE-MAX )
                    OR   ( IDX-LMAX       >=   CONST-LINE-MAX )
      *
                   COMPUTE IDXA        =   IDXA    +   1
      *
                   COMPUTE IDX-LMAX    =   IDX-LMAX    +   1
      *
                   MOVE    "T"             TO  SPA-GMN-O03-TCHK
                                                   (IDX-PMAX IDX-LMAX)
      *
                   MOVE    SRTSYU-KENSU (IDXA)
                                           TO  SPA-GMN-O03-TKENSU
                                                   (IDX-PMAX IDX-LMAX)
                   MOVE    SRTSYU-PAGE  (IDXA)
                                           TO  SPA-GMN-O03-TPAGE
                                                   (IDX-PMAX IDX-LMAX)
      *
                   COMPUTE SPA-GMN-O03-PAGEH
                       =   SPA-GMN-O03-PAGEH
                       +   SRTSYU-PAGE (IDXA)
      *
                   COMPUTE SPA-GMN-O03-KENSUH
                       =   SPA-GMN-O03-KENSUH
                       +   SRTSYU-KENSU (IDXA)
      *
                   IF    ( SRTSYU-TAIINFLG (IDXA)  =   ZERO )
      *
                       EVALUATE    SPA-GMN-O03-PRTJYUN
                       WHEN    "01"
                       WHEN    "02"
                           PERFORM 31011-EDIT-PAGE-BTUNUM-SEC
                       WHEN    "03"
                           MOVE    "A1"    TO  SPA-NAI-O03-BTUNUM
                                                     (IDX-PMAX IDX-LMAX)
                           MOVE    ZERO    TO  SPA-NAI-O03-BRMNUM
                                                     (IDX-PMAX IDX-LMAX)
                           MOVE    "カナ氏名"
                                           TO  SPA-GMN-O03-TSYURUINAME
                                                     (IDX-PMAX IDX-LMAX)
                       WHEN    "04"
                           MOVE    "A2"    TO  SPA-NAI-O03-BTUNUM
                                                     (IDX-PMAX IDX-LMAX)
                           MOVE    ZERO    TO  SPA-NAI-O03-BRMNUM
                                                     (IDX-PMAX IDX-LMAX)
                           MOVE    "患者番号"
                                           TO  SPA-GMN-O03-TSYURUINAME
                                                     (IDX-PMAX IDX-LMAX)
                       WHEN    "05"
                       WHEN    "06"
                           PERFORM 31011-EDIT-PAGE-BRMNUM-SEC
                       END-EVALUATE
                   ELSE
                       MOVE    "A3"        TO  SPA-NAI-O03-BTUNUM
                                                   (IDX-PMAX IDX-LMAX)
                       MOVE    ZERO        TO  SPA-NAI-O03-BRMNUM
                                                   (IDX-PMAX IDX-LMAX)
                       MOVE    "退院患者"  TO  SPA-GMN-O03-TSYURUINAME
                                                   (IDX-PMAX IDX-LMAX)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    IDX-PMAX            TO  SPA-GMN-O03-MAX
      *
           .
       3101-EDIT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       31011-EDIT-SRTSYU-SEC           SECTION.
      *
           INITIALIZE                  SRTSYU-AREA
      *
           PERFORM 900-QUERY-TEIKI-SEL-SEC
      *
           PERFORM UNTIL ( FLG-QTEIKI  NOT =   ZERO )
                    OR   ( SRTSYU-MAX      >   CONST-SRTSYU-MAX )
      *
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               COMPUTE SRTSYU-MAX  =   SRTSYU-MAX  +   1
      *
               MOVE    QTEIKI-PTID     TO  SRTSYU-PTID    (SRTSYU-MAX)
               MOVE    QTEIKI-DENPNUM  TO  SRTSYU-DENPNUM (SRTSYU-MAX)
               MOVE    QTEIKI-SRYYM    TO  SRTSYU-SRYYM   (SRTSYU-MAX)
               MOVE    QTEIKI-KBT      TO  SRTSYU-KBT     (SRTSYU-MAX)
               MOVE    SYU-GRP-HAKHOUFLG
                                       TO  SRTSYU-HAKHOUFLG (SRTSYU-MAX)
               MOVE    SYU-SYUHKNNUM   TO  SRTSYU-HKNNUM  (SRTSYU-MAX)
               MOVE    QTEIKI-BTUNUM   TO  SRTSYU-BTUNUM  (SRTSYU-MAX)
               MOVE    QTEIKI-BRMNUM   TO  SRTSYU-BRMNUM  (SRTSYU-MAX)
               MOVE    QTEIKI-TENNYUYMD
                                       TO  SRTSYU-TENNYUYMD
                                                          (SRTSYU-MAX)
               MOVE    QTEIKI-TAIINYMD TO  SRTSYU-TAIINYMD(SRTSYU-MAX)
               MOVE    1               TO  SRTSYU-KENSU   (SRTSYU-MAX)
      *
               IF    ( SPA-GMN-O03-CHKTAIIN    =   "T" )
                   IF    ( QTEIKI-TAIINYMD NOT =   "99999999" )
                       MOVE    1               TO  SRTSYU-TAIINFLG 
                                                          (SRTSYU-MAX)
                   END-IF
               END-IF
      *
               EVALUATE    SPA-GMN-O03-PRTJYUN
               WHEN    "01"
               WHEN    "02"
                   MOVE    QTEIKI-BTUNUM
                                       TO  SRTSYU-R1-BRMNUM (SRTSYU-MAX)
               WHEN    "03"
               WHEN    "04"
                   CONTINUE
               WHEN    "05"
                   MOVE    QTEIKI-BRMNUM
                                       TO  SRTSYU-R1-BRMNUM (SRTSYU-MAX)
               WHEN    "06"
                   MOVE    QTEIKI-BRMNUM (3:)
                                       TO  SRTSYU-R2-BRMNUM (SRTSYU-MAX)
                   MOVE    QTEIKI-BTUNUM
                                       TO  SRTSYU-R2-BTUNUM (SRTSYU-MAX)
               END-EVALUATE
      *
               PERFORM 900-QUERY-TEIKI-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-SRTSYU-MAX )
      *
               IF    ( SRTSYU-HKNNUM (IDXA)
                                   =   "971" OR "973" OR "975" )
                OR   ( SRTSYU-HAKHOUFLG (IDXA)
                                   =   0 )
                   MOVE    1       TO  SRTSYU-PAGE (IDXA)
               ELSE
                   MOVE    1       TO  SRTSYU-PAGE (IDXA)
                   PERFORM VARYING IDXB    FROM    1   BY  1
                           UNTIL ( IDXB    >=  IDXA )
                            OR   ( SRTSYU-PAGE (IDXA)
                                           =   ZERO )
                       IF    ( SRTSYU-PTID  (IDXA)
                                        =   SRTSYU-PTID  (IDXB) )
                        AND  ( SRTSYU-SRYYM (IDXA)
                                        =   SRTSYU-SRYYM (IDXB) )
                        AND  ( SRTSYU-KBT   (IDXA)
                                        =   SRTSYU-KBT   (IDXB) )
      *
                           IF    ( SRTSYU-HKNNUM (IDXB)
                                       =   "971" OR "973" OR "975" )
                            OR   ( SRTSYU-HAKHOUFLG (IDXA)
                                   NOT =   SRTSYU-HAKHOUFLG (IDXB))
                               CONTINUE
                           ELSE
                               MOVE    ZERO TO  SRTSYU-PAGE  (IDXA)
                           END-IF
                       END-IF
      *
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >=  SRTSYU-MAX )
      *
               COMPUTE WRK-STIDX   =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDXB    >   SRTSYU-MAX )
      *
                   IF    ( SRTSYU-KEY (IDXA)   >   SRTSYU-KEY (IDXB))
                       MOVE    SRTSYU-OCC (IDXA)
                                       TO  SRTSYU-OCC (CONST-SRTSYU-SRT)
                       MOVE    SRTSYU-OCC (IDXB)
                                       TO  SRTSYU-OCC (IDXA)
                       MOVE    SRTSYU-OCC (CONST-SRTSYU-SRT)
                                       TO  SRTSYU-OCC (IDXB)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    1       TO      IDXA
           PERFORM VARYING IDXB    FROM    2   BY  1
                   UNTIL ( IDXB    >   SRTSYU-MAX )
      *
               IF    ( SRTSYU-KEY (IDXA)        =   SRTSYU-KEY (IDXB))
                OR   ( SRTSYU-TAIINFLG (IDXA)   =   1 )
      *
                   COMPUTE SRTSYU-KENSU (IDXA)
                       =   SRTSYU-KENSU (IDXA)
                       +   SRTSYU-KENSU (IDXB)
      *
                   COMPUTE SRTSYU-PAGE (IDXA)
                       =   SRTSYU-PAGE (IDXA)
                       +   SRTSYU-PAGE (IDXB)
      *
                   IF    ( SRTSYU-TENNYUYMD (IDXA)
                                   <   SRTSYU-TENNYUYMD (IDXB))
                       MOVE    SRTSYU-TENNYUYMD (IDXB)
                                   TO  SRTSYU-TENNYUYMD (IDXA)
                   END-IF
      *
                   INITIALIZE  SRTSYU-OCC (IDXB)
               ELSE
                   COMPUTE IDXA    =   IDXA    +   1
                   IF    ( IDXA    =   IDXB )
                       CONTINUE
                   ELSE
                       MOVE    SRTSYU-OCC (IDXB)
                                   TO  SRTSYU-OCC (IDXA)
                   END-IF
               END-IF
      *
           END-PERFORM
           IF    ( SRTSYU-MAX      >   IDXA )
               MOVE    IDXA        TO  SRTSYU-MAX
           END-IF
      *
           .
       31011-EDIT-SRTSYU-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称編集処理
      *****************************************************************
       31011-EDIT-PAGE-BTUNUM-SEC      SECTION.
      *
           MOVE    SRTSYU-BTUNUM (IDXA)
                                       TO  SPA-NAI-O03-BTUNUM
                                                   (IDX-PMAX IDX-LMAX)
      *
           PERFORM 31011-SYS5001-SEL-SEC
           MOVE    SYS-5001-BTU-NAME   TO  SPA-GMN-O03-TSYURUINAME
                                                   (IDX-PMAX IDX-LMAX)
      *
           .
       31011-EDIT-PAGE-BTUNUM-EXT.
           EXIT.
      *****************************************************************
      *    病室名称編集処理
      *****************************************************************
       31011-EDIT-PAGE-BRMNUM-SEC      SECTION.
      *
           MOVE    SRTSYU-BTUNUM (IDXA)
                                       TO  SPA-NAI-O03-BTUNUM
                                                   (IDX-PMAX IDX-LMAX)
           MOVE    SRTSYU-BRMNUM (IDXA)
                                       TO  SPA-NAI-O03-BRMNUM
                                                   (IDX-PMAX IDX-LMAX)
           PERFORM 31011-SYS5001-SEL-SEC
           PERFORM 31011-SYS5002-SEL-SEC
      *
           STRING  SYS-5001-BTU-NAME       DELIMITED   BY  SPACE
                   "-"                     DELIMITED   BY  SIZE
                   SYS-5002-BRM-NUM        DELIMITED   BY  SPACE
           INTO    SPA-GMN-O03-TSYURUINAME (IDX-PMAX IDX-LMAX)
           END-STRING
      *
           .
       31011-EDIT-PAGE-BRMNUM-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       31011-SYS5001-SEL-SEC           SECTION.
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SRTSYU-BTUNUM (IDXA)TO  SYS-5001-KBNCD
           IF    ( SPA-O01-PRTKBN  =   "1" OR "2" )
               MOVE    SPA-SYSYMD      TO  SYS-5001-STYUKYMD
                                           SYS-5001-EDYUKYMD
           ELSE
               MOVE    SRTSYU-TENNYUYMD (IDXA)
                                       TO  SYS-5001-STYUKYMD
                                           SYS-5001-EDYUKYMD
           END-IF
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           .
       31011-SYS5001-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室情報検索処理
      *****************************************************************
       31011-SYS5002-SEL-SEC           SECTION.
      *
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    SRTSYU-BRMNUM(IDXA) TO  SYS-5002-KBNCD
           IF    ( SPA-O01-PRTKBN  =   "1" OR "2" )
               MOVE    SPA-SYSYMD      TO  SYS-5002-STYUKYMD
                                           SYS-5002-EDYUKYMD
           ELSE
               MOVE    SRTSYU-TENNYUYMD (IDXA)
                                       TO  SYS-5002-STYUKYMD
                                           SYS-5002-EDYUKYMD
           END-IF
           MOVE    SYS-5002-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           .
       31011-SYS5002-SEL-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＯＩＤ１）ＯＫ処理
      *****************************************************************
       330-OID1-SET-SEC            SECTION.
      *
           IF    ( SPA-OID1-FLG    =   "OK" )
               EVALUATE    SPA-OIDCD
               WHEN    "6666"
                   MOVE    "JOIN"  TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    "8888"
                   CONTINUE
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
           MOVE    SPACE           TO  SPA-OIDCD
      *
           .
       330-OID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＯＩＤ２）ＯＫ処理
      *****************************************************************
       340-OID2-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
                                       SPA-O99-FLG
           MOVE    SPACE           TO  SPA-OIDCD
           MOVE    SPACE           TO  SPA-OIDCD2
           .
       340-OID2-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＯＩＤ３）ＯＫ処理
      *****************************************************************
       350-OID3-SET-SEC            SECTION.
      *
           IF    ( SPA-OID1-FLG    NOT =   SPACE )
               EVALUATE    SPA-OIDCD
               WHEN    "1001"
               WHEN    "1002"
                   PERFORM 4501-JOBKANRI-CHK-SEC
                   IF    ( SPA-ERRCD   =   SPACE )
                       PERFORM 450-PRINT-SEC
                   END-IF
                   IF    ( SPA-ERRCD   =   SPACE )
                       IF    ( SPA-OID1-FLG    =   "OK"  OR "PV")
                           PERFORM 451-PRINT-SEC
                       END-IF
                   END-IF
                   IF    ( SPA-ERRCD   =   SPACE )
                       PERFORM 490-KAKUNIN-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-OIDCD
                                       SPA-O99-FLG
           MOVE    SPACE           TO  SPA-OIDCD2
      *
           .
       350-OID3-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理結果画面から戻り時の処理
      *****************************************************************
       340-O99-SET-SEC            SECTION.
      *
           IF    ( SPA-O99-FLG    =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
                                       SPA-O99-FLG
           MOVE    SPACE           TO  SPA-OIDCD
           MOVE    SPACE           TO  SPA-OIDCD2
           .
       340-O99-SET-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "O03"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    0                   TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-O01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "O01"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-O01FREE         TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 460-MAEGMN-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 470-ATOGMN-SEC
      *    処理区分
           WHEN    "CLICKED"       ALSO    "B08"
           WHEN    "CLICKED"       ALSO    "SYOKBN"
               PERFORM 410-SYOKBN-CHK-SEC
      *    請求確認画面へ
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 490-O04-SENI-SEC
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-PRINT-GIDMSG-SEC
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 490-KAKUNIN-SEC
      *    個別出力
           WHEN    "CLICKED"       ALSO    "CHK01" THRU "CHK30"
               MOVE    MCP-WIDGET  TO  WRK-WIDGET
               MOVE    WRK-WIDGET2 TO  IDX2
               PERFORM 420-CHK-CHK-SEC
      *    退院済患者集計チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKTAIIN"
               MOVE    O03-CHKTAIIN TO SPA-GMN-O03-CHKTAIIN
               MOVE    1               TO  SPA-GMN-O03-PAGE
               PERFORM 3101-EDIT-PAGE-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "PRTJYUN"
               PERFORM 4105-PRTJYUN-CHK-SEC
           WHEN    OTHER
      *
               MOVE    ZERO        TO  SPA-GMN-O03-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    請求書発行区分
           MOVE    O03-SKYPRTKBN   TO  SPA-GMN-O03-SKYPRTKBN-G
      *
      *    明細書発行区分
           MOVE    O03-SRYPRTKBN
                                   TO  SPA-GMN-O03-SRYPRTKBN-G
      *
      *    患者情報参照区
           MOVE    O03-PTINFREFKBN
                                   TO  SPA-GMN-O03-PTINFREFKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    請求書発行区分チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4105-SKYPRTKBN-CHK-SEC
           END-IF
      *
      *    明細書発行区分チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4105-SRYPRTKBN-CHK-SEC
           END-IF
      *
      *    患者情報参照区分チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4105-PTINFREFKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行区分チェック
      *****************************************************************
       4105-SRYPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-O03-SRYPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-SRYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-O03-SRYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-O03-SRYPRTKBNLST (1)
                                       TO  SPA-GMN-O03-SRYPRTKBN-G
           END-IF
      *
           .
       4105-SRYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報参照区分チェック
      *****************************************************************
       4105-PTINFREFKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-O03-PTINFREFKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-PTINFREFKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-O03-PTINFREFKBN-G
           ELSE
               MOVE    SPA-GMN-O03-PTINFREFKBNLST (2)
                                   TO  SPA-GMN-O03-PTINFREFKBN-G
           END-IF
      *
           .
       4105-PTINFREFKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分チェック
      *****************************************************************
       4105-SKYPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-O03-SKYPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-O03-SKYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-O03-SKYPRTKBNLST (1)
                                       TO  SPA-GMN-O03-SKYPRTKBN-G
           END-IF
      *
           .
       4105-SKYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷順入力処理
      *****************************************************************
       4105-PRTJYUN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-O03-CUR
      *
           MOVE    O03-PRTJYUN         TO  SPA-GMN-O03-PRTJYUN-G
           MOVE    SPA-GMN-O03-PRTJYUN TO  WRK-CMB-CD
           PERFORM 4200-PRTJYUNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    1               TO  SPA-GMN-O03-PAGE
               PERFORM 3101-EDIT-PAGE-SEC
           END-IF
      *
           .
       4105-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力順コンボチェック
      *****************************************************************
       4200-PRTJYUNLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O03-PRTJYUN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O03-PRTJYUNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O03-PRTJYUN-MAX )
               MOVE    SPA-GMN-O03-PRTJYUNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PRTJYUNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボチェック
      *****************************************************************
       4200-SKYPRTKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O03-SKYPRTKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O03-SKYPRTKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O03-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-O03-SKYPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行区分コンボチェック
      *****************************************************************
       4200-SRYPRTKBNLST-CHK-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O03-SRYPRTKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O03-SRYPRTKBNL
                                                           (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O03-SRYPRTKBN-MAX )
               MOVE    SPA-GMN-O03-SRYPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SRYPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定区分コンボチェック
      *****************************************************************
       4200-PTINFREFKBNLST-CHK-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O03-PTINFREFKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O03-PTINFREFKBNL
                                                           (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O03-PTINFREFKBN-MAX )
               MOVE    SPA-GMN-O03-PTINFREFKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTINFREFKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る前処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-OIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "O01"               TO  SPA-SAKIPG
           MOVE    "O03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分  入力処理
      *****************************************************************
       410-SYOKBN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-O03-CUR
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-PAGE-MAX )
               PERFORM VARYING IDX2  FROM    1   BY  1
                       UNTIL ( IDX2  >     CONST-LINE-MAX )
                       OR    ( SPA-GMN-O03-TPAGE (IDX1 IDX2)
                                         =     ZERO )
                   MOVE    "F"     TO  SPA-GMN-O03-TCHK  (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           EVALUATE    SPA-GMN-O03-SYOKBN
      *    *指定印刷
           WHEN    "T"
               MOVE    "F"             TO  SPA-GMN-O03-SYOKBN
               MOVE    4               TO  SPA-GMN-O03-CUR
      *
      *    *全件印刷
           WHEN    "F"
               MOVE    "T"             TO  SPA-GMN-O03-SYOKBN
               MOVE    212             TO  SPA-GMN-O03-CUR
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >       CONST-PAGE-MAX )
                   PERFORM VARYING IDX2  FROM    1   BY  1
                           UNTIL ( IDX2  >     CONST-LINE-MAX )
                           OR    ( SPA-GMN-O03-TPAGE (IDX1 IDX2)
                                         =     ZERO )
                       MOVE    "T"     TO  SPA-GMN-O03-TCHK  (IDX1 IDX2)
                   END-PERFORM
               END-PERFORM
           END-EVALUATE 
      *
           .
       410-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別出力  入力処理
      *****************************************************************
       420-CHK-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-O03-SYOKBN  =   "T" )
               MOVE    "F"             TO  SPA-GMN-O03-SYOKBN
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-O03-CUR
      *
           COMPUTE WRK-CURSOR  =   IDX2    +   2
      *
           MOVE    SPA-GMN-O03-PAGE        TO  IDX1
      *
           IF    ( O03-TCHK-VALUE (IDX2)   =   "T" )
           AND   ( SPA-GMN-O03-TPAGE  (IDX1 IDX2)
                                       =   ZERO )
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               MOVE    O03-TCHK-VALUE (IDX2)
                                       TO  SPA-GMN-O03-TCHK (IDX1 IDX2)
           END-IF
      *
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力前処理
      *****************************************************************
       450-PRINT-GIDMSG-SEC                SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
      *    請求データが選択されているか
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-O03-MAX )
                    OR   ( IDX3    >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-LINE-MAX )
                        OR   ( IDX3    >   ZERO )
                   IF    ( SPA-GMN-O03-TCHK (IDX1 IDX2)    =   "T" )
                       MOVE    1        TO  IDX3
                   END-IF
               END-PERFORM
           END-PERFORM
           IF  (   IDX3            =   ZERO )
               MOVE    "0008"      TO  SPA-ERRCD
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
           IF  (   SPA-GMN-O03-SKYPRTKBN   =   "0" )
            AND(   SPA-GMN-O03-SRYPRTKBN   =   "0" )
               MOVE    "0011"      TO  SPA-ERRCD
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
      *    ガイドメッセージセット
           EVALUATE    SPA-GMN-O03-SYOKBN
           WHEN    "T"
               MOVE    "1001"          TO  SPA-OIDCD
           WHEN    "F"
               MOVE    "1002"          TO  SPA-OIDCD
           END-EVALUATE
      *
           .
       450-PRINT-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   MOVE    "7777"  TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出力処理
      *****************************************************************
       450-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      SPA-O01-TBL-GROUP
      *
           PERFORM 4501-BTPARA-DEL-SEC
      *
           MOVE    "ALL"               TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
      *
           IF    ( SPA-GMN-O03-SYOKBN  =   "F" )
               PERFORM 4501-BTPARA-INS-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  450-PRINT-EXT
               END-IF
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SMCNDATE-YMD        TO  LNK-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS        TO  LNK-PRTKANRI-TBL-GROUP (9:)
           MOVE    1                   TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    SPA-O01-SRYYM       TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "teiki2.sh"         TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    SPA-HOSPNUM         TO  TEIKIERR-HOSPNUM
           MOVE    "OR033ERR"          TO  TEIKIERR-FILE-ID
           MOVE    SPA-TERMID          TO  TEIKIERR-TERMID
           MOVE    3                   TO  TEIKIERR-SYOKBN1
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "teiki2.sh"         TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    CONST-JOBID         TO  SHELLTBL-JOBID
           MOVE    CONST-SHELLID       TO  SHELLTBL-SHELLID
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
            AND  ( SPA-NAI-O03-CUSTOM-ENABLED
                                   =   "1" )
               MOVE    SPA-NAI-O03-REPOS-NAME
                                       TO  SHELLTBL-REPOS-NAME
           END-IF
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                       TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                       TO  SHELLTBL-ARG10
      *
           IF    ( SPA-GMN-O03-SYOKBN  =   "T" )
               MOVE    "0"             TO  SHELLTBL-ARG11
           ELSE
               MOVE    "1"             TO  SHELLTBL-ARG11
           END-IF
           MOVE    SPA-GMN-O03-PRTJYUN TO  SHELLTBL-ARG12
           MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG13
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG14
           MOVE    CONST-JOBID         TO  SHELLTBL-ARG15
           MOVE    CONST-SHELLID       TO  SHELLTBL-ARG16
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG17
           MOVE    SPA-STAFFCD         TO  SHELLTBL-ARG18
           MOVE    TEIKIERR-BASENAME   TO  SHELLTBL-ARG19
           MOVE    SPA-GMN-O03-PTINFREFKBN
                                       TO  SHELLTBL-ARG20(1:1)
           MOVE    SPA-GMN-O03-SKYPRTKBN
                                       TO  SHELLTBL-ARG20(2:1)
           MOVE    SPA-GMN-O03-SRYPRTKBN
                                       TO  SHELLTBL-ARG20(3:1)
           MOVE    SPA-O01-PRTKBN      TO  SHELLTBL-ARG21
           MOVE    SPA-O01-SRYYM       TO  SHELLTBL-ARG22
           MOVE    SPA-O01-KBT-G       TO  SHELLTBL-ARG23
           MOVE    SPA-OID1-FLG        TO  SHELLTBL-ARG24
           IF    ( SPA-GMN-O03-CHKTAIIN    =   "T" )
               MOVE    "1"             TO  SHELLTBL-ARG25
           ELSE
               MOVE    "0"             TO  SHELLTBL-ARG25
           END-IF
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "monthly"           TO  MCP-PATHNAME
           MOVE    "SHELL"             TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPA-O01-TBL-GROUP
      *
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       451-PRINT-SEC                   SECTION.
      *
           MOVE    ZERO                    TO  WRK-STPAGE
           MOVE    99999                   TO  WRK-EDPAGE
      *
           MOVE    SPACE                   TO  SHELLTBL
           INITIALIZE                          SHELLTBL
      *
           MOVE    "print_prv.sh"          TO  SHELLTBL-NAME
      *
           MOVE    "print_prv.sh"          TO  LNK-PRTKANRI-SHELLID
      *
           MOVE    LNK-PRTKANRI-RENNUM     TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM      TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID    TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID     TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID       TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM      TO  SHELLTBL-ARG11
      *
           MOVE    TEIKIERR-BASENAME       TO  SHELLTBL-ARG12
      *
      *    ジョブ管理は作成しない(下記はDUMMY)
           MOVE    CONST-PRT-JOBID         TO  SHELLTBL-ARG13
           MOVE    CONST-PRT-SHELLID       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE              TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE              TO  SHELLTBL-ARG16
           EVALUATE    SPA-OID1-FLG
           WHEN    "PV"
                MOVE    "3"                TO  SHELLTBL-ARG17
      *    WHEN    "NO"
      *         MOVE    "3"                TO  SHELLTBL-ARG17
           WHEN    "OK"
               IF    SPA-CLIENT-PRT-FLG  =  2
                 MOVE    "5"             TO  SHELLTBL-ARG17
               ELSE
                 MOVE    "2"             TO  SHELLTBL-ARG17
               END-IF
           END-EVALUATE
           MOVE    SPA-HOSPNUM             TO  SHELLTBL-ARG18
           MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *
           MOVE    SHELLTBL                TO  MCPDATA-REC
           MOVE    "SHELL"                 TO  MCP-FUNC
           MOVE    "shell"                 TO  MCP-TABLE
           MOVE    "allways"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       451-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    バッチパラメタ削除処理
      *****************************************************************
       4501-BTPARA-DEL-SEC            SECTION.
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-SHELLID       TO  BTPARA-SHELLID
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       4501-BTPARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ更新処理
      *****************************************************************
       4501-BTPARA-INS-SEC            SECTION.
      *
           INITIALIZE              BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-SHELLID   TO  BTPARA-SHELLID
           MOVE    ZERO            TO  BTPARA-RENNUM
           MOVE    CONST-JOBID     TO  BTPARA-JOBID
           MOVE    SPA-TERMID      TO  BTPARA-TERMID
           MOVE    SPA-OPID        TO  BTPARA-OPID
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD    TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  BTPARA-UPHMS
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-O03-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   CONST-LINE-MAX  )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-O03-TCHK (IDX1 IDX2)    =   "T" )
      *
                       COMPUTE BTPARA-RENNUM   =   BTPARA-RENNUM   +   1
      *
                       INITIALIZE              TEIKI2BTPARA
                       MOVE    SPA-NAI-O03-BTUNUM (IDX1 IDX2)
                                               TO  TEIKI2BT-BTUNUM
                       MOVE    SPA-NAI-O03-BRMNUM (IDX1 IDX2)
                                               TO  TEIKI2BT-BRMNUM
                       MOVE    TEIKI2BTPARA    TO  BTPARA-INFO-PARA
      *
                       MOVE    BTPARA-REC      TO  MCPDATA-REC
                       MOVE    "tbl_btpara"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                           MOVE    "4001"      TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       4501-BTPARA-INS-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC                  SECTION.
      *
           IF    ( SPA-GMN-O03-PAGE       >   1 )
               COMPUTE SPA-GMN-O03-PAGE   =   SPA-GMN-O03-PAGE    -   1
               MOVE    212                TO  SPA-GMN-O03-CUR
           END-IF
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC                  SECTION.
      *
           IF    ( SPA-GMN-O03-PAGE       <   SPA-GMN-O03-MAX )
               COMPUTE SPA-GMN-O03-PAGE   =   SPA-GMN-O03-PAGE    +   1
               MOVE    212                TO  SPA-GMN-O03-CUR
           END-IF
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    "O03"           TO  SPA-MOTOPG
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGO99"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "O03"           TO  SPA-MOTOPG
           MOVE    "O99"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "O99"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-O04-SENI-SEC                SECTION.
      *
           MOVE    "O04"               TO  SPA-SAKIPG
           MOVE    "O03"               TO  SPA-MOTOPG
           MOVE    "O03"               TO  SPA-O01-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-O04-SENI-EXT.
           EXIT.
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-O01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "O03"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID   TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-OIDCD       NOT =   SPACE )
               EVALUATE    SPA-OIDCD
               WHEN    "1001"
               WHEN    "1002"
                   PERFORM 530-OID3SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-OIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "O03    "           TO  MCP-WINDOW.
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
           INITIALIZE     O03
      *
           MOVE    SPA-GMN-O03-SYOKBN      TO  O03-SYOKBN-VALUE
           EVALUATE    SPA-GMN-O03-SYOKBN
           WHEN    "T"
               MOVE    "全件印刷"          TO  O03-SYOKBN-LABEL
           WHEN    "F"
               MOVE    "指定印刷"          TO  O03-SYOKBN-LABEL
           END-EVALUATE      
      *
           MOVE    SPA-GMN-O03-FILEKBN     TO  O03-FILEKBN
      *
           MOVE    SPA-GMN-O03-CREYMD      TO  O03-CREYMD
      *
           MOVE    SPA-GMN-O03-PAGEH       TO  O03-PAGE
      *
           MOVE    SPA-GMN-O03-KENSUH      TO  O03-KENSU
      *
           MOVE    SPA-GMN-O03-SKYPRTKBN-G TO  O03-SKYPRTKBN
           MOVE    SPA-GMN-O03-SKYPRTKBN-MAX
                                           TO  O03-SKYPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-O03-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-O03-SKYPRTKBNLST(IDX1)
                                           TO  O03-SKYPRTKBNLST (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-O03-SRYPRTKBN-G TO  O03-SRYPRTKBN
           MOVE    SPA-GMN-O03-SRYPRTKBN-MAX
                                           TO  O03-SRYPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-O03-SRYPRTKBN-MAX )
               MOVE    SPA-GMN-O03-SRYPRTKBNLST(IDX1)
                                           TO  O03-SRYPRTKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-O03-PTINFREFKBN-G TO  O03-PTINFREFKBN
           MOVE    SPA-GMN-O03-PTINFREFKBN-MAX
                                           TO  O03-PTINFREFKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-O03-PTINFREFKBN-MAX )
               MOVE    SPA-GMN-O03-PTINFREFKBNLST(IDX1)
                                           TO  O03-PTINFREFKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-O03-PRTJYUN-G   TO  O03-PRTJYUN
           MOVE    SPA-GMN-O03-PRTJYUN-MAX TO  O03-PRTJYUN-COUNT
           PERFORM VARYING IDX1    FROM    1       BY  1
                   UNTIL ( IDX1            >   SPA-GMN-O03-PRTJYUN-MAX )
               MOVE    SPA-GMN-O03-PRTJYUNLST(IDX1)
                                           TO  O03-PRTJYUNLST (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-O03-CHKTAIIN    TO  O03-CHKTAIIN
      *
           MOVE    SPA-GMN-O03-PAGE        TO  IDX1
      *
           PERFORM VARYING   IDX2  FROM    1   BY  1
                   UNTIL   ( IDX2  >       CONST-LINE-MAX )
      *
               IF    ( SPA-GMN-O03-TPAGE (IDX1 IDX2)   =   ZERO )
                   MOVE    WIDGET-INSENSITIVE
                                           TO  O03-TCHK-STATE (IDX2)
               ELSE 
                   MOVE    WIDGET-NORMAL   TO  O03-TCHK-STATE (IDX2)
               END-IF
      *
               IF    ( SPA-GMN-O03-SYOKBN  =   "F" )
                AND  ( SPA-GMN-O03-TCHK    (IDX1 IDX2) =   "T" )
                   MOVE    "blue-rev"      TO  O03-TPAGE-STYLE  (IDX2)
                                               O03-TKENSU-STYLE (IDX2)
                   MOVE    "blue-bar"      TO  O03-TCHK-STYLE   (IDX2)
               ELSE
                   MOVE    "black"         TO  O03-TPAGE-STYLE  (IDX2)
                                               O03-TKENSU-STYLE (IDX2)
                                               O03-TCHK-STYLE   (IDX2)
               END-IF
      *
               MOVE    SPA-GMN-O03-TKENSU  (IDX1 IDX2)
                                           TO  O03-TKENSU     (IDX2)
               MOVE    SPA-GMN-O03-TPAGE   (IDX1 IDX2)
                                           TO  O03-TPAGE      (IDX2)
               MOVE    SPA-GMN-O03-TCHK    (IDX1 IDX2)
                                           TO  O03-TCHK-VALUE (IDX2) 
               MOVE    SPA-GMN-O03-TSYURUINAME (IDX1 IDX2)
                                           TO  O03-TCHK-LABEL (IDX2)
           END-PERFORM
      *
           MOVE    "red"                   TO  O03-LBLMSG-STYLE
           MOVE    SPA-O99-MSG             TO  O03-LBLMSG
      *
           IF    ( SPA-O01-PRTKBN      =   "1" OR "2" )
               MOVE    "作成日"            TO  O03-LBLCREYMD
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE      TO  O03-B06-STATE
                                               O03-B07-STATE
      *
           IF    ( SPA-GMN-O03-PAGE    >   1 )
               MOVE    WIDGET-NORMAL       TO  O03-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-O03-PAGE    <   SPA-GMN-O03-MAX )
               MOVE    WIDGET-NORMAL       TO  O03-B07-STATE
           END-IF
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
           IF    ( SPA-GMN-O03-CUR         =   ZERO )
               PERFORM 50011-GMN-CUR-SET-SEC
           END-IF
      *
           PERFORM 50011-GMN-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-O03-CUR
           WHEN    001
               MOVE  "SYOKBN"      TO  MCP-WIDGET
           WHEN    002
               MOVE  "PRTJYUN"     TO  MCP-WIDGET
           WHEN    003
               MOVE  "CHK01"       TO  MCP-WIDGET
           WHEN    004
               MOVE  "CHK02"       TO  MCP-WIDGET
           WHEN    005
               MOVE  "CHK03"       TO  MCP-WIDGET
           WHEN    006
               MOVE  "CHK04"       TO  MCP-WIDGET
           WHEN    007
               MOVE  "CHK05"       TO  MCP-WIDGET
           WHEN    008
               MOVE  "CHK06"       TO  MCP-WIDGET
           WHEN    009
               MOVE  "CHK07"       TO  MCP-WIDGET
           WHEN    010
               MOVE  "CHK08"       TO  MCP-WIDGET
           WHEN    011
               MOVE  "CHK09"       TO  MCP-WIDGET
           WHEN    012
               MOVE  "CHK10"       TO  MCP-WIDGET
           WHEN    013
               MOVE  "CHK11"       TO  MCP-WIDGET
           WHEN    014
               MOVE  "CHK12"       TO  MCP-WIDGET
           WHEN    015
               MOVE  "CHK13"       TO  MCP-WIDGET
           WHEN    016
               MOVE  "CHK14"       TO  MCP-WIDGET
           WHEN    017
               MOVE  "CHK15"       TO  MCP-WIDGET
           WHEN    018
               MOVE  "CHK16"       TO  MCP-WIDGET
           WHEN    019
               MOVE  "CHK17"       TO  MCP-WIDGET
           WHEN    020
               MOVE  "CHK18"       TO  MCP-WIDGET
           WHEN    021
               MOVE  "CHK19"       TO  MCP-WIDGET
           WHEN    022
               MOVE  "CHK20"       TO  MCP-WIDGET
           WHEN    023
               MOVE  "CHK21"       TO  MCP-WIDGET
           WHEN    024
               MOVE  "CHK22"       TO  MCP-WIDGET
           WHEN    025
               MOVE  "CHK23"       TO  MCP-WIDGET
           WHEN    026
               MOVE  "CHK24"       TO  MCP-WIDGET
           WHEN    027
               MOVE  "CHK25"       TO  MCP-WIDGET
           WHEN    028
               MOVE  "CHK26"       TO  MCP-WIDGET
           WHEN    029
               MOVE  "CHK27"       TO  MCP-WIDGET
           WHEN    030
               MOVE  "CHK28"       TO  MCP-WIDGET
           WHEN    031
               MOVE  "CHK29"       TO  MCP-WIDGET
           WHEN    032
               MOVE  "CHK30"       TO  MCP-WIDGET
           WHEN    101
               MOVE  "SRYPRTKBN"
                                   TO  MCP-WIDGET
           WHEN    102
               MOVE  "SKYPRTKBN"   TO  MCP-WIDGET
           WHEN    103
               MOVE  "PTINFREFKBN" TO  MCP-WIDGET
           WHEN    212
               MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力カーソル位置編集処理
      *****************************************************************
       50011-GMN-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SRYPRTKBN"
               MOVE    103         TO  SPA-GMN-O03-CUR
           WHEN    "SYOKBN"
               MOVE    102         TO  SPA-GMN-O03-CUR
           WHEN    "SKYPRTKBN"
               MOVE    101         TO  SPA-GMN-O03-CUR
           WHEN    "PTINFREFKBN"
               MOVE    002         TO  SPA-GMN-O03-CUR
           WHEN    "PRTJYUN"
               MOVE    102         TO  SPA-GMN-O03-CUR
           WHEN    "CHK01"
               MOVE    004         TO  SPA-GMN-O03-CUR
           WHEN    "CHK02"
               MOVE    005         TO  SPA-GMN-O03-CUR
           WHEN    "CHK03"
               MOVE    006         TO  SPA-GMN-O03-CUR
           WHEN    "CHK04"
               MOVE    007         TO  SPA-GMN-O03-CUR
           WHEN    "CHK05"
               MOVE    008         TO  SPA-GMN-O03-CUR
           WHEN    "CHK06"
               MOVE    009         TO  SPA-GMN-O03-CUR
           WHEN    "CHK07"
               MOVE    010         TO  SPA-GMN-O03-CUR
           WHEN    "CHK08"
               MOVE    011         TO  SPA-GMN-O03-CUR
           WHEN    "CHK09"
               MOVE    012         TO  SPA-GMN-O03-CUR
           WHEN    "CHK10"
               MOVE    013         TO  SPA-GMN-O03-CUR
           WHEN    "CHK11"
               MOVE    014         TO  SPA-GMN-O03-CUR
           WHEN    "CHK12"
               MOVE    015         TO  SPA-GMN-O03-CUR
           WHEN    "CHK13"
               MOVE    016         TO  SPA-GMN-O03-CUR
           WHEN    "CHK14"
               MOVE    017         TO  SPA-GMN-O03-CUR
           WHEN    "CHK15"
               MOVE    018         TO  SPA-GMN-O03-CUR
           WHEN    "CHK16"
               MOVE    019         TO  SPA-GMN-O03-CUR
           WHEN    "CHK17"
               MOVE    020         TO  SPA-GMN-O03-CUR
           WHEN    "CHK18"
               MOVE    021         TO  SPA-GMN-O03-CUR
           WHEN    "CHK19"
               MOVE    022         TO  SPA-GMN-O03-CUR
           WHEN    "CHK20"
               MOVE    023         TO  SPA-GMN-O03-CUR
           WHEN    "CHK21"
               MOVE    024         TO  SPA-GMN-O03-CUR
           WHEN    "CHK22"
               MOVE    025         TO  SPA-GMN-O03-CUR
           WHEN    "CHK23"
               MOVE    026         TO  SPA-GMN-O03-CUR
           WHEN    "CHK24"
               MOVE    027         TO  SPA-GMN-O03-CUR
           WHEN    "CHK25"
               MOVE    028         TO  SPA-GMN-O03-CUR
           WHEN    "CHK26"
               MOVE    029         TO  SPA-GMN-O03-CUR
           WHEN    "CHK27"
               MOVE    030         TO  SPA-GMN-O03-CUR
           WHEN    "CHK28"
               MOVE    031         TO  SPA-GMN-O03-CUR
           WHEN    "CHK29"
               MOVE    032         TO  SPA-GMN-O03-CUR
           WHEN    "CHK30"
               MOVE    212         TO  SPA-GMN-O03-CUR
           END-EVALUATE
      *
           .
       50011-GMN-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力カーソルスキップ処理
      *****************************************************************
       50011-GMN-CUR-SKIP-SEC          SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-LINE-MAX )
      *
               COMPUTE WRK-CURSOR  =   IDX2    +   2
      *
               IF    ( SPA-GMN-O03-CUR =   WRK-CURSOR )
                   IF    ( O03-TCHK-STATE (IDX2)
                                   =   WIDGET-INSENSITIVE )
                       IF    ( IDX2    =   CONST-LINE-MAX )
                           MOVE    212 TO  SPA-GMN-O03-CUR
                       ELSE
                           COMPUTE SPA-GMN-O03-CUR
                               =   SPA-GMN-O03-CUR +   1
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       50011-GMN-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラーです"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "印刷する請求書がありません" 
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE  "印刷用パラメタファイルが作成できませんでした"
                                           TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "一括出力のとき入力できません"
                                           TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "個別出力のとき入力できません"
                                           TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "印刷する請求書が選択してありません"
                                           TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "前頁はありません"  TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "次頁はありません"  TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "処理対象の帳票が選択してありません"
                                           TO  SPA-ERRMSG
           WHEN    "4001"
               MOVE    "バッチパラメタの更新に失敗しました"
                                           TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    "請求書印刷中です"  TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    WRK-TEIKIERR        TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  OERR
           MOVE    SPA-ERRCD            TO  OERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  OERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "O03"                TO  SPA-MOTOPG
           MOVE    "OERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "OERR"               TO  MCP-WINDOW
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
       520-OIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD
           WHEN    "1001"
           WHEN    "1002"
               MOVE    "印刷処理を開始します。よろしいですか"
                                       TO  WRK-OIDMSG
           WHEN    "6666"
               MOVE    "請求書印刷中です"
                                       TO  WRK-OIDMSG
           WHEN    "8888"
               MOVE    "処理が正常に終了しました"
                                       TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  OID1
           INITIALIZE                      OID1
           MOVE    SPA-OIDCD           TO  OID1-ID1CODE
           MOVE    WRK-OIDMSG          TO  OID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "O03"               TO  SPA-MOTOPG
           MOVE    "OID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "OID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-OIDSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       530-OID3SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD
           WHEN    "1001"
           WHEN    "1002"
               MOVE    "印刷処理を開始します。よろしいですか"
                                       TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  OID3
           INITIALIZE                      OID3
           MOVE    SPA-OIDCD           TO  OID3-ID1CODE
           MOVE    WRK-OIDMSG          TO  OID3-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "O03"               TO  SPA-MOTOPG
           MOVE    "OID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "OID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-OID3SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       800-HIZUKE-HEN-SEC              SECTION.
      *
           IF    ( WRK-SYMD            =   ALL "9"  OR   SPACE )
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
      *
           .
       800-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-JOBID         TO  JOB-JOBID
           MOVE    CONST-SHELLID       TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC 
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期請求管理検索処理(ALL)
      *****************************************************************
       900-TEIKIKANRI-ALL-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIKANRI
      *
           INITIALIZE                      TEIKIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  TEIKIKANRI-HOSPNUM
           MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIKANRI
               INITIALIZE                  TEIKIKANRI-REC
           END-IF
      *
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIKANRI-ALL-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求クエリ検索処理
      *****************************************************************
       900-QUERY-TEIKI-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
           INITIALIZE                  QTEIKI-REC
      *
           IF    ( SPA-O01-PRTKBN  =   "3" )
               MOVE    SPA-O01-SRYYM   TO  QTEIKI-SRYYM
               MOVE    ZERO            TO  CNT-SELKBT
               PERFORM VARYING IDX-SELKBT  FROM    1   BY  1
                       UNTIL ( IDX-SELKBT  >   3 )
                   IF    ( SPA-O01-KBT (IDX-SELKBT)    =   "1" )
                       COMPUTE CNT-SELKBT
                           =   CNT-SELKBT  +   1
                       IF    ( CNT-SELKBT  =   1 )
                           MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (1)
                                       QTEIKI-SELKBT (2)
                                       QTEIKI-SELKBT (3)
                       ELSE
                           MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (CNT-SELKBT)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF    ( SPA-GMN-O03-CHKTAIIN    =   "T" )
               IF    ( FLG-SELTAIIN    =   ZERO )
                   MOVE    "99999999"  TO  QTEIKI-SELTAIINYMD(1)
                                           QTEIKI-SELTAIINYMD(2)
               ELSE
                   MOVE    "1%"        TO  QTEIKI-SELTAIINYMD(1)
                   MOVE    "2%"        TO  QTEIKI-SELTAIINYMD(2)
               END-IF
           ELSE
                   MOVE    "%"         TO  QTEIKI-SELTAIINYMD(1)
                                           QTEIKI-SELTAIINYMD(2)
           END-IF
           MOVE    SPA-HOSPNUM     TO  QTEIKI-HOSPNUM
           MOVE    QTEIKI-REC      TO  MCPDATA-REC
           PERFORM 9001-EDIT-QTEIKI-KEY-SEC
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           .
       900-QUERY-TEIKI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求クエリキー選択処理
      *****************************************************************
       9001-EDIT-QTEIKI-KEY-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-PATHNAME
      *
           IF    ( SPA-O01-PRTKBN  =   "1" OR "2" )
               MOVE    "key21"     TO  WRK-PATHNAME
           ELSE
               MOVE    "key22"     TO  WRK-PATHNAME
           END-IF
      *
           .
       9001-EDIT-QTEIKI-KEY-EXT.
           EXIT.
      *****************************************************************
      *    定期請求クエリFETCH処理
      *****************************************************************
       900-QUERY-TEIKI-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           .
       900-QUERY-TEIKI-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "1"             TO  SYU-NYUGAIKBN
           MOVE    QTEIKI-PTID     TO  SYU-PTID
           MOVE    QTEIKI-DENPNUM  TO  SYU-DENPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
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
      *    テーブルクローズ処理
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
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.

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
       PROGRAM-ID.             ORCBG009.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次処理
      *  コンポーネント名  : 診療内容参考資料
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/07/23    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.01.01     NACL-森脇     03/08/21  分類区分判定
      * 01.01.02     NACL-森脇     03/08/26  使用量カウント構成変更
      * 01.01.03     NACL-森脇     03/08/27  患者指定時修正
      * 01.02.01     NACL-森脇     04/01/23  包括分読み飛ばし
      * 01.02.02     NACL-森脇     04/08/31  条件修正
      * 03.05.01     NACL-森脇     07/06/04  グループ診療対応
      * 04.08.01     NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      * 04.08.02     NACL-森脇     16/09/01  持参薬対象外
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
      *----(01.01.02)--ADD-START---
               05  TOKEI-NYUGAIKBN     PIC X(01).
      *----(01.01.02)--ADD-END-----
               05  TOKEI-PTID          PIC 9(10).
               05  TOKEI-YKZKBN        PIC 9(01).
      *----(01.01.01)--ADD-START---
               05  TOKEI-BUNRUI        PIC 9(02).
      *----(01.01.01)--ADD-END-----
               05  TOKEI-SRYCD         PIC X(09).
      *----(01.01.02)--UPD-START---
               05  TOKEI-ZAINUM        PIC 9(08).
           03  TOKEI-NAME              PIC X(54).
           03  TOKEI-TANINAME          PIC X(16).
      *----(01.01.02)--UPD-END-----
           03  TOKEI-CNT-G.
               05  TOKEI-CNT           PIC 9(05)V9(03) OCCURS  31.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    統計ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
grpsys*     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
grpsys*DD                              BY         //RECEERR//.
grpsys*DD  03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    印刷用領域
           COPY    "HCM36.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *----(01.01.01)--ADD-START---
           03  FLG-TENSUPLUS           PIC 9(01).
      *----(01.01.01)--ADD-END-----
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-D                   PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(04).
           03  CNT-TOKEI               PIC 9(04).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
           03  CNT-DAY                 PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  PARA-JOBID              PIC 9(07).
           03  PARA-SHELLID            PIC X(08).
grpsys*DD  03  PARA-HOSPID             PIC X(24).
grpsys     03  PARA-HOSPNUM            PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  PARA-DENPPRTYM          PIC X(06).
           03  PARA-SYORIKBN           PIC X(01).
           03  PARA-NAIYO              PIC X(20).
      *
      *    キー領域
       01  KEY-AREA.
      *----(01.01.02)--ADD-START---
           03  KEY-NYUGAIKBN           PIC X(01).
      *----(01.01.02)--ADD-END-----
           03  KEY-PTID                PIC 9(10).
      *
      *    作業領域
       01  WRK-AREA.
      *
           03  WRK-DBPATH              PIC X(100).
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-SURYO               PIC 9(05)V9(03).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(05).
               05  WRK-SURYO-S2        PIC 9(03).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZ.
               05  WRK-SURYO-Z1-9      REDEFINES   WRK-SURYO-Z1
                                       PIC ZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZ.
      *
           03  WRK-DATAYM              PIC X(16).
           03  WRK-NAIYO               PIC 9(08).
           03  WRK-SYUBETU             PIC 9(01).
           03  WRK-YKZKBN              PIC 9(01).
      *
           03  WRK-HENSURYO            PIC X(02).
           03  WRK-COM                 PIC X(01).
           03  WRK-DD                  PIC 9(02).
           03  WRK-DAY                 PIC 9(02).
           03  WRK-DAYED               PIC 9(02).
      *
           03  WRK-NAME                PIC X(100).
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-OUT-INPUT           PIC X(100).
           03  WRK-OUT-MAX             PIC 9(03).
           03  WRK-HENSURYO2           PIC X(03).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *    患者番号構成管理
           COPY    "CPSK1009.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *----(01.01.01)--ADD-START---
      *    点数付加情報マスタ
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *----(01.01.01)--ADD-END-----
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    半角全角変換
           COPY    "CPORCSKANACHK.INC".
      *
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ****************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END         =   1
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  PARA-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
grpsys     INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       PARA-JOBID
                                       PARA-SHELLID
grpsys*DD                              PARA-HOSPID
grpsys                                 PARA-HOSPNUM
grpsys                                 RECEERR
                                       PARA-DENPPRTYM
                                       PARA-SYORIKBN
                                       PARA-NAIYO
grpsys     END-UNSTRING
grpsys     MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-JOBID          TO  JOB-JOBID
           MOVE    PARA-SHELLID        TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG009"          TO  JOB-PGID
           MOVE    "診療内容参考資料表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
      *    ファイル名編集
grpsys     MOVE    "BG00901"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
grpsys*DD  MOVE    "RECEERR"           TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
      *     MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      *
      *    対象年月編集
           MOVE    PARA-DENPPRTYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-DATAYM
      *
      *    月末日取得
           MOVE    PARA-DENPPRTYM      TO  WRK-SYMD(1:6)
           PERFORM 20021-GETUMATU-GET-SEC
           MOVE    WRK-SYMD(7:2)       TO  WRK-DAYED
      *
           MOVE    WRK-SYMD            TO  WRK-SYSYMD
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
grpsys     MOVE    WRK-SYSYMD          TO  SYS-1001-STYUKYMD
grpsys                                     SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
               GO  TO  110-PARA-HENSYU-EXT
           END-IF 
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
grpsys     MOVE    WRK-SYSYMD          TO  SYS-1002-STYUKYMD
grpsys                                     SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
               GO  TO  110-PARA-HENSYU-EXT
           END-IF 
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    PARA-NAIYO      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF        ( KANACHK-RC      NOT =   ZERO )
               OR    ( KANACHK-SYUBETU     =   2 )
               MOVE    "半角で入力してください"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           EVALUATE    PARA-SYORIKBN
           WHEN    "1"
               INITIALIZE                  ORCSNUMAREA
               MOVE    PARA-NAIYO          TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             =   ZERO
                   IF         SNUM-KETSUU  <=  8
                       MOVE   SNUM-OUTNUM      TO  WRK-NAIYO
                   ELSE
                       MOVE    "点数が大きすぎます"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1               TO  FLG-END
                       GO  TO  110-PARA-HENSYU-EXT
                   END-IF
               ELSE
                   MOVE    "点数が正しく入力されていません"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-END
                   GO  TO  110-PARA-HENSYU-EXT
               END-IF
           WHEN    "2"
      *    患者番号構成管理情報編集
               MOVE    SPACE               TO  SYS-1009-REC
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
grpsys         MOVE    WRK-SYSYMD          TO  SYS-1009-STYUKYMD
grpsys                                         SYS-1009-EDYUKYMD
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 800-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
                   MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
                   MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
      *            拡張構成区分
                   MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
                   MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
                   MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
                   MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
               ELSE
                   MOVE    "医療機関情報が取得できませんでした"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
                   GO  TO  110-PARA-HENSYU-EXT
               END-IF
      *
      *        患者番号変換
               INITIALIZE                  ORCSPTIDAREA
grpsys*DD      MOVE    PARA-HOSPID         TO  SPTID-HOSPID
grpsys         MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    PARA-NAIYO          TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
grpsys*DD                                  SPA-1009-TBL
grpsys                                     SPA-AREA
               IF          SPTID-RC        =   ZERO
                   MOVE    SPTID-PTID      TO  WRK-NAIYO
               ELSE
                   MOVE    "指定された患者番号は登録されていません"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
                   GO  TO  110-PARA-HENSYU-EXT
               END-IF
           WHEN    OTHER
               MOVE    "処理区分が正しくありません"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
               GO  TO  110-PARA-HENSYU-EXT
           END-EVALUATE
      *
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *
      *    統計ファイルセット
           PERFORM 210-TOKEI-SET-SEIKYU-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL   FLG-TOKEI       =   1
               MOVE    TOKEI-NYUGAIKBN     TO  KEY-NYUGAIKBN
               MOVE    TOKEI-PTID          TO  KEY-PTID
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           MOVE    1                   TO  FLG-END
      *
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
           MOVE    TOKEIPARA   TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(01.01.03)--UPD-START---
      *****************************************************************
      *    統計ファイルセット
      *****************************************************************
       210-TOKEI-SET-SEIKYU-SEC    SECTION.
      *
           EVALUATE    PARA-SYORIKBN
           WHEN    "1"
      *        請求管理情報読み込み
               PERFORM 900-SEIKYU-SELECT-SEC
      *
      *        請求管理情報セット
               PERFORM UNTIL   FLG-SEIKYU      =   1
      *----(01.02.02)--UPD-START---
                   IF    ( RECE10-TOTALTEN(1)  >=  WRK-NAIYO ) OR
                         ( RECE10-TOTALTEN(2)  >=  WRK-NAIYO ) OR
                         ( RECE10-TOTALTEN(3)  >=  WRK-NAIYO ) OR
                         ( RECE10-TOTALTEN(4)  >=  WRK-NAIYO ) OR
                         ( RECE10-TOTALTEN(5)  >=  WRK-NAIYO )
      *----(01.02.02)--UPD-END-----
                       INITIALIZE                  SRYACCT-REC
grpsys*DD              MOVE    RECE10-HOSPID       TO  ACCT-HOSPID
grpsys                 MOVE    RECE10-HOSPNUM      TO  ACCT-HOSPNUM
                       MOVE    RECE10-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                       MOVE    RECE10-PTID         TO  ACCT-PTID
                       MOVE    RECE10-SRYYM        TO  ACCT-SRYYM
      *                統計ファイルセット
                       PERFORM 220-MAIN-ACCT-SEC
                   END-IF
      *            請求管理情報読み込み
                   PERFORM 900-SEIKYU-FETCH-SEC
               END-PERFORM
      *
      *        請求管理情報読み込み
               PERFORM 900-SEIKYU-CLOSE-SEC
           WHEN    "2"
               INITIALIZE                  SRYACCT-REC
grpsys*DD      MOVE    PARA-HOSPID         TO  ACCT-HOSPID
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    PARA-DENPPRTYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    WRK-NAIYO           TO  ACCT-PTID
      *        統計ファイルセット
               PERFORM 220-MAIN-ACCT-SEC
      *
               INITIALIZE                  SRYACCT-REC
grpsys*DD      MOVE    PARA-HOSPID         TO  ACCT-HOSPID
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    PARA-DENPPRTYM      TO  ACCT-SRYYM
               MOVE    "2"                 TO  ACCT-NYUGAIKBN
               MOVE    WRK-NAIYO           TO  ACCT-PTID
      *        統計ファイルセット
               PERFORM 220-MAIN-ACCT-SEC
           END-EVALUATE
      *
           .
       210-TOKEI-SET-SEIKYU-EXT.
           EXIT.
      *
      *----(01.01.03)--UPD-END-----
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       220-MAIN-ACCT-SEC           SECTION.
      *
      *    対象診療年月で検索
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_sryacct"   TO  MCP-TABLE
grpsys     MOVE    "key7"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               PERFORM 900-ACCT-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL   FLG-SRYACCT     =   1
               PERFORM 230-MAIN-SRY-SEC
               PERFORM 900-ACCT-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-ACCT-DBCLOSE-SEC
      *
           .
       220-MAIN-ACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為処理
      *****************************************************************
       230-MAIN-SRY-SEC            SECTION.
      *
      *----(04.08.02)--ADD-START---
           IF  ( ACCT-JIHOKBN        = "1" )   AND
               ( ACCT-TEIGENRATE     =  3 OR 4 )
               GO  TO  230-MAIN-SRY-EXT
           END-IF
      *----(04.08.02)--ADD-END-----
      *
           EVALUATE     ACCT-SRYKBN(1:1)
           WHEN    "2"
               MOVE    1                   TO  WRK-SYUBETU
           WHEN    "3"
               MOVE    2                   TO  WRK-SYUBETU
           WHEN    "4"
               MOVE    3                   TO  WRK-SYUBETU
           WHEN    "5"
               MOVE    4                   TO  WRK-SYUBETU
           WHEN    OTHER
               GO  TO  230-MAIN-SRY-EXT
           END-EVALUATE
      *
           INITIALIZE                  SRYACT-REC
grpsys*DD  MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
grpsys     MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_sryact"    TO  MCP-TABLE
grpsys     MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
              PERFORM 900-SRY-DBFETCH-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL   FLG-SRYACT      =   1
               PERFORM 240-MAIN-HEN-SEC
               PERFORM 900-SRY-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-SRY-DBCLOSE-SEC
            .
       230-MAIN-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    主編集処理
      *****************************************************************
       240-MAIN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   5 )
                   OR    ( SRY-SRYCD(IDX)  =   SPACE )
      *        薬剤を対象とする
               IF      SRY-SRYCD(IDX)(1:1)     =   "6"
                   PERFORM 250-TOKEI-HEN-SEC 
               END-IF
           END-PERFORM
            .
       240-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       250-TOKEI-HEN-SEC           SECTION.
      *
      *----(01.01.01)--ADD-START---
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SRY-SRYCD(IDX)      TO  TNSP-SRYCD
           MOVE    WRK-SYSYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SYSYMD          TO  TNSP-YUKOEDYMD
grpsys     MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *    点数付加情報マスタ検索
           PERFORM 900-TENSUPLUS-READ-SEC
      *
           IF      TNSP-TENPUYAKKBN    =   SPACE
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *----(01.01.01)--ADD-END-----
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX)  TO  TNS-SRYCD
grpsys     MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
grpsys     MOVE    WRK-SYSYMD      TO  TNS-YUKOSTYMD
grpsys     MOVE    WRK-SYSYMD      TO  TNS-YUKOEDYMD
      *    点数マスタ検索
           PERFORM 900-TNS-READ-SEC
      *
      *    薬剤区分チェック
           EVALUATE    TNS-YKZKBN
      *    内服薬
           WHEN    1
               MOVE    1               TO  WRK-YKZKBN
      *    外用薬
           WHEN    6
               MOVE    2               TO  WRK-YKZKBN
      *    注射薬
           WHEN    4
               MOVE    3               TO  WRK-YKZKBN
      *    その他
           WHEN    3
               MOVE    4               TO  WRK-YKZKBN
      *    歯科用薬剤
           WHEN    8
               MOVE    5               TO  WRK-YKZKBN
      *    歯科用特定薬剤
           WHEN    9
               MOVE    1               TO  WRK-YKZKBN
           END-EVALUATE
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
      *----(01.01.02)--ADD-START---
           MOVE    ACCT-NYUGAIKBN      TO  TOKEI-NYUGAIKBN
      *----(01.01.02)--ADD-END-----
           MOVE    ACCT-PTID           TO  TOKEI-PTID
           MOVE    WRK-YKZKBN          TO  TOKEI-YKZKBN
           MOVE    TNSP-TENPUYAKKBN    TO  TOKEI-BUNRUI
           MOVE    SRY-SRYCD(IDX)      TO  TOKEI-SRYCD
      *----(01.01.02)--ADD-START---
           MOVE    SRY-ZAINUM          TO  TOKEI-ZAINUM
      *----(01.01.02)--ADD-END-----
      *
           PERFORM 900-TOKEI-READ-SEC
      *
           IF          FLG-TOKEI       =   1
      *----(01.01.02)--UPD-START---
               INITIALIZE                  TOKEI-REC
      *----(01.01.02)--ADD-START---
               MOVE    ACCT-NYUGAIKBN      TO  TOKEI-NYUGAIKBN
      *----(01.01.02)--ADD-END-----
               MOVE    ACCT-PTID           TO  TOKEI-PTID
               MOVE    WRK-YKZKBN          TO  TOKEI-YKZKBN
               MOVE    TNSP-TENPUYAKKBN    TO  TOKEI-BUNRUI
               MOVE    SRY-SRYCD(IDX)      TO  TOKEI-SRYCD
               MOVE    SRY-ZAINUM          TO  TOKEI-ZAINUM
               MOVE    TNS-NAME            TO  TOKEI-NAME
      *
               MOVE    SRY-SRYSURYO(IDX)   TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET2-SEC
               MOVE    WRK-HENSURYO2       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
      *
               MOVE    SPACE               TO  WRK-NAME
               STRING  WRK-OUT-INPUT       DELIMITED  BY  SPACE
                       TNS-TANINAME        DELIMITED  BY  SPACE
                                   INTO    WRK-NAME
               END-STRING
               MOVE    WRK-NAME        TO  TOKEI-TANINAME
      *
               MOVE    ZERO            TO  TOKEI-CNT-G
      *
               PERFORM 260-TOKEI-SET-SEC
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI           =   CNT-TOKEI   +   1
           ELSE
               PERFORM 260-TOKEI-SET-SEC
               REWRITE TOKEI-REC
           END-IF
      *----(01.01.02)--UPD-END-----
             .
       250-TOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       260-TOKEI-SET-SEC           SECTION.
      *
      *    最初の算定日を取得する
           MOVE    ZERO                TO  WRK-DD
           PERFORM VARYING     IDX-D   FROM    1   BY   1
                   UNTIL       IDX-D   >   31
               IF          ACCT-DAY(1 IDX-D)   >   ZERO
                   MOVE    IDX-D               TO  WRK-DD
                   COMPUTE TOKEI-CNT(WRK-DD)
                           =   TOKEI-CNT(WRK-DD)
                           +   ACCT-DAY(1 WRK-DD)
               END-IF
           END-PERFORM
             .
       260-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                  HCM36
           MOVE    SPACE               TO  HCM36
      *
           MOVE    WRK-DATAYM          TO  HCM36-SHINRYOYM
      *
           MOVE    SYS-1002-ADRS       TO  HCM36-HOSPADRS
           MOVE    SYS-1001-HOSPNAME   TO  HCM36-HOSPNAME
      *
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    PTINF-NAME          TO  HCM36-PTNAME
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           PERFORM UNTIL ( CNT-LINE        >=  21 )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( KEY-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( KEY-PTID    NOT =   TOKEI-PTID )
      *
               ADD     1                   TO  CNT-LINE
      *
               MOVE    TOKEI-NAME          TO  HCM36-USENAME(CNT-LINE)
               MOVE    TOKEI-TANINAME      TO  HCM36-USETANI(CNT-LINE)
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       31
      *            数量編集
                   MOVE    TOKEI-CNT(IDX)      TO  WRK-SURYO
                   PERFORM 400-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO
                                       TO  HCM36-USERYO(CNT-LINE IDX)
                   MOVE    WRK-COM     TO  HCM36-USETEN(CNT-LINE IDX)
               END-PERFORM
      *
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    PARA-DENPPRTYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCM36.red"         TO  SPRT-PRTID
           MOVE    "診療内容参考資料"  TO  SPRT-TITLE
           MOVE    HCM36               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF          SPRT-RETURN     =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF          STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    PARA-JOBID          TO  JOB-JOBID
               MOVE    PARA-SHELLID        TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF          CNT-TOKEI       =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG009 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-JOBID          TO  JOB-JOBID
           MOVE    PARA-SHELLID        TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       20021-GETUMATU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       20021-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
      *----(01.01.02)--UPD-START---
      *    数量小数点以下編集
      *     MOVE    ZERO                TO  FLG-SP
      *     PERFORM VARYING CNT-KETA    FROM    3   BY  -1
      *             UNTIL   CNT-KETA    <   1
      *         IF          WRK-SURYO-S2(CNT-KETA:1)    NOT =   ZERO
      *             MOVE    1               TO  FLG-SP
      *         END-IF
      *         IF          FLG-SP          =   1
      *             MOVE    WRK-SURYO-S2(CNT-KETA:1)
      *                     TO  WRK-SURYO-Z3(CNT-KETA:1)
      *         END-IF
      *     END-PERFORM
      *
      *     IF          WRK-SURYO-Z3    NOT =   SPACE
      *         MOVE    "."             TO  WRK-SURYO-Z2
      *         IF          WRK-SURYO-X(1:5)    =   SPACE
      *             MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
      *         END-IF
      *     END-IF
      *
           MOVE    SPACE               TO  WRK-HENSURYO
      *     MOVE    SPACE               TO  WRK-COM
           MOVE    ZERO                TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   5
                   OR      CNT-KETA    >=  2
               IF          WRK-SURYO-X(CNT-KETA2:1)  NOT =   SPACE
      *             IF          WRK-SURYO-X(CNT-KETA2:1)  =   "."
      *                 MOVE    "."       TO  WRK-COM
      *             ELSE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE  WRK-SURYO-X(CNT-KETA2:1)
                                       TO  WRK-HENSURYO(CNT-KETA:1)
      *             END-IF
      *----(01.01.02)--UPD-END-----
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET2-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING CNT-KETA    FROM    3   BY  -1
                   UNTIL   CNT-KETA    <   1
               IF          WRK-SURYO-S2(CNT-KETA:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF          FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(CNT-KETA:1)
                           TO  WRK-SURYO-Z3(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           IF          WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF          WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-HENSURYO2
           MOVE    ZERO                TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   8
                   OR      CNT-KETA    >=  3
               IF        WRK-SURYO-X(CNT-KETA2:1)  NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE  WRK-SURYO-X(CNT-KETA2:1)
                                       TO  WRK-HENSURYO2(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスタ検索処理
      *****************************************************************
       900-ACCT-DBFETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     MOVE    "tbl_sryacct"   TO  MCP-TABLE
grpsys     MOVE    "key7"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
      *----(01.02.01)--ADD-START---
               IF          ACCT-HKNCOMBI   =   9999
                   GO  TO  900-ACCT-DBFETCH-SEC
               END-IF
      *----(01.02.01)--ADD-END-----
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           .
       900-ACCT-DBFETCH-EXT.
           EXIT.
      *101

      *****************************************************************
      *    診療会計マスタクローズ処理
      *****************************************************************
       900-ACCT-DBCLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_sryacct"   TO  MCP-TABLE
grpsys     MOVE    "key7"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-ACCT-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為マスタ検索処理
      *****************************************************************
       900-SRY-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     MOVE    "tbl_sryact"    TO  MCP-TABLE
grpsys     MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    ZERO                TO  FLG-SRYACT
               MOVE    MCPDATA-REC         TO  SRYACT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           .
       900-SRY-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタクローズ処理
      *****************************************************************
       900-SRY-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_sryact"    TO  MCP-TABLE
grpsys     MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-SRY-DBCLOSE-EXT.
           EXIT.
      *
      *----(01.01.02)--ADD-START---
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *----(01.01.02)--ADD-END-----
      *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1               TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO            TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1               TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO            TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読み込み
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE                  PTINF-REC
grpsys*DD  MOVE    PARA-HOSPID         TO  PTINF-HOSPID
           MOVE    TOKEI-PTID          TO  PTINF-PTID
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys         MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
            .
       900-PTINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TNS-READ-SEC          SECTION.
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     MOVE    "tbl_tensu"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         MOVE    "tbl_tensu"     TO  MCP-TABLE
grpsys         MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_tensu"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-TNS-READ-EXT.
           EXIT.
      *
      *----(01.01.01)--ADD-START---
      ******************************************************************
      *    点数付加情報マスター読込
      *****************************************************************
       900-TENSUPLUS-READ-SEC      SECTION.
      *
           MOVE    TENSUPLUS-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     MOVE    "tbl_tensuplus" TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         MOVE    "tbl_tensuplus" TO  MCP-TABLE
grpsys         MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_tensuplus" TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *----(01.01.01)--ADD-END-----
      *****************************************************************
      *    請求管理読み込み
      *****************************************************************
       900-SEIKYU-SELECT-SEC       SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    PARA-DENPPRTYM      TO  RECE10-SRYYM
grpsys     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_seikyu"    TO  MCP-TABLE
grpsys     MOVE    "all2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-FETCH-SEC
           ELSE
               INITIALIZE                  RECE10-REC
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読み込み
      *****************************************************************
       900-SEIKYU-FETCH-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM UNTIL   FLG-OK  =   1
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "all2"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
                   IF    ( RECE10-HKNNUM-S =   "975" ) OR
                         ( RECE10-HKNNUM-S =   "971" ) OR
                         ( RECE10-HKNNUM-S =   "973" )
                       MOVE    ZERO            TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SEIKYU
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           .
       900-SEIKYU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読み込み
      *****************************************************************
       900-SEIKYU-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_seikyu"    TO  MCP-TABLE
grpsys     MOVE    "all2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF          MCP-RC          =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys         MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.

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
       PROGRAM-ID.             ORCR0810.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      対象患者抽出（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *                                     診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  01.01.02    NACL-門脇    05/01/07  自賠責担当医コード、自賠責保
      *                                     険会社名の編集
      *  01.01.03    NACL-門脇    05/01/26  システム管理４００１に関する修正
      *  01.01.04    NACL-門脇    05/06/27  加算率３桁対応
      *  01.01.05    NACL-門脇    05/08/18  転帰事由編集修正
      *  01.01.06    NACL-門脇    05/12/01  労災レセプト回数対応
      *  01.01.07    NACL-門脇    06/01/26  対象データ編集修正
      *
      *  03.03.01    NACL-藤原    06/09/19  月途中での氏名変更対応
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.01.01    NACL-門脇    07/11/01  公務災害対応
      *  04.02.01    NACL-門脇    08/02/26  請求区分編集
      *
      *  04.04.01    NACL-門脇    09/05/11  抽出対象条件修正
      *
      *  04.05.01    NACL-藤原    11/11/22  同日再入院対応
      *
      *  04.07.01    NACL-門脇    11/09/07  労災請求書作成対応
      *  04.07.02    NACL-門脇    13/04/16  労災レセ電対応
      *                                     労災指定医療機関区分編集
      *  04.07.03    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.04    NACL-門脇    14/02/21  公務災害（労災準拠）の点数単価
      *                                     設定対応
      *  04.07.05    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.06    NACL-門脇    14/05/27  第三者行為の給付割合の設定
      *                                     （社保前期高齢２割）
      *  04.07.07    NACL-門脇    14/07/31  第三者行為の現物給付対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.04    NACL-門脇    15/09/16  自賠責（健保準拠）の点数単価
      *                                     入外別対応
      *  04.08.05    NACL-門脇    17/02/07  労災保険登録時の労働基準監督
      *                                     署コードの編集方法変更に伴う修正
      *  04.08.06    NACL-門脇    17/02/24  未コード化特定器材コードを使用した
      *                                     レセ電請求が不可となる事に伴う対応
      *  04.08.07    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *
      *  05.00.01    NACL-門脇    19/02/01  療養期間編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-KYUSEIRRK           PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01). 
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      * 
           03  FLG-RECE82              PIC 9(01).
      *
           03  FLG-ARI                 PIC 9(01).
           03  FLG-SET                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-IN-CANCEL           PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(03). 
           03  IDX                     PIC 9(03). 
           03  IDX1                    PIC 9(03). 
           03  IDX2                    PIC 9(03). 
           03  IDX3                    PIC 9(03). 
           03  IDX4                    PIC 9(03).
           03  IDX5                    PIC 9(03).
           03  IDY                     PIC 9(03). 
           03  IDZ                     PIC 9(03).
           03  TBL-MAX                 PIC 9(03).
           03  SYSKAN-IDX              PIC 9(03).
           03  IDXX                    PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC                         OCCURS  100.
               05  TBL-SYS4001                         PIC X(610).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-SYOKBN                     PIC  X(01).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
               05  WRK-PARA-PTID                       PIC  X(10).
               05  WRK-PARA-SRYYM-O                    PIC  X(06).
           03  WRK-PARA-JIBAIPRTKBN                    PIC  X(01).
           03  WRK-PARA-PRTKBN                         PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-HKNNUM              PIC 9(03).     
      *
           03  WRK-KSNRATE             PIC 9(03).      
           03  WRK-DBKBN               PIC X(01).
      *
           03  WRK-JIBAIKBN            PIC X(01).
           03  WRK-SYUTENTANKA         PIC 9(02).
           03  WRK-YKZTENTANKA         PIC 9(02).
      *
           03  WRK-KOMUKBN             PIC X(01).
           03  WRK-KOMUTENTANKA        PIC 9(02).
      *
           03  WRK-RSIIRYOKBN          PIC X(01).
      *
           03  WRK-KOMURSITANKAKBN     PIC X(01).
           03  WRK-KOMURSITANKA-G.
               05  WRK-KOMURSITANKA    PIC 99V99.
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-CONS-HKNNUM         PIC X(03).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(02).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-SRYACCT-PATHNAME    PIC X(64).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).     
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM         PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).
      *              
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
           03  WRK-HKNCOMBI-ERR-DAY    PIC 9(02).   
      *
       01  WRK-RECE82-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE82//.
      *
       01  CONST-AREA.
           03  WRK-CONS-SRYYM-200804
                               PIC 9(06)   VALUE   200804.
           03  WRK-CONS-SRYYM-201109
                               PIC 9(06)   VALUE   201109.
           03  WRK-CONS-SRYYM-201707-X.
               05  WRK-CONS-SRYYM-201707
                               PIC 9(06)   VALUE   201707.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM    =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN     =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM      =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN     =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-RECEKBN    =" WRK-PARA-RECEKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           EVALUATE  WRK-PARA-RECEKBN
             WHEN      "3"
                 MOVE    "971"         TO  WRK-CONS-HKNNUM
             WHEN      "4"
             WHEN      "5"
                 MOVE    "973"         TO  WRK-CONS-HKNNUM
             WHEN      "8"
                 MOVE    "970"         TO  WRK-CONS-HKNNUM
           END-EVALUATE
      *
           MOVE    SPACE               TO  RECE82PARA
           INITIALIZE                      RECE82PARA
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0810"      TO  JOB-PGID
               MOVE    "対象患者抽出（労災・自賠責）"                              
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC         TO  SYS-1001-REC
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                            TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD(7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       システム管理マスタ（４００１）から点数単価の取得
              INITIALIZE                         SYS-4001-REC
              MOVE    "4001"               TO    SYS-4001-KANRICD
              MOVE    "*"                  TO    SYS-4001-KBNCD
              MOVE    WRK-PARA-SRYYM       TO    SYS-4001-STYUKYMD (1:6)
              MOVE    "01"                 TO    SYS-4001-STYUKYMD (7:2)
              MOVE    SYS-4001-STYUKYMD    TO    SYS-4001-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM          TO    SYS-4001-HOSPNUM
              MOVE    SYS-4001-REC         TO    MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC         TO  SYS-4001-REC
                  IF    SYS-4001-KSNRATE1(1:3)   =  SPACE
                    IF   SYS-4001-KSNRATE(1:2)   =  SPACE
                      MOVE   100           TO    WRK-KSNRATE
                    ELSE
                      COMPUTE WRK-KSNRATE  =  SYS-4001-KSNRATE
                                           +  100
                    END-IF
                  ELSE
                    MOVE SYS-4001-KSNRATE1 TO    WRK-KSNRATE
                  END-IF
                  IF     WRK-PARA-RECEKBN  =   "8"
                      MOVE    "2"                TO    WRK-JIBAIKBN
                      MOVE    10                 TO    WRK-SYUTENTANKA
                      MOVE    10                 TO    WRK-YKZTENTANKA
                  ELSE
                      IF     SYS-4001-JIBAIKBN  =    SPACE
                          MOVE "1"               TO    WRK-JIBAIKBN
                      ELSE
                          MOVE SYS-4001-JIBAIKBN TO    WRK-JIBAIKBN
                      END-IF
                      IF     SYS-4001-SYUTENTANKA2-X =  SPACE
                        IF   SYS-4001-SYUTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-SYUTENTANKA
                        ELSE
                          MOVE SYS-4001-SYUTENTANKA
                                                 TO    WRK-SYUTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-SYUTENTANKA2
                                                 TO    WRK-SYUTENTANKA
                      END-IF
                      IF     SYS-4001-YKZTENTANKA2-X =  SPACE
                        IF   SYS-4001-YKZTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-YKZTENTANKA
                        ELSE
                          MOVE SYS-4001-YKZTENTANKA
                                                 TO    WRK-YKZTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-YKZTENTANKA2
                                                 TO    WRK-YKZTENTANKA
                      END-IF
                  END-IF
                  IF     SYS-4001-KOMUKBN  =    SPACE
                    MOVE "1"               TO    WRK-KOMUKBN
                  ELSE
                    MOVE SYS-4001-KOMUKBN  TO    WRK-KOMUKBN
                  END-IF
                  IF     SYS-4001-KOMUTENTANKA-X  =  SPACE
                    MOVE ZERO              TO    WRK-KOMUTENTANKA
                  ELSE
                    MOVE SYS-4001-KOMUTENTANKA
                                           TO    WRK-KOMUTENTANKA
                  END-IF
                  IF     SYS-4001-RSIIRYOKBN      =  SPACE
                    MOVE "0"               TO    WRK-RSIIRYOKBN
                  ELSE
                    MOVE SYS-4001-RSIIRYOKBN
                                           TO    WRK-RSIIRYOKBN
                  END-IF
                  MOVE   SYS-4001-KOMURSITANKAKBN
                                           TO    WRK-KOMURSITANKAKBN
                  MOVE   SYS-4001-KOMURSITANKA-G
                                           TO    WRK-KOMURSITANKA-G
              ELSE
                  MOVE    "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS4001-T
              INITIALIZE                      TBL-SYS4001-T
      *
              INITIALIZE                      SYS-4001-REC
              MOVE    "4001"              TO  SYS-4001-KANRICD
              MOVE    "*"                 TO  SYS-4001-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
              MOVE    SYS-4001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-4001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-4001-REC  TO  TBL-SYS4001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE82-FILE
           CLOSE                       RECE82-FILE
      *
           OPEN    I-O                 RECE82-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-END             =   ZERO 
      *        診療年月編集
               MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
               MOVE    "01"                TO  WRK-STYMD (7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
      *
               MOVE    "1"                 TO  WRK-DBKBN 
      *
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key60"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key36"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-IKKATU-SRYACCT-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF    
      *
           IF      FLG-END             =   ZERO 
      *        入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
      *
               MOVE    "2"                 TO  WRK-DBKBN 
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *
               INITIALIZE                  NYUINACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT   =   1
                       OR          FLG-END         =   1
                   PERFORM 20012-IKKATU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    一括編集処理
      *****************************************************************
       20011-IKKATU-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT   PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR          FLG-END     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            同日再入院情報取得
                   PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
      *                対象の保険であるか
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "5"
                               IF      COMB-HKNNUM  =   WRK-CONS-HKNNUM
      *                            診療月内に摘要日が存在するか
                                   IF      COMB-TEKSTYMD   <=  WRK-STYMD
                                   AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                       MOVE    SPACE   TO  WRK-UPDKBN
                                   ELSE
                                       MOVE    "1"     TO  WRK-UPDKBN
                                   END-IF
                               ELSE     
                                   MOVE    "1"         TO  WRK-ROUSAIKBN
                               END-IF    
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                                   =   WRK-CONS-HKNNUM
                                   IF      COMB-TEKSTYMD   <=  WRK-STYMD
                                   AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                       MOVE    SPACE   TO  WRK-UPDKBN
                                   ELSE
                                       MOVE    "1"     TO  WRK-UPDKBN
                                   END-IF
                               ELSE
                                   MOVE    "1"     TO  WRK-ROUSAIKBN
                               END-IF    
                       END-EVALUATE
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN    
                       MOVE    1               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO  WRK-ERR-HKNCOMBINUM
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       IF      WRK-ROUSAIKBN           =   SPACE            
                           PERFORM 200111-SRYACCT-HENSYU-SEC 
                       END-IF    
      *
      *                対象保険以外または日付変更がないとき
      *                または既にエラー対象のとき
      *                同じ患者ＩＤ・保険組合せは読み飛ばす
                       IF    ( WRK-ROUSAIKBN   =   "1"   )
                       OR    ( WRK-UPDKBN      =   SPACE )
                       OR    ( WRK-TEISEIKBN   =   "1"   )
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-END     =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"  TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                      TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "tbl_sryacct"  TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                  TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-SRYACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       200111-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    労災保険情報読み込み     
           INITIALIZE              PTRSIINF-REC
grpsys     MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNID      TO  PTRSI-HKNID
               WHEN    "8"
                   MOVE    COMB-KOHID (1)  TO  PTRSI-KOHID
           END-EVALUATE
           MOVE    COMB-PTID       TO  PTRSI-PTID
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"  TO  WRK-MCP-TABLE
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-PTRSIINF-INV-SEC
           IF      FLG-PTRSIINF    =   1
               MOVE    "1"             TO  WRK-ROUSAIKBN
           END-IF
      *
           DISPLAY "WRK-ROUSAIKBN="     WRK-ROUSAIKBN
                   " PTRSI-JIBAIKBN="   PTRSI-JIBAIKBN
      *
           IF      PTRSI-JIBAIKBN  =   SPACE   OR  "0"
               MOVE    WRK-JIBAIKBN    TO  PTRSI-JIBAIKBN
           END-IF
      *
      *    労災・自賠責保険以外のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  200111-SRYACCT-HENSYU-EXT
           END-IF
      *
      *    新様式で健保準拠のときは処理しない
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201707-X )
           AND   ( WRK-PARA-RECEKBN    =   "4"                     )
           AND   ( PTRSI-JIBAIKBN      =   "2"                     )
               GO  TO  200111-SRYACCT-HENSYU-EXT
           END-IF
      *
           IF      WRK-UPDKBN          =   "1"
      *    保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    2               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    3               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    4               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    5               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                   END-IF    
               END-IF    
           END-IF
      *
      *    ファイル編集処理
           PERFORM 20011-FILE-HENSYU-SEC
           .
       200111-SRYACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-NYUINACCT-HEN-SEC              SECTION.
      *     
      *    テスト患者のときは読み飛ばす
           MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "NYUINACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
               IF      FLG-END                 =   ZERO 
      *            同日再入院情報取得
                   PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *            同日再入院のチェック
                   PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
      *            入院会計より保険組合せ読込     
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF    ((WRK-DAY (1 IDX)        >   ZERO) OR
                              (LNK-NYUINDAY-KBN(IDX)  =   "1" ))  AND
                             ( WRK-DAY (2 IDX)        >   ZERO )
                           PERFORM 200121-NYUINACCT-CHK-SEC
                       END-IF
                   END-PERFORM        
      *
      *            入院会計の保険組合せを追加
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       10
                           OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                           CONTINUE
                       ELSE    
                           MOVE    SPACE               TO  WRK-ERR-AREA
                           INITIALIZE                      WRK-ERR-AREA
      *
                           MOVE    WRK-HKNCOMBI-REC (IDX2)
                                                   TO  HKNCOMBI-REC    
                           IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                               MOVE    "1"         TO  WRK-TEISEIKBN
                               IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   =   ZERO
                                   MOVE    1       TO  WRK-ERR-ERRKBN
                               ELSE
                                   MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                                   MOVE    6       TO  WRK-ERR-ERRKBN
                               END-IF
                               MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                                   WRK-ERR-HKNCOMBINUM
                           END-IF        
      *
                           PERFORM 200122-NYUINACCT-HENSYU-SEC
                       END-IF 
                   END-PERFORM
               END-IF    
           END-IF    
           .
       20012-IKKATU-NYUINACCT-HEN-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       200121-NYUINACCT-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                   MOVE    KEY-O-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                     MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                     EVALUATE    WRK-PARA-RECEKBN
                         WHEN    "3"
                         WHEN    "4"
                         WHEN    "5"
                             IF      COMB-HKNNUM NOT =
                                                   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                             END-IF
                         WHEN    "8"
                             IF      COMB-KOHHKNNUM (1)
                                               NOT =   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                             END-IF
                     END-EVALUATE
                   ELSE
                     IF   COMB-DELKBN         =   "1"
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "5"
                               IF      COMB-HKNNUM NOT =
                                                   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               ELSE
                                   MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               END-IF
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                               NOT =   WRK-CONS-HKNNUM
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               ELSE
                                   MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               END-IF
                       END-EVALUATE
                     ELSE
                       MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "2"     TO  WRK-HKNCOMBI-SYOKBN  (IDX1)
                     END-IF
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE                    
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE   
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           MOVE    IDX         TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF            
           END-PERFORM
           .
       200121-NYUINACCT-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計ファイル処理
      *****************************************************************
       200122-NYUINACCT-HENSYU-SEC           SECTION.
      *
           MOVE    WRK-HKNCOMBI-REC  (IDX2)  TO  HKNCOMBI-REC
      *    労災保険情報読み込み     
           INITIALIZE              PTRSIINF-REC
grpsys     MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNID      TO  PTRSI-HKNID
               WHEN    "8"
                   MOVE    COMB-KOHID (1)  TO  PTRSI-KOHID
           END-EVALUATE
           MOVE    COMB-PTID       TO  PTRSI-PTID
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"  TO  WRK-MCP-TABLE
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-PTRSIINF-INV-SEC
           IF      FLG-PTRSIINF    =   "1"
               MOVE    "1"             TO  WRK-ROUSAIKBN
           END-IF
      *??
           DISPLAY "WRK-ROUSAIKBN="     WRK-ROUSAIKBN
                   " PTRSI-JIBAIKBN="   PTRSI-JIBAIKBN
      *
           IF      PTRSI-JIBAIKBN  =   SPACE   OR  "0"
               MOVE    WRK-JIBAIKBN    TO  PTRSI-JIBAIKBN
           END-IF
      *
      *    労災・自賠責保険以外のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  200122-NYUINACCT-HENSYU-EXT
           END-IF
      *
      *    新様式で健保準拠のときは処理しない
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201707-X )
           AND   ( WRK-PARA-RECEKBN    =   "4"                     )
           AND   ( PTRSI-JIBAIKBN      =   "2"                     )
               GO  TO  200122-NYUINACCT-HENSYU-EXT
           END-IF
      *
           PERFORM 20011-FILE-HENSYU-SEC
           .
        200122-NYUINACCT-HENSYU-EXT.
           EXIT. 
           .
      *            
      *****************************************************************
      *    編集処理
      *****************************************************************
       20011-FILE-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      WRK-RECE82-REC
           MOVE    SPA-HOSPNUM             TO  WRK-RECE82-HOSPNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    SYS-2001-SRYYM      TO  WRK-RECE82-SRYYM
           ELSE
               MOVE    SYS-2002-SRYYM      TO  WRK-RECE82-SRYYM
           END-IF
           MOVE    "1"                 TO  WRK-RECE82-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE82-PTID
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNID      TO  WRK-RECE82-HKNID
               WHEN    "8"
                   MOVE    COMB-KOHID (1)  TO  WRK-RECE82-HKNID
           END-EVALUATE
           MOVE    COMB-TEKSTYMD       TO  WRK-RECE82-TEKSTYMD
           MOVE    WRK-RECE82-KEY      TO  RECE82-KEY 
           PERFORM 900-RECE82-READ-SEC
           IF      FLG-RECE82          =   ZERO
               MOVE    RECE82-REC          TO  WRK-RECE82-REC 
               PERFORM 200111-RECE82-REWRITE-SEC
           ELSE
               PERFORM 200112-RECE82-WRITE-SEC
           END-IF
      *   
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       200112-RECE82-WRITE-SEC              SECTION.
      *
      *    保険組合せ番号情報
           MOVE    COMB-HKNCOMBINUM   TO  WRK-RECE82-HKNCOMBI
      *
      *    診療科情報（診療会計のとき）
           IF      WRK-DBKBN          =   "2"
               CONTINUE
           ELSE              
               MOVE    ACCT-SRYKA         TO  WRK-RECE82-SRYKA (1)
           END-IF
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE82-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                        (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                       ( WRK-DAY (2 IDX3)        >   ZERO )
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE82-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
      *
      *    患者番号取得
           INITIALIZE                      ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-RECE82-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-RECE82-PTNUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME       TO  WRK-RECE82-KANANAME
           MOVE   PTINF-NAME           TO  WRK-RECE82-NAME
           MOVE   PTINF-SEX            TO  WRK-RECE82-SEX
           MOVE   PTINF-BIRTHDAY       TO  WRK-RECE82-BIRTHDAY
      *
      *    患者年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    WRK-RECE82-PTID     TO  AGECHK-PTID
           MOVE    WRK-RECE82-SRYYM    TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-RECE82-AGE
           END-IF
      *
           MOVE   PTINF-TSTPTNUMKBN    TO  WRK-RECE82-TSTPTNUMKBN
      *
      *    患者情報（旧姓履歴の取得）
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    WRK-RECE82-PTID     TO  KYUSEI-PTID
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-RECE82-SRYYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF    ( WRK-TEISEIKBN     NOT =   "1"         )
           AND   ( COMB-TEKEDYMD        <   LNK-DAY7-KOYOMI )
               MOVE    COMB-TEKEDYMD        TO  KYUSEI-CHGYMD
           ELSE
               MOVE    LNK-DAY7-KOYOMI      TO  KYUSEI-CHGYMD
           END-IF
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK      =   ZERO
               MOVE    KYUSEI-NAME         TO  WRK-RECE82-NAME
               MOVE    KYUSEI-KANANAME     TO  WRK-RECE82-KANANAME
           END-IF         
      *          
      *    患者労災保険情報
           MOVE   PTRSI-HKNKBN         TO  WRK-RECE82-HKNKBN
           MOVE   PTRSI-HKNNUM         TO  WRK-RECE82-HKNNUM
           MOVE   PTRSI-KOFUNUM        TO  WRK-RECE82-KOFUNUM
           MOVE   PTRSI-JIGYOUNAME     TO  WRK-RECE82-JIGYOUNAME
           MOVE   PTRSI-PREFNAME       TO  WRK-RECE82-PREFNAME
           MOVE   PTRSI-PREFKBN        TO  WRK-RECE82-PREFKBN  
           MOVE   PTRSI-CITYNAME       TO  WRK-RECE82-CITYNAME
           MOVE   PTRSI-CITYKBN        TO  WRK-RECE82-CITYKBN
      *
           MOVE   PTRSI-SINKEI         TO  WRK-RECE82-SINKEI
           EVALUATE    PTRSI-HKNKBN 
               WHEN    "1"  
                   IF    ( PTRSI-SINKEI        =   "1" OR  "3" OR
                                                   "7"              )
                   AND   ( PTRSI-RYOSTYMD(1:6) <   WRK-RECE82-SRYYM )
                       MOVE    "5"         TO  WRK-RECE82-SINKEI
                   END-IF
               WHEN    "2"  
                   IF    ( PTRSI-SINKEI        =   "1" OR  "7"      )
                   AND   ( PTRSI-RYOSTYMD(1:6) <   WRK-RECE82-SRYYM )
                       MOVE   "5"          TO  WRK-RECE82-SINKEI
                   END-IF
           END-EVALUATE
      *
           MOVE   PTRSI-TENKI          TO  WRK-RECE82-TENKI
           IF    (PTRSI-TENKI  = "1" OR "5" OR "7" OR "9"   )
           AND   (WRK-RECE82-SRYYM  <   PTRSI-RYOEDYMD(1:6) )
                  MOVE   "3"           TO  WRK-RECE82-TENKI
           END-IF
           IF     WRK-RECE82-SRYYM  =   PTRSI-RYOEDYMD(1:6)
                  MOVE  PTRSI-RYOEDYMD TO  WRK-RECE82-TENKIYMD
           END-IF
           MOVE   PTRSI-SHOBYOYMD      TO  WRK-RECE82-SHOBYOYMD
           MOVE   PTRSI-SISIKBN        TO  WRK-RECE82-SISIKBN
           MOVE   PTRSI-RYOSTYMD       TO  WRK-RECE82-RYOSTYMD
           MOVE   PTRSI-RYOEDYMD       TO  WRK-RECE82-RYOEDYMD
           MOVE   PTRSI-COMMENT        TO  WRK-RECE82-COMMENT
           MOVE   PTRSI-SAIGAIKBN      TO  WRK-RECE82-SAIGAIKBN 
           MOVE   PTRSI-RYOSTYMD       TO  WRK-RECE82-SHOSHINYMD
      *
      *    病院・診療所区分
           MOVE    SYS-1001-HOSPSBT    TO  WRK-RECE82-HOSPSBT
      *
           IF      WRK-TEISEIKBN      =   "1"
               MOVE    WRK-ERR-ERRKBN     TO  WRK-RECE82-ERR-ERRKBN
               MOVE    WRK-ERR-DAY        TO  WRK-RECE82-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                          TO  WRK-RECE82-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD   TO  WRK-RECE82-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD   TO  WRK-RECE82-ERR-TEKEDYMD
           END-IF 
      *
           MOVE   PTRSI-DRCD           TO  WRK-RECE82-DRCD
           MOVE   PTRSI-HOKENNAME      TO  WRK-RECE82-HOKENNAME
      *
           MOVE   PTRSI-KIJYUNYM       TO  WRK-RECE82-KIJYUNYM-KSI
           MOVE   PTRSI-KAISU          TO  WRK-RECE82-KAISU-KSI
           MOVE   PTRSI-JIBAISEIKBN    TO  WRK-RECE82-SEIKYUKBN
           MOVE   PTRSI-SYOCD          TO  WRK-RECE82-SYOCD
      *
           IF     WRK-RECE82-SRYYM   >=  "201204"
      *      現物給付対象とする
             IF     PTRSI-DAISAN-GENBTKBN  =  "2"
               MOVE   1   TO   WRK-RECE82-DAISAN-GENBUTU-GAITO
             END-IF
           END-IF
      *
      *    点数単価
           EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                    MOVE     11.5        TO   WRK-RECE82-TANKA
               WHEN  "2"
                    MOVE     12.0        TO   WRK-RECE82-TANKA
           END-EVALUATE
      *    公務災害（労災準拠）の点数単価
           IF   (WRK-RECE82-HKNKBN       =   "5" )
           AND  (WRK-KOMUKBN             =   "1" )
           AND  (WRK-KOMURSITANKAKBN     =   "2" )
           AND  (WRK-KOMURSITANKA-G  NOT =  SPACE)
               MOVE   WRK-KOMURSITANKA   TO   WRK-RECE82-TANKA
           END-IF
      *    自賠責加算率
           MOVE    WRK-KSNRATE           TO   WRK-RECE82-KSNRATE
      *    自賠責文書料消費税
           MOVE    SYS-4001-TAXKBN       TO   WRK-RECE82-TAXKBN
      *    自賠責食事療養費
           MOVE    SYS-4001-SKJKBN       TO   WRK-RECE82-SKJKBN
      *    自賠責特定器材等消費税
           MOVE    SYS-4001-TAXKBN-KZI   TO   WRK-RECE82-TAXKBN-KZI
      *    自賠責固定帯加算等取扱
           MOVE    SYS-4001-KOTEIKBN     TO   WRK-RECE82-KOTEIKBN
      *    労災レセ回数記載
           MOVE    SYS-4001-ROUKAISU     TO   WRK-RECE82-ROUKAISU
      *    自賠責区分
           MOVE    WRK-JIBAIKBN          TO   WRK-RECE82-JIBAIKBN
           IF      WRK-RECE82-SRYYM      >=  WRK-CONS-SRYYM-201707
               IF     WRK-PARA-RECEKBN       =   "8"
                   CONTINUE
               ELSE
      *            自賠責区分（患者別設定）
                   MOVE    PTRSI-JIBAIKBN      TO  WRK-RECE82-JIBAIKBN
               END-IF
           END-IF
      *
      *    技術点単価
           MOVE    WRK-SYUTENTANKA       TO   WRK-RECE82-SYUTENTANKA
      *    薬剤等点単価
           MOVE    WRK-YKZTENTANKA       TO   WRK-RECE82-YKZTENTANKA
      *    公務災害区分
           MOVE    WRK-KOMUKBN           TO   WRK-RECE82-KOMUKBN
      *    公務災害点単価
           MOVE    WRK-KOMUTENTANKA      TO   WRK-RECE82-KOMUTENTANKA
      *    労災指定医療機関区分
           MOVE    WRK-RSIIRYOKBN        TO   WRK-RECE82-RSIIRYOKBN
           IF      WRK-RECE82-HKNKBN     =   "4" OR  "6"
               MOVE    "0"               TO   WRK-RECE82-RSIIRYOKBN
           END-IF
           IF    WRK-RECE82-SYOCD        =   SPACE
           AND   PTRSI-RYOSTYMD(1:6)    <=  "201403"
             EVALUATE  WRK-RECE82-HKNKBN
               WHEN       "1"
                 MOVE    WRK-RECE82-KOFUNUM(1:5)
                                               TO  WRK-RECE82-SYOCD
               WHEN       "2"
                 INITIALIZE                        PTRSIINF-REC
                 MOVE    WRK-RECE82-HOSPNUM    TO  PTRSI-HOSPNUM
                 MOVE    WRK-RECE82-PTID       TO  PTRSI-PTID
                 MOVE    "1"                   TO  PTRSI-HKNKBN
                 MOVE    WRK-RECE82-SHOBYOYMD  TO  PTRSI-SHOBYOYMD
                 MOVE    PTRSIINF-REC          TO  MCPDATA-REC
                 MOVE    "tbl_ptrsiinf"        TO  WRK-MCP-TABLE
                 MOVE    "key4"                TO  WRK-MCP-PATHNAME
                 PERFORM 900-PTRSIINF-INV-SEC
                 IF   FLG-PTRSIINF   =   ZERO
                   IF    PTRSI-SYOCD  NOT = SPACE
                     MOVE   PTRSI-SYOCD        TO  WRK-RECE82-SYOCD
                   ELSE
                     MOVE   PTRSI-KOFUNUM(1:5) TO  WRK-RECE82-SYOCD
                   END-IF
                 END-IF
             END-EVALUATE
           END-IF
      *
      *    入院料情報取得フラグ
           MOVE    1               TO  WRK-RECE82-DOUJITSU-FLG
      *    同日再入院（入院分）の保険組合せ、食事開始時間区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       31
               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX1)
                                           TO
                                   WRK-RECE82-DOUJITSU-HKNCOMBI (IDX1)
               MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX1)
                                           TO
                                   WRK-RECE82-DOUJITSU-SKJKBN   (IDX1)
           END-PERFORM
      *    入院区分
           MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE82-NYUINDAY-TBL
      *
      *    第三者行為時の医保分主保険情報
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "8"
                   MOVE    COMB-HKNNUM         TO  WRK-RECE82-HKNNUM-X
                   MOVE    COMB-HKNID          TO  WRK-RECE82-HKNID-X
      * 
                   INITIALIZE                      PTHKNINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                   MOVE    WRK-RECE82-PTID     TO  PTHKN-PTID
                   MOVE    WRK-RECE82-HKNID-X  TO  PTHKN-HKNID
                   MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                   PERFORM 900-PTHKNINF-READ-SEC
                   IF      FLG-PTHKNINF        =   ZERO
                       MOVE    PTHKN-HKNJANUM  TO  WRK-RECE82-HKNJANUM
                       MOVE    PTHKN-KIGO      TO  WRK-RECE82-KIGO
                       MOVE    PTHKN-NUM       TO  WRK-RECE82-NUM
                       IF      WRK-RECE82-HKNJANUM
                                               NOT =   SPACE
                           MOVE    SPACE               TO  HKNJAINF-REC
                           INITIALIZE                      HKNJAINF-REC
                           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                           MOVE    WRK-RECE82-HKNJANUM TO
                                                       HKNJA-HKNJANUM
                           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                           PERFORM 900-HKNJAINF-READ-SEC
                           MOVE    HKNJA-HKNJANAME     TO
                                                   WRK-RECE82-HKNJANAME
                       END-IF
                   END-IF
      *
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE    WRK-RECE82-SRYYM    TO  HKN-TEKSTYMD (1:6)
                   MOVE    "01"                TO  HKN-TEKSTYMD (7:2)
                   MOVE    HKN-TEKSTYMD        TO  HKN-TEKEDYMD
                   IF      WRK-RECE82-HKNNUM-X =   "069"
                       MOVE    "067"               TO  HKN-HKNNUM
                   ELSE
                       MOVE   WRK-RECE82-HKNNUM-X  TO  HKN-HKNNUM
                   END-IF
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-READ-SEC
      *
      *            負担割合セット処理
                   PERFORM 2001121-FTNRATE-SET-SEC
           END-EVALUATE
      *??
           DISPLAY "WRK-RECE82-JIBAIKBN=" WRK-RECE82-JIBAIKBN "#"
      *
           MOVE    WRK-RECE82-REC      TO  RECE82-REC
           WRITE   RECE82-REC
      *
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
           .
       200112-RECE82-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担割合セット処理
      *****************************************************************
       2001121-FTNRATE-SET-SEC            SECTION.
      *
      *    平成２０年４月以降のみ設定
           IF      WRK-RECE82-SRYYM    >=  WRK-CONS-SRYYM-200804
               EVALUATE    WRK-RECE82-HKNNUM-X
                   WHEN    "060"
                   WHEN    "068"
                       IF      AGECHK-SYUGAKUKBN   =   1
                           IF      PTHKN-HOJOKBN   =   "0" OR  "1" OR
                                                       "4" OR  "5"
                               EVALUATE    PTHKN-HOJOKBN
                                   WHEN    "0"
                                       MOVE    ZERO   TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN    "1"
                                       MOVE    10     TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN    "4"
                                       MOVE    ZERO   TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN    "5"
                                       MOVE    10     TO
                                                   WRK-RECE82-FTNRATE
                               END-EVALUATE
                           ELSE
                               MOVE    20      TO  WRK-RECE82-FTNRATE
                           END-IF
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "0"
                                   MOVE    ZERO
                                               TO  WRK-RECE82-FTNRATE
                               WHEN    "1"
                                   MOVE    10  TO  WRK-RECE82-FTNRATE
                               WHEN    "2"
                                   MOVE    20  TO  WRK-RECE82-FTNRATE
                               WHEN    "3"
                                   MOVE    30  TO  WRK-RECE82-FTNRATE
                               WHEN    "4"
                                   MOVE    ZERO
                                               TO  WRK-RECE82-FTNRATE
                               WHEN    "5"
                                   MOVE    10  TO  WRK-RECE82-FTNRATE
                               WHEN    "6"
                                   MOVE    20  TO  WRK-RECE82-FTNRATE
                               WHEN    "8"
                                   MOVE    20  TO  WRK-RECE82-FTNRATE
                               WHEN    "9"
                                   MOVE    10  TO  WRK-RECE82-FTNRATE
                           END-EVALUATE
                       END-IF
                   WHEN    "067"
                   WHEN    "069"
                       IF      AGECHK-SYUGAKUKBN   =   1
                           MOVE    20          TO   WRK-RECE82-FTNRATE
                       ELSE
                           IF      PTHKN-HOJOKBN   NOT =  SPACE
                               EVALUATE    PTHKN-HOJOKBN
                                   WHEN    "7"
                                       MOVE    30  TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN    "8"
                                       MOVE    20  TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN    "9"
                                       MOVE    10  TO
                                                   WRK-RECE82-FTNRATE
                               END-EVALUATE
                           ELSE
                               IF    ( RECE82-HKNJANUM =   "67330753" )
                               AND   ( RECE82-SRYYM    <=
                                               WRK-CONS-SRYYM-201109  )
                                   MOVE    20      TO
                                                   WRK-RECE82-FTNRATE
                               ELSE
                                   IF      PTHKN-HONKZKKBN     =   "1"
                                       MOVE    HKN-HON-NYUKFTNRATE
                                                   TO
                                                   WRK-RECE82-FTNRATE
                                   ELSE
                                       MOVE    HKN-KZK-NYUKFTNRATE
                                                   TO
                                                   WRK-RECE82-FTNRATE
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   WHEN    SPACE
                       MOVE    ZERO            TO  WRK-RECE82-FTNRATE
                   WHEN    "039"
                   WHEN    "040"
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "1"
                               MOVE    10      TO  WRK-RECE82-FTNRATE
                           WHEN    "3"
                               MOVE    30      TO  WRK-RECE82-FTNRATE
                       END-EVALUATE
                   WHEN    OTHER
                       IF      AGECHK-SYUGAKUKBN   =   1
                           MOVE  20            TO  WRK-RECE82-FTNRATE
                       ELSE
                           IF      PTHKN-HOJOKBN   =   SPACE
                               IF      PTHKN-HONKZKKBN =   "1"
                                   MOVE    HKN-HON-NYUKFTNRATE
                                               TO  WRK-RECE82-FTNRATE
                               ELSE
                                   MOVE    HKN-KZK-NYUKFTNRATE
                                               TO  WRK-RECE82-FTNRATE
                               END-IF
                           ELSE
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN   "8"
                                       MOVE    20  TO
                                                   WRK-RECE82-FTNRATE
                                   WHEN   "9"
                                       MOVE    10  TO
                                                   WRK-RECE82-FTNRATE
                               END-EVALUATE
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
      *
           .
       2001121-FTNRATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       200111-RECE82-REWRITE-SEC            SECTION.
      *
           IF      WRK-DBKBN          =   "2"
               CONTINUE
           ELSE              
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                   IF      WRK-RECE82-SRYKA    (IDX)       =   SPACE
                       MOVE    ACCT-SRYKA      TO  WRK-RECE82-SRYKA(IDX)
                       MOVE    20              TO  IDX
                   ELSE
                       IF      WRK-RECE82-SRYKA    (IDX)   
                                                   =   ACCT-SRYKA
                           MOVE    20                  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE82-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                        (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                       ( WRK-DAY (2 IDX3)        >   ZERO )
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE82-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-RECE82-DOUJITSU-FLG =   ZERO
      *        入院料情報取得フラグ
               MOVE    1               TO  WRK-RECE82-DOUJITSU-FLG
      *        同日再入院（入院分）の保険組合せ、食事開始時間区分
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       31
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX1)
                                               TO
                                   WRK-RECE82-DOUJITSU-HKNCOMBI (IDX1)
                   MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX1)
                                               TO
                                   WRK-RECE82-DOUJITSU-SKJKBN   (IDX1)
               END-PERFORM
      *        入院区分
               MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE82-NYUINDAY-TBL
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
           AND     WRK-RECE82-ERR-ERRKBN   =   SPACE
               MOVE    WRK-ERR-ERRKBN      TO  WRK-RECE82-ERR-ERRKBN
               MOVE    WRK-ERR-DAY         TO  WRK-RECE82-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE82-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD    TO  WRK-RECE82-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD    TO  WRK-RECE82-ERR-TEKEDYMD
           END-IF          
      *
           MOVE    WRK-RECE82-REC      TO  RECE82-REC
           REWRITE RECE82-REC
      *
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           .
       200111-RECE82-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
                   PERFORM 20021-KOBETU-REAL-SEC
                   MOVE    "1"   TO  PTINF-TSTPTNUMKBN
                   GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
      *        レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYSKANRI    =   1
                              OR          FLG-END         =   1
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "1"              TO  SYS-2002-NYUGAIKBN
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      *
      *    医療機関ＩＤ編集
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
      *
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    システム管理マスタ（４００１）から点数単価の取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS4001(SYSKAN-IDX)  TO  SYS-4001-REC
                IF   ( SYS-4001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-4001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                    IF    SYS-4001-KSNRATE1(1:3)   =  SPACE
                      IF   SYS-4001-KSNRATE(1:2)   =  SPACE
                        MOVE   100           TO    WRK-KSNRATE
                      ELSE
                        COMPUTE WRK-KSNRATE  =  SYS-4001-KSNRATE
                                             +  100
                      END-IF
                    ELSE
                      MOVE SYS-4001-KSNRATE1 TO    WRK-KSNRATE
                    END-IF
                    IF     WRK-PARA-RECEKBN  =   "8"
                      MOVE    "2"                TO    WRK-JIBAIKBN
                      MOVE    10                 TO    WRK-SYUTENTANKA
                      MOVE    10                 TO    WRK-YKZTENTANKA
                    ELSE
                      IF     SYS-4001-JIBAIKBN  =    SPACE
                          MOVE "1"               TO    WRK-JIBAIKBN
                      ELSE
                          MOVE SYS-4001-JIBAIKBN TO    WRK-JIBAIKBN
                      END-IF
                      IF     SYS-4001-SYUTENTANKA2-X =  SPACE
                        IF   SYS-4001-SYUTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-SYUTENTANKA
                        ELSE
                          MOVE SYS-4001-SYUTENTANKA
                                                 TO    WRK-SYUTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-SYUTENTANKA2
                                                 TO    WRK-SYUTENTANKA
                      END-IF
                      IF     SYS-4001-YKZTENTANKA2-X =  SPACE
                        IF   SYS-4001-YKZTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-YKZTENTANKA
                        ELSE
                          MOVE SYS-4001-YKZTENTANKA
                                                 TO    WRK-YKZTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-YKZTENTANKA2
                                                 TO    WRK-YKZTENTANKA
                      END-IF
                    END-IF
                    IF     SYS-4001-KOMUKBN   =    SPACE
                      MOVE "1"               TO    WRK-KOMUKBN
                    ELSE
                      MOVE SYS-4001-KOMUKBN  TO    WRK-KOMUKBN
                    END-IF
                    IF     SYS-4001-KOMUTENTANKA-X =  SPACE
                      MOVE ZERO              TO    WRK-KOMUTENTANKA
                    ELSE
                      MOVE SYS-4001-KOMUTENTANKA
                                             TO    WRK-KOMUTENTANKA
                    END-IF
                    IF     SYS-4001-RSIIRYOKBN     =  SPACE
                      MOVE "0"               TO    WRK-RSIIRYOKBN
                    ELSE
                      MOVE SYS-4001-RSIIRYOKBN
                                             TO    WRK-RSIIRYOKBN
                    END-IF
                    MOVE   SYS-4001-KOMURSITANKAKBN
                                             TO    WRK-KOMURSITANKAKBN
                    MOVE   SYS-4001-KOMURSITANKA-G
                                             TO    WRK-KOMURSITANKA-G
                END-IF
      *
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    同日再入院情報取得
           PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *    診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *      
           INITIALIZE                  SRYACCT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
               MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
           ELSE
               MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
           END-IF
           MOVE    WRK-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                   OR          FLG-END         =   1 
               PERFORM 200211-KOBETU-SRYACCT-HEN-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-END             =   ZERO
      *        入院会計読み込み
               MOVE    "2"                 TO  WRK-DBKBN 
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *     
               INITIALIZE                  NYUINACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                       OR          FLG-END           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID 
                   " SRYYM="   SYS-2002-SRYYM
      *
      *    医療機関ＩＤ編集
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
      *
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-HENSYU-EXT
           END-IF
      *
      *    システム管理マスタ（４００１）から点数単価の取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS4001(SYSKAN-IDX)  TO  SYS-4001-REC
                IF   ( SYS-4001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-4001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                    IF    SYS-4001-KSNRATE1(1:3)   =  SPACE
                      IF   SYS-4001-KSNRATE(1:2)   =  SPACE
                        MOVE   100           TO    WRK-KSNRATE
                      ELSE
                        COMPUTE WRK-KSNRATE  =  SYS-4001-KSNRATE
                                             +  100
                      END-IF
                    ELSE
                      MOVE SYS-4001-KSNRATE1 TO    WRK-KSNRATE
                    END-IF
                    IF     WRK-PARA-RECEKBN  =   "8"
                      MOVE    "2"                TO    WRK-JIBAIKBN
                      MOVE    10                 TO    WRK-SYUTENTANKA
                      MOVE    10                 TO    WRK-YKZTENTANKA
                    ELSE
                      IF     SYS-4001-JIBAIKBN  =    SPACE
                          MOVE "1"               TO    WRK-JIBAIKBN
                      ELSE
                          MOVE SYS-4001-JIBAIKBN TO    WRK-JIBAIKBN
                      END-IF
                      IF     SYS-4001-SYUTENTANKA2-X =  SPACE
                        IF   SYS-4001-SYUTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-SYUTENTANKA
                        ELSE
                          MOVE SYS-4001-SYUTENTANKA
                                                 TO    WRK-SYUTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-SYUTENTANKA2
                                                 TO    WRK-SYUTENTANKA
                      END-IF
                      IF     SYS-4001-YKZTENTANKA2-X =  SPACE
                        IF   SYS-4001-YKZTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-YKZTENTANKA
                        ELSE
                          MOVE SYS-4001-YKZTENTANKA
                                                 TO    WRK-YKZTENTANKA
                        END-IF
                      ELSE
                          MOVE SYS-4001-YKZTENTANKA2
                                                 TO    WRK-YKZTENTANKA
                      END-IF
                    END-IF
                    IF     SYS-4001-KOMUKBN   =    SPACE
                      MOVE "1"               TO    WRK-KOMUKBN
                    ELSE
                      MOVE SYS-4001-KOMUKBN  TO    WRK-KOMUKBN
                    END-IF
                    IF     SYS-4001-KOMUTENTANKA-X =  SPACE
                      MOVE ZERO              TO    WRK-KOMUTENTANKA
                    ELSE
                      MOVE SYS-4001-KOMUTENTANKA
                                             TO    WRK-KOMUTENTANKA
                    END-IF
                    IF     SYS-4001-RSIIRYOKBN     =  SPACE
                      MOVE "0"               TO    WRK-RSIIRYOKBN
                    ELSE
                      MOVE SYS-4001-RSIIRYOKBN
                                             TO    WRK-RSIIRYOKBN
                    END-IF
                    MOVE   SYS-4001-KOMURSITANKAKBN
                                             TO    WRK-KOMURSITANKAKBN
                    MOVE   SYS-4001-KOMURSITANKA-G
                                             TO    WRK-KOMURSITANKA-G
                END-IF
      *
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-HENSYU-EXT
           END-IF
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-END             =   ZERO
      *        同日再入院情報取得
               PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *        診療会計読み込み
               MOVE    "1"                 TO  WRK-DBKBN 
      *      
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  ACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
               MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1 
                   PERFORM 200211-KOBETU-SRYACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
      *        入院会計読み込み
               MOVE    "2"                 TO  WRK-DBKBN 
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *     
               INITIALIZE                  NYUINACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  NACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                       OR          FLG-END           =   1
                   PERFORM 200212-KOBETU-NYUINACCT-HEN-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       200211-KOBETU-SRYACCT-HEN-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1      
      *****    MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
      *****    MOVE    ACCT-PTID           TO  KEY-N-PTID 
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI    =   ZERO
                   EVALUATE    WRK-PARA-RECEKBN
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                           IF      COMB-HKNNUM  =   WRK-CONS-HKNNUM
                               IF      COMB-TEKSTYMD   <=  WRK-STYMD
                               AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                   MOVE    SPACE       TO  WRK-UPDKBN
                               ELSE
                                   MOVE    "1"         TO  WRK-UPDKBN
                               END-IF
                           ELSE     
                               MOVE    "1"             TO  WRK-ROUSAIKBN
                           END-IF    
                       WHEN    "8"
                           IF      COMB-KOHHKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                               IF      COMB-TEKSTYMD   <=  WRK-STYMD
                               AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                   MOVE    SPACE       TO  WRK-UPDKBN
                               ELSE
                                   MOVE    "1"         TO  WRK-UPDKBN
                               END-IF
                           ELSE     
                               MOVE    "1"             TO  WRK-ROUSAIKBN
                           END-IF    
                   END-EVALUATE
               ELSE
                   MOVE    "1"             TO  WRK-TEISEIKBN    
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1      
                               OR      KEY-NEW     NOT =   KEY-OLD
                   IF      WRK-ROUSAIKBN           =   SPACE            
                       PERFORM 200111-SRYACCT-HENSYU-SEC
                   END-IF    
      *
                   IF    ( WRK-ROUSAIKBN     =   "1"   )  
                   OR    ( WRK-UPDKBN        =   SPACE )         
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1      
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"    TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                    TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"    TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       200211-KOBETU-SRYACCT-HEN-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       200212-KOBETU-NYUINACCT-HEN-SEC              SECTION.
      *     
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBI-TABLE
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      FLG-END         =   1      
                           OR      KEY-NEW     NOT =   KEY-OLD
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                               WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
      *        同日再入院のチェック
               PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
      *        入院会計より保険組合せ読込     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF    ((WRK-DAY (1 IDX)        >   ZERO) OR
                          (LNK-NYUINDAY-KBN(IDX)  =   "1" ))  AND
                         ( WRK-DAY (2 IDX)        >   ZERO )
                       PERFORM 200121-NYUINACCT-CHK-SEC
                   END-IF
               END-PERFORM        
      *
      *        入院会計の保険組合せを追加
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                       CONTINUE
                   ELSE    
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC    
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                           MOVE    "1"         TO  WRK-TEISEIKBN
                           IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   =   ZERO
                               MOVE    1           TO  WRK-ERR-ERRKBN
                           ELSE
                               MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                               MOVE    6           TO  WRK-ERR-ERRKBN
                           END-IF
                           MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                               WRK-ERR-HKNCOMBINUM
                       END-IF        
                       PERFORM 200122-NYUINACCT-HENSYU-SEC
                   END-IF 
               END-PERFORM
           END-IF
           .
       200212-KOBETU-NYUINACCT-HEN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0810 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
      *
           IF      CNT-OUT             =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82  CNT " CNT-OUT 
           DISPLAY "*** ORCR0810 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院情報取得処理
      *****************************************************************
       500-ORCSNYUINDAY-CALL-SEC      SECTION.
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SYS-1001-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   MOVE    KEY-O-PTID      TO  LNK-NYUINDAY-PTID
                   MOVE    SYS-2001-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
               WHEN    "2"
                   MOVE    SYS-2002-PTID   TO  LNK-NYUINDAY-PTID
                   MOVE    SYS-2002-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
           END-EVALUATE
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
           END-IF
           .
       500-ORCSNYUINDAY-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       500-ORCSNYUINDAY-CHECK-SEC          SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
             IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                           =   "1"
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >   10
      *
      *            保険組合せ追加
                   IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                       INITIALIZE                      HKNCOMBI-REC
                       MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
                       MOVE    KEY-O-PTID          TO  COMB-PTID
                       MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                                   TO  COMB-HKNCOMBINUM 
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM  910-HKNCOMBI-INV-SEC
      *                （対象外の組合せは１をセット）
                       IF      FLG-HKNCOMBI    =   ZERO
                           MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                           EVALUATE  WRK-PARA-RECEKBN
                            WHEN     "3"
                            WHEN     "4"
                            WHEN     "5"
                             IF    COMB-HKNNUM NOT =   WRK-CONS-HKNNUM
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                             END-IF
                            WHEN     "8"
                             IF    COMB-KOHHKNNUM (1)
                                               NOT =   WRK-CONS-HKNNUM
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                             END-IF
                           END-EVALUATE
                       ELSE
                           IF      COMB-DELKBN         =   "1"
                               MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
                               EVALUATE  WRK-PARA-RECEKBN
                                WHEN     "3"
                                WHEN     "4"
                                WHEN     "5"
                                 IF  COMB-HKNNUM NOT = WRK-CONS-HKNNUM
                                     MOVE    "1"         TO
                                             WRK-HKNCOMBI-SYOKBN (IDX1)
                                 ELSE
                                    MOVE     "2"         TO
                                             WRK-HKNCOMBI-SYOKBN (IDX1)
                                 END-IF
                                WHEN     "8"
                                 IF  COMB-KOHHKNNUM (1)
                                                 NOT = WRK-CONS-HKNNUM
                                     MOVE    "1"         TO
                                             WRK-HKNCOMBI-SYOKBN (IDX1)
                                 ELSE
                                    MOVE     "2"         TO
                                             WRK-HKNCOMBI-SYOKBN (IDX1)
                                 END-IF
                               END-EVALUATE
                           ELSE
                               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                                (IDXX)
                                                   TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                               MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       END-IF
                   END-IF
      *
      *            適用範囲かチェック（範囲外のとき２をセット）
                   IF      LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                           =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                       IF      WRK-HKNCOMBI-SYOKBN (IDX1)
                                           NOT =   SPACE
                           CONTINUE                    
                       ELSE     
                           MOVE    WRK-STYMD       TO  WRK-YMD
                           MOVE    IDXX            TO  WRK-DD
                           IF      WRK-COMB-TEKSTYMD (IDX1)
                                                   <=  WRK-YMD
                           AND     WRK-COMB-TEKEDYMD (IDX1)
                                                   >=  WRK-YMD
                               CONTINUE
                           ELSE   
                               MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                               MOVE    IDXX        TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                           END-IF
                       END-IF
                       MOVE    10              TO  IDX1
                   END-IF            
               END-PERFORM
             END-IF
           END-PERFORM
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE
               INVALID
                   MOVE    1           TO    FLG-RECE82
               NOT  INVALID
                   MOVE    ZERO        TO    FLG-RECE82
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1           TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1               TO  FLG-PTRSIINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
                   INITIALIZE                      PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   INITIALIZE                      KYUSEIRRK-REC
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者読込
      *****************************************************************
       900-HKNJAINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
                   INITIALIZE                      HKNJAINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
               INITIALIZE                      HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号読込
      *****************************************************************
       900-HKNNUM-READ-SEC           SECTION.
      *
           MOVE   "tbl_hknnum"         TO  MCP-TABLE
           MOVE   "key5"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE   "tbl_hknnum"         TO  MCP-TABLE
               MOVE   "key5"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   INITIALIZE                  HKNNUM-REC
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE   "tbl_hknnum"         TO  MCP-TABLE
           MOVE   "key5"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0810 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.

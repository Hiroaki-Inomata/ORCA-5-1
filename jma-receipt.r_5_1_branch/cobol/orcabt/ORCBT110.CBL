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
       PROGRAM-ID.             ORCBT110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 日次統計データ用基本情報作成（外来）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/31    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.03.01    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.01.01    NACL-小豆沢  07/11/16  公務災害、公害の種別判定追加
      *  04.01.01    NACL-小豆沢  08/02/05  レセプト種別セット修正
      *
      *  04.02.01    NACL-藤原    08/04/10  平成２０年４月改正対応
      *
      *  04.04.01    NACL-藤原    09/02/26  平成２０年４月以降に７５歳到
      *                                     達時に海外に住所を有する場合
      *                                     の対応
      *  04.04.03    NACL-藤原    10/08/23  後期高齢非該当対応
      *                                     （高齢受給者票がない場合）
      * 
      *  04.05.01    NACL-小豆沢  10/01/07  治験のレセプト種別対応
      *
      *  04.06.01    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *  04.06.02    NACL-門脇    13/02/27  平成２５年４月改正対応
      *  04.06.03    NACL-門脇    13/09/25  労災マスタ追加対応
      *                                     （電話再診料　等）
      *
      *  04.07.01    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.02    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
      *  04.07.03    NACL-門脇    14/12/15  平成２７年１月改正対応
      *                                     （初再診料・低妥結率対応）
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.03    NACL-門脇    18/04/02  平成３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    日次統計データ
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    日次統計データ
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC. 
           COPY    "CPTCF004.INC".
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI04PARA//.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01). 
           03  FLG-HKNNUM                      PIC 9(01). 
           03  FLG-TOUKEI04                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-KYUSEIRRK                   PIC 9(01).
           03  FLG-MONTHLYNUM                  PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
      * 
           03  FLG-SYORI                       PIC 9(01).
           03  FLG-CHK-END                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-TOUKEI04                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDX4                            PIC 9(03). 
           03  IDX5                            PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDXX                            PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-DENPNUM               PIC 9(07).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-DENPNUM               PIC 9(07).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-PREFNUM            PIC X(02). 
               05  WRK-PARA-SRYYMD             PIC X(08).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-HOUKATUKBN         PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
           03  WRK-ERRMSG                  PIC X(300).
      *
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *    患者毎
           03  WRK-HENSYU-AREA.
               05  WRK-KIHONKBN            PIC 9(01).
               05  WRK-JIKANKBN            PIC 9(01).
               05  WRK-SYOKAIKBN           PIC 9(01).
      *
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-PAYKBN                  PIC X(02).
      *
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
           03  WRK-AGE                     PIC 9(03).
           03  WRK-SYUGAKUKBN              PIC 9(01).
      *
           03  WRK-HKNNUM-AREA.
               05  WRK-HKNNUM-TBL          OCCURS   4.
      *            法別番号
                   07  WRK-HKN-HBTNUM                PIC  X(02).
      *            レセプト請求区分
                   07  WRK-HKN-RECESKYKBN            PIC  X(01).
      *            保険番号
                   07  WRK-HKN-HKNNUM                PIC  X(03).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
      *    診療科テーブル
       01  TBL-SYS-1005-AREA.
           03  TBL-SYS-1005-REC        OCCURS  99.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-RECESRYKA       PIC X(02).
           03  IDX-SRYKA-MAX            PIC 9(04).
      *
           COPY    "MCPAREA".
      *
      *    日次統計データ保存
       01  HZN-TOUKEI04-REC. 
           COPY    "CPTCF004.INC"  REPLACING  //TOUKEI04-//
                                   BY         //HZN-TOUKEI04-//.
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM PIC X(03)    VALUE   "975".       
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201404   PIC 9(06)   VALUE   201404.      
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
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
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
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  TTNS-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-PREFNUM 
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOUKATUKBN 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "TOKEI110"          TO  TOUKEI04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI04PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI04PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           MOVE    TOUKEI04PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TOUKEI04PARA-FULLNAME
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT110"      TO  JOB-PGID
           MOVE    "日次統計データ作成（外来）"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD 
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
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-SYMD            TO  SYS-2005-STYUKYMD
                                           SYS-2005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
      *    患者登録機能制御情報
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1017-STYUKYMD
                                           SYS-1017-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
           ELSE    
               INITIALIZE                      SYS-1017-REC
           END-IF
           IF      SYS-1017-SEIHO-JKYSNUM
                                       =   SPACE
               MOVE    ZERO                TO  SYS-1017-SEIHO-JKYSNUM
           END-IF
      * 
      *    県内扱い保険者情報
           MOVE    SPACE           TO  WRK-1033-TABLE
      *     
           MOVE    SPACE           TO  SYS-1033-REC
           INITIALIZE                  SYS-1033-REC
           MOVE    "1033"          TO  SYS-1033-KANRICD
           MOVE    "*"             TO  SYS-1033-KBNCD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
           MOVE    SYS-1033-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *    診療科テーブル
           INITIALIZE                  TBL-SYS-1005-AREA
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SYMD            TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  99
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1005-KBNCD(1:2)
                                           TO  TBL-SRYKA  (IDX)
               MOVE    SYS-1005-RECESRYKA  TO  TBL-RECESRYKA  (IDX)
               MOVE    IDX                 TO  IDX-SRYKA-MAX
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           CLOSE                       TOUKEI04-FILE
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    受診履歴読み込み
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-SRYYMD
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM UNTIL       FLG-JYURRK  =   1
      *
               PERFORM 2001-INIT-SYORI-SEC
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM UNTIL    (FLG-JYURRK    =   1  )  OR
                                (KEY-N-PTID    NOT =   KEY-O-PTID)
                   PERFORM 2002-HENSYU-SYORI-SEC
               END-PERFORM
               IF      FLG-SYORI               =   1
                   PERFORM 2003-OUT-SYORI-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    患者毎の編集処理
      *****************************************************************
       2001-INIT-SYORI-SEC              SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *    テスト患者のときは読み飛ばす
           MOVE    JYURRK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "JYURRK PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
      *    テスト患者の時読み飛ばし
           IF     (FLG-PTINF           =   ZERO )  AND
                  (PTINF-TSTPTNUMKBN   =   "1"  )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM UNTIL ( FLG-JYURRK      =   1  )
                       OR    ( KEY-N-PTID  NOT =   KEY-O-PTID)
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
           END-IF
      *
           .
       2001-INIT-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC              SECTION.
      *
      *    患者情報編集
           PERFORM 20021-PTINF-HENSYU-SEC
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM UNTIL ( FLG-JYURRK      =   1  )
                   OR    ( KEY-NEW     NOT =   KEY-OLD)
               IF      JYURRK-DENPNUM      =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               ELSE
                   PERFORM 20022-SYORI-01-SEC
               END-IF
           END-PERFORM
      *
           .
       2002-HENSYU-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20021-PTINF-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      HZN-TOUKEI04-REC
           MOVE    WRK-PARA-HOSPID     TO  HZN-TOUKEI04-HOSPID
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  HZN-TOUKEI04-HOSPCD
      *    診療年月日
           MOVE    WRK-PARA-SRYYMD     TO  HZN-TOUKEI04-SRYYMD
      *    入外区分
           MOVE    2                   TO  HZN-TOUKEI04-NYUGAIKBN
      *    患者番号
           INITIALIZE                  ORCSPTIDAREA
           MOVE    JYURRK-HOSPNUM  TO  SPTID-HOSPNUM
           MOVE    JYURRK-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM     TO  HZN-TOUKEI04-PTNUM
           MOVE    JYURRK-PTID     TO  HZN-TOUKEI04-PTID
      *
      *    患者情報
           MOVE    PTINF-KANANAME  TO  HZN-TOUKEI04-KANANAME
           MOVE    PTINF-NAME      TO  HZN-TOUKEI04-NAME
           MOVE    PTINF-SEX       TO  HZN-TOUKEI04-SEX
           MOVE    PTINF-BIRTHDAY  TO  HZN-TOUKEI04-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  HZN-TOUKEI04-HOME-POST
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO    HZN-TOUKEI04-HOME-ADRS
           END-STRING
           MOVE    PTINF-HOME-TEL1 TO  HZN-TOUKEI04-HOME-TEL
           INITIALIZE                  ADRS-REC
           MOVE    PTINF-HOME-POST TO  ADRS-POST
grpsys     MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           MOVE    ADRS-LPUBCD     TO  HZN-TOUKEI04-LPUBCD
      *
           .
       20021-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー毎編集処理
      *****************************************************************
       20022-SYORI-01-SEC              SECTION.
      *
           MOVE   HZN-TOUKEI04-REC     TO  TOUKEI04-REC
      *    レコード識別
           IF      JYURRK-GRP-DENPNUM  =   ZERO
               MOVE    1                   TO  TOUKEI04-RECSKB
               MOVE    ZERO                TO  WRK-KIHONKBN
               MOVE    ZERO                TO  WRK-JIKANKBN
           ELSE
               IF      JYURRK-GRP-RENNUM   =   1
                   MOVE    1                   TO  TOUKEI04-RECSKB
                   MOVE    ZERO                TO  WRK-KIHONKBN
                   MOVE    ZERO                TO  WRK-JIKANKBN
               ELSE
                   MOVE    2                   TO  TOUKEI04-RECSKB
               END-IF
           END-IF
           MOVE    ZERO                TO  WRK-SYOKAIKBN
      *    診療科
           MOVE    JYURRK-SRYKA        TO  TOUKEI04-SRYKA
           PERFORM 20023-SRYKA-HENSYU-SEC
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  TOUKEI04-HKNCOMBINUM
           PERFORM 20024-HKNCOMBI-HENSYU-SEC
      *    ドクター
           MOVE    JYURRK-DRCD         TO  TOUKEI04-DRCD
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  TOUKEI04-DENPNUM
      *    収納情報
           PERFORM 20026-SYUNOU-HENSYU-SEC
      *    受診履歴の内容判定
           PERFORM UNTIL    (FLG-JYURRK    =   1  )  OR
                            (KEY-NEW   NOT =   KEY-OLD)
      *        診察料算定あり
               IF      JYURRK-SRYKBN1      NOT =   SPACE
                   PERFORM 200227-SRYACCT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           END-PERFORM
      *
      *    出力処理
           PERFORM 2003-OUT-SYORI-SEC
           .
       20022-SYORI-01-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       200227-SRYACCT-HEN-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )  OR
                              (JYURRK-ZAINUM(IDX)  =   ZERO )
      *
               MOVE    SPACE                   TO  SRYACT-REC
               INITIALIZE                      SRYACT-REC
               MOVE    JYURRK-HOSPNUM          TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN        TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID             TO  SRY-PTID
               MOVE    JYURRK-SRYKA            TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)      TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM(IDX)      TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
               MOVE    ZERO                   TO  FLG-CHK-END
               PERFORM UNTIL  (FLG-SRYACT     =   1  )  OR
                              (FLG-CHK-END    =   1  )
                   EVALUATE    SRY-SRYKBN
                      WHEN     "11"
      *                   初診
                          PERFORM 2002271-SYOSIN-CHK11-SEC
                      WHEN     "12"
      *                   再診
                          PERFORM 2002272-SYOSIN-CHK12-SEC
                      WHEN     "13"
      *                   指導
                          PERFORM 2002273-SYOSIN-CHK13-SEC
                      WHEN     "14"
      *                   往診
                          PERFORM 2002273-SYOSIN-CHK14-SEC
                      WHEN     OTHER
                          MOVE     1              TO   FLG-CHK-END
                   END-EVALUATE
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-PERFORM
           .
       200227-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料判定処理
      *****************************************************************
       2002271-SYOSIN-CHK11-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               IF       SRY-SRYCD(IDY)(1:1)    =   "1"
                   MOVE    SRY-SRYCD(IDY)      TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      TNS-DATAKBN         =   "1"
                       MOVE    1                   TO  WRK-KIHONKBN
                   ELSE
                       IF      TNS-TIMEKSNKBN  NOT =   ZERO
      *                    時間外加算判定
                           PERFORM 2002271-TIME-CHK-SEC
                       ELSE
      *                    紹介加算判定
                           PERFORM 2002272-SYOKAI-CHK-SEC
                       END-IF
                   END-IF
               ELSE
                   IF  (SRY-SRYCD(IDY)         =   "099110001") OR
                       (SRY-SRYCD(IDY)         =   "830000020")
                       MOVE    1                   TO  WRK-KIHONKBN
                   END-IF
               END-IF
           END-PERFORM
           .
       2002271-SYOSIN-CHK11-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分判定処理
      *****************************************************************
       2002271-TIME-CHK-SEC              SECTION.
      *
           EVALUATE    TNS-TIMEKSNKBN
               WHEN    1
      *            時間外
                   MOVE    2               TO  WRK-JIKANKBN
               WHEN    2
               WHEN    3
      *            休日
                   MOVE    3               TO  WRK-JIKANKBN
               WHEN    4
      *            深夜
                   MOVE    4               TO  WRK-JIKANKBN
               WHEN    5
      *            時間外特例
                   MOVE    2               TO  WRK-JIKANKBN
           END-EVALUATE
           .
       2002271-TIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    紹介加算判定処理
      *****************************************************************
       2002272-SYOKAI-CHK-SEC              SECTION.
      *
           EVALUATE    SRY-SRYCD(IDY)
      *        初診時（診療所）紹介患者加算
               WHEN    "111003770"
      *        紹介患者加算３
               WHEN    "111009970"
      *        紹介患者加算４
               WHEN    "111010070"
      *        紹介患者加算５
               WHEN    "111010170"
      *        紹介患者加算６
               WHEN    "111010270"
      *        紹介患者加算１
               WHEN    "111011270"
      *        紹介患者加算２
               WHEN    "111011370"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        小児科外来診療料（初診時診療所の紹介患者）加算
               WHEN    "111010370"
      *        小児科外来診療料（紹介患者加算
               WHEN    "111010870"
               WHEN    "111010970"
               WHEN    "111011070"
               WHEN    "111011170"
               WHEN    "113005070"
               WHEN    "113005170"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        診療情報提供料（Ｂ）逆紹介加算         
               WHEN    "113006370"
      *        診療情報提供料（Ｃ）逆紹介加算         
               WHEN    "113006470"
                   MOVE    1               TO  WRK-SYOKAIKBN
      *        初診（診療所）紹介患者加算
               WHEN    "111700470"
               WHEN    "111702370"
               WHEN    "111702470"
               WHEN    "111702570"
               WHEN    "111702670"
               WHEN    "111702770"
               WHEN    "111702870"
                   MOVE    1               TO  WRK-SYOKAIKBN
           END-EVALUATE
           .
       2002272-SYOKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料判定処理
      *****************************************************************
       2002272-SYOSIN-CHK12-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            再診料（一般 病院）
                   WHEN    "112007410"
      *            電話再診料（一般 病院）
                   WHEN    "112007950"
      *            再診料（一般 診療所）
                   WHEN    "112009210"
      *            電話再診（一般 診療所）
                   WHEN    "112009750"
      *            再診（妥結率５割以下）
                   WHEN    "112016610"
      *            電話等再診（妥結率５割以下）
                   WHEN    "112016750"
      *            電話等再診料（３０年３月以前継続）
                   WHEN    "112023350"
      *            電話等特定妥結率再診料（３０年３月以前継続）
                   WHEN    "112023650"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            同日再診（一般 病院）
                   WHEN    "112008350"
      *            電話同日再診（一般 病院）
                   WHEN    "112008850"
      *            同日再診（一般 診療所）
                   WHEN    "112010150"
      *            電話同日再診（一般 診療所）
                   WHEN    "112010650"
      *
                   WHEN    "112015810"
                   WHEN    "112015950"
      *            同日再診（妥結率５割以下）
                   WHEN    "112016850"
      *            同日電話等再診（妥結率５割以下）
                   WHEN    "112016950"
      *            再診料（同一日２科目・減点規定該当の場合）
                   WHEN    "112017010"
      *            電話等再診（同一日２科目・減点規定該当の場合）
                   WHEN    "112017150"
      *            同日電話等再診料（３０年３月以前継続）
                   WHEN    "112023450"
      *            同日電話等特定妥結率再診料（３０年３月以前継続）
                   WHEN    "112023750"
      *            電話等再診料（同一日複数科受診時の２科目）（３０年３月以前継続）
                   WHEN    "112023550"
      *            電話等特定妥結率再診料（同一日複数科２科目・３０年３月以前継続）
                   WHEN    "112023850"
                       MOVE    3                   TO  WRK-KIHONKBN
      *
      *            再診料（老人 病院）
                   WHEN    "112702310"
      *            電話再診料（老人 病院）
                   WHEN    "112702750"
      *            再診料（老人 診療所）
                   WHEN    "112703710"
      *            電話再診（老人 診療所）
                   WHEN    "112704150"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            同日再診（老人 病院）
                   WHEN    "112703050"
      *            電話同日再診（老人 病院）
                   WHEN    "112703450"
      *            同日再診（老人 診療所）
                   WHEN    "112704450"
      *            電話同日再診（老人 診療所）
                   WHEN    "112704850"
                       MOVE    3                   TO  WRK-KIHONKBN
      *
      *            外来診療料
                   WHEN    "112011310"
                   WHEN    "112016310"
      *            外来診療料（老人）
                   WHEN    "112705510"
      *            外来診療料（妥結率５割以下）
                   WHEN    "112017310"
                       MOVE    2                   TO  WRK-KIHONKBN
      *
      *            同日外来診療料
                   WHEN    "112011710"
                   WHEN    "112016550"
      *            同日外来診療料（老人）
                   WHEN    "112705850"
      *
                   WHEN    "112016210"
                   WHEN    "112016410"
      *            同日外来診療料（妥結率５割以下）
                   WHEN    "112017450"
      *            外来診療料（同一日複数科受診時の２科目・妥結率５割以下）
                   WHEN    "112017610"
                       MOVE    3                   TO  WRK-KIHONKBN
      *
      *            労災再診料
                   WHEN    "101120040"
                       IF     SRY-SRYYM       <=   "201306"
                           MOVE    2               TO  WRK-KIHONKBN
                       ELSE
                           MOVE    3               TO  WRK-KIHONKBN
                       END-IF
                   WHEN    "101120010"
                   WHEN    "101120050"
                       IF     SRY-SRYYM       >=   "201307"
                           MOVE    2               TO  WRK-KIHONKBN
                       END-IF
                   WHEN    "101120060"
                   WHEN    "101120070"
                   WHEN    "101120080"
                       IF     SRY-SRYYM       >=   "201307"
                           MOVE    3               TO  WRK-KIHONKBN
                       END-IF
      *
      *            他科受診
                   WHEN    "830000021"
                       IF      JYURRK-GRP-RENNUM   =   1  OR
                                                       ZERO
                           MOVE    2               TO  WRK-KIHONKBN
                       END-IF
      *
                   WHEN    OTHER
                       IF       SRY-SRYCD(IDY)(1:1)    =   "1"
                           MOVE    SRY-SRYCD(IDY)      TO  TNS-SRYCD
                           PERFORM 900-TENSU-READ-SEC
                           IF     (TNS-DATAKBN         =   "2" ) AND
                                  (TNS-TIMEKSNKBN  NOT =   ZERO)
      *                        時間外判定
                               PERFORM 2002271-TIME-CHK-SEC
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       2002272-SYOSIN-CHK12-EXT.
           EXIT.
      *****************************************************************
      *    指導料判定処理
      *****************************************************************
       2002273-SYOSIN-CHK13-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            小児科外来診察料(院外処方 初診)
                   WHEN    "113003510"
      *            小児科外来診察料(院内処方 初診)
                   WHEN    "113003710"
      *            小児かかりつけ診療料（処方せんを交付）（初診時）
                   WHEN    "113019710"
      *            小児かかりつけ診療料（処方せんを交付しない）（初診時）
                   WHEN    "113019910"
                       MOVE    1                   TO  WRK-KIHONKBN
      *
      *            小児科外来診察料(院外処方 再診)
                   WHEN    "113003610"
      *            小児科外来診察料(院内処方 再診)
                   WHEN    "113003810"
      *            小児かかりつけ診療料（処方せんを交付）（再診時）
                   WHEN    "113019810"
      *            小児かかりつけ診療料（処方せんを交付しない）（再診時）
                   WHEN    "113020010"
                       MOVE    2                   TO  WRK-KIHONKBN
               END-EVALUATE
           END-PERFORM
           .
       2002273-SYOSIN-CHK13-EXT.
           EXIT.
      *****************************************************************
      *    往診判定処理
      *****************************************************************
       2002273-SYOSIN-CHK14-SEC              SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    5  )  OR
                              (SRY-SRYCD (IDY) =   SPACE )
               EVALUATE    SRY-SRYCD(IDY)
      *            往診
                   WHEN    "114000110"
      *            特別往診
                   WHEN    "114001610"
      *            在宅患者訪問診察料
                   WHEN    "114001110"
                   WHEN    "114012910"
                   WHEN    "114018010"
                   WHEN    "114030310"
                   WHEN    "114042110"
                   WHEN    "114042210"
                   WHEN    "114042810"
                   WHEN    "114046310"
      *                往診区分
                       MOVE    1                   TO  TOUKEI04-OSINKBN
               END-EVALUATE
           END-PERFORM
           .
       2002273-SYOSIN-CHK14-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       2003-OUT-SYORI-SEC              SECTION.
      *
      *    基本料区分
           MOVE    WRK-KIHONKBN        TO  TOUKEI04-KIHONKBN
      *    時間外区分
           MOVE    WRK-JIKANKBN        TO  TOUKEI04-TIMEKBN
      *    紹介区分
           MOVE    WRK-SYOKAIKBN       TO  TOUKEI04-SHOUKAIKBN
      *
           WRITE   TOUKEI04-REC
      *
           IF      STS-TOUKEI04        =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-TOUKEIERR
               STRING "日次統計データ 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-TOUKEI04            DELIMITED  BY  SIZE
                                               INTO    WRK-TOUKEIERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-TOUKEI04
           .
       2003-OUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       20023-SRYKA-HENSYU-SEC              SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDX-SRYKA-MAX
               IF      JYURRK-SRYKA    =   TBL-SRYKA(IDX2)
                   MOVE    TBL-RECESRYKA (IDX2)
                                           TO  TOUKEI04-RECEKA
                   MOVE    IDX-SRYKA-MAX   TO  IDX2
               END-IF
           END-PERFORM
           .
       20023-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20024-HKNCOMBI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    JYURRK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    JYURRK-PTID         TO  COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI        =   1
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *    主保険情報読み出し
           INITIALIZE                      PTHKNINF-REC
           INITIALIZE                      PTRSIINF-REC
           IF      COMB-HKNID      NOT =   ZERO
               IF      COMB-HKNNUM         =   WRK-CONS-ROUSAI-HKNNUM
                                           OR  WRK-CONS-JIBAI-HKNNUM
      *            労災・自賠責
                   INITIALIZE                  PTRSIINF-REC
                   MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
                   MOVE    COMB-HKNID          TO  PTRSI-HKNID
                   MOVE    COMB-PTID           TO  PTRSI-PTID
                   MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                   PERFORM 900-PTRSIINF-INV-SEC
               ELSE
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
                   MOVE    COMB-PTID           TO  PTHKN-PTID
                   MOVE    COMB-HKNID          TO  PTHKN-HKNID
                   MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                                   PTHKN-TEKEDYMD
                   MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                   PERFORM 910-PTHKNINF-INV-SEC
               END-IF
           END-IF
      *    保険者名
           MOVE    PTHKN-HKNJANUM      TO  TOUKEI04-HKNJANUM
      *    主保険
           MOVE    COMB-HKNNUM         TO  TOUKEI04-HKNNUM
           MOVE    COMB-HKNID          TO  TOUKEI04-HKNID
           MOVE    COMB-HONKZKKBN      TO  TOUKEI04-HONKZKKBN
           MOVE    COMB-HOJOKBN        TO  TOUKEI04-HOJOKBN
           MOVE    COMB-CONTKBN        TO  TOUKEI04-CONTKBN
      *    公費１
           MOVE    COMB-KOH1ID         TO  TOUKEI04-KOH1ID
           MOVE    COMB-KOH1HKNNUM     TO  TOUKEI04-KOH1HKNNUM
           MOVE    COMB-KOH1PAYKBN     TO  TOUKEI04-KOH1PAYKBN
      *    公費２
           MOVE    COMB-KOH2ID         TO  TOUKEI04-KOH2ID
           MOVE    COMB-KOH2HKNNUM     TO  TOUKEI04-KOH2HKNNUM
           MOVE    COMB-KOH2PAYKBN     TO  TOUKEI04-KOH2PAYKBN
      *    公費３
           MOVE    COMB-KOH3ID         TO  TOUKEI04-KOH3ID
           MOVE    COMB-KOH3HKNNUM     TO  TOUKEI04-KOH3HKNNUM
           MOVE    COMB-KOH3PAYKBN     TO  TOUKEI04-KOH3PAYKBN
      *    公費４
           MOVE    COMB-KOH4ID         TO  TOUKEI04-KOH4ID
           MOVE    COMB-KOH4HKNNUM     TO  TOUKEI04-KOH4HKNNUM
           MOVE    COMB-KOH4PAYKBN     TO  TOUKEI04-KOH4PAYKBN
      *    労災・自賠責
           IF      COMB-HKNNUM         =   WRK-CONS-ROUSAI-HKNNUM
                                       OR  WRK-CONS-JIBAI-HKNNUM
      *        労災−労災保険区分
               MOVE    PTRSI-HKNKBN        TO  TOUKEI04-RSIHKNKBN
      *        労災−自賠責請求区分
               MOVE   PTRSI-JIBAISEIKBN    TO  TOUKEI04-JIBAISEIKBN
           END-IF
      *    レセプト種別
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 20021-RECESYUBETU-SET-SEC
           END-IF
           .
       20024-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       20026-SYUNOU-HENSYU-SEC              SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  SYU-PTID
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_syunou"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    伝票状態区分
           MOVE    SYU-DENPJTIKBN      TO  TOUKEI04-DENPJTIKBN
      *    伝票作成区分
           MOVE    SYU-CREATEKBN       TO  TOUKEI04-CREATEKBN
      *    保険点数
           MOVE    SYU-TOTAL-HKNTEN    TO  TOUKEI04-HKNTEN
      *    保険負担金額
           COMPUTE TOUKEI04-HKNMONEY   =   SYU-TOTAL-MONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
      *    保険適用外金額
           COMPUTE TOUKEI04-TGMONEY    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       -   SYU-TGMONEY-TAX-SAI
      *    保険適用外金額（消費税あり）
           MOVE    SYU-TGMONEY-TAX-SAI
                                       TO  TOUKEI04-TGMONEY-TAX
      *    自費金額 
           COMPUTE TOUKEI04-JIHI       =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *    自費金額消費税
           MOVE    SYU-JIHI-TAX        TO  TOUKEI04-JIHI-TAX
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  TOUKEI04-SKYMONEY
      *    労災／自賠責がある場合の請求額
           IF      SYU-RSIJIBAI-SKYMONEY   NOT =  ZERO
               MOVE   SYU-RSIJIBAI-SKYMONEY  TO  TOUKEI04-SKYMONEY
           END-IF
           .
       20026-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    労災・自賠責のとき
           IF      TOUKEI04-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM
               EVALUATE    TOUKEI04-RSIHKNKBN
                                       ALSO    PTRSI-SAIGAIKBN
                                           ALSO    WRK-PARA-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"   
                       MOVE    7081        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"   
                       MOVE    7071        TO    TOUKEI04-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    TOUKEI04-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    TOUKEI04-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    TOUKEI04-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7001        TO    TOUKEI04-RECESYUBETU
      *            公務災害
                   WHEN    "5"         ALSO    ANY     ALSO    "1"
                       MOVE    7061        TO    TOUKEI04-RECESYUBETU
                   WHEN    "5"         ALSO    ANY     ALSO    "2"
                       MOVE    7051        TO    TOUKEI04-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    自費のとき
           IF     TOUKEI04-HKNNUM(1:2) =   "98"
               MOVE    8888                TO  TOUKEI04-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    治験のとき
           IF     WRK-PARA-SRYYMD      >=  "20080401"
               IF    (TOUKEI04-HKNNUM  >=   "900")   AND
                     (TOUKEI04-HKNNUM  <=   "919")
                   MOVE    8888                TO  TOUKEI04-RECESYUBETU
                   GO  TO  20021-RECESYUBETU-SET-EXT
               END-IF
           END-IF
      *
      *    公害保険（入院）のとき
           IF     TOUKEI04-HKNNUM     =    WRK-CONS-KOUGAI-HKNNUM
               MOVE    7200                TO  TOUKEI04-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    TOUKEI04-PTID       TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYMD     TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE    AGECHK-NENREI       TO  WRK-AGE
               MOVE    AGECHK-SYUGAKUKBN   TO  WRK-SYUGAKUKBN
           ELSE
               MOVE    ZERO                TO  WRK-AGE
                                               WRK-SYUGAKUKBN
           END-IF
      *
      *    後期高齢非該当の有無設定
           MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
           IF    ( WRK-AGE               >=  70                    )
           AND   ( WRK-PARA-SRYYMD (1:6) >=  WRK-CONS-SRYYM-200804 )
               IF    ( COMB-HKNNUM       =   SPACE   OR  "040"   
                                           OR  "039"   OR  "067"
                                           OR  "069"             )
               OR    ( COMB-HKNNUM (1:2)   =   "98"    OR  "90"
                                           OR  "91"              )
                   CONTINUE
               ELSE
                   IF   (  COMB-HKNNUM         =   "060"   OR  "068" )
                   OR   (( COMB-HKNNUM     NOT =   "060"   AND "068" )
                           AND
                         ( COMB-HOJOKBN        =   SPACE             ))
                       MOVE    WRK-PARA-SRYYMD (1:6)   TO  WRK-TEKSTYYMM
                       IF      COMB-TEKSTYMD (1:6)  >=
                                               WRK-PARA-SRYYMD (1:6)
                            MOVE  COMB-TEKSTYMD (7:2)  TO  WRK-TEKSTDD
                       ELSE
                            MOVE  01                   TO  WRK-TEKSTDD
                       END-IF
      *
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
                       MOVE    COMB-PTID           TO  PTKOH-PTID
                       MOVE    "976"               TO  PTKOH-KOHNUM
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-PTKOHINF-READ-N-SEC
                           PERFORM UNTIL   FLG-PTKOHINF    =   1
                               IF    (PTKOH-TEKSTYMD   <=  WRK-TEKSTYMD)
                               AND   (WRK-TEKSTYMD     <=
                                                       PTKOH-TEKEDYMD  )
                                   MOVE    1           TO  WRK-KOUKI-FLG
                                   MOVE    1           TO  FLG-PTKOHINF
                               ELSE
                                   MOVE    "tbl_ptkohinf"
                                                    TO  MCP-TABLE
                                   MOVE    "key4"   TO  MCP-PATHNAME
                                   PERFORM  900-PTKOHINF-READ-N-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM  900-CLOSE-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      TOUKEI04-HKNNUM     NOT =   SPACE
               EVALUATE    TOUKEI04-HKNNUM
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF      TOUKEI04-HKNNUM =   "060"   OR   "068"
                           MOVE   "1"      TO  WK-KOKUHO
                       ELSE
                           MOVE   "2"      TO  WK-KOKUHO
                       END-IF
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           INITIALIZE                  WRK-HKNNUM-AREA
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   4  )  OR
                              (COMB-KOHID(IDX)   =   ZERO)
               MOVE    COMB-KOHHKNNUM(IDX)     TO  WRK-HKNNUM
               MOVE    COMB-KOHPAYKBN(IDX)     TO  WRK-PAYKBN
               PERFORM 200211-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM          TO  WRK-HKN-HBTNUM(IDX)
               MOVE    HKN-RECESKYKBN      TO  WRK-HKN-RECESKYKBN(IDX)
               MOVE    HKN-HKNNUM          TO  WRK-HKN-HKNNUM(IDX)
           END-PERFORM
      *
           IF      WRK-PARA-SRYYMD(1:6)    <      "200210" 
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE        "1"             TO  WK-RJN
               END-IF
           END-IF
      *
           MOVE    TOUKEI04-HONKZKKBN  TO  WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
      *        公費数
               IF     (WRK-HKN-HKNNUM (IDX)   NOT =   SPACE) AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "027") AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "972") AND
                      (WRK-HKN-HKNNUM (IDX)   NOT =   "974")
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  TOUKEI04-HKNNUM     NOT =   SPACE
               IF      WRK-PARA-SRYYMD(1:6)    >=  "200210"
      *            ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
                   IF      WRK-AGE             <     3
                       MOVE    "3"             TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *        平成２０年４月改正対応
               IF      WRK-PARA-SRYYMD(1:6)    >=  WRK-CONS-SRYYM-200804
                   IF      WRK-SYUGAKUKBN      =   1
                       MOVE   "3"              TO  WK-HONKZKKBN
                   END-IF
               END-IF
      * 
               IF      WRK-PARA-SRYYMD(1:6)    >=  WRK-CONS-SRYYM-201404
                   IF    ( COMB-HKNNUM     =   "002" )
                   AND  ( WRK-AGE          >=  75    )
                       EVALUATE    COMB-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                           WHEN    "D"
                               MOVE    "1"     TO  COMB-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                           WHEN    "E"
                               MOVE    "2"     TO  COMB-HOJOKBN
                           WHEN    "C"
                           WHEN    "I"
                           WHEN    "F"
                               MOVE    "3"     TO  COMB-HOJOKBN
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *********************IF  (WRK-AGE  >=   70)  AND
      *********************    (WRK-AGE  <=   74)
                           IF  (WRK-AGE  >=   70)
                           AND (WRK-KOUKI-FLG
                                         =    0 )
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                   WHEN   "6"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-AGE       >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
      *
                   IF      COMB-KOH1HKNNUM      =    "027"
                       MOVE   "?"               TO   WK-RJN
                   END-IF
               END-IF
      * 
               IF    ( WRK-PARA-SRYYMD(1:6)  >= WRK-CONS-SRYYM-200804 )
               AND   ( WRK-PARA-SRYYMD(1:6)  <= WRK-CONS-SRYYM-201404 )
                   IF    ( COMB-HKNNUM     =   "002" )
                   AND  ( WRK-AGE          >=  75    )
                       EVALUATE    COMB-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                               MOVE    "1"     TO  COMB-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                               MOVE    "2"     TO  COMB-HOJOKBN
                           WHEN    "C"
                           WHEN    "I"
                               MOVE    "3"     TO  COMB-HOJOKBN
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *********************IF  (WRK-AGE  >=   70)  AND
      *********************    (WRK-AGE  <=   74)
                           IF  (WRK-AGE  >=   70)
                           AND (WRK-KOUKI-FLG
                                         =    0 )
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                   WHEN   "6"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-AGE       >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
      *
                   IF      COMB-KOH1HKNNUM      =    "027"
                       MOVE   "?"               TO   WK-RJN
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYMD(1:6)  >= "200610"              )
               AND   ( WRK-PARA-SRYYMD(1:6)  <  WRK-CONS-SRYYM-200804 )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE  >=   70)  AND
                               (WRK-AGE  <=   74)
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COMB-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COMB-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COMB-KOH1HKNNUM      =    "027"
                       EVALUATE   COMB-KOH1PAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYMD(1:6)
                                           >=  "200210" )
               AND   ( WRK-PARA-SRYYMD(1:6)
                                           <=  "200609" )
      *
      *            １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *            "９"または"８"をセット
      *            老人保険の場合はＷＫ−ＲＪＮにセットする	
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (WRK-AGE  >=   70)  AND
                               (WRK-AGE  <=   74)
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COMB-KOH1HKNNUM      =    "027"
                       EVALUATE   COMB-KOH1PAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( TOUKEI04-HONKZKKBN  =   "1"   )
               IF      TOUKEI04-HKNNUM =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    TOUKEI04-NYUGAIKBN  TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-PARA-SRYYMD(1:6)
                                       TO  LNK-SYUBETUGET-SRYYM
           MOVE    TOUKEI04-HKNNUM     TO  LNK-SYUBETUGET-HKNNUM
           MOVE    TOUKEI04-BIRTHDAY   TO  LNK-SYUBETUGET-BIRTHDAY
      *
      *    国保のとき９７７の公費の有無
           IF    ( LNK-SYUBETUGET-BIRTHDAY
                                       <   19440402 )
           AND   ( LNK-SYUBETUGET-HKNNUM
                                       =   "060"    )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >   "201404" )
               MOVE    "99"                TO  LNK-SYUBETUGET-RJNPAYKBN
      *
               MOVE    WRK-PARA-SRYYMD (1:6)   TO  WRK-TEKSTYYMM
               IF      COMB-TEKSTYMD (1:6)     >=  WRK-PARA-SRYYMD (1:6)
                   MOVE  COMB-TEKSTYMD (7:2)      TO  WRK-TEKSTDD
               ELSE
                   MOVE  01                       TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
               MOVE    COMB-PTID           TO  PTKOH-PTID
               MOVE    "977"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
                   PERFORM UNTIL   FLG-PTKOHINF    =   1
                       IF    ( PTKOH-TEKSTYMD  <=  WRK-TEKSTYMD   )
                       AND   ( WRK-TEKSTYMD    <=  PTKOH-TEKEDYMD )
                           MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                           MOVE    1               TO  FLG-PTKOHINF
                       ELSE
                           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                           MOVE    "key4"          TO  MCP-PATHNAME
                           PERFORM  900-PTKOHINF-READ-N-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
           END-IF
      *
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                        TO  TOUKEI04-RECESYUBETU
      *
      *    DISPLAY "TOUKEI04-RECESYUBETU=" TOUKEI04-RECESYUBETU
           .
       20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ処理
      *****************************************************************
       200211-HKNNUM-SET-SEC                SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    COMB-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN   
           MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_hknnum"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNNUM
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
                   INITIALIZE                      HKNNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200211-HKNNUM-SET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBT110 "         DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI04-FILE
      *
           IF      CNT-TOUKEI04           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI04  CNT-TOUKEI04:" CNT-TOUKEI04
           DISPLAY "*** ORCBT110 END ***"
           .
       300-TERM-EXT.
           EXIT.
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
      *    受診履歴マスタＲＥＡＤ
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-PTID         TO  KEY-N-PTID
               MOVE    JYURRK-DENPNUM      TO  KEY-N-DENPNUM
           END-IF
           .
       900-JYURRK-READ-EXT.
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
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
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
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスタ検索
      *****************************************************************
       900-ADRS-INV-SEC            SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               INITIALIZE                  ADRS-REC
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込処理
      *****************************************************************
       980-SYUNOU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1                   TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE                 PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3 "            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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

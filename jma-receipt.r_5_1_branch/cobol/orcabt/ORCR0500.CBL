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
       PROGRAM-ID.             ORCR0500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 労災レセプト帳票ＤＢの印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.00.02    NACL-藤原    02/09/27  帳票ＩＤをＤＢより編集
      *                                     パラメタに入外区分の追加
      *  01.00.03    NACL-藤原    02/10/21  印刷用ファイルにサイト区分を追加
      *  01.00.04    NACL-藤原    03/01/10  印刷順通し番号を追加
      *  01.00.05    NACL-門脇    03/01/31  事業所名の長さ変更のため
      *                                     帳票ＩＤ追加
      *  01.00.06    NACL-門脇    03/02/12  医療機関名の長さ変更のため
      *                                     帳票ＩＤ追加
      *  01.00.07    NACL-藤原    03/02/14  頁数のジョブ管理ＤＢへの更新
      *  01.00.08    NACL-藤原    03/02/24  プリンタ名をオンラインより渡す
      *  01.00.09    NACL-藤原    03/03/03  再印刷機能のためにパラメタを追加
      *
      *  労災・自賠責入院機能追加
      *  01.01.01    NACL-藤原    03/07/20  労災・自賠責の入院帳票ＩＤ追加
      *  01.01.02    NACL-門脇    04/01/26  カナ氏名の長さ等修正
      *                                     HCM22V02.REDを追加
      *                                     HCM24V02.REDを追加
      *                                     HCM21V02.REDを追加
      *                                     HCM18V02.REDを追加
      *                                     HCM20V02.REDを追加
      *                                     HCM23V02.REDを追加
      *                                     HCM29V01.REDを追加
      *                                     HCN22V01.REDを追加
      *                                     HCN21V01.REDを追加
      *                                     HCN23V01.REDを追加
      *
      *  ２００４年４月改正
      *  01.02.01    NACL-藤原    04/03/29  請求点の桁数の変更
      *                                     HCN21V02.red
      *                                     HCN22V02.red
      *                                     HCN23V02.red
      *  01.02.02    NACL-藤原    04/09/13  医療機関名称・所在地の長さを
      *                                     変更
      *
      *  中止処理追加
      *  01.05.01    NACL-藤原    04/12/01  印刷中止処理の追加
      *
      *  02.06.01    NACL-藤原    05/06/22 自賠責レセプトの様式追加
      *                                     HCM25.red
      *                                     HCM26.red
      *                                     HCN25.red
      *  02.06.02    NACL-藤原    05/09/05  複数枚レセの連番記載
      *                                     HCM18V04.red
      *                                     HCM20V04.red
      *                                     HCM21V04.red
      *                                     HCM22V04.red
      *                                     HCM24V04.red
      *                                     HCM29V02.red
      *                                     HCN22V04.red
      *                                     HCN21V04.red
      *  03.00.01    NACL-太田    06/06/19  PRT-PRTDATAの項目長変更
      *                                     (10000->20000)
      *
      *  03.02.01    NACL-藤原    06/07/11  労災・自賠責のプリンタ指定対応
      *                                     （入院外・入院別）
      *  03.02.02    NACL-藤原    06/07/24  自賠責の枠印刷選択機能対応
      *                                     HCM48.red
      *                                     HCN48.red
      *                                     HCM49.red
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *                                     主科用出力順識別番号追加
      *
      *  03.03.01    NACL-藤原    06/10/13  ２００６年１０月改正対応
      *                                     労災（入院）
      *                                     HCN50.red
      *                                     HCN51.red
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/06/26  自賠責平成１９年４月改正様式対応
      *                                     HCM53.red  HCN53.red
      *                                     HCM54.red  HCN54.red
      *                                     HCM55.red  HCN55.red
      *
      *  04.01.01    NACL-藤原    07/10/25  公務災害対応
      *                                     HCM56.red  HCM57.red
      *  04.01.02    NACL-藤原    07/11/22  公害レセプト対応
      *                                     HCM58.red  HCM59.red
      *                                     HCN58.red
      *  04.02.01    NACL-門脇    08/03/10  自賠責平成１９年４月改正様式
      *                                     （従来様式：入院外）
      *                                     電話番号桁数修正
      *                                     HCM55V01.red
      *  04.02.02    NACL-藤原    08/05/09  エラー時終了処理の追加
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.02    NACL-藤原    08/08/07  労災の枠印刷選択機能対応
      *
      *  04.05.00    NACL-太田    10/01/28  拡張漢字対応
      *  04.05.01    NACL-藤原    11/08/02  点検レセ印字位置変更対応
      *  04.05.02    NACL-藤原    11/08/10  労災新様式対応
      *  04.05.03    NACL-藤原    11/09/02  起動方法の変更
      *  04.05.04    NACL-藤原    12/04/11  平成２４年４月改正対応
      *                                     入院略称の変更
      *  04.05.05    NACL-門脇    13/02/07  労災・自賠責（入院）
      *                                     金額項目７桁対応
      *
      *  04.06.01    NACL-藤原    13/04/12  帳票タイトル設定の条件追加
      *
      *  04.07.01    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.02    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-太田    16/02/02  レセプトプレビュー不具合対応
      *  04.08.03    NACL-藤原    16/11/01  リアルタイムプレビュー時の修正
      *  04.08.04    NACL-太田    16/11/30  PRT-RECをコピー句に変更
      *
      *  05.00.01    NACL-藤原    17/02/07  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    17/11/29  クライアント印刷対応
      *  05.00.03    NACL-藤原    19/03/25  元号改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    印刷用データ 名称領域 
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
           COPY    "CPCOMMONPRT.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECEPRT         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(04).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(04).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
      *
      *    通し番号
           03  WRK-RENNUM              PIC 9(05).
      *
      *    通し番号編集エリア
           03  WRK-HEN-RENNUM-AREA.
               05  WRK-HEN-RENNUM1     PIC ZZZZZZ9.
               05  WRK-HEN-RENNUM-R    REDEFINES   WRK-HEN-RENNUM1.   
                   07  FILLER          PIC X(01).
                   07  WRK-HEN-FL1     PIC X(01).
                   07  WRK-HEN-RENNUM2 PIC ZZZ9.
                   07  WRK-HEN-FL2     PIC X(01).
      *
      *    一時ディレクトリ名
           03  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
           03  WRK-PARA.
               05  WRK-PARA-JOBID                PIC 9(07).
               05  WRK-PARA-SHELLID              PIC X(08).
               05  WRK-PARA1.  
                   07  WRK-PARA-SRYYM            PIC X(06).
                   07  WRK-PARA-TERMID           PIC X(16).
                   07  WRK-PARA-SYSYMD           PIC X(08).
               05  WRK-PARA-RECE09-KEY. 
                   07  WRK-PARA-RECE09-PRTID     PIC X(04).
                   07  WRK-PARA-RECE09-SRYYM     PIC X(06).
                   07  WRK-PARA-RECE09-CREYMD    PIC X(08).
                   07  WRK-PARA-RECE09-CREHMS    PIC X(06).
                   07  WRK-PARA-RECE09-PREFKBN   PIC X(01).
                   07  WRK-PARA-RECE09-TEISYUTUSAKI
                                                 PIC X(01).
                   07  WRK-PARA-RECE09-RECESYUBETU
                                                 PIC X(04).
                   07  WRK-PARA-RECE09-NYUGAIKBN PIC X(01).                              
      *********05  WRK-PARA-TERMID64             PIC X(64).
               05  WRK-PARA-SYOKBN               PIC X(01).
               05  WRK-PARA-PRTNM                PIC X(16).
               05  WRK-PARA-PRTKBN               PIC X(01). 
               05  WRK-PARA-RECEKBN              PIC X(01). 
               05  WRK-PARA-HOSPNUM              PIC 9(02).
               05  WRK-PARA-JOB-PARAM            PIC X(10).
      *
           COPY    "CPORCSPRTLNK.INC".
      *
      *    労災レセプト
           COPY    "HCM21.INC".
           COPY    "HCM21V01.INC".
           COPY    "HCM21V02.INC".
           COPY    "HCM21V03.INC".
           COPY    "HCM21V04.INC".
           COPY    "HCM21V05.INC".
           COPY    "HCM21V06.INC".
           COPY    "HCM22.INC".
           COPY    "HCM22V01.INC".
           COPY    "HCM22V02.INC".
           COPY    "HCM22V03.INC".
           COPY    "HCM22V04.INC".
           COPY    "HCM22V05.INC".
           COPY    "HCM22V06.INC".
           COPY    "HCM24.INC".
           COPY    "HCM24V01.INC".
           COPY    "HCM24V02.INC".
           COPY    "HCM24V03.INC".
           COPY    "HCM24V04.INC".
           COPY    "HCM24V05.INC".
           COPY    "HCM24V06.INC".
           COPY    "HCM18.INC".
           COPY    "HCM18V01.INC".
           COPY    "HCM18V02.INC".
           COPY    "HCM18V03.INC".
           COPY    "HCM18V04.INC".
           COPY    "HCM18V05.INC".
           COPY    "HCM18V06.INC".
           COPY    "HCM20.INC".
           COPY    "HCM20V01.INC".
           COPY    "HCM20V02.INC".
           COPY    "HCM20V03.INC".
           COPY    "HCM20V04.INC".
           COPY    "HCM20V05.INC".
           COPY    "HCM20V06.INC".
           COPY    "HCM56.INC".
           COPY    "HCM56V01.INC".
           COPY    "HCM56V02.INC".
           COPY    "HCM56V03.INC".
           COPY    "HCM57.INC".
           COPY    "HCM57V01.INC".
           COPY    "HCM57V02.INC".
           COPY    "HCM69.INC".
           COPY    "HCM70.INC".
           COPY    "HCM71.INC".
           COPY    "HCM83.INC".
           COPY    "HCM84.INC".
      *
           COPY    "HCN21.INC".
           COPY    "HCN21V01.INC".
           COPY    "HCN21V02.INC".
           COPY    "HCN21V03.INC".
           COPY    "HCN21V04.INC".
           COPY    "HCN22.INC".
           COPY    "HCN22V01.INC".
           COPY    "HCN22V02.INC".
           COPY    "HCN22V03.INC".
           COPY    "HCN22V04.INC".
           COPY    "HCN50.INC".
           COPY    "HCN50V01.INC".
           COPY    "HCN50V02.INC".
           COPY    "HCN50V03.INC".
           COPY    "HCN50V04.INC".
           COPY    "HCN51.INC".
           COPY    "HCN51V01.INC".
           COPY    "HCN51V02.INC".
           COPY    "HCN51V03.INC".
           COPY    "HCN51V04.INC".
           COPY    "HCN66.INC".
           COPY    "HCN66V01.INC".
           COPY    "HCN67.INC".
           COPY    "HCN67V01.INC".
           COPY    "HCN72.INC".
           COPY    "HCN73.INC".
      *
      *    自賠責レセプト
           COPY    "HCM23.INC".
           COPY    "HCM23V01.INC".
           COPY    "HCM23V02.INC".
           COPY    "HCM23V03.INC".
           COPY    "HCM23V04.INC".
           COPY    "HCM23V05.INC".
           COPY    "HCM23V06.INC".
           COPY    "HCM29.INC".
           COPY    "HCM29V01.INC".
           COPY    "HCM29V02.INC".
           COPY    "HCM29V03.INC".
           COPY    "HCM29V04.INC".
           COPY    "HCM25.INC".
           COPY    "HCM25V01.INC".
           COPY    "HCM25V02.INC".
           COPY    "HCM26.INC".
           COPY    "HCM26V01.INC".
           COPY    "HCM26V02.INC".
           COPY    "HCM48.INC".
           COPY    "HCM48V01.INC".
           COPY    "HCM48V02.INC".
           COPY    "HCM49.INC".
           COPY    "HCM49V01.INC".
           COPY    "HCM49V02.INC".
           COPY    "HCM53.INC".
           COPY    "HCM53V01.INC".
           COPY    "HCM53V02.INC".
      *    COPY    "HCM54.INC".
           COPY    "HCM55.INC".
           COPY    "HCM55V01.INC".
           COPY    "HCM55V02.INC".
           COPY    "HCM55V03.INC".
           COPY    "HCM76.INC".
           COPY    "HCM77.INC".
           COPY    "HCM87.INC".
           COPY    "HCM88.INC".
      *
           COPY    "HCN23.INC".
           COPY    "HCN23V01.INC".
           COPY    "HCN23V02.INC".
           COPY    "HCN23V03.INC".
           COPY    "HCN23V04.INC".
           COPY    "HCN23V05.INC".
           COPY    "HCN23V06.INC".
           COPY    "HCN23V07.INC".
           COPY    "HCN23V08.INC".
           COPY    "HCN25.INC".
           COPY    "HCN25V01.INC".
           COPY    "HCN25V02.INC".
           COPY    "HCN25V03.INC".
           COPY    "HCN48.INC".
           COPY    "HCN48V01.INC".
           COPY    "HCN48V02.INC".
           COPY    "HCN48V03.INC".
           COPY    "HCN48V04.INC".
           COPY    "HCN53.INC".
           COPY    "HCN53V01.INC".
           COPY    "HCN53V02.INC".
           COPY    "HCN53V03.INC".
           COPY    "HCN53V04.INC".
      *    COPY    "HCN54.INC".
           COPY    "HCN55.INC".
           COPY    "HCN55V01.INC".
           COPY    "HCN55V02.INC".
           COPY    "HCN55V03.INC".
           COPY    "HCN70.INC".
           COPY    "HCN74.INC".
           COPY    "HCN75.INC".
      *
      *    公害レセプト
           COPY    "HCM58.INC".
           COPY    "HCM59.INC".
           COPY    "HCN58.INC".
           COPY    "HCM58V01.INC".
           COPY    "HCM59V01.INC".
           COPY    "HCN58V01.INC".
           COPY    "HCM58V02.INC".
           COPY    "HCM59V02.INC".
           COPY    "HCN58V02.INC".
           COPY    "HCN58V03.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *     
      *    レセプト印刷情報
           COPY    "CPSK2005.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    オフセット情報
           COPY    "CPSK1044.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  WRK-PARA-JOBID
                                             WRK-PARA-SHELLID
                                             WRK-PARA1
                                             WRK-PARA-RECE09-KEY
      ***************************************WRK-PARA-TERMID64
                                             WRK-PARA-SYOKBN
      ***************************************WRK-PARA-PRTNM
                                             LNK-PRTKANRI-RENNUM
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
                                             WRK-PARA-PRTKBN
                                             WRK-PARA-RECEKBN
                                             WRK-PARA-HOSPNUM
                                             RECEERR
                                             WRK-PARA-JOB-PARAM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-JOB-PARAM=" WRK-PARA-JOB-PARAM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    SGETTEMP-TEMPDIR    TO  WRK-SGETTEMP-TEMPDIR
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
           MOVE    MCP-USER            TO  SPA-OPID
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
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING        TO  WRK-ENCODING9
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0500"      TO  JOB-PGID
               MOVE    "レセプト印刷処理"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
              PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF      FLG-END             =   ZERO
      *        レセプト通し番号情報処理
               PERFORM 110-SYSKANRI-HENSYU-SEC
      *
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  HC01PARA-HOSPNUM
               MOVE    "HC21"              TO  HC01PARA-FORM-ID
               MOVE    LNK-PRTKANRI-TERMID TO  HC01PARA-TERMID
               MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
               MOVE    1                   TO  HC01PARA-CNT
      *
               INITIALIZE                      SGETTEMP-AREA
               MOVE    HC01PARA            TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
               MOVE   SGETTEMP-ST          TO  ASGNPARA-ST
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト通し番号情報処理
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC                   SECTION.
      *
      *    レセプト印刷情報編集
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               INITIALIZE                  SYS-2005-REC
           END-IF
      *
      *    患者番号構成管理情報編集
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD (7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
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
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-END             =   1
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    通し番号初期値設定
           MOVE    ZERO                TO  WRK-RENNUM 
      *
           MOVE    SPACE               TO  WRK-HEN-RENNUM-AREA
           IF      WRK-PARA-SYOKBN     =   "2"
               MOVE    "["                 TO  WRK-HEN-FL1
               MOVE    "]"                 TO  WRK-HEN-FL2
           END-IF              
      *
      *    帳票印刷処理
      *
           OPEN    OUTPUT              PRT-FILE
      *
           IF      STS-PRT             =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル ＯＰＥＮエラー  STS="
                                               DELIMITED  BY  SIZE
                       STS-PRT                 DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECEPRT-SELECT-SEC
      *
           PERFORM 2001-RECEPRT-PRINT-SEC
                                       UNTIL   FLG-RECEPRT  =   1
      *
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECEPRT-CLS-SEC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票クリア処理
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECEPRT-SELECT-SEC
      *
           PERFORM 2002-RECEPRT-CLEAR-SEC
                                       UNTIL   FLG-RECEPRT  =   1
      *
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 900-RECEPRT-CLS-SEC
      *
      *    処理中止設定処理
           IF      FLG-END             =   1
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-END             =   1
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *   印刷用スクリプト処理
      *
           IF      CNT-OUT             >   ZERO 
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG1
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    "1"         TO  SHELLTBL-ARG4
               ELSE
                   IF      WRK-PARA-JOB-PARAM  =   "orca"
                       IF    ( WRK-PARA-PRTKBN     =   "2"    )
                       AND   ( SPA-CLIENT-PRT-FLG  =   2      )
                           MOVE    "診療報酬明細書"
                                           TO  SHELLTBL-ARG5
                           MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG6
                       END-IF
                   END-IF
               END-IF
               MOVE    SHELLTBL        TO  MCPDATA-REC
               IF      WRK-PARA-JOB-PARAM
                                       =   "orca"
                   MOVE    "SHELL"         TO  MCP-FUNC
               ELSE
                   MOVE    "EXCOMMAND"     TO  MCP-FUNC
               END-IF
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
      *        最新以外の帳票管理ＤＢを削除
               MOVE    SPACE           TO  PRTKANRI-REC
               INITIALIZE                  PRTKANRI-REC
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  PRTKANRI-PRIORITY
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTKANRI-TBL-GROUP
grpsys         MOVE    SPA-HOSPNUM     TO  PRTKANRI-HOSPNUM
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    "RECEPTX"       TO  PRTKANRI-TBL-KEY
               ELSE
                   MOVE    "RECEPT2"       TO  PRTKANRI-TBL-KEY
               END-IF
               MOVE    PRTKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    "tbl_prtkanri"  TO  MCP-TABLE
                   MOVE    "del12"         TO  MCP-PATHNAME
               ELSE
                   MOVE    "tbl_prtkanri"  TO  MCP-TABLE
                   MOVE    "del17"         TO  MCP-PATHNAME
               END-IF
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   MOVE    MCP-RC          TO  WRK-MCP-RC
                   STRING  "帳票管理ＤＢ削除エラー MCP-RC="
                                                   DELIMITED  BY  SIZE
                           WRK-MCP-RC              DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
      *        最新以外の帳票ＤＢを削除
               MOVE    SPACE           TO  PRTDATA-REC
               INITIALIZE                  PRTDATA-REC
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  PRTDATA-PRIORITY
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTDATA-TBL-GROUP
grpsys         MOVE    SPA-HOSPNUM     TO  PRTDATA-HOSPNUM
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    "RECEPTX"       TO  PRTDATA-TBL-KEY
               ELSE
                   MOVE    "RECEPT2"       TO  PRTDATA-TBL-KEY
               END-IF
               MOVE    PRTDATA-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    "tbl_prtdata"   TO  MCP-TABLE
                   MOVE    "del12"         TO  MCP-PATHNAME
               ELSE
                   MOVE    "tbl_prtdata"   TO  MCP-TABLE
                   MOVE    "del17"         TO  MCP-PATHNAME
               END-IF
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   MOVE    MCP-RC          TO  WRK-MCP-RC
                   STRING  "帳票ＤＢ削除エラー MCP-RC="
                                                   DELIMITED  BY  SIZE
                           WRK-MCP-RC              DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-IF                                                
      *
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-RECEPRT-PRINT-SEC          SECTION.
      *
      *    リアルタイムプレビュー時の自賠責のときの連番の初期化
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               IF      WRK-PARA-RECEKBN    =   "3" OR  "6" 
                   CONTINUE
               ELSE
                   IF   ( RECE08-RECENUM   =   110 )
                   AND  ( RECE08-RENNUM    =   1   )
                       MOVE    ZERO            TO  WRK-RENNUM
                   END-IF
               END-IF     
           END-IF
      * 
           MOVE    SPACE               TO  PRT-REC 
           MOVE    RECE08-FORMID       TO  PRT-PRTID
           MOVE    RECE08-PRTNM        TO  PRT-PRTNM
      *
      *    オフセット
           PERFORM 2002-OFFSET-GET-SEC
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
      *
           MOVE    "1"                 TO  PRT-SITEKBN
      *    ＨＡＯＲＩのとき「７」を設定
           IF      WRK-PARA-JOB-PARAM  =   "orca"
               IF    ( WRK-PARA-PRTKBN     =   "2" )
               AND   ( SPA-CLIENT-PRT-FLG  =   2   )
                   MOVE    "5"                 TO  PRT-PRTKBN
               ELSE
                   MOVE    WRK-PARA-PRTKBN     TO  PRT-PRTKBN
               END-IF
           ELSE
               MOVE    "7"                 TO  PRT-PRTKBN
           END-IF
      **** MOVE    "/var/lib/jma-receipt/orca_ps/"
           MOVE    WRK-SGETTEMP-TEMPDIR
                                       TO  PRT-OUT
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRT-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  PRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRT-TBL-GROUP
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  PRT-SHORI-RENNUM
           ELSE
               IF      WRK-PARA-RECEKBN    =   "3" OR  "6"
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                               TO  PRT-SHORI-RENNUM
               ELSE
                   IF     RECE08-RECENUM   <   100
                       MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                              TO  PRT-SHORI-RENNUM
                   ELSE
                       IF     RECE08-RECENUM   <   200
                           COMPUTE PRT-SHORI-RENNUM
                               =   LNK-PRTKANRI-SHORI-RENNUM   +   1
                       ELSE
                           COMPUTE PRT-SHORI-RENNUM
                               =   LNK-PRTKANRI-SHORI-RENNUM   +   2
                       END-IF
                   END-IF
               END-IF     
           END-IF
           ADD     1                   TO  WRK-RENNUM
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-RENNUM          TO  PRT-PAGE
           ELSE
               IF      WRK-PARA-RECEKBN    =   "3" OR  "6"
                   MOVE    WRK-RENNUM          TO  PRT-PAGE
               ELSE
                   MOVE    RECE08-RENNUM       TO  PRT-PAGE
               END-IF
           END-IF
      **
      *??     MOVE    LNK-PRTKANRI-SHORI-RENNUM
      *??                                 TO  PRT-SHORI-RENNUM
           MOVE    WRK-RENNUM          TO  PRT-PAGE
      **
      *
      *    通し番号印刷のときのみ編集
           IF      SYS-2005-RENNUMHENKBN   =   "1"
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    WRK-RENNUM          TO  WRK-HEN-RENNUM1
               ELSE
                   MOVE    WRK-RENNUM          TO  WRK-HEN-RENNUM2
               END-IF        
      *         
               EVALUATE    RECE08-FORMID
                   WHEN    "HCM21.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21-RENNUM
                       MOVE    HCM21               TO  RECE08-PRTDATA
                   WHEN    "HCM21V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V01-RENNUM
                       MOVE    HCM21V01            TO  RECE08-PRTDATA
                   WHEN    "HCM21V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V02-RENNUM
                       MOVE    HCM21V02            TO  RECE08-PRTDATA
                   WHEN    "HCM21V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V03-RENNUM
                       MOVE    HCM21V03            TO  RECE08-PRTDATA
                   WHEN    "HCM21V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V04-RENNUM
                       MOVE    HCM21V04            TO  RECE08-PRTDATA
                   WHEN    "HCM21V05.red"                  
                   WHEN    "HCM21V05P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V05-RENNUM
                       MOVE    HCM21V05            TO  RECE08-PRTDATA
                   WHEN    "HCM21V06.red"                  
                   WHEN    "HCM21V06P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM21V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM21V06-RENNUM
                       MOVE    HCM21V06            TO  RECE08-PRTDATA
                   WHEN    "HCM22.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22-RENNUM
                       MOVE    HCM22               TO  RECE08-PRTDATA
                   WHEN    "HCM22V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V01-RENNUM
                       MOVE    HCM22V01            TO  RECE08-PRTDATA
                   WHEN    "HCM22V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V02-RENNUM
                       MOVE    HCM22V02            TO  RECE08-PRTDATA
                   WHEN    "HCM22V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V03-RENNUM
                       MOVE    HCM22V03            TO  RECE08-PRTDATA
                   WHEN    "HCM22V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V04-RENNUM
                       MOVE    HCM22V04            TO  RECE08-PRTDATA
                   WHEN    "HCM22V05.red"                  
                   WHEN    "HCM22V05P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V05-RENNUM
                       MOVE    HCM22V05            TO  RECE08-PRTDATA
                   WHEN    "HCM22V06.red"                  
                   WHEN    "HCM22V06P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM22V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM22V06-RENNUM
                       MOVE    HCM22V06            TO  RECE08-PRTDATA
                   WHEN    "HCM24.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24-RENNUM
                       MOVE    HCM24               TO  RECE08-PRTDATA
                   WHEN    "HCM24V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V01-RENNUM
                       MOVE    HCM24V01            TO  RECE08-PRTDATA
                   WHEN    "HCM24V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V02-RENNUM
                       MOVE    HCM24V02            TO  RECE08-PRTDATA
                   WHEN    "HCM24V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V03-RENNUM
                       MOVE    HCM24V03            TO  RECE08-PRTDATA
                   WHEN    "HCM24V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V04-RENNUM
                       MOVE    HCM24V04            TO  RECE08-PRTDATA
                   WHEN    "HCM24V05.red"                  
                   WHEN    "HCM24V05P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V05-RENNUM
                       MOVE    HCM24V05            TO  RECE08-PRTDATA
                   WHEN    "HCM24V06.red"                  
                   WHEN    "HCM24V06P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM24V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM24V06-RENNUM
                       MOVE    HCM24V06            TO  RECE08-PRTDATA
                   WHEN    "HCM18.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18-RENNUM
                       MOVE    HCM18               TO  RECE08-PRTDATA
                   WHEN    "HCM18V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V01-RENNUM
                       MOVE    HCM18V01            TO  RECE08-PRTDATA
                   WHEN    "HCM18V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V02-RENNUM
                       MOVE    HCM18V02            TO  RECE08-PRTDATA
                   WHEN    "HCM18V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V03-RENNUM
                       MOVE    HCM18V03            TO  RECE08-PRTDATA
                   WHEN    "HCM18V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V04-RENNUM
                       MOVE    HCM18V04            TO  RECE08-PRTDATA
                   WHEN    "HCM18V05.red"                  
                   WHEN    "HCM18V05P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V05-RENNUM
                       MOVE    HCM18V05            TO  RECE08-PRTDATA
                   WHEN    "HCM18V06.red"                  
                   WHEN    "HCM18V06P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM18V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM18V06-RENNUM
                       MOVE    HCM18V06            TO  RECE08-PRTDATA
                   WHEN    "HCM20.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20-RENNUM
                       MOVE    HCM20               TO  RECE08-PRTDATA
                   WHEN    "HCM20V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V01-RENNUM
                       MOVE    HCM20V01            TO  RECE08-PRTDATA
                   WHEN    "HCM20V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V02-RENNUM
                       MOVE    HCM20V02            TO  RECE08-PRTDATA
                   WHEN    "HCM20V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V03-RENNUM
                       MOVE    HCM20V03            TO  RECE08-PRTDATA
                   WHEN    "HCM20V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V04-RENNUM
                       MOVE    HCM20V04            TO  RECE08-PRTDATA
                   WHEN    "HCM20V05.red"                  
                   WHEN    "HCM20V05P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V05-RENNUM
                       MOVE    HCM20V05            TO  RECE08-PRTDATA
                   WHEN    "HCM20V06.red"                  
                   WHEN    "HCM20V06P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM20V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM20V06-RENNUM
                       MOVE    HCM20V06            TO  RECE08-PRTDATA
                   WHEN    "HCM23.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23-RENNUM
                       MOVE    HCM23               TO  RECE08-PRTDATA
                   WHEN    "HCM23V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V01-RENNUM
                       MOVE    HCM23V01            TO  RECE08-PRTDATA
                   WHEN    "HCM23V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V02-RENNUM
                       MOVE    HCM23V02            TO  RECE08-PRTDATA
                   WHEN    "HCM23V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V03-RENNUM
                       MOVE    HCM23V03            TO  RECE08-PRTDATA
                   WHEN    "HCM23V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V04-RENNUM
                       MOVE    HCM23V04            TO  RECE08-PRTDATA
                   WHEN    "HCM23V05.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V05-RENNUM
                       MOVE    HCM23V05            TO  RECE08-PRTDATA
                   WHEN    "HCM23V06.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM23V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM23V06-RENNUM
                       MOVE    HCM23V06            TO  RECE08-PRTDATA
                   WHEN    "HCM29.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM29
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM29-RENNUM
                       MOVE    HCM29               TO  RECE08-PRTDATA
                   WHEN    "HCM29V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM29V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM29V01-RENNUM
                       MOVE    HCM29V01            TO  RECE08-PRTDATA
                   WHEN    "HCM29V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM29V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM29V02-RENNUM
                       MOVE    HCM29V02            TO  RECE08-PRTDATA
                   WHEN    "HCM29V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM29V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM29V03-RENNUM
                       MOVE    HCM29V03            TO  RECE08-PRTDATA
                   WHEN    "HCM29V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM29V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM29V04-RENNUM
                       MOVE    HCM29V04            TO  RECE08-PRTDATA
                   WHEN    "HCM25.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM25
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM25-RENNUM
                       MOVE    HCM25               TO  RECE08-PRTDATA
                   WHEN    "HCM25V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM25V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM25V01-RENNUM
                       MOVE    HCM25V01            TO  RECE08-PRTDATA
                   WHEN    "HCM25V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM25V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM25V02-RENNUM
                       MOVE    HCM25V02            TO  RECE08-PRTDATA
                   WHEN    "HCM26.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM26
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM26-RENNUM
                       MOVE    HCM26               TO  RECE08-PRTDATA
                   WHEN    "HCM26V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM26V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM26V01-RENNUM
                       MOVE    HCM26V01            TO  RECE08-PRTDATA
                   WHEN    "HCM26V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM26V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM26V02-RENNUM
                       MOVE    HCM26V02            TO  RECE08-PRTDATA
                   WHEN    "HCM83.red"
                   WHEN    "HCM83P.red"
                       MOVE    RECE08-PRTDATA      TO  HCM83
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM83-RENNUM
                       MOVE    HCM83               TO  RECE08-PRTDATA
                   WHEN    "HCM84.red"
                   WHEN    "HCM84P.red"
                       MOVE    RECE08-PRTDATA      TO  HCM84
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM84-RENNUM
                       MOVE    HCM84               TO  RECE08-PRTDATA
                   WHEN    "HCN21.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN21
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN21-RENNUM
                       MOVE    HCN21               TO  RECE08-PRTDATA
                   WHEN    "HCN21V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN21V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN21V01-RENNUM
                       MOVE    HCN21V01            TO  RECE08-PRTDATA
                   WHEN    "HCN21V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN21V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN21V02-RENNUM
                       MOVE    HCN21V02            TO  RECE08-PRTDATA
                   WHEN    "HCN21V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN21V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN21V03-RENNUM
                       MOVE    HCN21V03            TO  RECE08-PRTDATA
                   WHEN    "HCN21V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN21V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN21V04-RENNUM
                       MOVE    HCN21V04            TO  RECE08-PRTDATA
                   WHEN    "HCN22.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN22
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN22-RENNUM
                       MOVE    HCN22               TO  RECE08-PRTDATA
                   WHEN    "HCN22V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN22V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN22V01-RENNUM
                       MOVE    HCN22V01            TO  RECE08-PRTDATA
                   WHEN    "HCN22V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN22V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN22V02-RENNUM
                       MOVE    HCN22V02            TO  RECE08-PRTDATA
                   WHEN    "HCN22V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN22V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN22V03-RENNUM
                       MOVE    HCN22V03            TO  RECE08-PRTDATA
                   WHEN    "HCN22V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN22V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN22V04-RENNUM
                       MOVE    HCN22V04            TO  RECE08-PRTDATA
                   WHEN    "HCN23.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23-RENNUM
                       MOVE    HCN23               TO  RECE08-PRTDATA
                   WHEN    "HCN23V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V01-RENNUM
                       MOVE    HCN23V01            TO  RECE08-PRTDATA
                   WHEN    "HCN23V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V02-RENNUM
                       MOVE    HCN23V02            TO  RECE08-PRTDATA
                   WHEN    "HCN23V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V03-RENNUM
                       MOVE    HCN23V03            TO  RECE08-PRTDATA
                   WHEN    "HCN23V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V04-RENNUM
                       MOVE    HCN23V04            TO  RECE08-PRTDATA
                   WHEN    "HCN23V05.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V05
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V05-RENNUM
                       MOVE    HCN23V05            TO  RECE08-PRTDATA
                   WHEN    "HCN23V06.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V06
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V06-RENNUM
                       MOVE    HCN23V06            TO  RECE08-PRTDATA
                   WHEN    "HCN23V07.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V07
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V07-RENNUM
                       MOVE    HCN23V07            TO  RECE08-PRTDATA
                   WHEN    "HCN23V08.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN23V08
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN23V08-RENNUM
                       MOVE    HCN23V08            TO  RECE08-PRTDATA
                   WHEN    "HCN25.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN25
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN25-RENNUM
                       MOVE    HCN25               TO  RECE08-PRTDATA
                   WHEN    "HCN25V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN25V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN25V01-RENNUM
                       MOVE    HCN25V01            TO  RECE08-PRTDATA
                   WHEN    "HCN25V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN25V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN25V02-RENNUM
                       MOVE    HCN25V02            TO  RECE08-PRTDATA
                   WHEN    "HCN25V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN25V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN25V03-RENNUM
                       MOVE    HCN25V03            TO  RECE08-PRTDATA
                   WHEN    "HCM48.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM48
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM48-RENNUM
                       MOVE    HCM48               TO  RECE08-PRTDATA
                   WHEN    "HCM48V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM48V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM48V01-RENNUM
                       MOVE    HCM48V01            TO  RECE08-PRTDATA
                   WHEN    "HCM48V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM48V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM48V02-RENNUM
                       MOVE    HCM48V02            TO  RECE08-PRTDATA
                   WHEN    "HCN48.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN48
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN48-RENNUM
                       MOVE    HCN48               TO  RECE08-PRTDATA
                   WHEN    "HCN48V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN48V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN48V01-RENNUM
                       MOVE    HCN48V01            TO  RECE08-PRTDATA
                   WHEN    "HCN48V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN48V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN48V02-RENNUM
                       MOVE    HCN48V02            TO  RECE08-PRTDATA
                   WHEN    "HCN48V03.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN48V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN48V03-RENNUM
                       MOVE    HCN48V03            TO  RECE08-PRTDATA
                   WHEN    "HCN48V04.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN48V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN48V04-RENNUM
                       MOVE    HCN48V04            TO  RECE08-PRTDATA
                   WHEN    "HCM49.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM49
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM49-RENNUM
                       MOVE    HCM49               TO  RECE08-PRTDATA
                   WHEN    "HCM49V01.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM49V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM49V01-RENNUM
                       MOVE    HCM49V01            TO  RECE08-PRTDATA
                   WHEN    "HCM49V02.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCM49V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM49V02-RENNUM
                       MOVE    HCM49V02            TO  RECE08-PRTDATA
                   WHEN    "HCN50.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN50
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN50-RENNUM
                       MOVE    HCN50               TO  RECE08-PRTDATA
                   WHEN    "HCN50V01.red"                  
                   WHEN    "HCN50V01P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN50V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN50V01-RENNUM
                       MOVE    HCN50V01            TO  RECE08-PRTDATA
                   WHEN    "HCN50V02.red"                  
                   WHEN    "HCN50V02P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN50V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN50V02-RENNUM
                       MOVE    HCN50V02            TO  RECE08-PRTDATA
                   WHEN    "HCN50V03.red"                  
                   WHEN    "HCN50V03P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN50V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN50V03-RENNUM
                       MOVE    HCN50V03            TO  RECE08-PRTDATA
                   WHEN    "HCN50V04.red"                  
                   WHEN    "HCN50V04P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN50V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN50V04-RENNUM
                       MOVE    HCN50V04            TO  RECE08-PRTDATA
                   WHEN    "HCN51.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN51
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN51-RENNUM
                       MOVE    HCN51               TO  RECE08-PRTDATA
                   WHEN    "HCN51V01.red"                  
                   WHEN    "HCN51V01P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN51V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN51V01-RENNUM
                       MOVE    HCN51V01            TO  RECE08-PRTDATA
                   WHEN    "HCN51V02.red"                  
                   WHEN    "HCN51V02P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN51V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN51V02-RENNUM
                       MOVE    HCN51V02            TO  RECE08-PRTDATA
                   WHEN    "HCN51V03.red"                  
                   WHEN    "HCN51V03P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN51V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN51V03-RENNUM
                       MOVE    HCN51V03            TO  RECE08-PRTDATA
                   WHEN    "HCN51V04.red"                  
                   WHEN    "HCN51V04P.red"                  
                       MOVE    RECE08-PRTDATA      TO  HCN51V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN51V04-RENNUM
                       MOVE    HCN51V04            TO  RECE08-PRTDATA
                   WHEN    "HCN72.red"
                   WHEN    "HCN72P.red"
                       MOVE    RECE08-PRTDATA      TO  HCN72
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN72-RENNUM
                       MOVE    HCN72               TO  RECE08-PRTDATA
                   WHEN    "HCN73.red"
                   WHEN    "HCN73P.red"
                       MOVE    RECE08-PRTDATA      TO  HCN73
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN73-RENNUM
                       MOVE    HCN73               TO  RECE08-PRTDATA
                   WHEN    "HCM53.red"
                   WHEN    "HCM54.red"
                       MOVE    RECE08-PRTDATA      TO  HCM53
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM53-RENNUM
                       MOVE    HCM53               TO  RECE08-PRTDATA
                   WHEN    "HCM53V01.red"
                   WHEN    "HCM54V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM53V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM53V01-RENNUM
                       MOVE    HCM53V01            TO  RECE08-PRTDATA
                   WHEN    "HCM53V02.red"
                   WHEN    "HCM54V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM53V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM53V02-RENNUM
                       MOVE    HCM53V02            TO  RECE08-PRTDATA
      ***          WHEN    "HCM54.red"
      ***              MOVE    RECE08-PRTDATA      TO  HCM54
      ***              MOVE    WRK-HEN-RENNUM-AREA TO  HCM54-RENNUM
      ***              MOVE    HCM54               TO  RECE08-PRTDATA
                   WHEN    "HCM55.red"
                       MOVE    RECE08-PRTDATA      TO  HCM55
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM55-RENNUM
                       MOVE    HCM55               TO  RECE08-PRTDATA
                   WHEN    "HCM55V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM55V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM55V01-RENNUM
                       MOVE    HCM55V01            TO  RECE08-PRTDATA
                   WHEN    "HCM55V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM55V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM55V02-RENNUM
                       MOVE    HCM55V02            TO  RECE08-PRTDATA
                   WHEN    "HCM55V03.red"
                       MOVE    RECE08-PRTDATA      TO  HCM55V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM55V03-RENNUM
                       MOVE    HCM55V03            TO  RECE08-PRTDATA
                   WHEN    "HCM76.red"
                       MOVE    RECE08-PRTDATA      TO  HCM76
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM76-RENNUM
                       MOVE    HCM76               TO  RECE08-PRTDATA
                   WHEN    "HCM77.red"
                       MOVE    RECE08-PRTDATA      TO  HCM77
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM77-RENNUM
                       MOVE    HCM77               TO  RECE08-PRTDATA
                   WHEN    "HCN53.red"
                   WHEN    "HCN54.red"
                       MOVE    RECE08-PRTDATA      TO  HCN53
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN53-RENNUM
                       MOVE    HCN53               TO  RECE08-PRTDATA
                   WHEN    "HCN53V01.red"
                   WHEN    "HCN54V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCN53V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN53V01-RENNUM
                       MOVE    HCN53V01            TO  RECE08-PRTDATA
                   WHEN    "HCN53V02.red"
                   WHEN    "HCN54V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCN53V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN53V02-RENNUM
                       MOVE    HCN53V02            TO  RECE08-PRTDATA
                   WHEN    "HCN53V03.red"
                   WHEN    "HCN54V03.red"
                       MOVE    RECE08-PRTDATA      TO  HCN53V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN53V03-RENNUM
                       MOVE    HCN53V03            TO  RECE08-PRTDATA
                   WHEN    "HCN53V04.red"
                   WHEN    "HCN54V04.red"
                       MOVE    RECE08-PRTDATA      TO  HCN53V04
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN53V04-RENNUM
                       MOVE    HCN53V04            TO  RECE08-PRTDATA
      ***          WHEN    "HCN54.red"
      ***              MOVE    RECE08-PRTDATA      TO  HCN54
      ***              MOVE    WRK-HEN-RENNUM-AREA TO  HCN54-RENNUM
      ***              MOVE    HCN54               TO  RECE08-PRTDATA
                   WHEN    "HCN55.red"
                       MOVE    RECE08-PRTDATA      TO  HCN55
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN55-RENNUM
                       MOVE    HCN55               TO  RECE08-PRTDATA
                   WHEN    "HCN55V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCN55V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN55V01-RENNUM
                       MOVE    HCN55V01            TO  RECE08-PRTDATA
                   WHEN    "HCN55V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCN55V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN55V02-RENNUM
                       MOVE    HCN55V02            TO  RECE08-PRTDATA
                   WHEN    "HCN55V03.red"
                       MOVE    RECE08-PRTDATA      TO  HCN55V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN55V03-RENNUM
                       MOVE    HCN55V03            TO  RECE08-PRTDATA
                   WHEN    "HCN70.red"
                       MOVE    RECE08-PRTDATA      TO  HCN70
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN70-RENNUM
                       MOVE    HCN70               TO  RECE08-PRTDATA
                   WHEN    "HCM56.red"
                       MOVE    RECE08-PRTDATA      TO  HCM56
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM56-RENNUM
                       MOVE    HCM56               TO  RECE08-PRTDATA
                   WHEN    "HCM56V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM56V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM56V01-RENNUM
                       MOVE    HCM56V01            TO  RECE08-PRTDATA
                   WHEN    "HCM56V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM56V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM56V02-RENNUM
                       MOVE    HCM56V02            TO  RECE08-PRTDATA
                   WHEN    "HCM56V03.red"
                       MOVE    RECE08-PRTDATA      TO  HCM56V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM56V03-RENNUM
                       MOVE    HCM56V03            TO  RECE08-PRTDATA
                   WHEN    "HCM57.red"
                       MOVE    RECE08-PRTDATA      TO  HCM57
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM57-RENNUM
                       MOVE    HCM57               TO  RECE08-PRTDATA
                   WHEN    "HCM57V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM57V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM57V01-RENNUM
                       MOVE    HCM57V01            TO  RECE08-PRTDATA
                   WHEN    "HCM57V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM57V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM57V02-RENNUM
                       MOVE    HCM57V02            TO  RECE08-PRTDATA
                   WHEN    "HCM58.red"
                       MOVE    RECE08-PRTDATA      TO  HCM58
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM58-RENNUM
                       MOVE    HCM58               TO  RECE08-PRTDATA
                   WHEN    "HCM58V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM58V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM58V01-RENNUM
                       MOVE    HCM58V01            TO  RECE08-PRTDATA
                   WHEN    "HCM58V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM58V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM58V02-RENNUM
                       MOVE    HCM58V02            TO  RECE08-PRTDATA
                   WHEN    "HCM59.red"
                       MOVE    RECE08-PRTDATA      TO  HCM59
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM59-RENNUM
                       MOVE    HCM59               TO  RECE08-PRTDATA
                   WHEN    "HCM59V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCM59V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM59V01-RENNUM
                       MOVE    HCM59V01            TO  RECE08-PRTDATA
                   WHEN    "HCM59V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCM59V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM59V02-RENNUM
                       MOVE    HCM59V02            TO  RECE08-PRTDATA
                   WHEN    "HCN58.red"
                       MOVE    RECE08-PRTDATA      TO  HCN58
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN58-RENNUM
                       MOVE    HCN58               TO  RECE08-PRTDATA
                   WHEN    "HCN58V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCN58V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN58V01-RENNUM
                       MOVE    HCN58V01            TO  RECE08-PRTDATA
                   WHEN    "HCN58V02.red"
                       MOVE    RECE08-PRTDATA      TO  HCN58V02
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN58V02-RENNUM
                       MOVE    HCN58V02            TO  RECE08-PRTDATA
                   WHEN    "HCN58V03.red"
                       MOVE    RECE08-PRTDATA      TO  HCN58V03
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN58V03-RENNUM
                       MOVE    HCN58V03            TO  RECE08-PRTDATA
                   WHEN    "HCN66.red"
                       MOVE    RECE08-PRTDATA      TO  HCN66
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN66-RENNUM
                       MOVE    HCN66               TO  RECE08-PRTDATA
                   WHEN    "HCN66V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCN66V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN66V01-RENNUM
                       MOVE    HCN66V01            TO  RECE08-PRTDATA
                   WHEN    "HCN67.red"
                       MOVE    RECE08-PRTDATA      TO  HCN67
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN67-RENNUM
                       MOVE    HCN67               TO  RECE08-PRTDATA
                   WHEN    "HCN67V01.red"
                       MOVE    RECE08-PRTDATA      TO  HCN67V01
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN67V01-RENNUM
                       MOVE    HCN67V01            TO  RECE08-PRTDATA
                   WHEN    "HCM69.red"
                       MOVE    RECE08-PRTDATA      TO  HCM69
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM69-RENNUM
                       MOVE    HCM69               TO  RECE08-PRTDATA
                   WHEN    "HCM70.red"
                       MOVE    RECE08-PRTDATA      TO  HCM70
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM70-RENNUM
                       MOVE    HCM70               TO  RECE08-PRTDATA
                   WHEN    "HCM71.red"
                       MOVE    RECE08-PRTDATA      TO  HCM71
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM71-RENNUM
                       MOVE    HCM71               TO  RECE08-PRTDATA
                   WHEN    "HCM87.red"
                   WHEN    "HCM87P.red"
                       MOVE    RECE08-PRTDATA      TO  HCM87
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM87-RENNUM
                       MOVE    HCM87               TO  RECE08-PRTDATA
                   WHEN    "HCM88.red"
                       MOVE    RECE08-PRTDATA      TO  HCM88
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCM88-RENNUM
                       MOVE    HCM88               TO  RECE08-PRTDATA
                   WHEN    "HCN74.red"
                   WHEN    "HCN74P.red"
                       MOVE    RECE08-PRTDATA      TO  HCN74
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN74-RENNUM
                       MOVE    HCN74               TO  RECE08-PRTDATA
                   WHEN    "HCN75.red"
                       MOVE    RECE08-PRTDATA      TO  HCN75
                       MOVE    WRK-HEN-RENNUM-AREA TO  HCN75-RENNUM
                       MOVE    HCN75               TO  RECE08-PRTDATA
               END-EVALUATE
           END-IF            
      *    MOVE    RECE08-PRTDATA      TO  PRT-PRTDATA
           INITIALIZE                  SFIXREDDATA-AREA
           MOVE    PRT-SITEKBN     TO  SFIXREDDATA-SITEKBN
           MOVE    PRT-PRTID       TO  SFIXREDDATA-FORMID
           MOVE    RECE08-PRTDATA  TO  SFIXREDDATA-DATA
           CALL    "ORCSFIXREDDATA"    USING
                                       SFIXREDDATA-AREA
                                       SPA-AREA
           MOVE    SFIXREDDATA-DATA
                                   TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           IF      STS-PRT             =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-PRT                 DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT     
                                           CNT-IN-CANCEL
      *
           PERFORM 390-PRINT-OUT-SEC
      *
           PERFORM 900-RECEPRT-FET-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL      =   50
               MOVE    ZERO               TO  CNT-IN-CANCEL 
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       2001-RECEPRT-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       2002-OFFSET-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    PRT-PRTID           TO  SYS-1044-FORMID
           STRING  PRT-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   PRT-PRTID)  AND
                        (SYS-1044-PRTNM   =   PRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-OFFSET-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *     
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *****IF      WRK-PARA-SYOKBN     =   "1"
      *****    MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
      *****ELSE
      *****    MOVE    ZERO                TO  SPRT-SRYYM
      *****END-IF
           MOVE    RECE08-SRYYM        TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
           ELSE
               IF      WRK-PARA-RECEKBN    =   "3" OR  "6"
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                               TO  SPRT-SHORI-RENNUM
               ELSE
                   IF      RECE08-RECENUM      <   100
                       MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                               TO  SPRT-SHORI-RENNUM
                   ELSE
                       IF      RECE08-RECENUM      <   200
                           COMPUTE SPRT-SHORI-RENNUM
                               =   LNK-PRTKANRI-SHORI-RENNUM   +   1
                       ELSE
                           COMPUTE SPRT-SHORI-RENNUM
                               =   LNK-PRTKANRI-SHORI-RENNUM   +   2
                       END-IF
                   END-IF
               END-IF     
           END-IF
      ****
      *??     MOVE    LNK-PRTKANRI-SHORI-RENNUM
      *??                                     TO  SPRT-SHORI-RENNUM
      ****
      **** MOVE    LNK-PRTKANRI-PRIORITY
      ****                             TO  SPRT-PRIORITY
           COMPUTE  SPRT-PRIORITY      =   LNK-PRTKANRI-PRIORITY
                                       *   10
           IF      WRK-PARA-SYOKBN     =   "1"
               COMPUTE  SPRT-PRIORITY      =   SPRT-PRIORITY
                                           +   1
           ELSE
               COMPUTE  SPRT-PRIORITY      =   SPRT-PRIORITY
                                           +   2
           END-IF
           MOVE    RECE08-FORMID       TO  SPRT-PRTID
           MOVE    RECE08-PRTDATA      TO  SPRT-PRTDATA
      *
         
           EVALUATE    RECE08-FORMID   ALSO    RECE08-NYUGAIKBN
               WHEN    "HC21"          ALSO    ANY 
               WHEN    "HCM21.red"     ALSO    ANY             
               WHEN    "HCM21V01.red"  ALSO    ANY
               WHEN    "HCM21V02.red"  ALSO    ANY
               WHEN    "HCM21V03.red"  ALSO    ANY
               WHEN    "HCM21V04.red"  ALSO    ANY
               WHEN    "HCM21V05.red"  ALSO    ANY
               WHEN    "HCM21V05P.red" ALSO    ANY
               WHEN    "HCM21V06.red"  ALSO    ANY
               WHEN    "HCM21V06P.red" ALSO    ANY
               WHEN    "HC22"          ALSO    ANY 
               WHEN    "HCM22.red"     ALSO    ANY
               WHEN    "HCM22V01.red"  ALSO    ANY
               WHEN    "HCM22V02.red"  ALSO    ANY
               WHEN    "HCM22V03.red"  ALSO    ANY
               WHEN    "HCM22V04.red"  ALSO    ANY
               WHEN    "HCM22V05.red"  ALSO    ANY
               WHEN    "HCM22V05P.red" ALSO    ANY
               WHEN    "HCM22V06.red"  ALSO    ANY
               WHEN    "HCM22V06P.red" ALSO    ANY
               WHEN    "HC18"          ALSO    ANY 
               WHEN    "HCM18.red"     ALSO    ANY
               WHEN    "HCM18V01.red"  ALSO    ANY
               WHEN    "HCM18V02.red"  ALSO    ANY
               WHEN    "HCM18V03.red"  ALSO    ANY
               WHEN    "HCM18V04.red"  ALSO    ANY
               WHEN    "HCM18V05.red"  ALSO    ANY
               WHEN    "HCM18V05P.red" ALSO    ANY
               WHEN    "HCM18V06.red"  ALSO    ANY
               WHEN    "HCM18V06P.red" ALSO    ANY
               WHEN    "HC20"          ALSO    ANY 
               WHEN    "HCM20.red"     ALSO    ANY
               WHEN    "HCM20V01.red"  ALSO    ANY
               WHEN    "HCM20V02.red"  ALSO    ANY
               WHEN    "HCM20V03.red"  ALSO    ANY
               WHEN    "HCM20V04.red"  ALSO    ANY
               WHEN    "HCM20V05.red"  ALSO    ANY
               WHEN    "HCM20V05P.red" ALSO    ANY
               WHEN    "HCM20V06.red"  ALSO    ANY
               WHEN    "HCM20V06P.red" ALSO    ANY
               WHEN    "HC24"          ALSO    ANY 
               WHEN    "HCM24.red"     ALSO    "2"
               WHEN    "HCM24V01.red"  ALSO    "2"
               WHEN    "HCM24V02.red"  ALSO    "2"
               WHEN    "HCM24V03.red"  ALSO    "2"
               WHEN    "HCM24V04.red"  ALSO    "2"
               WHEN    "HCM24V05.red"  ALSO    "2"
               WHEN    "HCM24V05P.red" ALSO    "2"
               WHEN    "HCM24V06.red"  ALSO    "2"
               WHEN    "HCM24V06P.red" ALSO    "2"
               WHEN    "HCM56.red"     ALSO    "2"
               WHEN    "HCM56V01.red"  ALSO    "2"
               WHEN    "HCM56V02.red"  ALSO    "2"
               WHEN    "HCM56V03.red"  ALSO    "2"
               WHEN    "HCM57.red"     ALSO    "2"
               WHEN    "HCM57V01.red"  ALSO    "2"
               WHEN    "HCM57V02.red"  ALSO    "2"
               WHEN    "HCM69.red"     ALSO    "2"
               WHEN    "HCM70.red"     ALSO    "2"
               WHEN    "HCM71.red"     ALSO    "2"
               WHEN    "HCM83.red"     ALSO    "2"
               WHEN    "HCM83P.red"    ALSO    "2"
               WHEN    "HCM84.red"     ALSO    "2"
               WHEN    "HCM84P.red"    ALSO    "2"
                   MOVE    "労災レセプト（入院外）"    TO  SPRT-TITLE
               WHEN    "HCM23.red"     ALSO    ANY
               WHEN    "HCM23V01.red"  ALSO    ANY
               WHEN    "HCM23V02.red"  ALSO    ANY
               WHEN    "HCM23V03.red"  ALSO    ANY
               WHEN    "HCM23V04.red"  ALSO    ANY
               WHEN    "HCM23V05.red"  ALSO    ANY
               WHEN    "HCM23V06.red"  ALSO    ANY
               WHEN    "HCM48.red"     ALSO    ANY
               WHEN    "HCM48V01.red"  ALSO    ANY
               WHEN    "HCM48V02.red"  ALSO    ANY
               WHEN    "HCM53.red"     ALSO    ANY
               WHEN    "HCM53V01.red"  ALSO    ANY
               WHEN    "HCM53V02.red"  ALSO    ANY
               WHEN    "HCM87.red"     ALSO    ANY
               WHEN    "HCM54.red"     ALSO    ANY
               WHEN    "HCM54V01.red"  ALSO    ANY
               WHEN    "HCM54V02.red"  ALSO    ANY
               WHEN    "HCM87P.red"    ALSO    ANY
               WHEN    "HCM76.red"     ALSO    ANY
               WHEN    "HCM29.red"     ALSO    "2"
               WHEN    "HCM29V01.red"  ALSO    "2"
               WHEN    "HCM29V02.red"  ALSO    "2"
               WHEN    "HCM29V03.red"  ALSO    "2"
               WHEN    "HCM29V04.red"  ALSO    "2"
               WHEN    "HCM49.red"     ALSO    "2"
               WHEN    "HCM49V01.red"  ALSO    "2"
               WHEN    "HCM49V02.red"  ALSO    "2"
               WHEN    "HCM77.red"     ALSO    "2"
                   IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                       MOVE    "自賠責（新様式）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                   ELSE
                       MOVE    "自賠（新）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                   END-IF
               WHEN    "HCM55.red"     ALSO    ANY
               WHEN    "HCM55V01.red"  ALSO    ANY
               WHEN    "HCM55V02.red"  ALSO    ANY
               WHEN    "HCM55V03.red"  ALSO    ANY
               WHEN    "HCM88.red"     ALSO    ANY
               WHEN    "HCM25.red"     ALSO    "2"
               WHEN    "HCM25V01.red"  ALSO    "2"
               WHEN    "HCM25V02.red"  ALSO    "2"
               WHEN    "HCM26.red"     ALSO    "2"
               WHEN    "HCM26V01.red"  ALSO    "2"
               WHEN    "HCM26V02.red"  ALSO    "2"
                   IF      WRK-PARA-RECEKBN    =   "8"
                       IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                           MOVE
                           "自賠責（第三者行為）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE    "自賠（第三者）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                       END-IF
                   ELSE
                       IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                           MOVE   "自賠責（従来様式）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE    "自賠（従）レセプト（入院外）"
                                                       TO  SPRT-TITLE
                       END-IF
                   END-IF
               WHEN    "HCM58.red"     ALSO    "2"
               WHEN    "HCM58V01.red"  ALSO    "2"
               WHEN    "HCM58V02.red"  ALSO    "2"
               WHEN    "HCM59.red"     ALSO    "2"
               WHEN    "HCM59V01.red"  ALSO    "2"
               WHEN    "HCM59V02.red"  ALSO    "2"
                   MOVE    "公害レセプト（入院外）"    TO  SPRT-TITLE
               WHEN    "HCN21.red"     ALSO    ANY
               WHEN    "HCN21V01.red"  ALSO    ANY
               WHEN    "HCN21V02.red"  ALSO    ANY
               WHEN    "HCN21V03.red"  ALSO    ANY
               WHEN    "HCN21V04.red"  ALSO    ANY
               WHEN    "HCN22.red"     ALSO    ANY
               WHEN    "HCN22V01.red"  ALSO    ANY
               WHEN    "HCN22V02.red"  ALSO    ANY
               WHEN    "HCN22V03.red"  ALSO    ANY
               WHEN    "HCN22V04.red"  ALSO    ANY
               WHEN    "HCN50.red"     ALSO    ANY
               WHEN    "HCN50V01.red"  ALSO    ANY
               WHEN    "HCN50V01P.red" ALSO    ANY
               WHEN    "HCN50V02.red"  ALSO    ANY
               WHEN    "HCN50V02P.red" ALSO    ANY
               WHEN    "HCN50V03.red"  ALSO    ANY
               WHEN    "HCN50V03P.red" ALSO    ANY
               WHEN    "HCN50V04.red"  ALSO    ANY
               WHEN    "HCN50V04P.red" ALSO    ANY
               WHEN    "HCN51.red"     ALSO    ANY
               WHEN    "HCN51V01.red"  ALSO    ANY
               WHEN    "HCN51V01P.red" ALSO    ANY
               WHEN    "HCN51V02.red"  ALSO    ANY
               WHEN    "HCN51V02P.red" ALSO    ANY
               WHEN    "HCN51V03.red"  ALSO    ANY
               WHEN    "HCN51V03P.red" ALSO    ANY
               WHEN    "HCN51V04.red"  ALSO    ANY
               WHEN    "HCN51V04P.red" ALSO    ANY
               WHEN    "HCN72.red"     ALSO    ANY
               WHEN    "HCN72P.red"    ALSO    ANY
               WHEN    "HCN73.red"     ALSO    ANY
               WHEN    "HCN73P.red"    ALSO    ANY
               WHEN    "HCM24V01.red"  ALSO    "1"
               WHEN    "HCM24V02.red"  ALSO    "1"
               WHEN    "HCM24V03.red"  ALSO    "1"
               WHEN    "HCM24V04.red"  ALSO    "1"
               WHEN    "HCM24V05.red"  ALSO    "1"
               WHEN    "HCM24V05P.red" ALSO    "1"
               WHEN    "HCM24V06.red"  ALSO    "1"
               WHEN    "HCM24V06P.red" ALSO    "1"
               WHEN    "HCM56.red"     ALSO    "1"
               WHEN    "HCM56V01.red"  ALSO    "1"
               WHEN    "HCM56V02.red"  ALSO    "1"
               WHEN    "HCM56V03.red"  ALSO    "1"
               WHEN    "HCM57.red"     ALSO    "1"
               WHEN    "HCM57V01.red"  ALSO    "1"
               WHEN    "HCM57V02.red"  ALSO    "1"
               WHEN    "HCN66.red"     ALSO    "1"
               WHEN    "HCN66V01.red"  ALSO    "1"
               WHEN    "HCN67.red"     ALSO    "1"
               WHEN    "HCN67V01.red"  ALSO    "1"
                   MOVE    "労災レセプト（入院）"      TO  SPRT-TITLE
               WHEN    "HCN23.red"     ALSO    ANY
               WHEN    "HCN23V01.red"  ALSO    ANY
               WHEN    "HCN23V02.red"  ALSO    ANY
               WHEN    "HCN23V03.red"  ALSO    ANY
               WHEN    "HCN23V04.red"  ALSO    ANY
               WHEN    "HCN23V05.red"  ALSO    ANY
               WHEN    "HCN23V06.red"  ALSO    ANY
               WHEN    "HCN23V07.red"  ALSO    ANY
               WHEN    "HCN23V08.red"  ALSO    ANY
               WHEN    "HCN48.red"     ALSO    ANY
               WHEN    "HCN48V01.red"  ALSO    ANY
               WHEN    "HCN48V02.red"  ALSO    ANY
               WHEN    "HCN48V03.red"  ALSO    ANY
               WHEN    "HCN48V04.red"  ALSO    ANY
               WHEN    "HCN53.red"     ALSO    ANY
               WHEN    "HCN53V01.red"  ALSO    ANY
               WHEN    "HCN53V02.red"  ALSO    ANY
               WHEN    "HCN53V03.red"  ALSO    ANY
               WHEN    "HCN53V04.red"  ALSO    ANY
               WHEN    "HCN74.red"     ALSO    ANY
               WHEN    "HCN54.red"     ALSO    ANY
               WHEN    "HCN54V01.red"  ALSO    ANY
               WHEN    "HCN54V02.red"  ALSO    ANY
               WHEN    "HCN54V03.red"  ALSO    ANY
               WHEN    "HCN54V04.red"  ALSO    ANY
               WHEN    "HCN74P.red"    ALSO    ANY
               WHEN    "HCN70.red"     ALSO    ANY
               WHEN    "HCM29.red"     ALSO    "1"
               WHEN    "HCM29V01.red"  ALSO    "1"
               WHEN    "HCM29V02.red"  ALSO    "1"
               WHEN    "HCM29V03.red"  ALSO    "1"
               WHEN    "HCM29V04.red"  ALSO    "1"
               WHEN    "HCM49.red"     ALSO    "1"
               WHEN    "HCM49V01.red"  ALSO    "1"
               WHEN    "HCM49V02.red"  ALSO    "1"
               WHEN    "HCM77.red"     ALSO    "1"
                   IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                       MOVE    "自賠責（新様式）レセプト（入院）"
                                                       TO  SPRT-TITLE
                   ELSE
                       MOVE    "自賠（新）レセプト（入院）"
                                                       TO  SPRT-TITLE
                   END-IF
               WHEN    "HCN55.red"     ALSO    ANY
               WHEN    "HCN55V01.red"  ALSO    ANY
               WHEN    "HCN55V02.red"  ALSO    ANY
               WHEN    "HCN55V03.red"  ALSO    ANY
               WHEN    "HCN75.red"     ALSO    ANY
               WHEN    "HCN25.red"     ALSO    "1"
               WHEN    "HCN25V01.red"  ALSO    "1"
               WHEN    "HCN25V02.red"  ALSO    "1"
               WHEN    "HCN25V03.red"  ALSO    "1"
               WHEN    "HCM26.red"     ALSO    "1"
               WHEN    "HCM26V02.red"  ALSO    "1"
                   IF      WRK-PARA-RECEKBN    =   "8"
                       IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                           MOVE   "自賠責（第三者行為）レセプト（入院）"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE    "自賠（第三者）レセプト（入院）"
                                                       TO  SPRT-TITLE
                       END-IF
                   ELSE
                       IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                           MOVE    "自賠責（従来様式）レセプト（入院）"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE    "自賠（従）レセプト（入院）"
                                                       TO  SPRT-TITLE
                       END-IF
                   END-IF
               WHEN    "HCN58.red"     ALSO    "1"
               WHEN    "HCN58V01.red"  ALSO    "1"
               WHEN    "HCN58V02.red"  ALSO    "1"
               WHEN    "HCN58V03.red"  ALSO    "1"
               WHEN    "HCM59.red"     ALSO    "1"
               WHEN    "HCM59V01.red"  ALSO    "1"
               WHEN    "HCM59V02.red"  ALSO    "1"
                   MOVE    "公害レセプト（入院）"      TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    RECE08-PRTNM        TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           MOVE    "1"                 TO  SPRT-PRTFLG
      *
           INITIALIZE                       ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)
                                       NOT =   "Z"
                   MOVE    RECE08-PTNUM (1:IDX)    TO  SPTID-PTNUM
                   CALL    "ORCSPTID"           USING   ORCSPTIDAREA
                                                        SPA-AREA
                   IF      SPTID-RC            =   ZERO
                       MOVE    SPTID-PTID          TO   SPRT-PTID
                   END-IF
                   MOVE    1                       TO  IDX
               END-IF
           END-PERFORM
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               STRING "帳票ＤＢに更新できませんでした SPRT-RETURN="
                                                   DELIMITED  BY  SIZE
                       SPRT-RETURN                 DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC                           
               PERFORM 500-COBABORT-SEC
           END-IF                                                              
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票クリア処理
      *****************************************************************
       2002-RECEPRT-CLEAR-SEC          SECTION.
      *
      *    出力順エリアのクリア
           MOVE    ZERO                 TO  RECE08-PRTJYUN-SYUKA
           MOVE    ZERO                 TO  RECE08-PRTRENNUM     
           MOVE    SPACE                TO  RECE08-PRTNM
      *
grpsys     MOVE    SPA-HOSPNUM          TO  RECE08-HOSPNUM
           MOVE    RECE08-REC           TO  MCPDATA-REC
           MOVE    "tbl_receprt"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           MOVE    "DBUPDATE"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-RECEPRT-FET-SEC
           ELSE
               MOVE    MCP-RC          TO  WRK-MCP-RC
               STRING
                   "レセプト明細ＤＢ更新エラー MCP-RC="
                                                   DELIMITED  BY  SIZE
                       WRK-MCP-RC                  DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
          END-IF    
           .
       2001-RECEPRT-CLEAR-EXT.
           EXIT.
      *
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
      *
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
           STRING  "ORCR0500 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
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
               AND   ( JOB-STOPFLG     >   ZERO )
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
                       IF      JOB-STOPFLG     =   1
      *                    ジョブ管理終了処理
                           MOVE    "CA2"           TO  SJOBKANRI-MODE
                           INITIALIZE                  JOBKANRI-REC
                           MOVE    WRK-RECEERR     TO  JOB-YOBI
                           MOVE    "8888"          TO  JOB-ERRCD
                           MOVE    2               TO  JOB-STOPFLG
grpsys                     PERFORM 900-CALL-ORCSJOB-SEC
                       END-IF
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT                         
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCR0500 IN  "  CNT-IN
           DISPLAY "*** ORCR0500 OUT "  CNT-OUT
      **** DISPLAY "*** ORCR0500 END ***"
      *****ACCEPT WRK-IN
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     レセプト管理を主キーで読み込む
      *****************************************************************
       900-RECEPRT-SELECT-SEC          SECTION.
      *
           INITIALIZE                  RECE08-REC
grpsys     MOVE    SPA-HOSPNUM              TO  RECE08-HOSPNUM
           MOVE    WRK-PARA-RECE09-PRTID    TO  RECE08-PRTID
      *****MOVE    WRK-PARA-RECE09-SRYYM    TO  RECE08-SRYYM
           MOVE    WRK-PARA-RECE09-SRYYM    TO  RECE08-RECEYM
           MOVE    WRK-PARA-RECE09-CREYMD   TO  RECE08-CREYMD
           MOVE    WRK-PARA-RECE09-CREHMS   TO  RECE08-CREHMS
           MOVE    WRK-PARA-RECE09-NYUGAIKBN
                                            TO  RECE08-NYUGAIKBN
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
      *
      *    エラーチェック
           IF      MCP-RC             =   ZERO
               PERFORM 900-RECEPRT-FET-SEC
           ELSE 
               DISPLAY "*** ORCR0500 SELECT ERR " MCP-RC 
               MOVE    1                   TO  FLG-RECEPRT
           END-IF
           .
       900-RECEPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタを主キーより検索処理
      *****************************************************************
       900-RECEPRT-FET-SEC            SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RECE08-REC
               ADD     1               TO  CNT-IN
               MOVE    ZERO            TO  FLG-RECEPRT
           ELSE
               MOVE    1               TO  FLG-RECEPRT
           END-IF
           .
       900-RECEPRT-FET-EXT.
           EXIT.
      *
      *******************************************************
      *     入力コードマスター クロース１処理
      *****************************************************************
       900-RECEPRT-CLS-SEC            SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-RECEPRT-CLS-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
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
       800-SYSKANRI-READ-EXT.
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

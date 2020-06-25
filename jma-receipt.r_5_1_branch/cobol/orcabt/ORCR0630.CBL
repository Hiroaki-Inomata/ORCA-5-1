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
       PROGRAM-ID.             ORCR0630.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : 診療実日数編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/11    NaCl−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.07  未実施減算の略称追加
      *  01.00.02    NACL-門脇    03.03.10  食事区分”４”の対応追加
      *  01.00.03    NACL-門脇    03.03.12  選定入院料対応
      *  01.00.04    NACL-門脇    03.03.20  入院料日数計算修正
      *  01.00.05    NACL-門脇    03.04.08  看護配置適合の略称追加対応
      *  01.00.06    NACL-門脇    03.04.10  看護配置適合の外泊対応
      *  01.00.07    NACL-藤原    03/04/21  地方公費データのとき不明分は
      *                                     処理の対象としない
      *  01.00.08    NACL-門脇    03/06/06  特定入院料の略称が無い場合の
      *                                     対応
      *  01.00.09    NACL-門脇    03/06/19  実日数テーブル編集の追加
      *  01.00.10    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT7→33
      *  01.00.11    NACL-門脇    04/02/27  未実施減算月途中変更対応
      *  01.00.12    NACL-門脇    04/03/17  愛媛県地方公費：福祉レセ対応
      *  01.00.13    NACL-門脇    04/03/18  有床診療所１群入院基本料１
      *                                     （医師配置適合）の外泊時
      *                                     の基本料取得対応等
      *  01.00.14    NACL-門脇    04/04/23  選定入院時の他医療機関受診の
      *                                     算定修正
      *  01.00.15    NACL-門脇    04/08/04  同一保険番号対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.01.02    NACL-小豆沢  04/12/10  入院略称マスタの読み込みワーク
      *                                     対応
      *  01.01.03    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.01.04    NACL-門脇    05/03/03  食事に関する編集修正
      *
      *  02.06.01    NACL-門脇    05/05/16  療養担当手当（北海道）対応
      *  02.06.02    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.03    NACL-小豆沢  05/09/16  以下の特定入院料の外泊計算を修正
      *                                     診療所老人医療管理料
      *                                     老人性認知症疾患治療病棟入院料１
      *                                     老人性認知症疾患治療病棟入院料２
      *  02.06.04    NACL-門脇    06/01/05  シス管（５０００）の食事情報取
      *                                     得修正
      *  02.06.05    NACL-門脇    06/01/20  標欠対応
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-門脇    06/05/16  老人特定入院基本料の診療区分
      *                                     編集修正
      *  02.09.03    NACL-門脇    06/05/17  略称編集(190123250等)
      *  02.09.04    NACL-門脇    06/05/23  外泊時の入院基本料取得修正
      *  03.01.01    NACL-門脇    06/06/28  実日数減算コード対応
      *  03.02.01    NACL-門脇    06/08/04  ９０入院欄の日数編集修正
      *  03.05.01    NACL-小豆沢  07/06/01  グループ診療対応
      *  03.05.02    NACL-門脇    07/06/27  実日数テーブル編集修正
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.01.01    NACL-門脇    07/12/19  入院料未算定日対応
      *  04.01.02    NACL-門脇    08/02/04  標欠外泊計算修正
      *  04.02.01    NACL-門脇    08/05/08  点滴３３２の手技料編集
      *  04.02.02    NACL-門脇    08/06/17  入院料未算定日対応
      *  04.02.03    NACL-門脇    08/09/24  入院略称編集追加
      *  04.03.01    NACL-門脇    09/02/09  公費分点数編集修正
      *  04.03.02    NACL-門脇    09/03/03  愛媛県地方公費対応
      *
      *  04.04.01    NACL-門脇    09/01/23  入院料算定保険以外の診療分実日数算定対応
      *  04.04.02    NACL-門脇    09/05/11  愛媛県地方公費対応
      *  04.04.03    NACL-門脇    09/09/04  小入管略称修正
      *  04.04.04    NACL-門脇    10/03/23  改正対応（外泊区分５の対応）
      *  04.04.05    NACL-門脇    10/04/13  分点編集
      *  04.04.06    NACL-門脇    10/05/21  負担区分編集対応
      *  04.04.07    NACL-門脇    10/06/01  他医療機関受診対応
      *  04.04.08    NACL-門脇    10/08/09  他医受診（外泊４）の選定時対応
      *
      *  04.05.01    NACL-門脇    10/01/12  治験対応
      *  04.05.02    NACL-門脇    10/02/08  地方公費の全国公費併用分請求点
      *                                     集計対応
      *  04.05.03    NACL-門脇    10/09/06  地方公費の全国公費併用分請求点
      *                                     集計対応（レセ電負担区分対応）
      *  04.05.04    NACL-門脇    11/08/01  難病患者等入院診療加算対応
      *  04.05.05    NACL-門脇    11/11/04  救急搬送診療料算定対応
      *  04.05.06    NACL-門脇    11/11/09  同日再入院食事対応
      *  04.05.07    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     外泊区分６．７の対応
      *                                     入院略称の文字数拡張対応
      *  04.05.08    NACL-門脇    12/05/16  愛知・三重県地方公費対応
      *  04.05.09    NACL-門脇    12/06/04  生保の２４併用時の対応
      *  04.05.10    NACL-門脇    12/06/28  生保まとめ分点欄摘要欄記載対応
      *  04.05.11    NACL-門脇    12/09/18  改正対応（外泊区分８，９の対応）
      *  04.05.12    NACL-門脇    12/10/22  生保まとめ分点欄点数集計修正
      *
      *  04.06.01    NACL-門脇    10/12/13  保険別請求チェック表の本体取込み
      *
      *  04.07.01    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.02    NACL-門脇    14/03/26  平成２６年４月改正対応
      *                                     （夜勤時間経過減算の選定対応）
      *  04.07.03    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.04    NACL-門脇    14/04/08  平成２６年４月改正対応
      *  04.07.05    NACL-門脇    14/06/17  栄養管理体制減算規定該当対応
      *  04.07.06    NACL-門脇    15/01/14  同日入院時のフラグ編集修正
      *  04.07.07    NACL-門脇    15/02/09  京都府地方公費対応（システム管理
      *                                     ２０１０のデフォルト設定修正）
      *  04.07.08    NACL-門脇    15/08/03  和歌山県地方公費対応（システム管理
      *                                     ２０１０のデフォルト設定修正）
      *
      *  04.08.01    NACL-門脇    14/07/29  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    15/12/10  システム管理２０１０のデフォルト
      *                                     設定修正（全国公費併用分請求点集計）
      *  04.08.04    NACL-門脇    16/03/15  生保まとめ分点欄記載修正
      *  04.08.05    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.06    NACL-門脇    16/08/23  持参薬対応
      *  04.08.07    NACL-門脇    16/11/12  徳島県地方公費対応
      *  04.08.08    NACL-門脇    17/02/23  茨城県地方公費対応（システム管理
      *                                     ２０１０のデフォルト設定修正）
      *  04.08.09    NACL-門脇    17/12/12  愛媛県地方公費対応（システム管理
      *                                     ２０１０のデフォルト設定修正）
      *  04.08.10    NACL-門脇    18/03/01  平成３０年４月改正対応
      *  04.08.11    NACL-門脇    18/03/27  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    18/06/11  ファイルのエラー処理追加
      *  05.00.02    NACL-門脇    19/06/04  生保に関わる保険組み合わせ作成
      *                                     対応（主保険なしの場合）
      *  05.00.03    NACL-門脇    20/03/16  他医療機関受診＋標欠の計算修正
      *  05.00.04    NACL-門脇    20/03/26  小児慢性の実施機関番号追加対応
      *                                     （７１０・８１０）
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE62PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE62                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-RECE62                              PIC 9(01).
           03  FLG-NYUKHN                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-GAIHAKU                             PIC 9(01).
           03  FLG-MIJISI-GENSAN                       PIC 9(01).       
           03  FLG-MIJISI-INNAI                        PIC 9(01).       
           03  FLG-MIJISI-IRYO                         PIC 9(01).       
           03  FLG-MIJISI-JYOKU                        PIC 9(01).       
           03  FLG-SENTEICDCHG                         PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-NYURYAKUWRK                         PIC 9(01).
           03  FLG-NISSUSET                            PIC 9(01).
           03  FLG-SYUKEI                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-IN                              PIC 9(06).
               05  CNT-MOD                             PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(02).
           03  IDX4                                    PIC 9(03).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  KH-IDX2                                 PIC 9(03).
           03  NYUINACT-IDX                            PIC 9(03).
           03  NYUINACT-IDX2                           PIC 9(03).
           03  ACCT-IDX                                PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  GENSAN-IDX                              PIC 9(03).
           03  GENSAN-IDX2                             PIC 9(03).
           03  IDV                                     PIC 9(02).
           03  TENTEK-IDX                              PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  IDX11                                   PIC 9(03).
           03  KOHFTN-IDX                              PIC 9(03).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN       OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *
      *    実日数カウント用領域２
       01  TBL-JNISSU2.
           03  TBL-JNISSU2-HKN                         OCCURS   5.
               05  TBL-JNISSU2-MON                     PIC 9(02).
               05  TBL-JNISSU2-DAY                     PIC X(01)
                                                       OCCURS   31.
      *
      *    実日数減カウント用領域
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-HKN-GEN   OCCURS   5.
               05  TBL-JNISSU-DAY-GEN                  PIC X(01)
                                                       OCCURS   31.
      *
      *    実日数減カウント用領域（まとめ）
       01  TBL-JNISSU-MATOME.
           03  TBL-JNISSU-HKN-MATOME                   OCCURS   31.
               05  TBL-JNISSU-DAY-M                    PIC X(01).
      *
      *    未実施減算退避用フラグテーブル
       01  TBL-GENSAN.
           03  TBL-GENSAN-INF       OCCURS  31.
               05  TBL-INNAI                           PIC X(01).
               05  TBL-IRYO                            PIC X(01).
               05  TBL-JYOKU                           PIC X(01).
      *
      *    未実施減算退避用点数テーブル
       01  TBL-GENSAN2.
           03  TBL-GENSAN2-INF      OCCURS  31.
               05  TBL-MIJISI-TENSU                    PIC 9(09).
      *
      *    医療機関（入院基本）カウント
       01  WRK-SYS5000-COUNT                           PIC 9(02).
      *
      *    合計点数（診療行為）集計用領域
       01  TBL-TOTALTEN.
           03  TBL-TOTALTEN-HKN     OCCURS   5.
               05  TBL-TOTALTEN-ACCT                   PIC S9(08).
      *    合計点数（診療行為）集計用領域
      *    （ぶん点・公費分離区分編集用）
       01  TBL-TOTALTEN2.
           03  TBL-TOTALTEN2-HKN    OCCURS   5.
               05  TBL-TOTALTEN2-ACCT                  PIC S9(08).
      *
       01  TBL-FUKUSHI.
           03  TBL-FUKUSHI-OCC                         OCCURS  4.
               05  TBL-FUKUSHIFLG                      PIC 9(01).
               05  TBL-FUKUSHI-DAYFLG                  PIC 9(01).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-SENTEI                     PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY41                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
               05  WRK-DAY6                            PIC 9(04).
               05  WRK-DAY6-ZAITEN                     PIC S9(08).
               05  WRK-DAY7                            PIC 9(04).
               05  WRK-DAY7-ZAITEN                     PIC 9(08).
               05  WRK-ACCT90                          PIC S9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
           03  WRK-SYS-SRYYMD                          PIC X(08).
           03  WRK-SRYYM-5000                          PIC X(06).
      *
           03  WRK-PREFNUM                             PIC X(02).
           03  WRK-HOSPSBT                             PIC 9(01).
           03  WRK-EIYOUKSNKBN                         PIC X(01).
      *
           03  WRK-HYOKETU-TEN                         PIC 9(09)V99.
      *
           03  WRK-NYUINF-TABLE.
               05  WRK-NYUINF-TBL                      OCCURS  20.
                   07  WRK-NYUKHNKEI                   PIC 9(09)
                                                           OCCURS 5.
                   07  WRK-NYUTOKKEI                   PIC 9(09)
                                                           OCCURS 5.
               05  WRK-NYUINF-TBL2                     OCCURS   5.
                   07  WRK-NYUKHNKEI2                  PIC S9(09).
                   07  WRK-NYUTOKKEI2                  PIC S9(09).
      *
      *    入院略称退避ワーク
           03  WRK-NYURYAKU-OCC                       OCCURS  200.
               05  WRK-NYURYAKU-SRYCD                 PIC X(09).
               05  WRK-NYURYAKU-YUKOSTYMD             PIC X(09).
               05  WRK-NYURYAKU-YUKOEDYMD             PIC X(09).
               05  WRK-NYURYAKU-DATA                  PIC X(400).
           03  WRK-NYUYAKU-CD                         PIC X(09).
           03  WRK-NYURYAKU-IDX                       PIC 9(03).
           03  WRK-NYURYAKU-MAX                       PIC 9(03).
      *
           03  WRK-TOKUREI-SRYCD                      PIC X(09).
           03  WRK-TOKUREI-SRYCD2                     PIC X(09).
           03  WRK-TOKUREI-FLG                        PIC 9(01).
      *
           03  WRK-TOKUREI-RYAKUSHO                   PIC X(20).
      *
           03  WRK-KEISAN2                            PIC 9(08).
           03  WRK-TENSU-KEISAN2                      PIC 9(08)V9(5).
      *
       01  WRK-TENTEK.
           03  WRK-TENTEK-SYUTEN                       PIC 9(07).
           03  WRK-TENTEK-ZAIKAISU                     PIC 9(04).
           03  WRK-TENTEK-TABLE.
               05  WRK-TENTEK-TBL                      OCCURS  10.
                   07  WRK-TENTEK-YKZTEN               PIC 9(07).
                   07  WRK-TENTEK-KIZAITEN             PIC 9(07).
           03  WRK-TENTEK-FLG-TABLE.
               05  WRK-TENTEK-FLG-TBL                  OCCURS  10.
                   07  WRK-TENTEK-FLG                  PIC 9(01)
                                                           OCCURS 5.
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *    入院略称読み込みワーク用
           03  WRK-NYURYAKU-CONST-MAX             PIC 9(03) VALUE 200.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    入院点数算定用ワークエリア
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                              PIC 9(08)V9(5).
           03  WRK-KHNTEN-MOTO                         PIC 9(08).
           03  WRK-KHNTEN-GEN                          PIC 9(08)V9(5).
           03  WRK-KHN-ZAITEN                          PIC 9(08)V9(5).
           03  WRK-KHN-ZAITEN-MOTO                     PIC 9(08).
           03  WRK-KHN-ZAITEN-GEN                      PIC 9(08)V9(5).
           03  WRK-SENTEI-KHNTEN                       PIC 9(08)V9(5).
           03  WRK-SENTEI-KHNTEN-MOTO                  PIC 9(08).
           03  WRK-SENTEI-KHNTEN-GEN                   PIC 9(08)V9(5).
           03  WRK-SENTEI-ZAITEN                       PIC 9(08)V9(5).
           03  WRK-SENTEI-ZAITEN-MOTO                  PIC 9(08).
           03  WRK-SENTEI-ZAITEN-GEN                   PIC 9(08)V9(5).
           03  WRK-KSNTEN                              PIC S9(08).
           03  WRK-NYUTENTTLKBN                        PIC 9(03).
           03  WRK-NYUKHNRYO                           PIC 9(09).
           03  WRK-NYUTOKRYO                           PIC 9(09).
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC 9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC 9(01)
                                                       OCCURS 31.
           03  WRK-SRYCD                               PIC X(09).
           03  WRK-RYAKUSHO                            PIC X(20).
           03  WRK-SRYYMD                              PIC X(08).       
           03  WRK-SRYYMD-TAIHI                        PIC X(08).   
           03  WRK-SENTEI-KEISU                        PIC 9V99.    
           03  WRK-GAIHAKU-SRYCD                       PIC X(09).
           03  WRK-GAIHAKU-KHNTEN                      PIC 9(08).
      *
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    医療機関（入院基本）退避用領域
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK2010.INC".
      *
           COPY    "CPSK2012.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    入院略称
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    選定入院料
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *    栄養管理経過措置入院料コード取得サブ
           COPY    "CPORCSEIYOUKKS.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    夜間看護体制特定日減算取得サブ
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0630"      TO  JOB-PGID
               MOVE    "診療実日数編集"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
      *    システム管理情報編集処理
           IF      FLG-END             =   ZERO
               PERFORM 1001-SYSKANRI-HENSYU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    システム管理情報編集処理
      ******************************************************************
       1001-SYSKANRI-HENSYU-SEC          SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key10"         TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
      *
                  IF      FLG-SYSKANRI     =   ZERO
                     MOVE   SYS-1001-PREFNUM  TO  WRK-PREFNUM
                     MOVE   SYS-1001-HOSPSBT  TO  WRK-HOSPSBT
                  ELSE
                     MOVE  "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                  END-IF    
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM  900-SYSKANRI-READ-SEC
      *
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
      *
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE     WRK-PARA-SRYYM     TO  WRK-SRYYM-5000
             PERFORM   10012-SYS-5000-CHK-SEC
      *      医療機関減算の取得（システム管理テーブルより）
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             MOVE     ZERO               TO  FLG-MIJISI-INNAI
                                             FLG-MIJISI-IRYO
                                             FLG-MIJISI-JYOKU
      *
             MOVE     SPACE              TO  TBL-GENSAN
             INITIALIZE                      TBL-GENSAN
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key10"         TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 IF      FLG-SYSKANRI        =   ZERO
                   PERFORM  10011-SYSKAN-HENSYU-SEC
                   IF        WRK-SYS5000-COUNT    =     ZERO
                      CONTINUE
                   ELSE
                      MOVE  WRK-PARA-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                      MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                      PERFORM  10013-SYS-5000-GET-SEC
                   END-IF
                 ELSE
                   MOVE "医療機関情報−入院基本が取得できませんでした。"
                                         TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                 END-IF
             ELSE
                  MOVE "医療機関情報−入院基本が取得できませんでした。"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
              MOVE    "tbl_syskanri"     TO  MCP-TABLE
              MOVE    "key10"            TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
           ELSE
      *
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                  MOVE "医療機関情報−入院基本が取得できませんでした。"
                                         TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
             END-IF
      *
              MOVE    "tbl_syskanri"     TO  MCP-TABLE
              MOVE    "key8"             TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内に複数の医療期間情報がないか検索（医療機関減算の確認）  *
      ******************************************************************
       10012-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key11"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key11"            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       10012-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内の医療期間情報取得（医療機関減算情報）                  *
      ******************************************************************
       10013-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                  IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                    EVALUATE  GENSAN-IDX
                      WHEN  2
                        MOVE    1     TO    FLG-MIJISI-INNAI    
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-INNAI(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  3
                        MOVE    1     TO    FLG-MIJISI-IRYO     
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-IRYO(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  4
                        MOVE    1     TO    FLG-MIJISI-JYOKU    
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  "1"   TO    TBL-JYOKU(GENSAN-IDX2)
                        END-PERFORM
                    END-EVALUATE
                  ELSE
                    EVALUATE  GENSAN-IDX
                      WHEN  2
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-INNAI(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  3
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-IRYO(GENSAN-IDX2)
                        END-PERFORM
                      WHEN  4
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                           UNTIL     GENSAN-IDX2   >   31
                          MOVE  SPACE   TO    TBL-JYOKU(GENSAN-IDX2)
                        END-PERFORM
                    END-EVALUATE
                  END-IF
                 END-PERFORM
      *
                 PERFORM    VARYING  IDX3  FROM  IDV  BY  1
                             UNTIL   IDX3    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                入院食事療養（１）
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX3)
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX3)
                            WHEN     "2"
                              MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX3)
                          END-EVALUATE
      *                入院食事療養（２）
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX3)
                          MOVE    ZERO    TO   WRK-TOKUKANRIKASAN(IDX3)
                      END-EVALUATE
                 END-PERFORM
      *
                 IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                   MOVE   SYS-5000-EIYOUKSNKBN   TO  WRK-EIYOUKSNKBN
                 END-IF
      *
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key11"             TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key11"            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       10013-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    シスカンの編集
      *****************************************************************
       10011-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                    UNTIL     GENSAN-IDX   >   4
                IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                    EVALUATE  GENSAN-IDX
                        WHEN  2
                            MOVE    1     TO    FLG-MIJISI-INNAI    
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-INNAI(GENSAN-IDX2)
                            END-PERFORM
                        WHEN  3
                            MOVE    1     TO    FLG-MIJISI-IRYO     
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-IRYO(GENSAN-IDX2)
                            END-PERFORM
                        WHEN  4
                            MOVE    1     TO    FLG-MIJISI-JYOKU    
                            PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                              MOVE  "1"   TO    TBL-JYOKU(GENSAN-IDX2)
                            END-PERFORM
                    END-EVALUATE
                END-IF
           END-PERFORM
      *
           PERFORM   VARYING   IDX3   FROM  1  BY  1
                      UNTIL    IDX3    >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         入院食事療養（１）
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX3)
                    EVALUATE  SYS-5000-SHOKUKBN(1)
                      WHEN     "1"
                        MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX3)
                      WHEN     "2"
                        MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX3)
                    END-EVALUATE
      *         入院食事療養（２）
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX3)
              END-EVALUATE
           END-PERFORM
      *
           MOVE   SYS-5000-EIYOUKSNKBN   TO  WRK-EIYOUKSNKBN
      *
           .
       10011-SYSKAN-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    レセプト処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE620"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAME
               ELSE
      *    請求書処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE62X"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                       MOVE    WRK-CONS-TERMID     TO
                                                   RECEDAT1PARA-TERMID
                   END-IF
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAME
               END-IF
               CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME   TO  RECE62PARA
      *
               PERFORM 2001-HENSYU-SEC
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    1           TO  FLG-END
               END-IF
           END-PERFORM 
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       2001-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE62
      *
           OPEN    I-O                 RECE62-FILE
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           PERFORM    900-RECE62-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE62  =   1
                                       OR      FLG-END     =   1
               PERFORM     20011-HENSYU-SEC
      *
               PERFORM     900-RECE62-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE62-FILE
           .
       2001-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       20011-HENSYU-SEC                   SECTION.
      *
      *    地方公費データのとき不明分は処理の対象としない
           IF    ( IDZ                  =   2    )
           AND   ( RECE62-RECESYUBETU   =   ZERO )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
           MOVE       SPACE             TO     TBL-JNISSU2
           INITIALIZE                          TBL-JNISSU2
           MOVE       SPACE             TO     TBL-JNISSU-GEN
           INITIALIZE                          TBL-JNISSU-GEN
           MOVE       SPACE             TO     TBL-JNISSU-MATOME
           INITIALIZE                          TBL-JNISSU-MATOME
      *
           MOVE       SPACE             TO     TBL-TOTALTEN
           INITIALIZE                          TBL-TOTALTEN
           MOVE       SPACE             TO     TBL-TOTALTEN2
           INITIALIZE                          TBL-TOTALTEN2
           MOVE       SPACE             TO     WRK-NYUINF-TABLE
           INITIALIZE                          WRK-NYUINF-TABLE
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       SPACE             TO     TBL-GENSAN2
           INITIALIZE                          TBL-GENSAN2
      *
           MOVE       ZERO              TO     FLG-MIJISI-GENSAN        
      *
           INITIALIZE                          TBL-FUKUSHI
      *
           PERFORM     200118-SYUKEIKBN-HANBETU-SEC
      *
           PERFORM     20011A-ETCKISAIFLGINF-HEN-SEC
           PERFORM     20011B-HOTEIGAI-HEN-SEC
      *
      *    保険組み合わせの公費チェック（生保まとめ）
           IF  (RECE62-SRYYM        >=  "200804")
           AND (RECE62-SEIHOFLG      =  "1"     )
              PERFORM  200119-SEIHO-HKNCOMBI-CHECK-SEC
           END-IF
      *
      *    夜間看護体制特定日減算取得
           IF   RECE62-SRYYM        >=  "201804"
             INITIALIZE                 ORCSKANGOGEN-AREA
             MOVE  RECE62-HOSPNUM   TO  LNK-KANGOGEN-HOSPNUM
             MOVE  RECE62-PTID      TO  LNK-KANGOGEN-PTID
             MOVE  RECE62-SRYYM     TO  LNK-KANGOGEN-SANTEIYM
             CALL  "ORCSKANGOGEN"   USING  ORCSKANGOGEN-AREA
                                           SPA-AREA
             IF    LNK-KANGOGEN-RC  =   ZERO
               PERFORM  VARYING  IDY  FROM  1  BY  1
                         UNTIL   IDY   >   31
                 MOVE  LNK-KANGOGEN-SRYCD(IDY)
                                    TO  RECE62-KANGOGEN-SRYCD(IDY)
               END-PERFORM
             END-IF
           END-IF
      *
      ***  PERFORM    VARYING    IDX    FROM   1   BY   1
      ***             UNTIL     (IDX      >   10)  OR
      ***                       (RECE62-SRYKA(IDX)   =   SPACE)
               PERFORM    200111-RECE62-HENSYU-SEC
               PERFORM    200112-RECE62-HENSYU2-SEC
               PERFORM    200113-RECE62-HENSYU3-SEC
               IF   RECE62-SRYYM  >= "200604"
                 CONTINUE
               ELSE
                 PERFORM  200114-RECE62-HENSYU4-SEC
               END-IF
               PERFORM    200115-RECE62-HENSYU9-SEC
      ***  END-PERFORM
      *
           PERFORM     200116-JNISSU-SET-SEC
      *
           PERFORM     200117-RECE62-REWRITE-SEC
           .
       20011-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200111-RECE62-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR  
                                                "4"  OR  "5"  OR
                                                "6"  OR  "7"
                       PERFORM    2001111-NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key7"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1    >       10
                          OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                 IF        WRK-DAY5(IDY)   =   RECE62-HKNCOMBI (IDX1)
                      MOVE    ZERO    TO    FLG-SHOSAISIN
                      IF  (WRK-DAY1(IDY)   NOT =    ZERO )
                      OR  (RECE62-NYUINDAY-KBN(IDY) = "1")
                         MOVE   1     TO    FLG-SHOSAISIN
                         MOVE  "1"    TO    RECE62-EF-NYUINDAY-KBN(IDY)
                      END-IF
      *
                      PERFORM   500-KUMIAWASE-HANBETU-SEC
      *
                      PERFORM   2001112-RECE62-TENSOU11-SEC
                      MOVE    10              TO    IDX1
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *+++同日再入院
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1    >       10
                          OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                 IF   RECE62-HKNCOMBI(IDX1)   =
                                   RECE62-DOUJITSU-HKNCOMBI(IDY)
      ***             MOVE    SPACE   TO    RECE62-NYUINRYOFLG
                      IF   RECE62-HKNCOMBI(IDX1) = WRK-DAY5(IDY)
                        CONTINUE
                      ELSE
                        MOVE  SPACE   TO    RECE62-NYUINRYOFLG
                      END-IF
      *
                      MOVE    ZERO    TO    FLG-SHOSAISIN
                      IF  (WRK-DAY1(IDY)   NOT =    ZERO )
                      OR  (RECE62-NYUINDAY-KBN(IDY) = "1")
                         MOVE   1     TO    FLG-SHOSAISIN
      *****              MOVE  "1"    TO    RECE62-EF-NYUINDAY-KBN(IDY)
                      END-IF
      *
                      PERFORM   500-KUMIAWASE-HANBETU-SEC
      *
                      PERFORM   2001112-RECE62-TENSOU11-SEC
                      MOVE    10              TO    IDX1
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key33"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF     (ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1))
                       AND    (ACCT-ZAIKBN     =  ZERO               )
      *
                           PERFORM    500-KUMIAWASE-HANBETU-SEC
      *
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                                       FLG-SHOSAISIN-GEN
      ***                       IF   ACCT-SRYKBN  =  "11"  OR
      ***                                            "13"  OR  "14"
      ***                           PERFORM 2001113-SHOSAISIN-HANTEI-SEC
      ***                       END-IF
                                IF   ACCT-SRYKBN  =  "95"  OR  "96"
                                    CONTINUE
                                ELSE
                                 IF   ACCT-SRYKBN  =  "80"
                                  PERFORM 2001115-SHOSAISIN-HANTEI2-SEC
                                 ELSE
                                  MOVE    1    TO    FLG-SHOSAISIN
                                 END-IF
                                END-IF
                                PERFORM   2001114-RECE62-TENSOU12-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    =        ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                                       FLG-SHOSAISIN-GEN
                                IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                                    PERFORM 2001113-SHOSAISIN-HANTEI-SEC
                                END-IF
                                PERFORM   2001114-RECE62-TENSOU12-SEC
                           END-IF
                           MOVE    10              TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO  MCP-TABLE
                   MOVE    "key33"           TO  MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key33"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RECE62-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       2001111-NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1-SENTEI(IDY)
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                    IF  ( NACCT-SRYKBN     = "90"      )
                    OR  ( NACCT-SRYCDTOTAL = 190799410 )
                    OR  ( NACCT-SRYCDTOTAL = 190799510 )
                       MOVE   903  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                       MOVE   "90" TO  RECE62-NYUDAY-SRYKBN(IDY)
                    ELSE                                                
                       MOVE   920  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                       MOVE   "92" TO  RECE62-NYUDAY-SRYKBN(IDY)
                    END-IF
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF   RECE62-SRYYM  >= "200604"
                 CONTINUE
               ELSE
                 IF     NACCT-ZAISKBKBN    =    "4"
                   IF    NACCT-SRYCDTOTAL  =    197000570 
                      MOVE   NACCT-DAY(IDY)   TO  WRK-DAY41(IDY)
                   ELSE
                      MOVE   NACCT-DAY(IDY)   TO  WRK-DAY4(IDY)
                   END-IF
                 END-IF
               END-IF
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY5(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "6"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY6(IDY) 
                    IF  NACCT-SRYCDTOTAL  =  190080470   OR             
                                             190799670   OR             
                                             190080570   OR             
                                             190799770   OR             
                                             190111690   OR             
                                             190809090   OR             
                                             190111790   OR             
                                             190809190                  
                      CONTINUE                                          
                    ELSE                                                
                     IF NACCT-SRYCDTOTAL  =  190182590   OR
                                             190182690
                      COMPUTE  WRK-DAY6-ZAITEN(IDY) =
                               WRK-DAY6-ZAITEN(IDY) -
                               NACCT-ZAITEN
                     ELSE
                      ADD    NACCT-ZAITEN   TO  WRK-DAY6-ZAITEN(IDY)
                     END-IF
                    END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "7"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY7(IDY) 
                    ADD    NACCT-ZAITEN     TO  WRK-DAY7-ZAITEN(IDY)
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       2001111-NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001112-RECE62-TENSOU11-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                       MOVE    "1"   TO   TBL-JNISSU-DAY(1 IDY)
               END-IF
           END-IF
      *
           MOVE       IDX1           TO   HC-IDX
      *
           MOVE       ZERO           TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                      IF   TBL-FUKUSHI-DAYFLG(IDX2) NOT = ZERO
                         CONTINUE
                      ELSE
                         MOVE    "1"   TO   TBL-JNISSU-DAY(KH-IDX IDY)
                      END-IF
                      MOVE    "1"      TO   TBL-JNISSU2-DAY(KH-IDX IDY)
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001112-RECE62-TENSOU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2001113-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      ****                                               =   SPACE)  OR
      ****                            (FLG-SHOSAISIN     =    1)
      **               MOVE    SPACE         TO   TNS-TENSU-REC
      **               INITIALIZE                 TNS-TENSU-REC
      **               MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
      **               MOVE    ACCT-SRYYM         TO  TNS-YUKOSTYMD(1:6)
      **               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
      **               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
      **               PERFORM  900-TENSU-READ-SEC
      **               IF       FLG-TENSU      =    ZERO
      **                 IF ( (TNS-JITUDAY    =     2)    AND
      **                      (TNS-DAYCNT     =     1   OR   2) ) OR
      **                    (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
      **                                                "099999902" OR
      **                                                "099110001" OR
      **                                                "099120001" OR
      **                                                "111011810")
      **                     MOVE    1      TO    FLG-SHOSAISIN
      **                 END-IF
      **                 IF    SRY-SRYCD(SRYACT-IDX) =  "099409903"
      **                     MOVE    1      TO    FLG-SHOSAISIN-GEN
      **                 END-IF
      **               END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                    "099999902" OR
                                                    "099110001" OR
                                                    "099120001"
                             MOVE    1       TO   FLG-SHOSAISIN
                       END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409903"
                             MOVE    1       TO   FLG-SHOSAISIN-GEN
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key9"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2001113-SHOSAISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2001115-SHOSAISIN-HANTEI2-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               IF   FLG-SRYACT = ZERO
                 IF   SRY-SRYCD(1)(1:5) =
                      "09591" OR "09592" OR "09593" OR "09594"
                      CONTINUE
                 ELSE
                      MOVE    1         TO      FLG-SHOSAISIN
                 END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2001115-SHOSAISIN-HANTEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001114-RECE62-TENSOU12-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                           IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-JNISSU-DAY(1 ACCT-IDX)
                           END-IF
                    END-PERFORM
               END-IF
               IF   FLG-SHOSAISIN-GEN    =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                           IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               MOVE    "1"       TO
                                         TBL-JNISSU-DAY-GEN(1 ACCT-IDX)
                           END-IF
                    END-PERFORM
               END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               IF   TBL-FUKUSHI-DAYFLG(IDX2) NOT = ZERO
                                 CONTINUE
                               ELSE
                                 MOVE    "1"       TO
                                      TBL-JNISSU-DAY(KH-IDX ACCT-IDX)
                               END-IF
                               MOVE    "1"       TO
                                      TBL-JNISSU2-DAY(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                   END-IF
                   IF   FLG-SHOSAISIN-GEN  =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                                 MOVE    "1"       TO
                                  TBL-JNISSU-DAY-GEN(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001114-RECE62-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200112-RECE62-HENSYU2-SEC                SECTION.
      *
           PERFORM      2001121-MIJISI-GET-SEC
      *
           PERFORM      2001122-SENTEI-KEISU-GET-SEC
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
      *            入院基本料と加減算点数の取得
                   PERFORM   2001123-NYUINKHN-GET-SEC
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      MOVE     ZERO    TO   FLG-NISSUSET
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM 500-KUMIAWASE-HANBETU-SEC
                                   PERFORM 2001124-RECE62-TENSOU21-SEC
                              END-IF
                              MOVE    10        TO     IDX1
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                            UNTIL   IDX1    >       10
                            OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                       IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
                       OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)   =  RECE62-HKNCOMBI (IDX1)
                              IF   (NACCT-ZAITEN    NOT =    ZERO)  
                              AND  (NACCT-DAY(IDY)  NOT =    ZERO)  
                              AND  ((WRK-DAY2(IDY)  =    ZERO   ) OR
                                    (WRK-DAY2(IDY) >=     4     ))
                                   PERFORM 500-KUMIAWASE-HANBETU-SEC
                                   PERFORM 2001125-RECE62-TENSOU22-SEC
                              END-IF
                              MOVE    10   TO     IDX1
                         END-IF
                       ELSE
                         MOVE    10        TO     IDX1
                       END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)
                           AND  (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"  
                                    PERFORM 500-KUMIAWASE-HANBETU-SEC
                                    PERFORM 2001126-RECE62-TENSOU23-SEC
                                END-IF
                           END-IF
                           MOVE    10            TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key33"           TO   MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200112-RECE62-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    未実施減算額取得
      *****************************************************************
       2001121-MIJISI-GET-SEC                 SECTION.
      *
           IF   FLG-MIJISI-INNAI  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190080570"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190799770"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-INNAI(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
      *
           END-IF
      *
           IF   FLG-MIJISI-IRYO   =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111690"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190809090"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-IRYO(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
           END-IF
      *
           IF   FLG-MIJISI-JYOKU  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
      ***      IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111790"   TO   TNS-SRYCD
      ***      ELSE
      ***           MOVE    "190809190"   TO   TNS-SRYCD
      ***      END-IF
               MOVE    RECE62-SRYYM       TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                               UNTIL     GENSAN-IDX2   >   31
                     IF  TBL-JYOKU(GENSAN-IDX2) =  "1"
                          ADD   TNS-TEN   TO    
                                    TBL-MIJISI-TENSU(GENSAN-IDX2)
                     END-IF
                  END-PERFORM
               END-IF
      *
           END-IF
      *
           .
       2001121-MIJISI-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    選定外泊時の係数取得
      **************************************************************
       2001122-SENTEI-KEISU-GET-SEC           SECTION.
      *
           MOVE      ZERO           TO     WRK-SENTEI-KEISU
      *
           IF   RECE62-SRYYM    <=    "200303"
               MOVE   0.1           TO     WRK-SENTEI-KEISU
           END-IF
      *
           IF ( RECE62-SRYYM    >=    "200304" )   AND
              ( RECE62-SRYYM    <=    "200403" )
               MOVE   0.05           TO    WRK-SENTEI-KEISU
           END-IF
      *
           IF   RECE62-SRYYM    >=    "200404"
               MOVE   ZERO          TO     WRK-SENTEI-KEISU
           END-IF
      *
           .
      *
       2001122-SENTEI-KEISU-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001124-RECE62-TENSOU21-SEC                 SECTION.
      *
           MOVE       ZERO               TO        KH-IDX
           MOVE       ZERO               TO        KH-IDX2
           MOVE       ZERO               TO        FLG-SYUKEI
      *    入院基本料と加減算点数の計算
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      IF   (RECE62-SEIHOFLG             =   "1"  )
                      AND  (RECE62-SYUTANARIFLG         =    1   )
                      AND  (RECE62-HKNNUMHC(IDX1)   NOT =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 1)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 2)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 3)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 4)     =   SPACE)
                        MOVE     1       TO        KH-IDX2
                      END-IF
                      PERFORM      20011241-NYUINACCT-GET-SEC
                      IF  FLG-SYUKEI  =  ZERO
                        MOVE     1       TO        FLG-SYUKEI
                      END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           MOVE       ZERO              TO   KH-IDX2
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      20011241-NYUINACCT-GET-SEC
                      IF  FLG-SYUKEI  =  ZERO
                        MOVE     1       TO        FLG-SYUKEI
                      END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001124-RECE62-TENSOU21-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の計算
      **************************************************************
       20011241-NYUINACCT-GET-SEC           SECTION.
      *
           IF    RECE62-KANGOGEN-SRYCD(IDY)  =  SPACE
             MOVE  WRK-KHNTEN-MOTO         TO  WRK-KHNTEN
             MOVE  WRK-KHN-ZAITEN-MOTO     TO  WRK-KHN-ZAITEN
             MOVE  WRK-SENTEI-KHNTEN-MOTO  TO  WRK-SENTEI-KHNTEN
             MOVE  WRK-SENTEI-ZAITEN-MOTO  TO  WRK-SENTEI-ZAITEN
           ELSE
             MOVE  WRK-KHNTEN-GEN          TO  WRK-KHNTEN
             MOVE  WRK-KHN-ZAITEN-GEN      TO  WRK-KHN-ZAITEN
             MOVE  WRK-SENTEI-KHNTEN-GEN   TO  WRK-SENTEI-KHNTEN
             MOVE  WRK-SENTEI-ZAITEN-GEN   TO  WRK-SENTEI-ZAITEN
           END-IF
      *
      *    入院料と外泊を比較して点数算定
      *
           EVALUATE   WRK-NYUTENTTLKBN
               WHEN   903
                  MOVE      ZERO   TO  WRK-NYUKHNRYO
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         COMPUTE  WRK-KEISAN2        =
                                  WRK-SENTEI-ZAITEN  +  0.5
                         MOVE     WRK-KEISAN2    TO  WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   3
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   4
                        IF    RECE62-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-SENTEI-KHNTEN * 0.6 )
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                          IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                          ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                          END-IF
                        ELSE
                         IF   RECE62-SRYYM  >=  "201004"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-SENTEI-KHNTEN * 0.3 )
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                          IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                          ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                          END-IF
                         ELSE
                          COMPUTE  WRK-NYUKHNRYO     =
      ****                        (WRK-KHNTEN        * 0.15)
                                  (WRK-KHNTEN * WRK-SENTEI-KEISU)
                                                     + 0.5
                         END-IF
                        END-IF
                       WHEN   5
                       WHEN   6
                       WHEN   7
                       WHEN  14
                       WHEN  15
                       WHEN  16
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       5
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.7 )
                            END-IF
                           WHEN       6
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.8 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.45)
                            END-IF
                           WHEN       7
                            IF    RECE62-SRYYM  >=  "201604"
                             CONTINUE
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.85)
                            END-IF
                           WHEN      14
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.95)
                           WHEN      15
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.65)
                           WHEN      16
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.85)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                       WHEN   8
                       WHEN   9
      ***              WHEN  11
                       WHEN  13
                       WHEN  17
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       8
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92)
                           WHEN       9
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92)
      ***                  WHEN      11
      ***                    COMPUTE  WRK-TENSU-KEISAN2 =
      ***                            (WRK-SENTEI-KHNTEN * 0.92
      ***                                               * 0.7 )
                           WHEN      13
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.7 )
                            END-IF
                           WHEN      17
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.95)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         COMPUTE  WRK-KEISAN2     =
                                  WRK-KHN-ZAITEN  +  0.5
                         MOVE     WRK-KEISAN2    TO  WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                       WHEN   2
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                       WHEN   4
                       WHEN   5
                       WHEN   6
                       WHEN   7
                       WHEN  14
                       WHEN  15
                       WHEN  16
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       4
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.6 )
                            ELSE
                             IF   RECE62-SRYYM  >=  "200404"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.3 )
                             ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.15)
                             END-IF
                            END-IF
                           WHEN       5
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.9 )
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.7 )
                            END-IF
                           WHEN       6
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.8 )
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.45)
                            END-IF
                           WHEN       7
                            IF    RECE62-SRYYM  >=  "201604"
                               CONTINUE
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.85)
                            END-IF
                           WHEN      14
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.95)
                           WHEN      15
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.65)
                           WHEN      16
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.85)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                       WHEN   8
                       WHEN   9
      ***              WHEN  11
                       WHEN  13
                       WHEN  17
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       8
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92)
                           WHEN       9
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92)
      ***                  WHEN      11
      ***                    COMPUTE  WRK-TENSU-KEISAN2 =
      ***                            (WRK-KHNTEN * 0.92
      ***                                        * 0.7 )
                           WHEN      13
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.7 )
                            END-IF
                           WHEN      17
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.95)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUKHNRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUKHNRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                    END-EVALUATE
                  END-IF
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                         AND    KH-IDX                >   1
                            CONTINUE
                         ELSE
                            COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                     WRK-NYUTOKKEI2(KH-IDX)  -
                                     TBL-MIJISI-TENSU(IDY)
                         END-IF
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  TBL-MIJISI-TENSU(IDY)
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  IF   WRK-NYUKHNRYO  NOT = ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                                UNTIL   ID1                   > 20
                                  OR    RECE62-NYUKHNNISSU(ID1)
                                                              = ZERO
                                  OR    FLG-ARI               = 1
                      IF  RECE62-NYUKHNRYO(ID1)  =  WRK-NYUKHNRYO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUKHNNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUKHNRYO
                                      TO   WRK-NYUKHNKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUKHNRYO
                                      TO   RECE62-NYUKHNKEI(ID1 KH-IDX)
                            IF  KH-IDX2  =  1
                             ADD  WRK-NYUKHNRYO
                                      TO   RECE62-SYUTANTEN-NYUKHN(ID1)
                            END-IF
                            IF  FLG-SYUKEI  =  ZERO
                             ADD  WRK-NYUKHNRYO
                                      TO   RECE62-ALLTENSU-NYUKHN(ID1)
                            END-IF
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUKHNNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
                      MOVE   WRK-NYUKHNRYO   TO  RECE62-NYUKHNRYO(ID1)
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUKHNRYO   
                                       TO  WRK-NYUKHNKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUKHNRYO   
                                       TO  RECE62-NYUKHNKEI(ID1 KH-IDX)
                      IF  KH-IDX2  =  1
                       ADD   WRK-NYUKHNRYO
                                       TO  RECE62-SYUTANTEN-NYUKHN(ID1)
                      END-IF
                      IF  FLG-SYUKEI  =  ZERO
                       ADD   WRK-NYUKHNRYO
                                       TO  RECE62-ALLTENSU-NYUKHN(ID1)
                      END-IF
                    END-IF
                  END-IF
               WHEN   920
                  MOVE      ZERO   TO  WRK-NYUTOKRYO
                  MOVE      ZERO   TO  FLG-GAIHAKU
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         COMPUTE  WRK-KEISAN2        =
                                  WRK-SENTEI-ZAITEN  +  0.5
                         MOVE     WRK-KEISAN2    TO  WRK-NYUTOKRYO
                       WHEN   1
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   3
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   4
                        IF    RECE62-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-SENTEI-KHNTEN * 0.6 )
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                          IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                          ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                          END-IF
                          MOVE   1   TO   FLG-GAIHAKU
                        ELSE
                         IF   RECE62-SRYYM  >=  "201004"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-SENTEI-KHNTEN * 0.3 )
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                          IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                          ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                          END-IF
                          MOVE   1   TO   FLG-GAIHAKU
                         ELSE
                          COMPUTE  WRK-NYUTOKRYO     =
      ****                        (WRK-KHNTEN        * 0.15)
                                  (WRK-KHNTEN * WRK-SENTEI-KEISU)
                                                     + 0.5
                          MOVE   1   TO   FLG-GAIHAKU
                         END-IF
                        END-IF
                       WHEN   5
                       WHEN   6
                       WHEN   7
                       WHEN  14
                       WHEN  15
                       WHEN  16
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       5
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.7 )
                            END-IF
                           WHEN       6
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.8 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.45)
                            END-IF
                           WHEN       7
                            IF    RECE62-SRYYM  >=  "201604"
                             CONTINUE
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.85)
                            END-IF
                           WHEN      14
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.95)
                           WHEN      15
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.65)
                           WHEN      16
                            COMPUTE  WRK-TENSU-KEISAN2 =
                                    (WRK-SENTEI-KHNTEN * 0.85)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   8
                       WHEN   9
      ***              WHEN  11
                       WHEN  13
                       WHEN  17
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       8
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92)
                           WHEN       9
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92)
      ***                  WHEN      11
      ***                    COMPUTE  WRK-TENSU-KEISAN2 =
      ***                            (WRK-SENTEI-KHNTEN * 0.92
      ***                                               * 0.7 )
                           WHEN      13
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.7 )
                            END-IF
                           WHEN      17
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-SENTEI-KHNTEN * 0.92
                                                        * 0.95)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         COMPUTE  WRK-KEISAN2     =
                                  WRK-KHN-ZAITEN  +  0.5
                         MOVE     WRK-KEISAN2    TO  WRK-NYUTOKRYO
                       WHEN   1
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   2
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   4
                       WHEN   5
                       WHEN   6
                       WHEN   7
                       WHEN  14
                       WHEN  15
                       WHEN  16
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       4
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.6 )
                            ELSE
                             IF   RECE62-SRYYM  >=  "200404"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.3 )
                             ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.15)
                             END-IF
                            END-IF
                           WHEN       5
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.9 )
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.7 )
                            END-IF
                           WHEN       6
                            IF    RECE62-SRYYM  >=  "201604"
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.8 )
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.45)
                            END-IF
                           WHEN       7
                            IF    RECE62-SRYYM  >=  "201604"
                               CONTINUE
                            ELSE
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.85)
                            END-IF
                           WHEN      14
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.95)
                           WHEN      15
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.65)
                           WHEN      16
                               COMPUTE  WRK-TENSU-KEISAN2 =
                                       (WRK-KHNTEN * 0.85)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                       WHEN   8
                       WHEN   9
      ***              WHEN  11
                       WHEN  13
                       WHEN  17
                         EVALUATE  WRK-DAY2(IDY)
                           WHEN       8
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92)
                           WHEN       9
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92)
      ***                  WHEN      11
      ***                    COMPUTE  WRK-TENSU-KEISAN2 =
      ***                            (WRK-KHNTEN * 0.92
      ***                                        * 0.7 )
                           WHEN      13
                            IF    RECE62-SRYYM  >=  "201604"
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.9 )
                            ELSE
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.7 )
                            END-IF
                           WHEN      17
                             COMPUTE  WRK-TENSU-KEISAN2 =
                                     (WRK-KHNTEN * 0.92
                                                 * 0.95)
                         END-EVALUATE
                         COMPUTE   WRK-TENSU-KEISAN2 =
                                   WRK-TENSU-KEISAN2 +
                                   WRK-KSNTEN
                         IF  WRK-HYOKETU-TEN  NOT =  ZERO
                           COMPUTE WRK-NYUTOKRYO     =
                                 ( WRK-TENSU-KEISAN2 -
                                  (WRK-TENSU-KEISAN2 *
                                  (WRK-HYOKETU-TEN   / 100)))
                                                     + 0.5
                         ELSE
                           COMPUTE WRK-NYUTOKRYO     =
                                   WRK-TENSU-KEISAN2 + 0.5
                         END-IF
                         MOVE   1   TO   FLG-GAIHAKU
                    END-EVALUATE
                  END-IF
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                         AND    KH-IDX                >   1
                            CONTINUE
                         ELSE
                            COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                     WRK-NYUTOKKEI2(KH-IDX)  -
                                     TBL-MIJISI-TENSU(IDY)
                         END-IF
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  TBL-MIJISI-TENSU(IDY)
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  IF  (WRK-DAY6(IDY)          NOT =  ZERO)
                  AND (WRK-DAY6-ZAITEN(IDY)   NOT =  ZERO)
                  AND ((WRK-DAY2(IDY)         =    ZERO ) OR
                       (WRK-DAY2(IDY)        >=     4   ))
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-DAY6-ZAITEN(IDY)
                  END-IF
                  PERFORM 200112411-NYUINRYO-TAIHI-SEC
                  IF  WRK-ACCT90(IDY)        NOT =  ZERO
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-ACCT90(IDY)
                  END-IF
                  IF   WRK-NYUTOKRYO  NOT = ZERO
                   IF     FLG-GAIHAKU  =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )  
                          OR  ( RECE62-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO-RYAKU(ID1)  =  WRK-RYAKUSHO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUTOKRYO
                                      TO   WRK-NYUTOKKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            IF  KH-IDX2  =  1
                             ADD  WRK-NYUTOKRYO
                                      TO   RECE62-SYUTANTEN-NYUTOK(ID1)
                            END-IF
                            IF  FLG-SYUKEI  =  ZERO
                             ADD  WRK-NYUTOKRYO
                                      TO   RECE62-ALLTENSU-NYUTOK(ID1)
                            END-IF
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
                      MOVE   WRK-RYAKUSHO    TO  RECE62-NYUTOKRYO-RYAKU
                                                                   (ID1)
      ***             略称が空白の場合の為の処置
                      MOVE   999999999       TO  RECE62-NYUTOKRYO(ID1)
      *
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUTOKRYO   
                                       TO  WRK-NYUTOKKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                      IF  KH-IDX2  =  1
                       ADD   WRK-NYUTOKRYO
                                       TO  RECE62-SYUTANTEN-NYUTOK(ID1)
                      END-IF
                      IF  FLG-SYUKEI  =  ZERO
                       ADD   WRK-NYUTOKRYO
                                       TO  RECE62-ALLTENSU-NYUTOK(ID1)
                      END-IF
                    END-IF
                   ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )  
                          OR  ( RECE62-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO(ID1)  =  WRK-NYUTOKRYO
      ****                  IF   KH-IDX =  1
      ****                  OR   RECE62-HKNNUM  =  SPACE
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                            AND    KH-IDX                >   1
                               CONTINUE
                            ELSE
                               ADD   WRK-NYUTOKRYO
                                      TO   WRK-NYUTOKKEI(ID1 KH-IDX)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            IF  KH-IDX2  =  1
                             ADD  WRK-NYUTOKRYO
                                      TO   RECE62-SYUTANTEN-NYUTOK(ID1)
                            END-IF
                            IF  FLG-SYUKEI  =  ZERO
                             ADD  WRK-NYUTOKRYO
                                      TO   RECE62-ALLTENSU-NYUTOK(ID1)
                            END-IF
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE       1           TO  FLG-NISSUSET
      ***             点数がゼロの場合の為の処置
                      MOVE   "９９９９９９９９９９"
                                             TO  RECE62-NYUTOKRYO-RYAKU
                                                                   (ID1)
      *
                      MOVE   WRK-NYUTOKRYO   TO  RECE62-NYUTOKRYO(ID1)
                      IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                      AND    KH-IDX                >   1
                         CONTINUE
                      ELSE
                         ADD    WRK-NYUTOKRYO   
                                       TO  WRK-NYUTOKKEI(ID1 KH-IDX)
                      END-IF
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                      IF  KH-IDX2  =  1
                       ADD   WRK-NYUTOKRYO
                                       TO  RECE62-SYUTANTEN-NYUTOK(ID1)
                      END-IF
                      IF  FLG-SYUKEI  =  ZERO
                       ADD   WRK-NYUTOKRYO
                                       TO  RECE62-ALLTENSU-NYUTOK(ID1)
                      END-IF
                    END-IF
                   END-IF
                  END-IF
           END-EVALUATE
      *
           .
       20011241-NYUINACCT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計の入院料加算の退避処理
      *****************************************************************
       200112411-NYUINRYO-TAIHI-SEC                 SECTION.
      *
           MOVE    ZERO                  TO   WRK-ACCT90(IDY)
      *
           INITIALIZE                         SRYACCT-REC
           MOVE    RECE62-HOSPNUM        TO   ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO   ACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO   ACCT-PTID
           MOVE    RECE62-SRYYM          TO   ACCT-SRYYM
           MOVE    RECE62-HKNCOMBI(IDX1) TO   ACCT-HKNCOMBI
           MOVE    SRYACCT-REC           TO   MCPDATA-REC
           MOVE    "tbl_sryacct"         TO   MCP-TABLE
           MOVE    "key58"               TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC                =    ZERO
               MOVE    "tbl_sryacct"     TO   MCP-TABLE
               MOVE    "key58"           TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACCT  =  1
                 IF   (ACCT-ZAIKBN         =  ZERO        )
                 AND  (ACCT-ZAITEN     NOT =  ZERO        )
                 AND  (ACCT-ZAIKAISU   NOT =  ZERO        )
                 AND  (ACCT-DAY(1 IDY) NOT =  ZERO        )
                   IF  ACCT-SRYCDTOTAL  =  190080470  OR  190799670  OR
                                           190080570  OR  190799770  OR
                                           190111690  OR  190809090  OR
                                           190111790  OR  190809190
                      CONTINUE
                   ELSE
                    IF ACCT-SRYCDTOTAL  =  190182590  OR  190182690
                      COMPUTE  WRK-ACCT90(IDY) =
                               WRK-ACCT90(IDY) -
                              (ACCT-ZAITEN * ACCT-DAY(1 IDY))
                    ELSE
                      COMPUTE  WRK-ACCT90(IDY) =
                               WRK-ACCT90(IDY) +
                              (ACCT-ZAITEN * ACCT-DAY(1 IDY))
                    END-IF
                   END-IF
                 END-IF
                 MOVE    "tbl_sryacct"     TO   MCP-TABLE
                 MOVE    "key58"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key58"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200112411-NYUINRYO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001125-RECE62-TENSOU22-SEC                 SECTION.
      *
           MOVE       ZERO               TO        KH-IDX
           MOVE       ZERO               TO        KH-IDX2
           MOVE       ZERO               TO        FLG-SYUKEI
      *    入院基本料加減算点数の計算
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      IF   (RECE62-SEIHOFLG             =   "1"  )
                      AND  (RECE62-SYUTANARIFLG         =    1   )
                      AND  (RECE62-HKNNUMHC(IDX1)   NOT =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 1)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 2)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 3)     =   SPACE)
                      AND  (RECE62-KOHNUMHC(IDX1 4)     =   SPACE)
                        MOVE     1       TO        KH-IDX2
                      END-IF
                      PERFORM      20011251-NYUINACCT-GET2-SEC
                      IF  FLG-SYUKEI  =  ZERO
                        MOVE     1       TO        FLG-SYUKEI
                      END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           MOVE       ZERO              TO   KH-IDX2
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      20011251-NYUINACCT-GET2-SEC
                      IF  FLG-SYUKEI  =  ZERO
                        MOVE     1       TO        FLG-SYUKEI
                      END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       2001125-RECE62-TENSOU22-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料加減算点数の計算
      **************************************************************
       20011251-NYUINACCT-GET2-SEC           SECTION.
      *
           IF  NACCT-SRYCDTOTAL       =  190080470   OR                 
                                         190799670   OR                 
                                         190080570   OR                 
                                         190799770   OR                 
                                         190111690   OR                 
                                         190809090   OR                 
                                         190111790   OR                 
                                         190809190                      
              CONTINUE                                                  
           ELSE                                                         
      *
             IF    NACCT-SRYCDTOTAL   =   190182590  OR  190182690
              EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
                 WHEN   903
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  -  NACCT-ZAITEN
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  -  NACCT-ZAITEN
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUKHN2   =
                             RECE62-SYUTANTEN-NYUKHN2   -  NACCT-ZAITEN
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUKHN2    =
                             RECE62-ALLTENSU-NYUKHN2    -  NACCT-ZAITEN
                    END-IF
              END-EVALUATE
             ELSE
              EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
                 WHEN   903
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUKHN2   =
                             RECE62-SYUTANTEN-NYUKHN2   +  NACCT-ZAITEN
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUKHN2    =
                             RECE62-ALLTENSU-NYUKHN2    +  NACCT-ZAITEN
                    END-IF
              END-EVALUATE
             END-IF
      *
           END-IF
      *
           IF   (WRK-DAY1(IDY)            = ZERO)
           AND  (RECE62-NYUINDAY-KBN(IDY) = "1" )
                 IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                 AND    KH-IDX                >   1
                    CONTINUE
                 ELSE
                    COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                             WRK-NYUTOKKEI2(KH-IDX)  +  NACCT-ZAITEN
                 END-IF
                 COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                          RECE62-NYUTOKKEI2(KH-IDX)  +  NACCT-ZAITEN
                 IF  KH-IDX2  =  1
                  COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                          RECE62-SYUTANTEN-NYUTOK2   +  NACCT-ZAITEN
                 END-IF
                 IF  FLG-SYUKEI  =  ZERO
                  COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                          RECE62-ALLTENSU-NYUTOK2    +  NACCT-ZAITEN
                 END-IF
           END-IF
      *
           .
       20011251-NYUINACCT-GET2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の取得
      **************************************************************
       2001123-NYUINKHN-GET-SEC           SECTION.
      *
           IF  ( NACCT-SRYKBN     = "90"      )
           OR  ( NACCT-SRYCDTOTAL = 190799410 )
           OR  ( NACCT-SRYCDTOTAL = 190799510 )
                 MOVE   903   TO  WRK-NYUTENTTLKBN                      
           ELSE                                                         
                 MOVE   920   TO  WRK-NYUTENTTLKBN                      
           END-IF                                                       
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    ZERO               TO   WRK-KHNTEN-MOTO
           MOVE    ZERO               TO   WRK-KHNTEN-GEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-MOTO
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-GEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN-MOTO
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN-GEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN-MOTO
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN-GEN
           MOVE    SPACE              TO   WRK-RYAKUSHO
           MOVE    ZERO               TO   NYUINACT-IDX2
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
      *
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE      ZERO          TO  FLG-NYUKHN  
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
                     MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
                     MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
                     PERFORM  900-TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              MOVE   TNS-TEN   TO   WRK-KHNTEN-MOTO
      *                       夜間看護体制特定日減算
                              COMPUTE  WRK-KHNTEN-GEN   =
                                      (WRK-KHNTEN-MOTO  * 0.95)
      *                       看護比率基準未達時の入院基本点数の取得
                              IF    RECE62-SRYYM  <=  "201203"
                               PERFORM 20011231-GAIHAKU-NYUINKHN-SEC
                              END-IF
                              IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN-MOTO
                              END-IF
                              MOVE   NYUINACT-IDX
                                               TO   NYUINACT-IDX2
      ***                     MOVE   1         TO   FLG-NYUKHN
                         ELSE
                              EVALUATE  TNS-TENSIKIBETU
                                WHEN     3
                                    MOVE  TNS-TEN  TO  WRK-KSNTEN
                                WHEN     8
                                    COMPUTE  WRK-KSNTEN = TNS-TEN * -1
                                WHEN     6
                                    MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                              END-EVALUATE
                         END-IF
                     END-IF                  
               END-PERFORM
      *
               COMPUTE  WRK-KHN-ZAITEN-MOTO     =
                        NACCT-ZAITEN
               COMPUTE  WRK-KHN-ZAITEN-GEN      =
                        WRK-KHNTEN-GEN          +
                        WRK-KSNTEN
               IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *          標欠：％減算
                 COMPUTE    WRK-KEISAN2             =
                          ( WRK-KHN-ZAITEN-GEN      -
                           (WRK-KHN-ZAITEN-GEN      *
                            (WRK-HYOKETU-TEN   /  100))) + 0.5
                 MOVE   WRK-KEISAN2   TO   WRK-KHN-ZAITEN-GEN
               END-IF
      *
               IF    WRK-NYUTENTTLKBN     =   920  
               AND   NYUINACT-IDX2    NOT =   ZERO
      *
                 MOVE    ZERO          TO   WRK-TOKUREI-FLG
                 MOVE    SPACE         TO   NYURYAKUSHO-REC
                 INITIALIZE                 NYURYAKUSHO-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2) 
                                       TO   NYURYAKUSHO-SRYCD
                                            WRK-TOKUREI-SRYCD2
                 MOVE    SPACE         TO   WRK-TOKUREI-SRYCD
                 PERFORM  500-RYAKUSYO-KEISO-CHECK-SEC
                 IF   WRK-TOKUREI-SRYCD  NOT = SPACE
                   MOVE  WRK-TOKUREI-SRYCD
                                       TO   NYURYAKUSHO-SRYCD
                 END-IF
      *
                 MOVE    NACCT-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
                 MOVE    "01"          TO   NYURYAKUSHO-YUKOSTYMD(7:2)
                 MOVE    NYURYAKUSHO-YUKOSTYMD
                                       TO   NYURYAKUSHO-YUKOEDYMD
                 PERFORM  900-NYURYAKUSHO-READ-SEC
                 IF       FLG-NYURYAKUSHO    =    ZERO
                   MOVE     NYURYAKUSHO-RYAKUSHO
                                       TO  WRK-RYAKUSHO
      *            精神科救急入院料１の略称記載
                   IF    (RECE62-SRYYM              >=  "200804")  AND
                         (NSRY-SRYCD(NYUINACT-IDX2)  =  "190121010" OR
                                                        "190111410")
                       MOVE   "精救１" TO  WRK-RYAKUSHO
                   END-IF
                 ELSE
      *            精神科救急入院料２の略称記載
                   IF    (RECE62-SRYYM              >=  "200804")  AND
                         (NSRY-SRYCD(NYUINACT-IDX2)  =  "190130010" OR
                                                        "190130110")
                       MOVE   "精救２" TO  WRK-RYAKUSHO
                   END-IF
                 END-IF                  
      *
               END-IF 
      *
      *        選定入院料の算定
      *
               IF    NYUINACT-IDX2          NOT   =   ZERO
      *
                 MOVE    SPACE                TO  SENTEICDCHG-REC
                 INITIALIZE                       SENTEICDCHG-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2)
                                              TO  SENCHG-IPNSRYCD
                 MOVE    SENTEICDCHG-REC      TO  MCPDATA-REC
                 MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC             =    ZERO
                     MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                     MOVE    "key2"              TO  MCP-PATHNAME
                     PERFORM  900-SENTEICDCHG-READ-SEC
                 ELSE
                     MOVE    1                TO  FLG-SENTEICDCHG
                 END-IF
      *
                 IF  FLG-SENTEICDCHG   =   ZERO
                  IF    SENCHG-IPNSRYCD  =  SENCHG-SENTEISRYCD
      *             夜勤時間経過減算の選定対応
      *             "190131410","190165810","190165910","190131310"
                    IF   RECE62-SRYYM >= "201404"
                       COMPUTE  WRK-SENTEI-KHNTEN-MOTO  =
                               (WRK-KHNTEN-MOTO * 0.85) +  0.5
      *                夜間看護体制特定日減算
                       COMPUTE  WRK-SENTEI-KHNTEN-GEN   =
                               (WRK-SENTEI-KHNTEN-MOTO  * 0.95)
                       COMPUTE  WRK-SENTEI-ZAITEN-MOTO  =
                                WRK-SENTEI-KHNTEN-MOTO  +
                                WRK-KSNTEN
                       COMPUTE  WRK-SENTEI-ZAITEN-GEN   =
                                WRK-SENTEI-KHNTEN-GEN   +
                                WRK-KSNTEN
                       IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *                  標欠：％減算
                         COMPUTE    WRK-SENTEI-ZAITEN-MOTO  =
                                  ( WRK-SENTEI-ZAITEN-MOTO  -
                                   (WRK-SENTEI-ZAITEN-MOTO  *
                                    (WRK-HYOKETU-TEN   /  100))) + 0.5
                         COMPUTE    WRK-KEISAN2             =
                                  ( WRK-SENTEI-ZAITEN-GEN   -
                                   (WRK-SENTEI-ZAITEN-GEN   *
                                    (WRK-HYOKETU-TEN   /  100))) + 0.5
                         MOVE   WRK-KEISAN2   TO   WRK-SENTEI-ZAITEN-GEN
                       END-IF
                    END-IF
                  ELSE
                    MOVE    SPACE               TO    TNS-TENSU-REC
                    INITIALIZE                        TNS-TENSU-REC
                    MOVE    SENCHG-SENTEISRYCD  TO    TNS-SRYCD
                    MOVE    NACCT-SRYYM         TO    TNS-YUKOSTYMD(1:6)
                    MOVE    "01"                TO    TNS-YUKOSTYMD(7:2)
                    MOVE    TNS-YUKOSTYMD       TO    TNS-YUKOEDYMD
                    PERFORM  900-TENSU-READ-SEC
                    IF       FLG-TENSU      =    ZERO
                       MOVE   TNS-TEN   TO   WRK-SENTEI-KHNTEN-MOTO
      *                夜間看護体制特定日減算
                       COMPUTE  WRK-SENTEI-KHNTEN-GEN   =
                               (WRK-SENTEI-KHNTEN-MOTO  * 0.95)
                       COMPUTE  WRK-SENTEI-ZAITEN-MOTO  =
                                WRK-SENTEI-KHNTEN-MOTO  +
                                WRK-KSNTEN
                       COMPUTE  WRK-SENTEI-ZAITEN-GEN   =
                                WRK-SENTEI-KHNTEN-GEN   +
                                WRK-KSNTEN
                       IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *                  標欠：％減算
                         COMPUTE    WRK-SENTEI-ZAITEN-MOTO  =
                                  ( WRK-SENTEI-ZAITEN-MOTO  -
                                   (WRK-SENTEI-ZAITEN-MOTO  *
                                    (WRK-HYOKETU-TEN   /  100))) + 0.5
                         COMPUTE    WRK-KEISAN2             =
                                  ( WRK-SENTEI-ZAITEN-GEN   -
                                   (WRK-SENTEI-ZAITEN-GEN   *
                                    (WRK-HYOKETU-TEN   /  100))) + 0.5
                         MOVE   WRK-KEISAN2   TO   WRK-SENTEI-ZAITEN-GEN
                       END-IF
                    END-IF  
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
               END-IF
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001123-NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊時の入院基本料取得
      **************************************************************
       20011231-GAIHAKU-NYUINKHN-SEC           SECTION.
      *
           EVALUATE  TNS-SRYCD
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        診療所老人医療管理料
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料１
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料２
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (RECE62-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN    TO  WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       20011231-GAIHAKU-NYUINKHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001126-RECE62-TENSOU23-SEC                 SECTION.
      *
           MOVE       ZERO               TO        KH-IDX
           MOVE       ZERO               TO        KH-IDX2
           MOVE       ZERO               TO        FLG-SYUKEI
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                 MOVE    1          TO        KH-IDX
                 IF   (RECE62-SEIHOFLG             =   "1"  )
                 AND  (RECE62-SYUTANARIFLG         =    1   )
                 AND  (RECE62-HKNNUMHC(IDX1)   NOT =   SPACE)
                 AND  (RECE62-KOHNUMHC(IDX1 1)     =   SPACE)
                 AND  (RECE62-KOHNUMHC(IDX1 2)     =   SPACE)
                 AND  (RECE62-KOHNUMHC(IDX1 3)     =   SPACE)
                 AND  (RECE62-KOHNUMHC(IDX1 4)     =   SPACE)
                   MOVE     1       TO        KH-IDX2
                 END-IF
                 PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                            UNTIL    ACCT-IDX   >      31
                        IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO   ) OR
                             (RECE62-NYUINDAY-KBN(ACCT-IDX) = "1"))
      ***               AND ( WRK-DAY5(ACCT-IDX) =
      ***                                  RECE62-HKNCOMBI (IDX1) )
                        AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO    )
                            PERFORM      20011261-SRYACCT-GET-SEC
                        END-IF
                 END-PERFORM
                 IF  FLG-SYUKEI  =  ZERO
                   MOVE     1       TO        FLG-SYUKEI
                 END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           MOVE       ZERO              TO   KH-IDX2
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                     PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                UNTIL    ACCT-IDX   >      31
                          IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO   ) OR
                               (RECE62-NYUINDAY-KBN(ACCT-IDX) = "1"))
      ***                 AND ( WRK-DAY5(ACCT-IDX) =
      ***                                  RECE62-HKNCOMBI (IDX1)   )
                          AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO    )
                              PERFORM      20011261-SRYACCT-GET-SEC
                          END-IF
                     END-PERFORM
                     IF  FLG-SYUKEI  =  ZERO
                       MOVE     1       TO        FLG-SYUKEI
                     END-IF
                 END-IF
               END-IF  
           END-PERFORM
      *
           .
       2001126-RECE62-TENSOU23-EXT.
           EXIT.
      *
      **************************************************************
      *    診療会計の入院基本料加減算点数の計算
      **************************************************************
       20011261-SRYACCT-GET-SEC           SECTION.
      *
           EVALUATE   WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
              WHEN   903
                 IF       (ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190)
                 OR       (ACCT-ZAIKBN        =    2        )
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                WRK-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                             RECE62-SYUTANTEN-NYUTOK2   -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                             RECE62-ALLTENSU-NYUTOK2    -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                 ELSE
                  IF   ACCT-SRYCDTOTAL  =  190182590  OR  190182690
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUKHN2   =
                             RECE62-SYUTANTEN-NYUKHN2   -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUKHN2    =
                             RECE62-ALLTENSU-NYUKHN2    -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                  ELSE
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUKHNKEI2(KH-IDX)  =
                                WRK-NYUKHNKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUKHN2   =
                             RECE62-SYUTANTEN-NYUKHN2   +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUKHN2    =
                             RECE62-ALLTENSU-NYUKHN2    +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                  END-IF
                 END-IF
              WHEN   920
                 IF       (ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190)
                 OR       (ACCT-ZAIKBN        =    2        )
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                WRK-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                             RECE62-SYUTANTEN-NYUTOK2   -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                             RECE62-ALLTENSU-NYUTOK2    -
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                 ELSE
                  IF  WRK-DAY5(ACCT-IDX)  NOT =  RECE62-HKNCOMBI(IDX1)
                    IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                    AND    KH-IDX                >   1
                       CONTINUE
                    ELSE
                       COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                                WRK-NYUTOKKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    IF  KH-IDX2  =  1
                     COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                             RECE62-SYUTANTEN-NYUTOK2   +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                    IF  FLG-SYUKEI  =  ZERO
                     COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                             RECE62-ALLTENSU-NYUTOK2    +
                             (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                    END-IF
                  END-IF
                 END-IF
           END-EVALUATE
      *
           IF   (WRK-DAY1(ACCT-IDX)            = ZERO)
           AND  (RECE62-NYUINDAY-KBN(ACCT-IDX) = "1" )
              IF ACCT-ZAIKBN = 2
               IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
               AND    KH-IDX                >   1
                  CONTINUE
               ELSE
                  COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                           WRK-NYUTOKKEI2(KH-IDX)  -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                        RECE62-NYUTOKKEI2(KH-IDX)  -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               IF  KH-IDX2  =  1
                COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                        RECE62-SYUTANTEN-NYUTOK2   -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               IF  FLG-SYUKEI  =  ZERO
                COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                        RECE62-ALLTENSU-NYUTOK2    -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
              ELSE
               IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
               AND    KH-IDX                >   1
                  CONTINUE
               ELSE
                  COMPUTE  WRK-NYUTOKKEI2(KH-IDX)  =
                           WRK-NYUTOKKEI2(KH-IDX)  +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                        RECE62-NYUTOKKEI2(KH-IDX)  +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               IF  KH-IDX2  =  1
                COMPUTE RECE62-SYUTANTEN-NYUTOK2   =
                        RECE62-SYUTANTEN-NYUTOK2   +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
               IF  FLG-SYUKEI  =  ZERO
                COMPUTE RECE62-ALLTENSU-NYUTOK2    =
                        RECE62-ALLTENSU-NYUTOK2    +
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
               END-IF
              END-IF
           END-IF
      *
           .
       20011261-SRYACCT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200113-RECE62-HENSYU3-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   WRK-DAY1-NYUTENTTLKBN(IDY) = 903
                              AND  NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                              MOVE    10    TO    IDX1
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院略称の取得（夜間看護体制特定日減算）
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                        UNTIL  (IDX1                  >  10 )
                         OR    (RECE62-HKNCOMBI(IDX1) = ZERO)
                IF   (WRK-DAY1(IDY)               NOT = ZERO )
                AND  (RECE62-KANGOGEN-SRYCD(IDY)  NOT = SPACE)
                  IF  WRK-DAY5(IDY) =  RECE62-HKNCOMBI(IDX1)
                     MOVE      RECE62-KANGOGEN-SRYCD(IDY)
                                 TO    WRK-SRYCD
                     PERFORM   2001134-YAGEN-GET3-SEC
                     MOVE  10    TO    IDX1
                  END-IF
                ELSE
                  MOVE     10    TO    IDX1
                END-IF
              END-PERFORM
           END-PERFORM
      *
      *    入院略称の取得（特定時間退院減算・特定曜日入退院減算）
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                        UNTIL  (IDX1                  >  10 )
                         OR    (RECE62-HKNCOMBI(IDX1) = ZERO)
                IF   (WRK-DAY1(IDY)   NOT  =  ZERO                    )
      ***       AND  (WRK-DAY2(IDY)        =  8 OR 9 OR 11 OR 13 OR 17)
                AND  (WRK-DAY2(IDY)        =  8 OR 9 OR 13 OR 17      )
                  IF  WRK-DAY5(IDY) =  RECE62-HKNCOMBI(IDX1)
                     PERFORM   2001133-GENSAN-GET3-SEC
                     MOVE  10    TO    IDX1
                  END-IF
                ELSE
                  MOVE     10    TO    IDX1
                END-IF
              END-PERFORM
           END-PERFORM
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                       IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
                       OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   (NACCT-ZAITEN    NOT =    ZERO)  
                              AND  (NACCT-DAY(IDY)  NOT =    ZERO)  
                              AND  ((WRK-DAY2(IDY)  =    ZERO   ) OR
                                    (WRK-DAY2(IDY) >=     4     ))
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                              MOVE    10    TO    IDX1
                         END-IF
                       ELSE
                         MOVE     10        TO    IDX1
                       END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                 MOVE    "key7"               TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF     (ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1))
                       AND    (ACCT-ZAIKBN     =  ZERO               )
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                             IF   ACCT-SRYKBN  =  "90" OR "92"  
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                  UNTIL    ACCT-IDX   >   31
                                IF  ((WRK-DAY1(ACCT-IDX) NOT = ZERO ) OR
                                     (RECE62-NYUINDAY-KBN(ACCT-IDX)
                                                               = "1"))
      ***                       AND ( WRK-DAY5(ACCT-IDX) =
      ***                                  RECE62-HKNCOMBI (IDX1)    )
                                AND ( ACCT-DAY(1 ACCT-IDX) NOT = ZERO)
                                        PERFORM 2001132-ACCTKHN-GET3-SEC
                                        MOVE  31  TO  ACCT-IDX
                                END-IF
                               END-PERFORM
                             END-IF
                           END-IF
                           MOVE    10    TO    IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"     TO      MCP-TABLE
                   MOVE    "key33"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key33"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    未実施減算（システム管理設定分）
      *
           IF   FLG-MIJISI-GENSAN   =   1
      *
               IF   FLG-MIJISI-INNAI   =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190080570"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190799770"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 MOVE    ZERO              TO   WRK-TOKUREI-FLG
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-IRYO    =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111690"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190809090"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 MOVE    ZERO              TO   WRK-TOKUREI-FLG
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-JYOKU   =   1
      ***        IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111790"    TO   WRK-SRYCD
      ***        ELSE
      ***           MOVE    "190809190"    TO   WRK-SRYCD
      ***        END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 MOVE    ZERO              TO   WRK-TOKUREI-FLG
                 PERFORM   500-RYAKUSYO-HENSYU-SEC
               END-IF
      *
           END-IF
      *
      *    特別な関係にある他院
      *
           IF   (RECE62-TAINFLG          =  "1"     )
           AND  (RECE62-SRYYM           >=  "201804")
             MOVE     ZERO     TO    FLG-ARI
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL  (ID1                    >  20  )
                        OR    (RECE62-NYUSYUBETU(ID1) = SPACE)
                        OR    (FLG-ARI                =   1  )
               IF  RECE62-NYUSYUBETU(ID1)(1:4) = "特別"
                 MOVE  1   TO   FLG-ARI
               END-IF
             END-PERFORM
             IF    FLG-ARI  =  ZERO
             AND   ID1     <=   20
               MOVE   "特別"   TO   RECE62-NYUSYUBETU(ID1)(1:4)
             END-IF
           END-IF
      *
           .
       200113-RECE62-HENSYU3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得
      **************************************************************
       2001131-NYUINKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinact"   TO   MCP-TABLE
               MOVE    "key9"           TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
      *
                 PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                      UNTIL  (NYUINACT-IDX       >   5)    OR
                             (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
      *
                       MOVE    ZERO           TO  WRK-TOKUREI-FLG
                       MOVE    NSRY-SRYCD(NYUINACT-IDX) 
                                              TO  WRK-SRYCD
                                                  WRK-TOKUREI-SRYCD2
                       MOVE    SPACE          TO  WRK-TOKUREI-SRYCD
                       IF   NACCT-ZAISKBKBN    =  "1"
                       AND  NYUINACT-IDX       =   1
                         PERFORM  500-RYAKUSYO-KEISO-CHECK-SEC
                       END-IF
                       IF   WRK-TOKUREI-SRYCD  NOT = SPACE
                         MOVE     WRK-TOKUREI-SRYCD
                                              TO  WRK-SRYCD
                       END-IF
      *
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD-TAIHI(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD-TAIHI(7:2)
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                 END-PERFORM
      *
                 MOVE    "tbl_nyuinact"     TO   MCP-TABLE
                 MOVE    "key9"             TO   MCP-PATHNAME
                 PERFORM        900-NYUINACT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001131-NYUINKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（診療会計）
      **************************************************************
       2001132-ACCTKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                       MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-SRYCD
                       MOVE    ACCT-SRYYM         TO  WRK-SRYYMD(1:6)
                       MOVE    "01"               TO  WRK-SRYYMD(7:2)
      *
                       MOVE    ZERO               TO  WRK-TOKUREI-FLG
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001132-ACCTKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（特定時間退院減算・特定曜日入退院減算）
      **************************************************************
       2001133-GENSAN-GET3-SEC           SECTION.
      *
           EVALUATE    WRK-DAY2(IDY)
              WHEN          8
      ***     WHEN         11
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL  (ID1                    >  20  )
                            OR    (RECE62-NYUSYUBETU(ID1) = SPACE)
                            OR    (FLG-ARI                =   1  )
                   IF  RECE62-NYUSYUBETU(ID1)(1:6)  =  "午前減"
                     MOVE     1      TO   FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI  =  ZERO
                 AND   ID1     <=   20
                     MOVE  "午前減"  TO   RECE62-NYUSYUBETU(ID1)(1:6)
                 END-IF
              WHEN          9
              WHEN         13
              WHEN         17
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL  (ID1                    >  20  )
                            OR    (RECE62-NYUSYUBETU(ID1) = SPACE)
                            OR    (FLG-ARI                =   1  )
                   IF  RECE62-NYUSYUBETU(ID1)(1:6)  =  "土日減"
                     MOVE     1      TO   FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI  =  ZERO
                 AND   ID1     <=   20
                     MOVE  "土日減"  TO   RECE62-NYUSYUBETU(ID1)(1:6)
                 END-IF
           END-EVALUATE
      *
           .
       2001133-GENSAN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（夜間看護体制特定日減算）
      **************************************************************
       2001134-YAGEN-GET3-SEC           SECTION.
      *
           MOVE     ZERO     TO    FLG-ARI
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL  (ID1                    >  20  )
                      OR    (RECE62-NYUSYUBETU(ID1) = SPACE)
                      OR    (FLG-ARI                =   1  )
             EVALUATE   WRK-SRYCD
      *        夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190200970"
                 IF  RECE62-NYUSYUBETU(ID1)(1:14)
                                             = "一般夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        療養病棟・夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190214470"
                 IF  RECE62-NYUSYUBETU(ID1)(1:14)
                                             = "一般夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（結核病棟入院基本料）
               WHEN     "190205270"
                 IF  RECE62-NYUSYUBETU(ID1)(1:12)
                                             = "結夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（精神病棟入院基本料）
               WHEN     "190205370"
                 IF  RECE62-NYUSYUBETU(ID1)(1:12)
                                             = "精夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（専門病院入院基本料）
               WHEN     "190205470"
                 IF  RECE62-NYUSYUBETU(ID1)(1:12)
                                             = "専夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（障害者施設等入院基本料）
               WHEN     "190205870"
                 IF  RECE62-NYUSYUBETU(ID1)(1:12)
                                             = "障夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（地域包括ケア病棟入院料）
               WHEN     "190212670"
                 IF  RECE62-NYUSYUBETU(ID1)(1:12)
                                             = "包夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
             END-EVALUATE
           END-PERFORM
           IF    FLG-ARI  =  ZERO
           AND   ID1     <=   20
             EVALUATE   WRK-SRYCD
      *        夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190200970"
                 MOVE   "一般夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:14)
      *        療養病棟・夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190214470"
                 MOVE   "一般夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:14)
      *        夜間看護体制特定日減算（結核病棟入院基本料）
               WHEN     "190205270"
                 MOVE   "結夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（精神病棟入院基本料）
               WHEN     "190205370"
                 MOVE   "精夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（専門病院入院基本料）
               WHEN     "190205470"
                 MOVE   "専夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（障害者施設等入院基本料）
               WHEN     "190205870"
                 MOVE   "障夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（地域包括ケア病棟入院料）
               WHEN     "190212670"
                 MOVE   "包夜看特定減"
                            TO   RECE62-NYUSYUBETU(ID1)(1:12)
             END-EVALUATE
           END-IF
      *
           .
       2001134-YAGEN-GET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200114-RECE62-HENSYU4-SEC                SECTION.
      *
      **   IF    WRK-SHOKUJIRYOYO   NOT =  ZERO
      *       食事略称の取得
              PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >       31
      *
                 PERFORM  VARYING  IDX1    FROM    1   BY  1
                                UNTIL   IDX1    >       10
                                OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                  IF  WRK-DAY1(IDY) NOT =  ZERO 
                    IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                         IF   (WRK-DAY4(IDY)   NOT =    ZERO    )
                         AND  ((WRK-DAY2(IDY)  =    ZERO       ) OR
                               (WRK-DAY2(IDY) >=     4         ))
                         AND  (WRK-SHOKUJIRYOYO(IDY)  NOT = ZERO)
                              PERFORM   201141-NYUINKHN-GET4-SEC
                         END-IF
                         MOVE    10    TO    IDX1
                    END-IF
                  ELSE
                    MOVE     10        TO    IDX1
                  END-IF
      *
                 END-PERFORM
      *
              END-PERFORM
      **   END-IF
      *
           .
       200114-RECE62-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称の取得
      **************************************************************
       201141-NYUINKHN-GET4-SEC           SECTION.
      *
           EVALUATE   WRK-DAY4(IDY)
             WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                  WHEN  2
                     MOVE    "197000710"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                END-EVALUATE
             WHEN  2
             WHEN  3
             WHEN  4
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  2011411-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  2011411-NYUINKHN-SET4-SEC
                     END-IF
           END-EVALUATE
      *
           .
       201141-NYUINKHN-GET4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称のセット
      **************************************************************
       2011411-NYUINKHN-SET4-SEC           SECTION.
      *
           MOVE    ZERO           TO   WRK-TOKUREI-FLG
           MOVE    SPACE          TO   NYURYAKUSHO-REC
           INITIALIZE                  NYURYAKUSHO-REC
           MOVE    WRK-SRYCD      TO   NYURYAKUSHO-SRYCD
           MOVE    RECE62-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
           MOVE    "01"           TO   NYURYAKUSHO-YUKOSTYMD(7:2)
           MOVE    NYURYAKUSHO-YUKOSTYMD
                                  TO   NYURYAKUSHO-YUKOEDYMD
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 3
                         OR    RECE62-SHOKUJISYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-SHOKUJISYUBETU(ID1)  
                               =  NYURYAKUSHO-RYAKUSHO
                        MOVE  1   TO   FLG-ARI
                    END-IF
                END-PERFORM  
                IF  FLG-ARI  =  ZERO  
                    MOVE   NYURYAKUSHO-RYAKUSHO
                             TO   RECE62-SHOKUJISYUBETU(ID1)
                END-IF
           END-IF                  
      *
           .
       2011411-NYUINKHN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200115-RECE62-HENSYU9-SEC                SECTION.
      *
      *     点滴薬剤（器材）まとめ処理
           PERFORM    2001151-TENTEK-MATOME-SEC
      *
           INITIALIZE    WRK-TENTEK-FLG-TABLE
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key33"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           MOVE   IDX1          TO   TENTEK-IDX
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               PERFORM   500-KUMIAWASE-HANBETU-SEC
                               PERFORM   2001152-RECE62-TENSOU9-SEC
                           END-IF
                           MOVE   10            TO   IDX1
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryacct"        TO      MCP-TABLE
                   MOVE    "key33"              TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key33"              TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    療養担当手当集計
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
             IF   (WRK-DAY1(IDY)   NOT  =  ZERO  )
             OR   (RECE62-NYUINDAY-KBN(IDY) = "1")
      *
                PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1     >     10
                           OR     RECE62-HKNCOMBI (IDX1)  =   ZERO
      *
                   IF   WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                      IF  (WRK-DAY7(IDY)          NOT =  ZERO)
                      AND (WRK-DAY7-ZAITEN(IDY)   NOT =  ZERO)
                      AND ((WRK-DAY2(IDY)         =    ZERO ) OR
                           (WRK-DAY2(IDY)        >=     4   ))
                           PERFORM 500-KUMIAWASE-HANBETU-SEC
                           PERFORM 2001153-RECE62-TENSOU9-RYO-SEC
                      END-IF
                      MOVE    10    TO     IDX1
                   END-IF
      *
                END-PERFORM
      *
             END-IF
      *
           END-PERFORM
      *
           .
       200115-RECE62-HENSYU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001152-RECE62-TENSOU9-SEC                 SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                 OR "96" OR "97"
                   CONTINUE
               ELSE
                 IF       (ACCT-SRYCDTOTAL    =    630010002)
      *                                       OR   190080470
      *                                       OR   190799670
      *                                       OR   190080570
      *                                       OR   190799770
      *                                       OR   190111690
      *                                       OR   190809090
      *                                       OR   190111790
      *                                       OR   190809190
                 OR       (ACCT-ZAIKBN        =    2        )
                   COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                             TBL-TOTALTEN-ACCT(1) -
                             (ACCT-ZAITEN  *  
                              ACCT-ZAIKAISU)
                   COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                             TBL-TOTALTEN2-ACCT(1) -
                             (ACCT-ZAITEN  *  
                              ACCT-ZAIKAISU)
                 ELSE
                   IF        WRK-TENTEK-FLG(TENTEK-IDX 1)  =   ZERO
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                       MOVE  1        TO   WRK-TENTEK-FLG(TENTEK-IDX 1)
                   END-IF
                   IF        ACCT-SRYKBN     =    "33"
                       PERFORM   20011521-TENTEK-SYUTEN-SEC
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-SYUTEN
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 WRK-TENTEK-SYUTEN
                   ELSE
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 (ACCT-ZAITEN  *  
                                  ACCT-ZAIKAISU)
                       COMPUTE   TBL-TOTALTEN2-ACCT(1) =  
                                 TBL-TOTALTEN2-ACCT(1) +
                                 (ACCT-ZAITEN  *  
                                  ACCT-ZAIKAISU)
                   END-IF
                 END-IF
               END-IF
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                     OR "96" OR "97"
                       CONTINUE
                   ELSE
                     IF       (ACCT-SRYCDTOTAL    =    630010002)
      *                                           OR   190080470
      *                                           OR   190799670
      *                                           OR   190080570
      *                                           OR   190799770
      *                                           OR   190111690
      *                                           OR   190809090
      *                                           OR   190111790
      *                                           OR   190809190
                     OR       (ACCT-ZAIKBN        =    2        )
                       IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                          CONTINUE
                       ELSE
                          COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                   TBL-TOTALTEN-ACCT(KH-IDX) -
                                   (ACCT-ZAITEN * 
                                    ACCT-ZAIKAISU)
                       END-IF
                       COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                TBL-TOTALTEN2-ACCT(KH-IDX) -
                                (ACCT-ZAITEN * 
                                 ACCT-ZAIKAISU)
                     ELSE
                       IF        WRK-TENTEK-FLG(TENTEK-IDX KH-IDX)
                                                             =   ZERO
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE   TBL-TOTALTEN-ACCT(KH-IDX) =  
                                        TBL-TOTALTEN-ACCT(KH-IDX) +
                                        WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                        WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                           END-IF
                           COMPUTE   TBL-TOTALTEN2-ACCT(KH-IDX) =  
                                     TBL-TOTALTEN2-ACCT(KH-IDX) +
                                     WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                     WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                           MOVE  1       TO   WRK-TENTEK-FLG
                                                   (TENTEK-IDX KH-IDX)
                       END-IF
                       IF        ACCT-SRYKBN     =    "33"
                           PERFORM   20011521-TENTEK-SYUTEN-SEC
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                       TBL-TOTALTEN-ACCT(KH-IDX) +
                                       WRK-TENTEK-SYUTEN
                           END-IF
                           COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                    TBL-TOTALTEN2-ACCT(KH-IDX) +
                                    WRK-TENTEK-SYUTEN
                       ELSE
                           IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                              CONTINUE
                           ELSE
                              COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                       TBL-TOTALTEN-ACCT(KH-IDX) +
                                      (ACCT-ZAITEN * 
                                       ACCT-ZAIKAISU)
                           END-IF
                           COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                                    TBL-TOTALTEN2-ACCT(KH-IDX) +
                                   (ACCT-ZAITEN * 
                                    ACCT-ZAIKAISU)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
               END-IF  
           END-PERFORM
      *
           .
       2001152-RECE62-TENSOU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の薬剤（器材）点数算定
      *****************************************************************
       2001151-TENTEK-MATOME-SEC                    SECTION.
      *
           INITIALIZE                  WRK-TENTEK-TABLE
      *
           PERFORM     VARYING   TENTEK-IDX  FROM  1  BY  1
                         UNTIL  (TENTEK-IDX  >  10)    OR
                                (RECE62-HKNCOMBI(TENTEK-IDX) = ZERO)
               INITIALIZE                  ORCSTENTEKAREA
               MOVE    "2"                 TO  ORCSTEN-KBN
               MOVE    RECE62-PTID        TO  ORCSTEN-PTID
               MOVE    RECE62-SRYYM       TO  ORCSTEN-SRYYM
               MOVE    RECE62-HKNCOMBI(TENTEK-IDX)
                                          TO  ORCSTEN-HKNCOMBI
               MOVE    RECE62-HOSPNUM     TO  SPA-HOSPNUM
               CALL    "ORCSTENTEK"        USING
                                           SPA-AREA
                                           ORCSTENTEKAREA
      *        薬剤点数と器材点数
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       31
      *
                        IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                            ADD    ORCSTEN-ZAITEN(IDX1)
                                   TO  WRK-TENTEK-YKZTEN(TENTEK-IDX)
                        END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       50
                           OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                        MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                        PERFORM  VARYING  IDX2    FROM    1   BY  1
                              UNTIL   IDX2     >       31
                            ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)
                                   TO  WRK-TENTEK-ZAIKAISU
                        END-PERFORM
      *
                        COMPUTE  WRK-TENTEK-KIZAITEN(TENTEK-IDX)  =
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)  +
                                 ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                   WRK-TENTEK-ZAIKAISU ) 
      *
               END-PERFORM
           END-PERFORM
      *
           .
       2001151-TENTEK-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の手技点数算定
      *****************************************************************
       20011521-TENTEK-SYUTEN-SEC                    SECTION.
      *
      *診療行為テーブルから診療種別区分の取得
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM       TO   SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
      ***  MOVE    ACCT-SRYKA         TO   SRY-SRYKA
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
                 MOVE    "tbl_sryact"     TO   MCP-TABLE
                 MOVE    "key9"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
           ELSE
                 MOVE    1                TO   FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                   TO   WRK-TENTEK-SYUTEN
      *
           IF   FLG-SRYACT   =    ZERO
                EVALUATE  SRY-SRYSYUKBN
                    WHEN  "330"
                    WHEN  "331"
                    WHEN  "332"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-SYUTEN1 * ACCT-ZAIKAISU)
                    WHEN  "340"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-ZAITEN * ACCT-ZAIKAISU)
                END-EVALUATE
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20011521-TENTEK-SYUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養担当手当転送
      *****************************************************************
       2001153-RECE62-TENSOU9-RYO-SEC               SECTION.
      *
      ***  IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
           IF    (RECE62-HKNNUMHC(IDX1)  NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
      *
                 COMPUTE  TBL-TOTALTEN-ACCT(1) =
                          TBL-TOTALTEN-ACCT(1) +
                          WRK-DAY7-ZAITEN(IDY)
                 COMPUTE  TBL-TOTALTEN2-ACCT(1) =
                          TBL-TOTALTEN2-ACCT(1) +
                          WRK-DAY7-ZAITEN(IDY)
      *
           END-IF
      *
           MOVE       IDX1              TO   HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                       UNTIL   (IDX2      >       4)
               MOVE        ZERO          TO        KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                 AND  RECE62-KOHIDHC(HC-IDX IDX2)  =  RECE62-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                     IF     TBL-FUKUSHIFLG(IDX2)  NOT =  ZERO
                        CONTINUE
                     ELSE
                        COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                 TBL-TOTALTEN-ACCT(KH-IDX) +
                                 WRK-DAY7-ZAITEN(IDY)
                     END-IF
                     COMPUTE  TBL-TOTALTEN2-ACCT(KH-IDX) =
                              TBL-TOTALTEN2-ACCT(KH-IDX) +
                              WRK-DAY7-ZAITEN(IDY)
                 END-IF
               END-IF
           END-PERFORM
      *
           .
       2001153-RECE62-TENSOU9-RYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       200116-JNISSU-SET-SEC                    SECTION.
      *
      *実日数から減する
           PERFORM    VARYING    IDX    FROM   1   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF  (TBL-JNISSU-DAY(IDX IDY)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDX IDY)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(IDX IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   3   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF  (TBL-JNISSU2-DAY(IDX IDY)     NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDX IDY)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU2-DAY(IDX IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                       UNTIL     IDX    >      5
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY    >      31
                IF   TBL-JNISSU-DAY(IDX IDY)      NOT =  SPACE
                    MOVE   "1"   TO  TBL-JNISSU-DAY-M(IDY)
                END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY(IDX IDY)  NOT =  SPACE
                          ADD    1      TO    RECE62-JNISSU(IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   3   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU2-DAY(IDX IDY) NOT =  SPACE
                          ADD    1      TO    TBL-JNISSU2-MON(IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                      UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY-M(IDY) NOT =  SPACE
                          ADD    1      TO    RECE62-SKYIRYO-NISSU
                    END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY(IDX IDY)  NOT =  SPACE
                          MOVE   "1"    TO    RECE62-JNISSUTBL(IDY)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      20
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUKHNKEI(IDX IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUTOKKEI(IDX IDY)
      *
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI(IDX IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUKHNKEI2(IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             WRK-NYUTOKKEI2(IDY)
      *
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI2(IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI2(IDY)
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             TBL-TOTALTEN-ACCT(IDY)
                    COMPUTE  RECE62-HENTOTALTEN(IDY)  = 
                             RECE62-HENTOTALTEN(IDY)  + 
                             TBL-TOTALTEN2-ACCT(IDY)
           END-PERFORM
      *
           IF   RECE62-SRYYM    >=    "200804"
             CONTINUE
           ELSE
             PERFORM    VARYING    IDX    FROM   1   BY   1
                        UNTIL      IDX    >      20
               PERFORM    VARYING    IDY    FROM    2   BY   1
                          UNTIL      IDY    >     5
                 IF  ( RECE62-HENTOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-HENTOTALTEN(1)   NOT =  
                                         RECE62-HENTOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI(IDX IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI(IDX IDY)
                 END-IF
               END-PERFORM
             END-PERFORM
      *
             PERFORM    VARYING    IDY    FROM    2   BY   1
                        UNTIL      IDY    >     5
                 IF  ( RECE62-HENTOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-HENTOTALTEN(1)   NOT =  
                                         RECE62-HENTOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI2(IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI2(IDY)
                 END-IF
             END-PERFORM
           END-IF
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      20
                 IF   RECE62-NYUTOKRYO(IDX)  =  999999999
                    MOVE    ZERO    TO   RECE62-NYUTOKRYO(IDX)
                 END-IF
                 IF   RECE62-NYUTOKRYO-RYAKU(IDX)  
                                             =  "９９９９９９９９９９"
                    MOVE    SPACE   TO   RECE62-NYUTOKRYO-RYAKU(IDX)
                 END-IF
           END-PERFORM
      *
      ***
      *
           IF    RECE62-FUKUSHIFLG(3)  =  "2"
              IF  (RECE62-KOHNUM(2)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(3)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(3)  TO  RECE62-TOTALTEN(3)
              END-IF
              IF  (RECE62-KOHNUM(2)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(3)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(3)     TO  RECE62-JNISSU(3)
              END-IF
           END-IF
           IF    RECE62-FUKUSHIFLG(4)  =  "2"
              IF  (RECE62-KOHNUM(3)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(4)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(4)  TO  RECE62-TOTALTEN(4)
              END-IF
              IF  (RECE62-KOHNUM(3)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(4)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(4)     TO  RECE62-JNISSU(4)
              END-IF
           END-IF
           IF    RECE62-FUKUSHIFLG(5)  =  "2"
              IF  (RECE62-KOHNUM(4)(1:1) >= "1" AND <= "8")
              AND (RECE62-TOTALTEN(5)     =  ZERO         )
                 MOVE  RECE62-HENTOTALTEN(5)  TO  RECE62-TOTALTEN(5)
              END-IF
              IF  (RECE62-KOHNUM(4)(1:1) >= "1" AND <= "8")
              AND (RECE62-JNISSU(5)       =  ZERO         )
                 MOVE  TBL-JNISSU2-MON(5)     TO  RECE62-JNISSU(5)
              END-IF
           END-IF
      *
           .
       200116-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       200117-RECE62-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE62-REC
      *
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-MOD (IDZ)
      *
           .
       200117-RECE62-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    全国公費併用分請求点集計判別
      *****************************************************************
       200118-SYUKEIKBN-HANBETU-SEC      SECTION.
      *
           IF    IDZ    =    1
      *
             PERFORM  VARYING  IDX11   FROM    1   BY  1
                       UNTIL  (IDX11                   >   10 )
                         OR   (RECE62-HKNCOMBI(IDX11)  =  ZERO)
      *
              IF ((RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)(1:1)  = "0"            ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
              OR ((WRK-PREFNUM                    = "13"           ) AND
                  (RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)(1:1) >= "1" AND <= "8" ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
              OR ((WRK-PREFNUM                    = "23"           ) AND
                  (RECE62-SRYYM                  >= "200910"       ) AND
                  (RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)       = "191"          ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
              OR ((WRK-PREFNUM                    = "23" OR "24"   ) AND
                  (RECE62-SRYYM                  >= "201104"       ) AND
                  (RECE62-SRYYM                  <= "201412"       ) AND
                  (RECE62-HKNNUMHC(IDX11)     NOT = SPACE          ) AND
                  (RECE62-KOHNUMHC(IDX11 1)       = "152"          ) AND
                  (RECE62-KOHNUMHC(IDX11 2)(1:1) >= "1" AND <= "8" ))
      *
                IF ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(2)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(2) ))
                OR ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(3)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(3) ))
                OR ((RECE62-KOHNUMHC(IDX11 2) =  RECE62-KOHNUM(4)) AND
                    (RECE62-KOHIDHC(IDX11 2)  =  RECE62-KOHID(4) ))
      *
                  IF    RECE62-SRYYM  >=  "200804"
                    INITIALIZE                   SYSKANRI-REC
                    MOVE    "2010"           TO  SYS-KANRICD
                    MOVE    RECE62-KOHNUMHC(IDX11 2)
                                             TO  SYS-KBNCD
                    MOVE    RECE62-SRYYM     TO  SYS-STYUKYMD (1:6)
                    MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                    MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                    MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                    MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-DBSELECT-SEC
                    IF      MCP-RC           =   ZERO
                      MOVE  "tbl_syskanri"   TO  MCP-TABLE
                      MOVE  "key10"          TO  MCP-PATHNAME
                      PERFORM 900-SYSKANRI-READ-SEC
                    ELSE
                      INITIALIZE                 SYS-2010-REC
                    END-IF
                    PERFORM   2001181-SYUKEIKBN-HENSYU-SEC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                  ELSE
                    EVALUATE   WRK-PREFNUM
                      WHEN     "08"
                      WHEN     "32"
                      WHEN     "13"
                      WHEN     "26"
                          MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                      WHEN     "30"
                      WHEN     "38"
                          MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                      WHEN     "36"
                          MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "0" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                      WHEN     "10"
                          MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "4" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                    END-EVALUATE
                  END-IF
      *
                  IF ((WRK-PREFNUM               =  "13"    ) AND
                      (RECE62-KOHNUMHC(IDX11 2)  =  "151"   ) AND
                      (RECE62-SRYYM             >=  "200905"))
                  OR ((WRK-PREFNUM               =  "23"    ) AND
                      (RECE62-KOHNUMHC(IDX11 2)  =  "191"   ) AND
                      (RECE62-SRYYM             >=  "200910"))
                  OR ((WRK-PREFNUM               =  "23"
                                                 OR "24"    ) AND
                      (RECE62-KOHNUMHC(IDX11 2)  =  "152"   ) AND
                      (RECE62-SRYYM             >=  "201104") AND
                      (RECE62-SRYYM             <=  "201412"))
                          MOVE "0" TO RECE62-SYUKEIKBN(IDX11 2)
                          MOVE "0" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                  END-IF
      *
                END-IF
      *
              END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           .
       200118-SYUKEIKBN-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全国公費併用分請求点集計判別編集
      *****************************************************************
       2001181-SYUKEIKBN-HENSYU-SEC      SECTION.
      *
           EVALUATE  SYS-2010-SYUKEIKBN
             WHEN      SPACE
                 EVALUATE   WRK-PREFNUM
                   WHEN     "32"
                   WHEN     "13"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                   WHEN     "36"
                       MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "0" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                   WHEN     "10"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "4" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
      ***          WHEN     "38"
      ***            IF  RECE62-SRYYM  <=  "201711"
      ***              MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
      ***              MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
      ***            END-IF
                   WHEN     "08"
                     IF  RECE62-SRYYM  <=  "201002"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "26"
                     IF  RECE62-SRYYM  <=  "201103"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "30"
                     IF  RECE62-SRYYM  <=  "201003"
                       MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                 END-EVALUATE
             WHEN      "0"
                 MOVE   SYS-2010-SYUKEIKBN
                                    TO   RECE62-SYUKEIKBN(IDX11 2)
                 MOVE   SYS-2010-SYUKEI-TAIKBN
                                    TO   RECE62-SYUKEI-TAIKBN(IDX11 2)
                 EVALUATE   WRK-PREFNUM
      ***          WHEN     "38"
      ***            IF  RECE62-SRYYM  <=  "201711"
      ***              MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
      ***              MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
      ***            END-IF
                   WHEN     "08"
                     IF  RECE62-SRYYM  <=  "201002"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "26"
                     IF  RECE62-SRYYM  <=  "201103"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                   WHEN     "30"
                     IF  RECE62-SRYYM  <=  "201003"
                       MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                       MOVE "1" TO RECE62-SYUKEI-TAIKBN(IDX11 2)
                     END-IF
                 END-EVALUATE
             WHEN      "1"
             WHEN      "2"
             WHEN      "3"
             WHEN      "4"
                 MOVE   SYS-2010-SYUKEIKBN
                                    TO   RECE62-SYUKEIKBN(IDX11 2)
                 MOVE   SYS-2010-SYUKEI-TAIKBN
                                    TO   RECE62-SYUKEI-TAIKBN(IDX11 2)
                 IF   RECE62-SYUKEIKBN(IDX11 2) = "1"
                       MOVE "3" TO RECE62-SYUKEIKBN(IDX11 2)
                 END-IF
                 IF   RECE62-SYUKEIKBN(IDX11 2) = "2"
                       MOVE "4" TO RECE62-SYUKEIKBN(IDX11 2)
                 END-IF
           END-EVALUATE
      *
           .
       2001181-SYUKEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせの公費チェック（生保まとめ）
      *****************************************************************
       200119-SEIHO-HKNCOMBI-CHECK-SEC      SECTION.
      *
           IF    IDZ  =  1
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                       UNTIL  (IDX4                  >   10 )
                        OR    (RECE62-HKNCOMBI(IDX4) =  ZERO)
               IF   (RECE62-HKNNUMHC(IDX4)   NOT =   SPACE)
               AND  (RECE62-KOHNUMHC(IDX4 1)     =   SPACE)
               AND  (RECE62-KOHNUMHC(IDX4 2)     =   SPACE)
               AND  (RECE62-KOHNUMHC(IDX4 3)     =   SPACE)
               AND  (RECE62-KOHNUMHC(IDX4 4)     =   SPACE)
                 MOVE    1    TO    RECE62-SYUTANARIFLG
               END-IF
             END-PERFORM
           END-IF
      *
           .
       200119-SEIHO-HKNCOMBI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他記載フラグの編集
      *    マル長レセプト記載処理区分の編集
      *****************************************************************
       20011A-ETCKISAIFLGINF-HEN-SEC      SECTION.
      *
           IF    IDZ    =    1
      *
             PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                         UNTIL    KOHFTN-IDX     >    4
      *
              IF   RECE62-KOHNUM(KOHFTN-IDX)(1:1) >=  "1" AND <=  "8"
      *
                 IF    RECE62-SRYYM        >=  "200804"
                    INITIALIZE                   SYSKANRI-REC
                    MOVE    "2010"           TO  SYS-KANRICD
                    MOVE    RECE62-KOHNUM(KOHFTN-IDX)
                                             TO  SYS-KBNCD
                    MOVE    RECE62-SRYYM     TO  SYS-STYUKYMD (1:6)
                    MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                    MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                    MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                    MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-DBSELECT-SEC
                    IF      MCP-RC           =   ZERO
                      MOVE  "tbl_syskanri"   TO  MCP-TABLE
                      MOVE  "key10"          TO  MCP-PATHNAME
                      PERFORM 900-SYSKANRI-READ-SEC
                    ELSE
                      INITIALIZE                 SYS-2010-REC
                    END-IF
                    PERFORM   20011A1-ETCKSIFLGINF-SET-SEC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                 ELSE
                    PERFORM   20011A2-ETCKSIFLGINF-SET2-SEC
                 END-IF
                 PERFORM      20011A3-ETCKSIFLGINF-KURIA-SEC
      *
              END-IF
      *
             END-PERFORM
      *
      ****
      *
             IF    RECE62-SRYYM  >=  "202001"
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "2010"           TO  SYS-KANRICD
                 MOVE    "972"            TO  SYS-KBNCD
                 MOVE    RECE62-SRYYM     TO  SYS-STYUKYMD (1:6)
                 MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                 MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                 MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key10"          TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                   MOVE  "tbl_syskanri"   TO  MCP-TABLE
                   MOVE  "key10"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                 ELSE
                   INITIALIZE                 SYS-2010-REC
                 END-IF
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key10"          TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
                 IF      SYS-2010-CYOKI-FTNKBN  =  "0"
                 AND     SYS-2010-CYOKI-KSIKBN  =  "1"
                   EVALUATE  SYS-2010-CYOKI-KSIKBN-TAIKBN
                     WHEN      "0"
                         MOVE  "1"  TO  RECE62-CYOKI-KSIKBN
                     WHEN      "1"
                       IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                         MOVE  "1"  TO  RECE62-CYOKI-KSIKBN
                       END-IF
                     WHEN      "2"
                       IF   RECE62-TEISYUTUSAKI  =  1
                         MOVE  "1"  TO  RECE62-CYOKI-KSIKBN
                       END-IF
                     WHEN      "3"
                       IF   RECE62-TEISYUTUSAKI  =  2
                         MOVE  "1"  TO  RECE62-CYOKI-KSIKBN
                       END-IF
                     WHEN      "4"
                       IF   RECE62-TEISYUTUSAKI  =  6
                         MOVE  "1"  TO  RECE62-CYOKI-KSIKBN
                       END-IF
                   END-EVALUATE
                 END-IF
             END-IF
      *
           END-IF
      *
           .
       20011A-ETCKISAIFLGINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他記載フラグセット
      *****************************************************************
       20011A1-ETCKSIFLGINF-SET-SEC               SECTION.
      *
           EVALUATE  SYS-2010-JNISSUKBN
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-JNISSU-TAIKBN
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               MOVE    SPACE   TO RECE62-EKF-JNISSUFLG(KOHFTN-IDX + 1)
               EVALUATE  SYS-2010-JNISSU-TAIKBN
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-MJK-JNISSUFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-MJK-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-MJK-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-MJK-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-MJK-JNISSUFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-TOTALTENKBN
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-TOTALTEN-TAIKBN
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               MOVE    SPACE   TO RECE62-EKF-TOTALTENFLG(KOHFTN-IDX + 1)
               EVALUATE  SYS-2010-TOTALTEN-TAIKBN
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-MJK-TOTALTENFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-MJK-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-MJK-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-MJK-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-MJK-TOTALTENFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-FTNMONEYKBN-N
             WHEN      "0"
                    MOVE SPACE TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-FTNMONEY-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               MOVE    SPACE   TO RECE62-KISAIFLG(KOHFTN-IDX + 1)
               EVALUATE  SYS-2010-FTNMONEY-TAIKBN-N
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-MJK-KISAIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-MJK-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-MJK-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-MJK-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-MJK-KISAIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-FTNZEROKBN-N
             WHEN      "0"
                    MOVE SPACE TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-FTNZERO-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               MOVE    SPACE   TO RECE62-ZEROKISAIFLG(KOHFTN-IDX + 1)
               EVALUATE  SYS-2010-FTNZERO-TAIKBN-N
                 WHEN        "0"
                    MOVE "2" TO RECE62-MJK-ZEROKISAIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE "2" TO RECE62-MJK-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE "2" TO RECE62-MJK-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE "2" TO RECE62-MJK-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE "2" TO RECE62-MJK-ZEROKISAIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-SKJKBN
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-SKJ-TAIKBN
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-SKJFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-OCRKBN
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-OCR-TAIKBN
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               MOVE    SPACE   TO RECE62-EKF-OCRFLG(KOHFTN-IDX + 1)
               EVALUATE  SYS-2010-OCR-TAIKBN
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-MJK-OCRFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-MJK-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-MJK-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-MJK-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-MJK-OCRFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-FTNKEIKBN-N
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "29"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6            )
                  AND (RECE62-KOHHBTNUM(KOHFTN-IDX)
                                    =  "71" OR "81" OR "91" OR "80")
                    MOVE "1" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "32"
                  IF ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "790"  ) AND
                      (RECE62-SRYYM         >=  "201107"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "393"  ) AND
                      (RECE62-SRYYM         >=  "201007"   ) AND
                      (RECE62-SRYYM         <=  "201309"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "493"  ) AND
                      (RECE62-SRYYM         >=  "201107"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =
                         "625" OR "627" OR "628" OR "629"  ) AND
                      (RECE62-SRYYM         >=  "201302"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                    MOVE "1" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "35"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-KOHNUM(KOHFTN-IDX) =  "781"  )
                  AND (RECE62-SRYYM         >=  "201004"   )
                    MOVE "1" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "39"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-KOHHBTNUM(KOHFTN-IDX) =  "72")
                    MOVE "1" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "45"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-KOHNUM(KOHFTN-IDX) =  "190"  )
                    MOVE "2" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
               MOVE SPACE TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
               EVALUATE   WRK-PREFNUM
                 WHEN     "32"
                  IF ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "393"  ) AND
                      (RECE62-SRYYM         >=  "201007"   ) AND
                      (RECE62-SRYYM         <=  "201309"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "790"  ) AND
                      (RECE62-SRYYM         >=  "201107"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =  "493"  ) AND
                      (RECE62-SRYYM         >=  "201107"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                  OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                      (RECE62-KOHNUM(KOHFTN-IDX) =
                         "625" OR "627" OR "628" OR "629"  ) AND
                      (RECE62-SRYYM         >=  "201302"   ) AND
                      (RECE62-SRYYM         <=  "201409"   ))
                    MOVE "1" TO
                          RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "1"
               EVALUATE  SYS-2010-FTNKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-FTNKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-HKNFTNKBN-K
             WHEN      SPACE
               IF  (WRK-PREFNUM               =  "42" )
               AND (RECE62-KOHNUM(KOHFTN-IDX) =  "186")
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
               ELSE
                 IF   RECE62-TEISYUTUSAKI   =   1
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                 END-IF
               END-IF
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-HKNFTN-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
           IF  SYS-2010-HKNFTNKBN-K = "0"
               MOVE  "0"  TO RECE62-EKF-HKNFTNFLG-K2(KOHFTN-IDX + 1)
           END-IF
      *
           EVALUATE  SYS-2010-HKNFTNKBN-Y
             WHEN      SPACE
               IF  (WRK-PREFNUM               =  "42" )
               AND (RECE62-KOHNUM(KOHFTN-IDX) =  "186")
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
               ELSE
                 IF   RECE62-TEISYUTUSAKI   =   1
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                 END-IF
               END-IF
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-HKNFTN-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
           IF  SYS-2010-HKNFTNKBN-Y = "0"
               MOVE  "0"  TO RECE62-EKF-HKNFTNFLG-Y2(KOHFTN-IDX + 1)
           END-IF
      *
      *@@@
      *
           EVALUATE  SYS-2010-KAKKBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "01"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
                  AND (RECE62-SRYYM             >=  "200810")
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK31KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "38"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "10"
                 WHEN     "23"
                 WHEN     "27"
                 WHEN     "28"
                 WHEN     "34"
                 WHEN     "45"
                  IF  (RECE62-TEISYUTUSAKI   =   6         )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK31-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK32KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "34"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "38"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
             WHEN      "3"
               EVALUATE  SYS-2010-KAK32-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-KAK32-TAIKBN-K
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK41KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "23"
                  IF  (RECE62-TEISYUTUSAKI   =   6         )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK41-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK42KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK42-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK43KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK43-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK21KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "38"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "23"
                 WHEN     "28"
                  IF  (RECE62-TEISYUTUSAKI   =   6         )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK21-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK22KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK22-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK23KBN-K
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                 WHEN     "38"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK23-TAIKBN-K
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-K(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
      *@@@
      *
           EVALUATE  SYS-2010-KAKKBN-Y
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "01"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
                  AND (RECE62-SRYYM             >=  "200810")
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK31KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK31-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK31KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK32KBN-Y
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "34"
                  IF  (RECE62-TEISYUTUSAKI   =   2         )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK32-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-KAK32-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-EKF-KAK32KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK41KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK41-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK41KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK42KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK42-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK42KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK43KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK43-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK43KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK21KBN-Y
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "27"
                  IF  (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK21-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK21KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK22KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK22-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK22KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KAK23KBN-Y
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KAK23-TAIKBN-Y
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KAK23KBN-Y(KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
      *@@@
      *
           EVALUATE  SYS-2010-KSINGKBN-K-N
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KSING-TAIKBN-K-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KSINGKBN-Y-N
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KSING-TAIKBN-Y-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KSINGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-ENCHGKBN-K-N
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "36"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
                  AND (RECE62-TEISYUTUSAKI   =   2 OR 6     )
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "01"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
                  AND (RECE62-SRYYM             >=  "200810")
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "27"
                  IF   RECE62-SRYYM             >=  "200804"
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "33"
                  IF  (RECE62-SRYYM             >=  "200610")
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "13"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                         "180" OR "380" OR "181" OR "381"   )
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-K-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-K-N
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-K-N
                 WHEN        "0"
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-ENCHGKBN-Y-N
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "36"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
                  AND (RECE62-TEISYUTUSAKI   =   2          )
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "01"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
                  AND (RECE62-SRYYM             >=  "200810")
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "27"
                  IF   RECE62-SRYYM             >=  "200804"
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "13"
                  IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                         "180" OR "380" OR "181" OR "381"   )
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-Y-N
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-Y-N
                 WHEN        "0"
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "2"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-ENCHG-TAIKBN-Y-N
                 WHEN        "0"
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KOH2SKJKBN
             WHEN      SPACE
               EVALUATE   WRK-PREFNUM
                 WHEN     "15"
                  IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN     "10"
                  IF  (RECE62-TEISYUTUSAKI   =   6         )
                  AND (RECE62-SRYYM         >=  "200804"   )
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
             WHEN      "1"
               EVALUATE  SYS-2010-KOH2SKJ-TAIKBN
                 WHEN        "0"
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                 WHEN        "1"
                  IF   RECE62-TEISYUTUSAKI  =  2 OR 6
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "2"
                  IF   RECE62-TEISYUTUSAKI  =  1
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "3"
                  IF   RECE62-TEISYUTUSAKI  =  2
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
                 WHEN        "4"
                  IF   RECE62-TEISYUTUSAKI  =  6
                    MOVE  "1"  TO RECE62-EKF-KOH2SKJKBN
                                                     (KOHFTN-IDX + 1)
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
      *@@@
      *
           EVALUATE  SYS-2010-FTNHASUKBN-N
             WHEN      "0"
                    MOVE SPACE TO RECE62-EKF-FTNHASUKBN(KOHFTN-IDX + 1)
             WHEN      "1"
                    MOVE  "1"  TO RECE62-EKF-FTNHASUKBN(KOHFTN-IDX + 1)
           END-EVALUATE
      *
      *@@@
      *
           IF   (RECE62-SRYYM   >=  "200604")
           AND  (WRK-PREFNUM     =  "04"    )
             EVALUATE  SYS-2010-SKJRYOKBN-HON
               WHEN      SPACE
                 IF ((RECE62-KOHNUM(KOHFTN-IDX) = "183" OR "483") AND
                     (RECE62-SRYYM             >= "200510"      ))
                 OR ((RECE62-KOHNUM(KOHFTN-IDX) = "583" OR "783") AND
                     (RECE62-SRYYM             >= "201010"      ))
                 OR ((RECE62-KOHNUM(KOHFTN-IDX) =
                                "263" OR "363" OR "463" OR "563") AND
                     (RECE62-SRYYM             >= "201310"      ))
                   MOVE  "2"  TO  RECE62-SKJRYOKBN-HON(KOHFTN-IDX + 1)
                 END-IF
               WHEN      "0"
               WHEN      "1"
               WHEN      "2"
               WHEN      "3"
                   MOVE  SYS-2010-SKJRYOKBN-HON
                              TO  RECE62-SKJRYOKBN-HON(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           .
       20011A1-ETCKSIFLGINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他記載フラグセット２
      *****************************************************************
       20011A2-ETCKSIFLGINF-SET2-SEC               SECTION.
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "29"
               IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
               AND (RECE62-KOHHBTNUM(KOHFTN-IDX)
                                 =  "71" OR "81" OR "91")
                 MOVE "1" TO
                       RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
               END-IF
             WHEN     "39"
               IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
               AND (RECE62-KOHHBTNUM(KOHFTN-IDX) =  "72")
               AND (RECE62-SRYYM         >=  "200510"   )
                 MOVE "1" TO
                       RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
               END-IF
           END-EVALUATE
           IF  (WRK-PREFNUM               =  "42" )
           AND (RECE62-KOHNUM(KOHFTN-IDX) =  "186")
               MOVE "1" TO
                     RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
               MOVE "1" TO
                     RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
           ELSE
             IF   RECE62-TEISYUTUSAKI   =   1
               MOVE "1" TO
                     RECE62-EKF-HKNFTNFLG-K(KOHFTN-IDX + 1)
               MOVE "1" TO
                     RECE62-EKF-HKNFTNFLG-Y(KOHFTN-IDX + 1)
             END-IF
           END-IF
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "36"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
              AND (RECE62-TEISYUTUSAKI   =   2 OR 6     )
              AND (RECE62-SRYYM             >=  "200604")
                MOVE  "1"  TO RECE62-EKF-ENCHGKBN-K
                                                 (KOHFTN-IDX + 1)
              END-IF
             WHEN     "33"
              IF  (RECE62-SRYYM             >=  "200610")
                MOVE  "2"  TO RECE62-EKF-ENCHGKBN-K
                                                 (KOHFTN-IDX + 1)
              END-IF
             WHEN     "13"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                     "180" OR "380" OR "181" OR "381"   )
              AND (RECE62-SRYYM             >=  "200604")
                MOVE  "3"  TO RECE62-EKF-ENCHGKBN-K
                                                 (KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "36"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
              AND (RECE62-TEISYUTUSAKI   =   2          )
              AND (RECE62-SRYYM             >=  "200604")
                MOVE  "1"  TO RECE62-EKF-ENCHGKBN-Y
                                                 (KOHFTN-IDX + 1)
              END-IF
             WHEN     "13"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                     "180" OR "380" OR "181" OR "381"   )
              AND (RECE62-SRYYM             >=  "200604")
                MOVE  "3"  TO RECE62-EKF-ENCHGKBN-Y
                                                 (KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
      *@@@
      *
           IF   (RECE62-SRYYM   >=  "200604")
           AND  (WRK-PREFNUM     =  "04"    )
             EVALUATE  SYS-2010-SKJRYOKBN-HON
               WHEN      SPACE
                 IF ((RECE62-KOHNUM(KOHFTN-IDX) = "183" OR "483") AND
                     (RECE62-SRYYM             >= "200510"      ))
                 OR ((RECE62-KOHNUM(KOHFTN-IDX) = "583" OR "783") AND
                     (RECE62-SRYYM             >= "201010"      ))
                 OR ((RECE62-KOHNUM(KOHFTN-IDX) =
                                "263" OR "363" OR "463" OR "563") AND
                     (RECE62-SRYYM             >= "201310"      ))
                   MOVE  "2"  TO  RECE62-SKJRYOKBN-HON(KOHFTN-IDX + 1)
                 END-IF
               WHEN      "0"
               WHEN      "1"
               WHEN      "2"
               WHEN      "3"
                   MOVE  SYS-2010-SKJRYOKBN-HON
                              TO  RECE62-SKJRYOKBN-HON(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           .
       20011A2-ETCKSIFLGINF-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他記載フラグセットクリア
      *****************************************************************
       20011A3-ETCKSIFLGINF-KURIA-SEC               SECTION.
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "29"
              IF  (RECE62-TEISYUTUSAKI   =   2 OR 6   )
              AND (RECE62-KOHHBTNUM(KOHFTN-IDX) = "80")
              AND (RECE62-SRYYM          <  "201410"  )
                MOVE  SPACE TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
              END-IF
             WHEN     "32"
              IF ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                  (RECE62-KOHNUM(KOHFTN-IDX) =  "790"  ) AND
                  (RECE62-SRYYM          <  "201107"   ))
              OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                  (RECE62-KOHNUM(KOHFTN-IDX) =  "493"  ) AND
                  (RECE62-SRYYM          <  "201107"   ))
              OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                  (RECE62-KOHNUM(KOHFTN-IDX) =
                     "625" OR "627" OR "628" OR "629"  ) AND
                  (RECE62-SRYYM          <  "201302"   ))
              OR ((RECE62-TEISYUTUSAKI   =   2 OR 6    ) AND
                  (RECE62-KOHNUM(KOHFTN-IDX) =  "393"  ) AND
                  (RECE62-SRYYM          <  "201007"   ))
                MOVE  SPACE TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
              END-IF
             WHEN     "35"
              IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
              AND (RECE62-KOHNUM(KOHFTN-IDX) =  "781"  )
              AND (RECE62-SRYYM          <  "201004"   )
                MOVE  SPACE TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
              END-IF
             WHEN     "39"
              IF  (RECE62-TEISYUTUSAKI   =   2 OR 6    )
              AND (RECE62-KOHHBTNUM(KOHFTN-IDX) =  "72")
              AND (RECE62-SRYYM          <  "200510"   )
                MOVE  SPACE TO RECE62-EKF-FTNKEIFLG(KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "01"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
              AND (RECE62-SRYYM              <  "200810")
                MOVE  SPACE TO RECE62-EKF-KAKKBN-K(KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "01"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
              AND (RECE62-SRYYM              <  "200810")
                MOVE  SPACE TO RECE62-EKF-KAKKBN-Y(KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "36"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
              AND (RECE62-TEISYUTUSAKI   =   2 OR 6     )
              AND (RECE62-SRYYM              <  "200604")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-K
                                                  (KOHFTN-IDX + 1)
              END-IF
             WHEN     "01"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
              AND (RECE62-SRYYM              <  "200810")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-K
                                                  (KOHFTN-IDX + 1)
              END-IF
             WHEN     "33"
              IF  (RECE62-SRYYM              <  "200610")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-K
                                                  (KOHFTN-IDX + 1)
              END-IF
             WHEN     "13"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                     "180" OR "380" OR "181" OR "381"   )
              AND (RECE62-SRYYM              <  "200604")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-K
                                                  (KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
           EVALUATE   WRK-PREFNUM
             WHEN     "36"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "245"   )
              AND (RECE62-TEISYUTUSAKI   =   2          )
              AND (RECE62-SRYYM              <  "200604")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-Y
                                                  (KOHFTN-IDX + 1)
              END-IF
             WHEN     "01"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =  "183"   )
              AND (RECE62-SRYYM              <  "200810")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-Y
                                                  (KOHFTN-IDX + 1)
              END-IF
             WHEN     "13"
              IF  (RECE62-KOHNUM(KOHFTN-IDX) =
                     "180" OR "380" OR "181" OR "381"   )
              AND (RECE62-SRYYM              <  "200604")
                MOVE  SPACE TO RECE62-EKF-ENCHGKBN-Y
                                                  (KOHFTN-IDX + 1)
              END-IF
           END-EVALUATE
      *
      *@@@
      *
           IF   (RECE62-SRYYM   >=  "200604")
           AND  (WRK-PREFNUM     =  "04"    )
              IF ((RECE62-KOHNUM(KOHFTN-IDX) =  "183" OR "483") AND
                  (RECE62-SRYYM              < "200510"       ))
              OR ((RECE62-KOHNUM(KOHFTN-IDX) =  "583" OR "783") AND
                  (RECE62-SRYYM              <  "201010"      ))
              OR ((RECE62-KOHNUM(KOHFTN-IDX) =
                              "263" OR "363" OR "463" OR "563") AND
                  (RECE62-SRYYM              <  "201310"      ))
                MOVE  "0"  TO RECE62-SKJRYOKBN-HON(KOHFTN-IDX + 1)
              END-IF
           END-IF
      *
           .
       20011A3-ETCKSIFLGINF-KURIA-EXT.
           EXIT.
      *
      *****************************************************************
      *    法定外現物給付の設定編集
      *****************************************************************
       20011B-HOTEIGAI-HEN-SEC      SECTION.
      *
           IF    IDZ    =    1
      *
             PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                         UNTIL    KOHFTN-IDX     >    4
      *
              IF  (RECE62-HKNJANUM       NOT =      SPACE     )
              AND (RECE62-RJNHKNNUM          =      SPACE     )
              AND (RECE62-KOHNUM(KOHFTN-IDX) =
                              "010" OR "011" OR "020" OR "021")
      *
                 IF    RECE62-SRYYM        >=  "200804"
                    INITIALIZE                   SYSKANRI-REC
                    MOVE    "2012"           TO  SYS-KANRICD
                    MOVE    RECE62-HKNJANUM  TO  SYS-KBNCD
                    MOVE    RECE62-SRYYM     TO  SYS-STYUKYMD (1:6)
                    MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                    MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                    MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                    MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-DBSELECT-SEC
                    IF      MCP-RC           =   ZERO
                      MOVE  "tbl_syskanri"   TO  MCP-TABLE
                      MOVE  "key10"          TO  MCP-PATHNAME
                      PERFORM 900-SYSKANRI-READ-SEC
                    ELSE
                      INITIALIZE                 SYS-2012-REC
                    END-IF
                    PERFORM   20011B1-HBTNUMKBN-SET-SEC
                    MOVE    "tbl_syskanri"   TO  MCP-TABLE
                    MOVE    "key10"          TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                 ELSE
                    PERFORM   20011B2-HBTNUMKBN-SET2-SEC
                 END-IF
      *
              END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           .
       20011B-HOTEIGAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    法定外現物給付の設定セット
      *****************************************************************
       20011B1-HBTNUMKBN-SET-SEC               SECTION.
      *
           IF  RECE62-KOHNUM(KOHFTN-IDX) = "010"
             EVALUATE  SYS-2012-HBTNUM10KBN
               WHEN      SPACE
               WHEN      "0"
                 PERFORM   20011B2-HBTNUMKBN-SET2-SEC
               WHEN      "1"
                 MOVE    "1"   TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           IF  RECE62-KOHNUM(KOHFTN-IDX) = "011"
             EVALUATE  SYS-2012-HBTNUM11KBN
               WHEN      SPACE
               WHEN      "0"
                 MOVE   SPACE  TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
               WHEN      "1"
                 MOVE    "1"   TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           IF  RECE62-KOHNUM(KOHFTN-IDX) = "020"
             EVALUATE  SYS-2012-HBTNUM20KBN
               WHEN      SPACE
               WHEN      "0"
                 MOVE   SPACE  TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
               WHEN      "1"
                 MOVE    "1"   TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           IF  RECE62-KOHNUM(KOHFTN-IDX) = "021"
             EVALUATE  SYS-2012-HBTNUM21KBN
               WHEN      SPACE
               WHEN      "0"
                 PERFORM   20011B2-HBTNUMKBN-SET2-SEC
               WHEN      "1"
                 MOVE    "1"   TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
             END-EVALUATE
           END-IF
      *
           .
       20011B1-HBTNUMKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    法定外現物給付の設定セット２
      *****************************************************************
       20011B2-HBTNUMKBN-SET2-SEC               SECTION.
      *
           IF ((RECE62-HKNJANUM(1:6) = "233064" OR "133280") AND
               (RECE62-SRYYM        <= "201103"            ))
           OR ((RECE62-HKNJANUM(1:6) = "283077"            ) AND
               (RECE62-SRYYM        <= "201111"            ))
           OR ((RECE62-HKNJANUM(1:6) =
                "133033" OR "133041" OR "133066" OR "133074" OR
                "133090" OR "133116" OR "133132" OR "133140" OR
                "133157" OR "133165" OR "133173" OR "133181" OR
                "133199" OR "133207" OR "133223" OR "133249" OR
                "133256" OR "133272"                       ) AND
               (RECE62-SRYYM        <= "201203"            ))
              IF  (WRK-PREFNUM          = "29"    )
              AND (RECE62-HKNJANUM(1:6) = "133033")
                CONTINUE
              ELSE
                IF  RECE62-KOHNUM(KOHFTN-IDX) = "010" OR "021"
                  MOVE    "1"   TO   RECE62-HOTEIGAIFLG(KOHFTN-IDX + 1)
                END-IF
              END-IF
           END-IF
      *
           .
       20011B2-HBTNUMKBN-SET2-EXT.
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
                   PERFORM   900-CALL-ORCSJOB-SEC
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
           STRING  "ORCR0630 "             DELIMITED   BY  SIZE
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE620 IN  :" CNT-IN   (1) 
           DISPLAY "        MOD :" CNT-MOD  (1) 
           DISPLAY "RECE62X IN  :" CNT-IN   (2) 
           DISPLAY "        MOD :" CNT-MOD  (2) 
           DISPLAY "*** ORCR0630 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院略称の編集
      *****************************************************************
       500-RYAKUSYO-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE         TO   NYURYAKUSHO-REC
           INITIALIZE                 NYURYAKUSHO-REC
      *
           MOVE    WRK-SRYCD     TO   NYURYAKUSHO-SRYCD
           MOVE    WRK-SRYYMD    TO   NYURYAKUSHO-YUKOSTYMD
                                      NYURYAKUSHO-YUKOEDYMD
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID1    FROM    1   BY  1
                    UNTIL   ID1                   > 20
                      OR    RECE62-NYUSYUBETU(ID1) = SPACE
                      OR    FLG-ARI                = 1 
                 IF  RECE62-NYUSYUBETU(ID1)  
                            =  NYURYAKUSHO-RYAKUSHO
                     MOVE  1   TO   FLG-ARI
                 END-IF
              END-PERFORM  
              IF  FLG-ARI  =  ZERO  
              AND ID1     <=   20
                     MOVE   NYURYAKUSHO-RYAKUSHO
                               TO   RECE62-NYUSYUBETU(ID1)
              END-IF
      *
              IF   RECE62-SRYYM  >= "200604"
                CONTINUE
              ELSE
                IF   WRK-SRYCD  =  "190109670"  OR  "190808870"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:4)   =   "有看"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                 AND ID1     <=   20
                        MOVE   "有看"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:4)
                 END-IF
      *
                END-IF
                IF   WRK-SRYCD  =  "190117070"  OR  "190813270"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:4)   =   "有医"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                 AND ID1     <=   20
                        MOVE   "有医"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:4)
                 END-IF
      *
                END-IF
                IF   WRK-SRYCD  =  "190117670"  OR  "190813870"
      *
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 20
                         OR    RECE62-NYUSYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-NYUSYUBETU(ID1)(1:6)   =   "有看医"
                        MOVE  1   TO   FLG-ARI
                    END-IF
                 END-PERFORM  
                 IF  FLG-ARI  =  ZERO  
                 AND ID1     <=   20
                        MOVE   "有看医"
                                  TO   RECE62-NYUSYUBETU(ID1)(1:6)
                 END-IF
      *
                END-IF
              END-IF
           ELSE
              IF   ( RECE62-SRYYM  >= "200604"    )
              AND  ( WRK-SRYCD      = "190123250" OR
                                      "190123350" OR
                                      "190123450" OR
                                      "190123550" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                   > 20
                        OR    RECE62-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "一般経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123350"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "結核経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123450"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "精神経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123550"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "障害経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                AND ID1     <=   20
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      MOVE   "一般経過"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123350"
                      MOVE   "結核経過"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123450"
                      MOVE   "精神経過"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123550"
                      MOVE   "障害経過"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                   END-EVALUATE
                END-IF
              END-IF
              IF   ( RECE62-SRYYM  >= "200807"    )
              AND  ( WRK-SRYCD      = "190130810" OR
                                      "190131010" OR
                                      "190131110" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                   > 20
                        OR    RECE62-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      IF  RECE62-NYUSYUBETU(ID1)(1:8) = "一般１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131010"
                      IF  RECE62-NYUSYUBETU(ID1)(1:6) = "結１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131110"
                      IF  RECE62-NYUSYUBETU(ID1)(1:6) = "専１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                AND ID1     <=   20
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      MOVE   "一般１０"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190131010"
                      MOVE   "結１０"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:6)
                     WHEN     "190131110"
                      MOVE   "専１０"
                                 TO   RECE62-NYUSYUBETU(ID1)(1:6)
                   END-EVALUATE
                END-IF
              END-IF
           END-IF
      *
           .
       500-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院略称（経措）チェック
      *****************************************************************
       500-RYAKUSYO-KEISO-CHECK-SEC                SECTION.
      *
           IF   (RECE62-SRYYM    >=  "201404")
           AND  (RECE62-SRYYM    <=  "201406")
           AND  (WRK-HOSPSBT      =   1      )
           AND  (WRK-EIYOUKSNKBN  =  "1"     )
      *
             INITIALIZE    ORCSEIYOUKKS-AREA
             MOVE   RECE62-SRYYM          TO  LNK-EIYOUKKS-SANTEIYM
             MOVE   WRK-TOKUREI-SRYCD2    TO  LNK-EIYOUKKS-SRYCD
             CALL   "ORCSEIYOUKKS" USING  ORCSEIYOUKKS-AREA
                                          SPA-AREA
             IF     LNK-EIYOUKKS-RECE-CHGSRYCD  NOT = SPACE
               MOVE LNK-EIYOUKKS-RECE-CHGSRYCD
                                          TO  WRK-TOKUREI-SRYCD
             END-IF
             IF     LNK-EIYOUKKS-RC  =  9
               IF   WRK-TOKUREI-SRYCD2(1:3)  =  "188"
                  CONTINUE
               ELSE
                  MOVE    1               TO  WRK-TOKUREI-FLG
               END-IF
             END-IF
      *
           END-IF
      *
           .
       500-RYAKUSYO-KEISO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    組み合わせ判別（福祉用）主保険＋全国公費＋地方公費
      *****************************************************************
       500-KUMIAWASE-HANBETU-SEC               SECTION.
      *
           INITIALIZE                 TBL-FUKUSHI
      *
           IF  RECE62-SYUKEIKBN(IDX1 2) = "0" OR SPACE
             CONTINUE
           ELSE
             EVALUATE  RECE62-SYUKEI-TAIKBN(IDX1 2)
               WHEN     "0"
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
               WHEN     "1"
                 IF   RECE62-TEISYUTUSAKI  =   2 OR 6
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "2"
                 IF   RECE62-TEISYUTUSAKI  =   1
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "3"
                 IF   RECE62-TEISYUTUSAKI  =   2
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
               WHEN     "4"
                 IF   RECE62-TEISYUTUSAKI  =   6
                   PERFORM  5001-KUMIAWASE-HENSYU-SEC
                 END-IF
             END-EVALUATE
           END-IF
      *
           .
       500-KUMIAWASE-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    組み合わせ判別編集
      *****************************************************************
       5001-KUMIAWASE-HENSYU-SEC               SECTION.
      *
           EVALUATE  RECE62-SYUKEIKBN(IDX1 2)
              WHEN      "1"
              WHEN      "3"
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(2)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(2)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(3)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(3)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(3)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(4)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(4)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(4)
                     MOVE  "1"  TO      RECE62-FUKUSHIFLG(5)
                     MOVE   1   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "3"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
              WHEN      "2"
              WHEN      "4"
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(2)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(2)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(3)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(3)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(3)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(4)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
                  IF   RECE62-KOHNUMHC(IDX1 2) =  RECE62-KOHNUM(4)
                  AND  RECE62-KOHIDHC(IDX1 2)  =  RECE62-KOHID(4)
                     MOVE  "2"  TO      RECE62-FUKUSHIFLG(5)
                     MOVE   2   TO      TBL-FUKUSHIFLG(2)
                     IF   RECE62-SYUKEIKBN(IDX1 2) = "4"
                      MOVE  1   TO      RECE62-FTNKBNFLG(IDX1 2)
                     END-IF
                  END-IF
           END-EVALUATE
      *
           IF   TBL-FUKUSHIFLG(2)   NOT = ZERO
             EVALUATE   WRK-PREFNUM
               WHEN    "38"
                  IF   RECE62-TEISYUTUSAKI  =   2 OR 6
                     MOVE   1    TO     TBL-FUKUSHI-DAYFLG(2)
                  END-IF
               WHEN    "36"
                  IF  (RECE62-KOHNUMHC(IDX1 2) = "245"   )
                  AND (RECE62-KOHNUMHC(IDX1 3) = "748"   )
                  AND (RECE62-SRYYM           >= "200805")
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(2)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(2)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(3)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(3)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(3)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(3)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(4)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(4)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                     IF   RECE62-KOHNUMHC(IDX1 3) =  RECE62-KOHNUM(4)
                     AND  RECE62-KOHIDHC(IDX1 3)  =  RECE62-KOHID(4)
                        EVALUATE  TBL-FUKUSHIFLG(2)
                          WHEN      1
                            MOVE    "1"   TO  RECE62-FUKUSHIFLG(5)
                          WHEN      2
                            MOVE    "2"   TO  RECE62-FUKUSHIFLG(5)
                        END-EVALUATE
                        MOVE   TBL-FUKUSHIFLG(2)
                                          TO  TBL-FUKUSHIFLG(3)
                        MOVE   RECE62-FTNKBNFLG(IDX1 2)
                                          TO  RECE62-FTNKBNFLG(IDX1 3)
                     END-IF
                  END-IF
             END-EVALUATE
           END-IF
      *
           .
       5001-KUMIAWASE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    入院診療行為マスター読込
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    点数マスター読込（主キー）
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    入院略称マスター読込（主キー）
      *****************************************************************
       900-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE     ZERO       TO    FLG-NYURYAKUSHO
      *
           PERFORM  910-NYURYAKUWRK-CHK-SEC
      *
           IF      FLG-NYURYAKUWRK     =   ZERO
               MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
                   IF      MCP-RC               =   ZERO
                       MOVE    MCPDATA-REC      TO  NYURYAKUSHO-REC
                       IF  WRK-TOKUREI-FLG  =  1
                          MOVE    SPACE   TO   WRK-TOKUREI-RYAKUSHO
                          STRING  NYURYAKUSHO-RYAKUSHO
                                    DELIMITED   BY  SPACE
                                  "経措"
                                    DELIMITED   BY  SIZE
                                  INTO  WRK-TOKUREI-RYAKUSHO
                          END-STRING
                          MOVE    SPACE   TO   NYURYAKUSHO-RYAKUSHO
                          MOVE    WRK-TOKUREI-RYAKUSHO
                                          TO   NYURYAKUSHO-RYAKUSHO
                       END-IF
                       MOVE    ZERO             TO  FLG-NYURYAKUSHO
                       PERFORM  910-NYURYAKUWRK-HEN-SEC
                   ELSE
      *                救命救急入院料１（４日以上７日以内）
      *                救命救急入院料２（４日以上７日以内）
      *                の入院略称テーブル対応
                       IF      NYURYAKUSHO-SRYCD   =  "190128610"  OR
                                                      "190128710"
                           MOVE   "20080401"  TO  NYURYAKUSHO-YUKOSTYMD
                           MOVE   "99999999"  TO  NYURYAKUSHO-YUKOEDYMD
                           EVALUATE  NYURYAKUSHO-SRYCD
                              WHEN   "190128610"
                                  MOVE  "救命１"
                                              TO  NYURYAKUSHO-RYAKUSHO
                              WHEN   "190128710"
                                  MOVE  "救命２"
                                              TO  NYURYAKUSHO-RYAKUSHO
                           END-EVALUATE
                           MOVE   ZERO        TO  FLG-NYURYAKUSHO
                           PERFORM  910-NYURYAKUWRK-HEN-SEC
                       ELSE
                           MOVE    1            TO  FLG-NYURYAKUSHO
                           INITIALIZE               NYURYAKUSHO-REC
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                    TO  FLG-NYURYAKUSHO
                   INITIALIZE                       NYURYAKUSHO-REC
               END-IF
      *
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       900-NYURYAKUSHO-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院略称マスター退避ワークチェック処理
      *****************************************************************
       910-NYURYAKUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-NYURYAKUWRK
      *
           PERFORM VARYING WRK-NYURYAKU-IDX  FROM    1   BY  1
             UNTIL ( WRK-NYURYAKU-IDX  >   WRK-NYURYAKU-CONST-MAX ) OR
                   ( FLG-NYURYAKUWRK   =     1 )      OR
                   ( WRK-NYURYAKU-IDX  >   200 )
               IF    ( WRK-NYURYAKU-SRYCD (WRK-NYURYAKU-IDX)
                                       =   NYURYAKUSHO-SRYCD )
                 AND ( WRK-NYURYAKU-YUKOSTYMD (WRK-NYURYAKU-IDX)
                                       <=  NYURYAKUSHO-YUKOSTYMD)
                 AND ( WRK-NYURYAKU-YUKOEDYMD (WRK-NYURYAKU-IDX)
                                       >=  NYURYAKUSHO-YUKOSTYMD)
                   MOVE    1           TO  FLG-NYURYAKUWRK
                   MOVE    WRK-NYURYAKU-DATA (WRK-NYURYAKU-IDX)
                                       TO  NYURYAKUSHO-REC
               END-IF
           END-PERFORM
      *
           .
       910-NYURYAKUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院略称マスター退避ワーク編集処理
      *****************************************************************
       910-NYURYAKUWRK-HEN-SEC               SECTION.
      *
           IF    ( WRK-NYURYAKU-CONST-MAX  >   WRK-NYURYAKU-MAX )
               COMPUTE WRK-NYURYAKU-MAX    =  WRK-NYURYAKU-MAX   +   1
               MOVE    NYURYAKUSHO-SRYCD   TO  WRK-NYURYAKU-SRYCD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOSTYMD
                                           TO  WRK-NYURYAKU-YUKOSTYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOEDYMD
                                           TO  WRK-NYURYAKU-YUKOEDYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-REC     TO  WRK-NYURYAKU-DATA
                                                   (WRK-NYURYAKU-MAX)
           END-IF
      *
           .
       910-NYURYAKUWRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定療養マスター読込
      *****************************************************************
       900-SENTEICDCHG-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       900-SENTEICDCHG-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-2010-REC
                                           SYS-2012-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-2010-REC
                                           SYS-2012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE62-READ-SEC            SECTION.
      *
           READ    RECE62-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE62
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE62 " IDZ " PTNUM=" RECE62-PTNUM     
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE62-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               CONTINUE
           ELSE
      *        医療機関ＩＤ編集
               MOVE  ZERO           TO  FLG-SET
               MOVE  RECE62-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                     UNTIL  ( SYSKAN-IDX              >  100  )
                      OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                      OR    ( FLG-SET                 =   1   )
      *
                 MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                 IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1               TO    FLG-SET
                    MOVE   SYS-1001-PREFNUM  TO    WRK-PREFNUM
                    MOVE   SYS-1001-HOSPSBT  TO    WRK-HOSPSBT
                 END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
                    "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       RECE62-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE62
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
               MOVE      RECE62-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   10012-SYS-5000-CHK-SEC
      *        医療機関減算の取得（システム管理テーブルより）
               INITIALIZE                 WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
               MOVE     ZERO      TO  FLG-MIJISI-INNAI
                                      FLG-MIJISI-IRYO
                                      FLG-MIJISI-JYOKU
      *
               MOVE     SPACE     TO  TBL-GENSAN
               INITIALIZE             TBL-GENSAN
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE62-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                     MOVE     1       TO    FLG-SET
                     PERFORM  10011-SYSKAN-HENSYU-SEC
                     IF        WRK-SYS5000-COUNT    =     ZERO
                       CONTINUE
                     ELSE
                       PERFORM  10013-SYS-5000-GET-SEC
                     END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "医療機関情報−入院基本が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       RECE62-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1              TO  FLG-RECE62
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.

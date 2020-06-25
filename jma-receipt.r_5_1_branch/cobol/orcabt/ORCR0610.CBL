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
       PROGRAM-ID.             ORCR0610.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 月次業務　明細書・請求書用ファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/01    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.03    NACL-藤原    03/03/15  入院会計からの診療科の編集の修正
      *  01.00.04    NACL-藤原    03/04/21  レセのとき地方公費のみの場合は
      *                                     出力しない
      *  01.00.05    NACL-藤原    03/07/28  診療回数テーブルを編集
      *  01.00.06    NACL-藤原    03/11/18  社保・国保別で作成のとき
      *                                     読み飛ばしをしない
      *  01.00.07    NACL-藤原    03/11/19  老人＋原爆のレセ記載をシス管
      *                                     より取得
      *  01.00.08    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  01.00.09    NACL-藤原    04/04/15  ＤＢ更新エラー時の処理
      *  01.00.10    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *
      *  レセ電機能追加
      *  01.02.01    NACL-藤原    04/07/30  入院レセ電処理の追加
      *
      *  中止処理追加
      *  01.03.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.03.02    NACL-藤原    04/10/30  福岡県の地方公費対応（国保）
      *  01.03.03    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.03.04    NACL-藤原    05/03/03  老人又は前期高齢者のとき、
      *                                     月途中に保険者は変わらずに
      *                                     保険が変更になった場合は１枚
      *                                     にする
      *  01.03.05    NACL-藤原    05/07/05  前期高齢者のとき、月途中に
      *                                     給付割合が変更になった場合は
      *                                     それぞれにレセプトを作成する
      *
      *  02.06.00    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.01    NACL-門脇    05/06/01  長期高額療養レセ記載判定追加
      *  02.06.02    NACL-藤原    05/07/12  国保で本人・家族の変更があった
      *                                     場合、給付割合の変更がないとき
      *                                     は１枚にまとめる
      *  02.06.03    NACL-藤原    05/07/12  診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  02.06.04    NACL-藤原    05/07/28  レセプト特記事項編集情報のため
      *                                     のレセプト請求しない地方公費の
      *                                     を追加
      *  02.06.05    NACL-門脇    05/10/05  省庁対応
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *
      *  03.02.01    NACL-門脇    06/09/05  ２００６年１０月改正対応
      *                                     長期９７４対応
      *                                     福岡地方公費対応
      *
      *  03.03.01    NACL-藤原    06/10/15  ２００６年１０月改正対応
      *                                     公費の追加
      *  03.03.02    NACL-藤原    06/10/10  主保険がない場合、精神入院は
      *                                     公費単独のレセプトとする
      *  03.03.03    NACL-藤原    06/10/25  ２００６年１０月改正対応
      *                                     福岡地方公費の障害
      *  03.03.04    NACL-藤原    06/10/26  福岡地方公費のレセ記載判定の修正
      *  03.03.05    NACL-藤原    06/10/27  レセプトで生活保護の請求がな
      *                                     い場合記載の対象としない
      *  03.03.06    NACL-藤原    06/10/31  療養介護医療２４の追加
      *  03.03.07    NACL-藤原    06/11/02  福岡地方公費の障害のついて全て
      *                                     公費付加情報より記載判定を行う
      *  03.03.08    NACL-藤原    06/12/14  不明分のとき生保単独の上限
      *                                     チェックはしない
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *                                     MONFUNC対応
      *  03.04.03    NACL-藤原    07/08/20  北海道の地方公費の記載順対応
      *
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *  04.01.02    NACL-藤原    07/12/08  長崎県対応
      *                                     生保と被爆８６の記載順 
      *  04.01.03    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/03/10  心神喪失者等医療
      *                                    （法第８１条関係）の追加
      *  04.02.03    NACL-藤原    08/03/25  広域連合対応
      *                                     肝炎治療特別促進事業
      *                                     中国残留法人等の追加
      *  04.02.04    NACL-藤原    08/04/14  月途中の負担者番号変更対応
      *                                     （中国残留法人等の公費単独） 
      *  03.02.05    NACL-藤原    08/04/18  福岡地方公費の国保のとき
      *                                     平成２０年４月以降は公費付加
      *                                     情報より記載判定はしない
      *  04.02.06    NACL-藤原    08/04/23  後期高齢者の船員保険を本人とする
      *  04.02.07    NACL-門脇    08/08/07  精神通院の請求点判定追加
      *
      *  04.03.01    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.02    NACL-門脇    08/12/17  平成２１年１月改正対応
      *
      *  04.04.01    NACL-藤原    09/01/14  月途中の生保変更時の保険履歴
      *                                     記載対応
      *  04.04.02    NACL-藤原    09/05/19  東京都地方公費対応
      *                                     マル都（８２）は再優先とする
      *  04.04.03    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独）
      * 
      *  04.05.01    NACL-藤原    10/03/12  平成２２年４月改正対応
      *                                     平成２２年４月分以降の政府管
      *                                     掌をエラーとする
      *  04.05.02    NACL-藤原    10/04/13  愛知県対応
      *                                     生保と特定疾患９１の記載順 
      *  04.05.03    NACL-藤原    11/03/01  山梨県対応
      *                                     老人医療費４１と特定疾患の
      *                                     記載順 
      *  04.05.04    NACL-藤原    11/03/02  徳島県対応
      *                                     乳児４５、４８が月途中で変更
      *                                     になった場合の記載順
      *  04.05.05    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.06    NACL-藤原    11/04/08  奈良県対応
      *                                     国保、後期高齢者のとき
      *                                     法別７１、８１、９１は公費請
      *                                     求書も作成する  
      *  04.05.08    NACL-藤原    11/05/16  月途中に公費の負担者番号等の
      *                                     変更があった場合の記載対応
      *                                     法別番号、適用開始日の順とする
      *                                     （全国公費のみ） 
      *  04.05.09    NACL-藤原    11/07/12  山梨県の地方公費対応
      *                                     ２４１と１４３の記載順
      *  04.05.10    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.11    NACL-藤原    11/12/06  東京都地方公費対応
      *                                     患者票（９３）をマル都（８２）
      *                                     より優先する
      *  04.05.12    NACL-藤原    11/12/27  東京都地方公費対応
      *                                     患者票（９３）をマル都（８２）
      *                                     より優先するときの条件追加
      *  04.05.13    NACL-藤原    12/04/11  平成２４年４月改正対応
      *                                     「６２Ｂ型肝炎」の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.03    NACL-藤原    11/02/24  月途中に医保と児童保護(53)の
      *                                     併用から単独になったときのまとめ
      *  04.06.04    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.06    NACL-藤原    12/09/27  生活保護期間重複対応
      *  04.06.07    NACL-藤原    12/12/21  入院料の算定がない入退院履歴
      *                                     の記載対応
      *
      *  04.07.01    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *  04.07.02    NACL-藤原    12/03/25  公費記載順設定対応
      *  04.07.03    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.04    NACL-藤原    13/08/01  公費記載順設定時の条件の修正
      *  04.07.05    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.06    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.07    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.08    NACL-藤原    14/04/22  ＥＦファイル作成対応
      *  04.07.09    NACL-藤原    14/12/05  平成２７年１月改正対応
      *  04.07.10    NACL-藤原    14/12/08  月途中に公費の負担者番号等の
      *                                     変更があった場合の記載対応
      *                                     法別番号、適用開始日の順とする
      *                                     （地方公費） 
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.04    NACL-藤原    18/06/14  北海道の地方公費の記載順対応
      *                                     ９０と９１の記載順
      *                                     適用開始日、法別番号の順とする
      *  04.08.05    NACL-藤原    18/06/11  Warning 対応
      *  04.08.06    NACL-藤原    18/09/09  北海道の地方公費の記載順対応
      *                                     ４５と４６、９３と９４の記載順
      *                                     適用開始日、法別番号の順とする
      *
      *  05.00.01    NACL-伊藤    18/01/11  診療情報システム対応
      *  05.00.02    NACL-藤原    18/11/29  肝がん治療対応
      *  05.00.03    NACL-藤原    19/04/04  印刷順の桁数変更
      *  05.00.04    NACL-藤原    20/04/01  令和２年４月改正対応
      *                                     感染症（新型コロナ）の法別番
      *                                     号による記載順対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE61-FILE     ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61-KEY
                                   FILE    STATUS  IS  STS-RECE61.
      *
      *    医療費請求書  
           SELECT  RECE61X-FILE    ASSIGN  RECE61XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61X-KEY
                                   FILE    STATUS  IS  STS-RECE61X.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE61-FILE.
       01  RECE61-REC. 
           COPY    "CPRCF061.INC".
      *
      *    医療費請求書  
       FD  RECE61X-FILE.
       01  RECE61X-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE61X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE61PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE61XPARA//.
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
           03  STS-RECE61                              PIC X(02).
           03  STS-RECE61X                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SYSKANRI-2013                       PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01). 
           03  FLG-HKNNUM                              PIC 9(01). 
           03  FLG-RECE61                              PIC 9(01).
           03  FLG-RECE61X                             PIC 9(01).
           03  FLG-ADDKBN                              PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-SEIKYU                              PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-BTPARA                              PIC 9(01).
           03  FLG-SYUKA                               PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-PT-HDIST-DAY                        PIC 9(01).
      * 
           03  FLG-KOH                                 PIC 9(01).
           03  FLG-CONTKBN                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-LASTHKNID                           PIC 9(01). 
           03  FLG-KOHRECEDSP                          PIC 9(01).
           03  FLG-KOHSKY-HENSYU                       PIC 9(01).
           03  FLG-TENKEN                              PIC 9(01). 
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
           03  IDX1                                    PIC 9(03). 
           03  IDX2                                    PIC 9(03). 
           03  IDX3                                    PIC 9(03). 
           03  IDX4                                    PIC 9(03). 
           03  IDX5                                    PIC 9(03). 
           03  IDY                                     PIC 9(03). 
           03  IDZ                                     PIC 9(03).
           03  IDXX                                    PIC 9(03).
           03  IDYY                                    PIC 9(03).
           03  IDZZ                                    PIC 9(03).
           03  IDY1                                    PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDY3                                    PIC 9(03).
           03  IDZ1                                    PIC 9(03).
           03  IDZ2                                    PIC 9(03).
           03  IDZ3                                    PIC 9(03).
           03  TBL-MAX                                 PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  WRK-SYS2013-IDX                         PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    公費記載順設定情報退避用領域
       01  TBL-SYS2013-T.
           03  TBL-SYS2013-OCC         OCCURS  100.
               05  TBL-SYS2013-STYUKYMD
                                       PIC X(08).
               05  TBL-SYS2013-EDYUKYMD
                                       PIC X(08).
               05  TBL-SYS2013-TBL     OCCURS  50.
                   07  TBL-SYS2013     PIC X(610).
           03  TBL-SYS2013-MAX         PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           03  WRK-PARA-SRYYM                      PIC  X(06).
           03  WRK-PARA-TERMID                     PIC  X(16).
           03  WRK-PARA-SYSYMD                     PIC  X(08).
           03  WRK-PARA-SYOKBN                     PIC  X(01).
           03  WRK-PARA-RECEKBN                    PIC  X(01).
           03  WRK-PARA-JOBID                      PIC  9(07).
           03  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-PTID                       PIC  X(10).
           03  WRK-PARA-SRYYM-O                    PIC  X(06).
           03  WRK-PARA-PRTKBN                     PIC X(01).
           03  WRK-PARA-HOSPNUM                    PIC  9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-CONTKBN             PIC X(01).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-SRYACCT-PATHNAME    PIC X(64).
      *
           03  WRK-PRTSEQ              PIC 9(04).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
           03  WRK-SYOKBN              PIC X(01).
           03  WRK-DBKBN               PIC X(01).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(01).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      * 
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      * 
           03  WRK-NENREI              PIC 9(03).
      *
           03  WRK-SYUKA-SRYYM         PIC X(06).
           03  WRK-SYUKA-PTID          PIC 9(10).
      *
           03  WRK-SEIHO-IDX           PIC 9(02).
           03  WRK-HIBAKU-IDX          PIC 9(02).
           03  WRK-KOH-IDX             PIC 9(02).
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
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
      *    公費並替え用ワーク
           03  WRK-KOHI-AREA.
               05  WRK-KOHI-TBL        OCCURS  10.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHPAYKBN-O PIC  X(02).
                   07  WRK-RECESKYKBN-O
                                       PIC  X(01).
                   07  WRK-KOHTEKSTYMD-O
                                       PIC  X(08).
                   07  WRK-KOHHBTNUM-O PIC  X(02).
                   07  WRK-KOHFTNJANUM-O
                                       PIC  X(08).
                   07  WRK-KOHJKYSNUM-O
                                       PIC  X(20).
           03  WRK-KOHI.
               05  WRK-KOHID           PIC  9(10).
               05  WRK-KOHHKNNUM       PIC  X(03).
               05  WRK-KOHPAYKBN       PIC  X(02).
               05  WRK-RECESKYKBN      PIC  X(01).
               05  WRK-KOHTEKSTYMD     PIC  X(08).
               05  WRK-KOHHBTNUM       PIC  X(02).
               05  WRK-KOHFTNJANUM     PIC  X(08).
               05  WRK-KOHJKYSNUM      PIC  X(20).
      *
      *    請求区分別公費情報（１：レセプト　２：請求書）
           03  WRK-SEIKYUKBN-KOHI-AREA.
               05  WRK-SEIKYU-TBL   OCCURS  2.
                   07  WRK-SEIKYU-OCC    OCCURS  10.
                       09  WRK-SEIKYU-KOHID      PIC  9(10).
                       09  WRK-SEIKYU-KOHHKNNUM  PIC  X(03).
                       09  WRK-SEIKYU-KOHPAYKBN  PIC  X(02).
                       09  WRK-SEIKYU-RECESKYKBN PIC  X(01).
                       09  WRK-SEIKYU-TEKSTYMD   PIC  X(08).
                       09  WRK-SEIKYU-KOHHBTNUM  PIC  X(02).
                       09  WRK-SEIKYU-KOHFTNJANUM
                                                 PIC  X(08).
                       09  WRK-SEIKYU-KOHJKYSNUM PIC  X(20).
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
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "062".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "093".
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "054".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "092".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "062".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
      *
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-OCC         OCCURS  4.
                   07  WRK-SRT-KOHHKNNUM
                                       PIC X(03)   OCCURS  32.
      *
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   32.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           02  WRK-HKNCOMBI-ERR        OCCURS  10. 
               03  WRK-HKNCOMBI-SYOKBN PIC X(01).   
               03  WRK-HKNCOMBI-ERR-DAY
                                       PIC 9(02).   
      *
      *    災害該当（９５９）退避用領域
       01  TABLE-PTKOHINF-AREA.
           02  TBL-PTKOHINF-REC        OCCURS  100.
           COPY    "CPPTKOHINF.INC"
                                   REPLACING   //PTKOH//
                                   BY          //TBL-PTKOH//.
           02  TBL-PTKOHINF-MAX    PIC 9(03).
      *
      *    患者レセプト分割情報退避用領域
       01  TBL-PT-HDIST-DAY-T.
         02  TBL-PT-HDIST-DAY-REC      OCCURS  10.
           COPY    "CPPT-HDIST-DAY.INC"
                                   REPLACING   //PT-HDIST-DAY//
                                   BY          //TBL-PT-HDIST-DAY//.
         02  TBL-PT-HDIST-DAY-MAX  PIC 9(02).
      *
       01  WRK-CONS-AREA.
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975". 
      *
           03  WRK-CONS-SRYYM          PIC 9(06)   VALUE   200604.      
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
      *
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.      
      *
           03  WRK-CONS-SRYYM-201501   PIC 9(06)   VALUE   201501.     
      *     
      * 
       01  WRK-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-RECE61//.
      *     
       01  WRK-SEIHO-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-SEIHO//.
      *     
      * 
       01  WRK-KOHSKY-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-KOHSKY//.
      *
      *    社保（継続）のとき 
       01  WRK-CONTKBN-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-CONTKBN//.
      *
           COPY    "MCPAREA".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *     
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *    公費記載順設定情報
           COPY    "CPSK2013.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者レセプト分割
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
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
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM=" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM  =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN =" WRK-PARA-PRTKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE610"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE61X"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE61PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE61XPARA
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
               MOVE    "ORCR0610"          TO  JOB-PGID
               MOVE    "対象患者抽出"      TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               IF   WRK-PARA-SRYYM    NOT =  ZERO
      *            医療機関ＩＤ編集
                   INITIALIZE                  SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO 
                                               SYS-1001-STYUKYMD (1:6)
                   MOVE    "01"                TO
                                               SYS-1001-STYUKYMD (7:2)
                   MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
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
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                                 OR    (SYSKAN-IDX   >=  100)
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
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
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
               MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
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
      *        県内扱い保険者情報
               MOVE    SPACE           TO  WRK-1033-TABLE
      *     
               MOVE    SPACE           TO  SYS-1033-REC
               INITIALIZE                  SYS-1033-REC
               MOVE    "1033"          TO  SYS-1033-KANRICD
               MOVE    "*"             TO  SYS-1033-KBNCD
               MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
               MOVE    SYS-1033-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
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
                   MOVE    SYS-1033-STYUKYMD   TO  
                                               WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD   TO
                                               WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL        TO
                                               WRK-1033-HKNJANUM-G (IDX)
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *     
      *        レセプト編集区分情報
               INITIALIZE                  SYS-2005-REC
               MOVE    "2005"              TO  SYS-2005-KANRICD
               MOVE    "*"                 TO  SYS-2005-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-2005-STYUKYMD  (7:2)
               MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
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
               INITIALIZE                  SYS-25-REC
               MOVE    "2005"              TO  SYS-25-KANRICD
               MOVE    "01"                TO  SYS-25-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-25-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
               MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
               MOVE    SYS-25-REC          TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-25-REC
               ELSE    
                   MOVE    SPACE               TO  SYS-25-REC
               END-IF
               IF      SYS-25-DISASTER-RECEIPT
                                           =   SPACE
                   MOVE    ZERO                TO
                                           SYS-25-DISASTER-RECEIPT
               END-IF
      *
      *        公費記載順設定情報
               MOVE     ZERO               TO  TBL-SYS2013-MAX
               MOVE     SPACE              TO  TBL-SYS2013-T
               INITIALIZE                      TBL-SYS2013-T
      *
               INITIALIZE                      SYS-2013-REC
               MOVE    SPA-HOSPNUM         TO  SYS-2013-HOSPNUM
               MOVE    "2013"              TO  SYS-2013-KANRICD
               MOVE    SYS-2013-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    FLG-SYSKANRI        TO  FLG-SYSKANRI-2013 
                   PERFORM UNTIL   FLG-SYSKANRI-2013   =   1
                           OR      TBL-SYS2013-MAX     >=  100
                       MOVE    MCPDATA-REC         TO  SYS-2013-REC
                       MOVE    ZERO                TO  IDX
      *
                       MOVE    SPA-HOSPNUM         TO  SYS-2013-HOSPNUM
                       MOVE    SYS-2013-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key7"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key7"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
                           PERFORM UNTIL ( FLG-SYSKANRI    =   1  )
                                   OR    ( IDX             >=  50 )
                               MOVE    MCPDATA-REC     TO  SYS-2013-REC
      *                        グループ内の公費が２つ以上のみ対象とする
                               IF      SYS-2013-KOHNUM (2) =   SPACE
                                   CONTINUE
                               ELSE
                                   IF      IDX             =   ZERO
                                       ADD     1       TO
                                                       TBL-SYS2013-MAX
                                       MOVE    SYS-2013-STYUKYMD
                                                       TO
                                                   TBL-SYS2013-STYUKYMD
                                                       (TBL-SYS2013-MAX)
                                       MOVE    SYS-2013-EDYUKYMD
                                                       TO
                                                   TBL-SYS2013-EDYUKYMD
                                                       (TBL-SYS2013-MAX)
                                   END-IF
                                   ADD     1           TO  IDX
                                   MOVE    SYS-2013-REC
                                                       TO  TBL-SYS2013
                                                   (TBL-SYS2013-MAX IDX)
                               END-IF
      *
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key7"          TO  MCP-PATHNAME
                               PERFORM 900-SYSKANRI-READ-SEC
                           END-PERFORM
                       END-IF     
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key7"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key12"             TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       MOVE    FLG-SYSKANRI        TO  FLG-SYSKANRI-2013 
                   END-PERFORM
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF        WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            パラメタＤＢ読み込み
                   INITIALIZE                  BTPARA-REC
                   MOVE    WRK-PARA-SHELLID
                                           TO  BTPARA-SHELLID
                   MOVE    WRK-PARA-JOBID  TO  BTPARA-JOBID
                   PERFORM 900-BTPARA-READ-SEC
                   IF      FLG-BTPARA      NOT =   ZERO
                       MOVE    "パラメタ情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           OPEN    OUTPUT              RECE61-FILE
           CLOSE                       RECE61-FILE
      *
           OPEN    I-O                 RECE61-FILE  
      *
           OPEN    OUTPUT              RECE61X-FILE
           CLOSE                       RECE61X-FILE
      *
           OPEN    I-O                 RECE61X-FILE  
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
      *    診療年月編集
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
           IF      FLG-END             =   ZERO 
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
      *
               MOVE    "1"                 TO  WRK-DBKBN 
      *      
               INITIALIZE                  SRYACCT-REC
               MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" OR  "9" OR
                                               "8" OR  "B"
                   MOVE    "key60"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key36"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
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
                   PERFORM 20011-IKKATU-HENSYU-SEC
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
               MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
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
                   PERFORM 20012-IKKATU-HENSYU-SEC
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
      *    診療会計一括処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者又は未請求設定時は読み飛ばす
           MOVE    SYS-1001-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SYS-2001-SRYYM          TO  WRK-SYUKA-SRYYM
           MOVE    KEY-N-PTID              TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SRYACCT PTID=" WRK-SYUKA-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   ( PTINF-TSTPTNUMKBN   =   "1"  )
           OR   ( SYUKA-MISEIKYU-FLG  =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
      *        災害該当情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
      *        患者年齢計算
               PERFORM 500-AGECHK-SEC
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *
      *            同日再入院情報取得
                   PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM 
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       ELSE     
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                           ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       END-IF    
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
                       PERFORM 20011-FILE-SYORI-SEC 
      *
                       IF      WRK-UPDKBN          =   SPACE                   
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-END     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計読込処理
      *****************************************************************
       20011-FILE-SYORI-SEC              SECTION.
      *
      *    労災保険・自賠責のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  20011-FILE-SYORI-EXT
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
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    8                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
           AND   ( WRK-NENREI          >=  65                    )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    9                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE     9                  TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    最終診療日
           MOVE    ACCT-SRYYM          TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      ACCT-DAY (1 IDX)    >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPNUM    TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
                                       FLG-KOHRECEDSP
                                       FLG-KOHSKY-HENSYU
                                       FLG-TENKEN
      *
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *    地方公費のときレセプト請求区分を取得して判別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      COMB-KOHHKNNUM (IDX)    
                                           =   SPACE
      *
      *      平成２０年４月以降の老人保険は対象外とする
             IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
             AND   ( COMB-KOHHKNNUM (IDX)  =  "027"                )
                 CONTINUE
             ELSE        
               IF     (COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8")
               OR     (COMB-KOHHKNNUM (IDX)(1:2)   =  "99"    )
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SYS-1001-HOSPNUM        TO  HKN-HOSPNUM
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                   MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                       HKN-TEKEDYMD
                   MOVE    "00"                    TO  HKN-PAYKBN   
                   MOVE    HKNNUM-REC              TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( SYS-1001-PREFNUM    =   "40"              )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )    
                   AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                              >=  "1" AND <=  "8"   )
                   AND   ( WRK-SRYYMD(1:6)    <
                                              WRK-CONS-SRYYM-200804  )
                       IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
                       AND   ( COMB-KOHHKNNUM (IDX)
                                               NOT =   "180"   )
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                                                           OR  "5"
                               MOVE    2               TO  IDX1
                           ELSE
                               MOVE    1               TO  IDX1
                           END-IF
                       ELSE
                           PERFORM 20011-PREFKBN-40-CHECK-SEC
                       END-IF
                   ELSE
      *                福岡県の社保のとき（４１老人）
                       IF    ( COMB-HKNNUM     NOT =   "060" AND "067"
                                                             AND "039" )
                       AND   ( SYS-1001-PREFNUM    =   "40"            )
                       AND   ( HKN-HBTNUM          =   "41"            )    
                           IF      COMB-HONKZKKBN      =   "1" 
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE
                           EVALUATE    COMB-HKNNUM
                               WHEN    "060"
                               WHEN    "067" 
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    "039"                
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "4"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    OTHER                
                                   IF      HKN-RECESKYKBN
                                                       =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
                   IF     IDX1          =        1
      *                地方公費レセ記載判定
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                                               FLG-KOHSKY-HENSYU
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF      WRK-PARA-PRTKBN     =   "3" OR  "5" OR
                                                       "9" OR  "8" OR
                                                       "B"
                           DISPLAY "LNK-FTNJGNCHK-KOHNUM="
                                               LNK-FTNJGNCHK-KOHNUM
                                   " LNK-FTNJGNCHK-TENKEN-RC="
                                               LNK-FTNJGNCHK-TENKEN-RC
                                   " LNK-FTNJGNCHK-RC="
                                               LNK-FTNJGNCHK-RC
       *
                           IF     LNK-FTNJGNCHK-TENKEN-RC  =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                           IF   ( LNK-FTNJGNCHK-TENKEN-RC  =   0 )
                           AND  ( LNK-FTNJGNCHK-RC         =   1 )
                               MOVE    1             TO  FLG-TENKEN
                           END-IF
                       ELSE
                           IF     LNK-FTNJGNCHK-RC   =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                       END-IF
                       IF     LNK-FTNJGNCHK-KOHSKYFLG  =  "1"
                           MOVE       1        TO    FLG-KOHSKY-HENSYU
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                      TO  IDX1
                   IF    ( WRK-ERR-ERRKBN          >   ZERO  )
                   AND   ( COMB-KOHHKNNUM (IDX)(1:1)  =  "0" )
                   AND   ( COMB-HKNNUM             =   SPACE )
                       CONTINUE
                   ELSE 
      *                全国公費レセ記載判定（公費単独の不明分はしない）
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                                               FLG-KOHSKY-HENSYU
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF      WRK-PARA-PRTKBN     =   "3" OR  "5" OR
                                                       "9" OR  "8" OR
                                                       "B"
                           DISPLAY "LNK-FTNJGNCHK-KOHNUM="
                                               LNK-FTNJGNCHK-KOHNUM
                                   " LNK-FTNJGNCHK-TENKEN-RC="
                                               LNK-FTNJGNCHK-TENKEN-RC
                                   " LNK-FTNJGNCHK-RC="
                                               LNK-FTNJGNCHK-RC
       *
                           IF     LNK-FTNJGNCHK-TENKEN-RC  =   1
                               MOVE       1        TO  FLG-KOHRECEDSP
                           END-IF
                           IF   ( LNK-FTNJGNCHK-TENKEN-RC  =   0 )
                           AND  ( LNK-FTNJGNCHK-RC         =   1 )
                               MOVE    1             TO  FLG-TENKEN
                           END-IF
                       ELSE
                           IF     LNK-FTNJGNCHK-RC   =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                       END-IF
                   END-IF
               END-IF       
      *
               IF     IDX1                =   1
                   IF      FLG-KOHRECEDSP  NOT =  1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                       MOVE    COMB-TEKSTYMD          TO
                                          WRK-SEIKYU-TEKSTYMD (IDX1 IDY)
                       MOVE    HKN-HBTNUM              TO
                                          WRK-SEIKYU-KOHHBTNUM(IDX1 IDY)
      *                レセプト記載と併せて公費請求テーブルの作成も行う
                       IF     FLG-KOHSKY-HENSYU   =   1
                           ADD     1                      TO  IDZ    
                           MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(2 IDZ)
                           MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (2 IDZ)
                           MOVE    1                      TO  FLG-KOH 
                       END-IF
                   END-IF    
               ELSE
                   ADD     1                      TO  IDZ    
                   MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                      WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                   MOVE    COMB-KOHID     (IDX)   TO
                                      WRK-SEIKYU-KOHID    (IDX1 IDZ)
                   MOVE    1                      TO  FLG-KOH 
               END-IF  
             END-IF  
          END-PERFORM
      *
           INITIALIZE                      WRK-KOHSKY-RECE61-REC
      * 
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
               MOVE    WRK-RECE61-REC      TO  WRK-KOHSKY-RECE61-REC
           END-IF   
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
           PERFORM 20011-FILE-HENSYU-SEC
           .
       20011-FILE-SYORI-EXT.
           EXIT. 
      *            
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SYS-2001-SRYYM          TO  WRK-SYUKA-SRYYM
           MOVE    KEY-N-PTID              TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "NYUINACCT PTID=" WRK-SYUKA-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   ( PTINF-TSTPTNUMKBN   =   "1"  )
           OR   ( SYUKA-MISEIKYU-FLG  =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *        災害該当情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
      *        患者年齢計算
               PERFORM 500-AGECHK-SEC
      *
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
       20012-IKKATU-HENSYU-EXT.
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
                   MOVE    SYS-1001-HOSPNUM TO  COMB-HOSPNUM
                   MOVE    KEY-O-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
      *                医保以外読み飛ばし
                       IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM  OR
                                           WRK-CONS-KOUGAI-HKNNUM 
                           MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                       END-IF
                   ELSE
                       IF      COMB-DELKBN         =   "1"
                           MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
      *                    医保以外読み飛ばし
                           IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                           ELSE
                               MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       ELSE
                           MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                           MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
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
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    8                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
           AND   ( WRK-NENREI          >=  65                    )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    9                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE     9                  TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    最終診療日
           MOVE    NACCT-SRYYM         TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      NACCT-DAY (IDX)     >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPNUM    TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
                                       FLG-KOHRECEDSP
                                       FLG-KOHSKY-HENSYU
                                       FLG-TENKEN
      *
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *    地方公費のときレセプト請求区分を取得して判別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      COMB-KOHHKNNUM (IDX)    
                                           =   SPACE
      *
      *      平成２０年４月以降の老人保険は対象外とする
             IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
             AND   ( COMB-KOHHKNNUM (IDX)  =  "027"                )
                 CONTINUE
             ELSE        
               IF     (COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8")
               OR     (COMB-KOHHKNNUM (IDX)(1:2)   =  "99"    )
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SYS-1001-HOSPNUM        TO  HKN-HOSPNUM
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                   MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                       HKN-TEKEDYMD
                   MOVE    "00"                    TO  HKN-PAYKBN   
                   MOVE    HKNNUM-REC              TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( SYS-1001-PREFNUM    =   "40"              )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )    
                   AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                              >=  "1" AND <=  "8"    )
                       IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
                       AND   ( COMB-KOHHKNNUM (IDX)
                                               NOT =   "180"   )
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                                                           OR  "5"
                               MOVE    2               TO  IDX1
                           ELSE
                               MOVE    1               TO  IDX1
                           END-IF
                       ELSE
                           PERFORM 20011-PREFKBN-40-CHECK-SEC
                       END-IF
                   ELSE
      *                福岡県の社保のとき（４１老人）
                       IF    ( COMB-HKNNUM     NOT =   "060" AND "067"
                                                             AND "039" )
                       AND   ( SYS-1001-PREFNUM    =   "40"            )
                       AND   ( HKN-HBTNUM          =   "41"            )    
                           IF      COMB-HONKZKKBN      =   "1" 
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE
                           EVALUATE    COMB-HKNNUM
                               WHEN    "060"
                               WHEN    "067" 
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    "039"                
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "4"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    OTHER                
                                   IF      HKN-RECESKYKBN
                                                       =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
                   IF     IDX1          =        1
      *                地方公費レセ記載判定
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                                               FLG-KOHSKY-HENSYU
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF      WRK-PARA-PRTKBN     =   "3" OR  "5" OR
                                                       "9" OR  "8" OR
                                                       "B"
                           DISPLAY "LNK-FTNJGNCHK-KOHNUM="
                                               LNK-FTNJGNCHK-KOHNUM
                                   " LNK-FTNJGNCHK-TENKEN-RC="
                                               LNK-FTNJGNCHK-TENKEN-RC
                                   " LNK-FTNJGNCHK-RC="
                                               LNK-FTNJGNCHK-RC
       *
                           IF     LNK-FTNJGNCHK-TENKEN-RC  =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                           IF   ( LNK-FTNJGNCHK-TENKEN-RC  =   0 )
                           AND  ( LNK-FTNJGNCHK-RC         =   1 )
                               MOVE    1             TO  FLG-TENKEN
                           END-IF
                       ELSE
                           IF     LNK-FTNJGNCHK-RC   =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                       END-IF
                       IF     LNK-FTNJGNCHK-KOHSKYFLG  =  "1"
                           MOVE       1        TO    FLG-KOHSKY-HENSYU
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                      TO  IDX1
                   IF    ( WRK-ERR-ERRKBN          >   ZERO  )
                   AND   ( COMB-KOHHKNNUM (IDX)(1:1)  =  "0" )
                   AND   ( COMB-HKNNUM             =   SPACE )
                       CONTINUE
                   ELSE 
      *                全国公費レセ記載判定（公費単独の不明分はしない）
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                                               FLG-KOHSKY-HENSYU
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF      WRK-PARA-PRTKBN     =   "3" OR  "5" OR
                                                       "9" OR  "8" OR
                                                       "B"
                           DISPLAY "LNK-FTNJGNCHK-KOHNUM="
                                               LNK-FTNJGNCHK-KOHNUM
                                   " LNK-FTNJGNCHK-TENKEN-RC="
                                               LNK-FTNJGNCHK-TENKEN-RC
                                   " LNK-FTNJGNCHK-RC="
                                               LNK-FTNJGNCHK-RC
       *
                           IF     LNK-FTNJGNCHK-TENKEN-RC  =   1
                               MOVE       1        TO  FLG-KOHRECEDSP
                           END-IF
                           IF   ( LNK-FTNJGNCHK-TENKEN-RC  =   0 )
                           AND  ( LNK-FTNJGNCHK-RC         =   1 )
                               MOVE    1             TO  FLG-TENKEN
                           END-IF
                       ELSE
                           IF     LNK-FTNJGNCHK-RC   =   1
                               MOVE       1        TO    FLG-KOHRECEDSP
                           END-IF
                       END-IF
                   END-IF
               END-IF       
      *
               IF     IDX1                =   1
                   IF      FLG-KOHRECEDSP  NOT =  1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                       MOVE    COMB-TEKSTYMD          TO
                                          WRK-SEIKYU-TEKSTYMD (IDX1 IDY)
                       MOVE    HKN-HBTNUM              TO
                                          WRK-SEIKYU-KOHHBTNUM(IDX1 IDY)
      *                レセプト記載と併せて公費請求テーブルの作成も行う
                       IF     FLG-KOHSKY-HENSYU   =   1
                           ADD     1                      TO  IDZ    
                           MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(2 IDZ)
                           MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (2 IDZ)
                           MOVE    1                      TO  FLG-KOH 
                       END-IF
                   END-IF    
               ELSE
                   ADD     1                      TO  IDZ    
                   MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                   MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDZ)
                   MOVE    1                      TO  FLG-KOH 
               END-IF  
             END-IF  
           END-PERFORM
      *
           INITIALIZE                      WRK-KOHSKY-RECE61-REC
      * 
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
               MOVE    WRK-RECE61-REC      TO  WRK-KOHSKY-RECE61-REC
           END-IF   
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
           PERFORM 20011-FILE-HENSYU-SEC
           .
       200122-NYUINACCT-HENSYU-EXT.
           EXIT. 
           .
      *            
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    ２００６年１０月以降は公費付加情報より設定
           IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
      *      県外扱いの保険者はレセプトへ記載しない（２００６年９月まで）
             IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
             ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
             END-IF
             IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
             ELSE
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM      TO  WRK-SYMD (1:6)
               ELSE
                   MOVE    SYS-2002-SRYYM      TO  WRK-SYMD (1:6)
               END-IF
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD      
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"     
                           IF      PTHKN-HKNJANUM = 
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF            
               END-PERFORM
             END-IF
           END-IF
      *        
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN      =   SPACE
               MOVE    2                   TO  IDX1
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM  TO  LNK-SKOHPLUS-SRYYMD (1:6)
               ELSE
                   MOVE    SYS-2002-SRYYM  TO  LNK-SKOHPLUS-SRYYMD (1:6)
               END-IF
               MOVE    "01"             TO  LNK-SKOHPLUS-SRYYMD (7:2)  
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
               IF      LNK-SKOHPLUS-RC =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    2                   TO  IDX1
                   ELSE
                       MOVE    1                   TO  IDX1
                   END-IF
               ELSE
                   MOVE    1                   TO  IDX1
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       20011-FILE-HENSYU-SEC              SECTION.
      *
      *    公費で不明分は出力しない
           IF    ( WRK-SYOKBN         =   "2" )
           AND   ( WRK-TEISEIKBN      =   1   )
               INITIALIZE                      WRK-RECE61-REC
               GO  TO  20011-FILE-HENSYU-EXT
           END-IF
      *     
           INITIALIZE                      WRK-RECE61-REC
           MOVE    SYS-1001-HOSPNUM    TO  WRK-RECE61-HOSPNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    SYS-2001-SRYYM      TO  WRK-RECE61-SRYYM
           ELSE
               MOVE    SYS-2002-SRYYM      TO  WRK-RECE61-SRYYM
           END-IF
           MOVE    "1"                 TO  WRK-RECE61-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE61-PTID
           MOVE    COMB-HKNNUM         TO  WRK-RECE61-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  WRK-RECE61-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOHID
                   MOVE    "0"                 TO  WRK-RECE61-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  WRK-RECE61-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *****                IF     (WRK-NENREI  >=   70)  AND
      *****                       (WRK-NENREI  <=   74)
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "0"
                                   WHEN   "4"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "3"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                              END-EVALUATE
      *****               END-IF     
                      WHEN    "040" 
                      WHEN    "039" 
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                          END-EVALUATE             
                      WHEN    OTHER 
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF 
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKNJANUM
      *
           MOVE    ZERO                TO  WRK-RECE61-TEKSTYMD 
           IF      COMB-HKNNUM         =   SPACE
               IF      COMB-KOH1HKNNUM     =   "020"   OR  "030"   OR
                                               "053"
                   MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOH-KOHID
               END-IF
           END-IF
      *****MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      WRK-STYMD (1:6)     <=   "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
      *****        IF     (WRK-NENREI  >=   70)  AND
      *****               (WRK-NENREI  <=   74)
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "0"
                           WHEN   "4"
                               MOVE    "0"             TO
                                               WRK-RECE61-HOJOKBN
                           WHEN   "1"
                           WHEN   "5"
                               MOVE    "1"             TO
                                               WRK-RECE61-HOJOKBN
                           WHEN   "2"
                           WHEN   "6"
                               MOVE    "2"             TO
                                               WRK-RECE61-HOJOKBN
                       END-EVALUATE
      *****        END-IF     
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( WRK-STYMD (1:6)
                                           <   WRK-CONS-SRYYM-200804 )
                       OR    ( WRK-NENREI  <=  74                    )  
                           MOVE    PTHKN-HOJOKBN   TO
                                                  WRK-RECE61-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "0"
                       WHEN   "4"
                           MOVE    "0"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "1"
                       WHEN   "5"
                           MOVE    "1"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "2"
                       WHEN   "6"
                           MOVE    "2"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "3"
                           MOVE    "3"             TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE
               WHEN    "040" 
               WHEN    "039" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           END-IF               
           MOVE    SPACE               TO WRK-RECE61-RECEKA
      *
      *    災害該当の有無（平成２８年１０月以降）
           IF    ( SYS-25-DISASTER-RECEIPT
                                           =   "1"     )
           AND   ( WRK-RECE61-SRYYM        >=   201610 )
               IF    ( WRK-RECE61-RECEIPT-KBN (5:1)
                                               =   SPACE )
               AND   ( TBL-PTKOHINF-MAX        >   ZERO  )
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       TBL-PTKOHINF-MAX
                       IF    ( TBL-PTKOH-TEKSTYMD (IDX1)
                                           <=  COMB-TEKSTYMD           )
                       AND   ( COMB-TEKEDYMD
                                           <=  TBL-PTKOH-TEKEDYMD(IDX1))
                           IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "959"
                               IF      WRK-RECE61-HKNNUM   =   SPACE
                                   IF        TBL-PTKOH-FTNJANUM
                                                       (IDX1) (1:1)
                                                               =   "1"
                                       MOVE    "1"         TO
                                           WRK-RECE61-RECEIPT-KBN  (5:1)
                                   END-IF
                               ELSE
                                   MOVE    "1"         TO
                                               WRK-RECE61-RECEIPT-KBN
                                                                   (5:1)
                               END-IF
                               MOVE    TBL-PTKOHINF-MAX
                                                   TO  IDX1
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    患者レセプト分割情報の有無
           IF    ( WRK-RECE61-RECEIPT-KBN (2:3)
                                           =   SPACE )
           AND   ( TBL-PT-HDIST-DAY-MAX    >   ZERO  )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       TBL-PT-HDIST-DAY-MAX
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       2
                       PERFORM VARYING IDX5    FROM    1   BY  1
                               UNTIL   IDX5    >       10
                               OR      TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                                               =   ZERO
                         IF      TBL-PT-HDIST-DAY-DLTFLG
                                                       (IDX1 IDX4 IDX5)
                                                       =   "1"
                           CONTINUE
                         ELSE
                           IF      COMB-HKNCOMBINUM    =
                                   TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                               MOVE    TBL-PT-HDIST-DAY-DAY (IDX1)
                                                       TO
                                           WRK-RECE61-RECEIPT-KBN (2:2)
                               IF      IDX4            =   1
                                   MOVE    "1"         TO
                                           WRK-RECE61-RECEIPT-KBN (4:1)
                               ELSE
                                   MOVE    "2"         TO
                                           WRK-RECE61-RECEIPT-KBN (4:1)
                               END-IF
                               IF    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKSTYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKSTYMD (7:2)     )
                               OR    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKEDYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKEDYMD (7:2)     )
                                   MOVE    10              TO  IDX5
                                   MOVE    2               TO  IDX4
                                   MOVE    TBL-PT-HDIST-DAY-MAX
                                                           TO  IDX1   
                               END-IF
                           END-IF
                         END-IF
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    レセのとき地方公費のみの場合は出力しない
           IF      WRK-SYOKBN             =   "1"        
               IF    ( WRK-RECE61-HKNNUM      =   SPACE )
               AND   ( WRK-RECE61-KOHID       =   ZERO  )
               AND   ( WRK-SEIKYU-KOHID (1 1) =   ZERO  )
                   GO  TO  20011-FILE-HENSYU-EXT
               END-IF
           END-IF 
      *   
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
                   PERFORM 900-RECE61-READ-SEC
                   IF      FLG-RECE61          =   ZERO
                       MOVE    RECE61-REC          TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
               WHEN    "2"
                   MOVE    WRK-RECE61-KEY      TO  RECE61X-KEY 
                   PERFORM 900-RECE61X-READ-SEC
                   IF      FLG-RECE61X         =   ZERO
                       MOVE    RECE61X-REC     TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
           END-EVALUATE
      *
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF    ( WRK-SYOKBN          =   "1"   )
           AND   ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOHHKNNUM (1)  =   "012"
                                       OR  "015"
                                       OR  "016"
                                       OR  "021"
                                       OR  "025" )
           AND   ( COMB-KOHHKNNUM (2)  =   SPACE )
               PERFORM 20013-SEIHO-RECE61-WRITE-SEC
           END-IF
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE61-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           MOVE    COMB-HKNCOMBINUM   TO  WRK-RECE61-HKNCOMBI-O (1)
           MOVE    COMB-HKNNUM        TO  WRK-RECE61-HKNNUMHC   (1)
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
               MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                           WRK-RECE61-KOHNUMHC (1 IDX1)
               MOVE    COMB-KOHID     (IDX1)   TO
                                           WRK-RECE61-KOHIDHC  (1 IDX1)
           END-PERFORM
      *
           IF      WRK-DBKBN          =   "2"
               CONTINUE
           ELSE              
               MOVE    ACCT-SRYKA     TO  WRK-RECE61-SRYKA      (1)
           END-IF
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                        (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                       ( WRK-DAY (2 IDX3)        >   ZERO )
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
                 IF    ( WRK-DAY (1 IDX3)        =   ZERO )
                 AND   ( LNK-NYUINDAY-KBN(IDX3)
                                             NOT =   "1"  )
                 AND   ( WRK-DAY (2 IDX3)        >   ZERO )
                    MOVE   WRK-DAY (2 IDX3)   TO  WRK-RECE61-DAY-1
                                                              (IDX3)
                 END-IF
               END-PERFORM
           END-IF
           MOVE    PTHKN-TEKSTYMD     TO  WRK-RECE61-TEKSTYMDX  (1)
           MOVE    PTHKN-TEKEDYMD     TO  WRK-RECE61-TEKEDYMDX  (1)
           IF    ( COMB-HKNNUM        =   SPACE )
           AND   ( COMB-KOH1HKNNUM    =   "020"
                                      OR  "030"
                                      OR  "053" )
      *********MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMDX  (1)
      *********MOVE    COMB-TEKEDYMD       TO  WRK-RECE61-TEKEDYMDX  (1)
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
               MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMDX  (1)
               MOVE    PTKOH-TEKEDYMD      TO  WRK-RECE61-TEKEDYMDX  (1)
           END-IF
      *    
      *    主保険情報
           MOVE    PTHKN-HKNID        TO  WRK-RECE61-HKNID
           MOVE    PTHKN-CONTKBN      TO  WRK-RECE61-CONTKBN
           MOVE    PTHKN-HKNJANUM     TO  WRK-RECE61-HKN-HKNJANUM
           MOVE    PTHKN-HONKZKKBN    TO  WRK-RECE61-HKN-HONKZKKBN
           MOVE    PTHKN-HOJOKBN      TO  WRK-RECE61-HKN-HOJOKBN
           IF      PTHKN-HKNNUM       =   "002"
               IF    ( WRK-STYMD (1:6)    <   WRK-CONS-SRYYM-200804 )
               OR    ( WRK-NENREI         <=  74                    )  
                   CONTINUE
               ELSE
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "A"
                       WHEN    "G"
                       WHEN    "D"
                           MOVE    "1" TO  WRK-RECE61-HKN-HOJOKBN
                       WHEN    "B"
                       WHEN    "H"
                       WHEN    "E"
                           MOVE    "2" TO  WRK-RECE61-HKN-HOJOKBN
                       WHEN    "C"
                       WHEN    "I"
                       WHEN    "F"
                           MOVE    "3" TO  WRK-RECE61-HKN-HOJOKBN
                   END-EVALUATE
               END-IF
           END-IF
           MOVE    PTHKN-KIGO         TO  WRK-RECE61-KIGO
           MOVE    PTHKN-NUM          TO  WRK-RECE61-NUM
           MOVE    SPACE              TO  WRK-RECE61-CERTNUM
      *
           MOVE    WRK-TEISEIKBN      TO  WRK-RECE61-TEISEIKBN
           IF      WRK-TEISEIKBN      =   "1"
               MOVE    WRK-ERR-ERRKBN     TO  WRK-RECE61-ERR-ERRKBN
               MOVE    WRK-ERR-DAY        TO  WRK-RECE61-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                          TO  WRK-RECE61-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD   TO  WRK-RECE61-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD   TO  WRK-RECE61-ERR-TEKEDYMD
           END-IF
      *
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    WRK-RECE61-HOSPNUM  TO  SPTID-HOSPNUM
           MOVE    WRK-RECE61-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-RECE61-PTNUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME       TO  WRK-RECE61-KANANAME
           MOVE   PTINF-NAME           TO  WRK-RECE61-NAME
           MOVE   PTINF-SEX            TO  WRK-RECE61-SEX
           MOVE   PTINF-BIRTHDAY       TO  WRK-RECE61-BIRTHDAY
           MOVE   PTINF-DEATHKBN       TO  WRK-RECE61-DEATHKBN
           MOVE   PTINF-TSTPTNUMKBN    TO  WRK-RECE61-TSTPTNUMKBN
      *
           MOVE   PTINF-HOME-POST      TO  WRK-RECE61-HOME-POST
           MOVE   PTINF-HOME-ADRS      TO  WRK-RECE61-HOME-ADRS
           MOVE   PTINF-HOME-BANTI     TO  WRK-RECE61-HOME-BANTI
           MOVE   PTINF-RENRAKU-POST   TO  WRK-RECE61-RENRAKU-POST
           MOVE   PTINF-RENRAKU-NAME   TO  WRK-RECE61-RENRAKU-NAME
           MOVE   PTINF-RENRAKU-ADRS   TO  WRK-RECE61-RENRAKU-ADRS
           MOVE   PTINF-RENRAKU-BANTI  TO  WRK-RECE61-RENRAKU-BANTI
           MOVE   PTINF-RENRAKU-TEL1   TO  WRK-RECE61-RENRAKU-TEL1
           MOVE   PTINF-RENRAKU-TEL2   TO  WRK-RECE61-RENRAKU-TEL2
      *    患者年齢
           MOVE   WRK-NENREI           TO  WRK-RECE61-AGE
           MOVE    AGECHK-SYUGAKUKBN   TO  WRK-RECE61-SYUGAKUKBN
      *    旧姓履歴の取得
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    WRK-RECE61-HOSPNUM      TO  KYUSEI-HOSPNUM
           MOVE    WRK-RECE61-PTID         TO  KYUSEI-PTID
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-RECE61-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK      =   ZERO
               MOVE    KYUSEI-NAME         TO  WRK-RECE61-NAME
               MOVE    KYUSEI-KANANAME     TO  WRK-RECE61-KANANAME
           END-IF
      *    県番号
           MOVE    SYS-1001-PREFNUM    TO  WRK-RECE61-PREFNUM
      *    印刷順
           IF    ( WRK-PARA-SYOKBN     =   "2" )
           AND   ( WRK-SYOKBN          =   "1" )
           AND   ( BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                   OR  "05" )
               ADD     1                       TO  WRK-PRTSEQ
               MOVE    WRK-PRTSEQ              TO  WRK-RECE61-PRTSEQ
           END-IF 
      *
      *レセプト請求しない地方公費情報
           IF    ( WRK-SYOKBN          =   "1"  )
           AND   ( FLG-KOH             =   1    )
           AND   ( WRK-KOHSKY-KOHID-O (1)
                                       >   ZERO )
               INITIALIZE              WRK-RECE61-KOHSKY-KOHI-AREA
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       4
                   MOVE    WRK-KOHSKY-KOHID-O     (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHID-O
                                                               (IDX1)
                   MOVE    WRK-KOHSKY-KOHHKNNUM-O (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHHKNNUM-O
                                                               (IDX1)
               END-PERFORM
           END-IF    
      *
      *未請求フラグ
           IF      WRK-SYOKBN          =   "1"
               MOVE    SYUKA-MISEIKYU-FLG  TO  WRK-RECE61-MISEIKYU-FLG
           END-IF
      *
      *点検レセ０点公費フラグ
           IF    ( WRK-SYOKBN          =   "1" )
           AND   ( FLG-TENKEN          =   1   ) 
               MOVE    "1"                 TO  WRK-RECE61-TENKEN-FLG
           END-IF 
      *
      *    入院料情報取得フラグ
           MOVE    1               TO  WRK-RECE61-DOUJITSU-FLG
      *    同日再入院（入院分）の保険組合せ、食事開始時間区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       31
               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX1)
                                           TO
                                   WRK-RECE61-DOUJITSU-HKNCOMBI (IDX1)
               MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX1)
                                           TO
                                   WRK-RECE61-DOUJITSU-SKJKBN   (IDX1)
           END-PERFORM
      *    入院区分
           MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE61-NYUINDAY-TBL
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   WRITE   RECE61-REC
      *
                   IF      STS-RECE61          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERRMSG-AREA
                       INITIALIZE                      WRK-ERRMSG-AREA
      *
                       MOVE    3                   TO  WRK-ERR-FLG
                       MOVE    RECE61PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE61          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT1
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   WRITE   RECE61X-REC
      *
                   IF      STS-RECE61X         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERRMSG-AREA
                       INITIALIZE                      WRK-ERRMSG-AREA
      *
                       MOVE    3                   TO  WRK-ERR-FLG
                       MOVE    RECE61XPARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE61X         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT2
           END-EVALUATE
      *
           .
       20011-RECE61-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE61-REWRITE-SEC            SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      WRK-RECE61-HKNCOMBI-O  (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                   MOVE    COMB-HKNNUM         TO
                                       WRK-RECE61-HKNNUMHC   (IDX)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       4
                       MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                       WRK-RECE61-KOHNUMHC (IDX IDX1)
                       MOVE    COMB-KOHID     (IDX1)   TO
                                       WRK-RECE61-KOHIDHC  (IDX IDX1)
                   END-PERFORM
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-DBKBN               =   "2"
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                   IF      WRK-RECE61-SRYKA    (IDX)       =   SPACE
                       MOVE    ACCT-SRYKA  TO  WRK-RECE61-SRYKA (IDX)
                       MOVE    20          TO  IDX
                   ELSE
                       IF      WRK-RECE61-SRYKA  (IDX) =   ACCT-SRYKA
                           MOVE    20          TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    1                       TO  FLG-LASTHKNID
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF    ( WRK-RECE61-TEKSTYMDX (IDX)
                                       =   PTHKN-TEKSTYMD )
               AND   ( WRK-RECE61-TEKEDYMDX (IDX)
                                       =   PTHKN-TEKEDYMD )
                   MOVE    10              TO  IDX
               ELSE
                   IF      WRK-RECE61-TEKSTYMDX (IDX)  =   SPACE
                       MOVE    PTHKN-TEKSTYMD  TO
                                           WRK-RECE61-TEKSTYMDX (IDX)
                       MOVE    PTHKN-TEKEDYMD  TO
                                           WRK-RECE61-TEKEDYMDX (IDX)
                       MOVE    10              TO  IDX
                   ELSE
                       IF      WRK-RECE61-TEKSTYMDX(IDX)
                                                   >=  PTHKN-TEKSTYMD
                           MOVE    ZERO                TO  FLG-LASTHKNID
                       END-IF
                   END-IF
               END-IF        
           END-PERFORM
           IF      FLG-LASTHKNID       =   1
               MOVE    PTHKN-HKNID         TO  WRK-RECE61-HKNID
               MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKN-HKNJANUM
               MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HKN-HONKZKKBN
               MOVE    PTHKN-HOJOKBN       TO  WRK-RECE61-HKN-HOJOKBN
               IF      PTHKN-HKNNUM        =   "002"
                   IF    ( WRK-STYMD (1:6) <   WRK-CONS-SRYYM-200804 )
                   OR    ( WRK-NENREI      <=  74                    )  
                       CONTINUE
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                           WHEN    "D"
                               MOVE    "1" TO  WRK-RECE61-HKN-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                           WHEN    "E"
                               MOVE    "2" TO  WRK-RECE61-HKN-HOJOKBN
                           WHEN    "C"
                           WHEN    "I"
                           WHEN    "F"
                               MOVE    "3" TO  WRK-RECE61-HKN-HOJOKBN
                       END-EVALUATE
                   END-IF
               END-IF
               MOVE    PTHKN-CONTKBN       TO  WRK-RECE61-CONTKBN
               MOVE    PTHKN-KIGO          TO  WRK-RECE61-KIGO
               MOVE    PTHKN-NUM           TO  WRK-RECE61-NUM
               MOVE    SPACE               TO  WRK-RECE61-CERTNUM
           END-IF
      *
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                        (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                       ( WRK-DAY (2 IDX3)        >   ZERO )
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
                 IF    ( WRK-DAY (1 IDX3)        =   ZERO )
                 AND   ( LNK-NYUINDAY-KBN(IDX3)
                                             NOT =   "1"  )
                 AND   ( WRK-DAY (2 IDX3)        >   ZERO )
                     MOVE   WRK-DAY (2 IDX3)   TO  WRK-RECE61-DAY-1
                                                              (IDX3)
                 END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
               IF      WRK-RECE61-TEISEIKBN    =   SPACE
                   MOVE    WRK-ERR-ERRKBN  TO  WRK-RECE61-ERR-ERRKBN
                   MOVE    WRK-ERR-DAY     TO  WRK-RECE61-ERR-DAY
                   MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE61-ERR-HKNCOMBINUM
                   MOVE    WRK-ERR-TEKSTYMD
                                           TO  WRK-RECE61-ERR-TEKSTYMD
                   MOVE    WRK-ERR-TEKEDYMD
                                           TO  WRK-RECE61-ERR-TEKEDYMD
               END-IF            
               MOVE    "1"                 TO  WRK-RECE61-TEISEIKBN     
           END-IF    
      *
      *レセプト請求しない地方公費情報
           IF    ( WRK-SYOKBN          =   "1"  )
           AND   ( FLG-KOH             =   1    )
           AND   ( WRK-KOHSKY-KOHID-O (1)
                                       >   ZERO )
               INITIALIZE              WRK-RECE61-KOHSKY-KOHI-AREA
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       4
                   MOVE    WRK-KOHSKY-KOHID-O     (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHID-O
                                                               (IDX1)
                   MOVE    WRK-KOHSKY-KOHHKNNUM-O (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHHKNNUM-O
                                                               (IDX1)
               END-PERFORM
           END-IF    
      *
      *点検レセ０点公費フラグ
           IF    ( WRK-SYOKBN          =   "1" )
           AND   ( FLG-TENKEN          =   1   ) 
               MOVE    "1"                 TO  WRK-RECE61-TENKEN-FLG
           END-IF 
      *
           IF      WRK-RECE61-DOUJITSU-FLG =   ZERO
      *        入院料情報取得フラグ
               MOVE    1               TO  WRK-RECE61-DOUJITSU-FLG
      *        同日再入院（入院分）の保険組合せ、食事開始時間区分
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       31
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX1)
                                               TO
                                   WRK-RECE61-DOUJITSU-HKNCOMBI (IDX1)
                   MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX1)
                                               TO
                                   WRK-RECE61-DOUJITSU-SKJKBN   (IDX1)
               END-PERFORM
      *        入院区分
               MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE61-NYUINDAY-TBL
           END-IF
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   REWRITE RECE61-REC
      *
                   IF      STS-RECE61          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERRMSG-AREA
                       INITIALIZE                      WRK-ERRMSG-AREA
      *
                       MOVE    4                   TO  WRK-ERR-FLG
                       MOVE    RECE61PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE61          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   REWRITE RECE61X-REC
      *
                   IF      STS-RECE61X         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERRMSG-AREA
                       INITIALIZE                      WRK-ERRMSG-AREA
      *
                       MOVE    4                   TO  WRK-ERR-FLG
                       MOVE    RECE61XPARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE61X         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
           END-EVALUATE
           .
       20011-RECE61-REWRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
      *****************************************************************
       20013-SEIHO-RECE61-WRITE-SEC              SECTION.
      *
           MOVE    WRK-RECE61-REC      TO  WRK-SEIHO-RECE61-REC
      *
           INITIALIZE                      WRK-RECE61-REC
           MOVE    WRK-SEIHO-KEY       TO  WRK-RECE61-KEY
      *
      *****MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
           INITIALIZE                      PTKOHINF-REC
           MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
           MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
           MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMD
      *    生活保護期間重複対応のため
           IF      COMB-KOHHKNNUM (1)  =   "012"
               MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOH-KOHID
           END-IF
      *
           MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
           PERFORM 900-RECE61-READ-SEC
           IF      FLG-RECE61          =   ZERO
               MOVE    RECE61-REC          TO  WRK-RECE61-REC 
               PERFORM 20011-RECE61-REWRITE-SEC
           ELSE
      *********MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD 
      *********MOVE    COMB-TEKEDYMD       TO  PTHKN-TEKEDYMD
               MOVE    PTKOH-TEKSTYMD      TO  PTHKN-TEKSTYMD
               MOVE    PTKOH-TEKEDYMD      TO  PTHKN-TEKEDYMD
      *  
               PERFORM 20011-RECE61-WRITE-SEC
           END-IF
      *
           .
       20013-SEIHO-RECE61-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                    TO  TBL-MAX
                                               FLG-ADDKBN 
      *
           MOVE    WRK-RECE61-KOHI-AREA    TO  WRK-KOHI-AREA
      *
           MOVE    WRK-SYOKBN              TO  IDX1   
      *
      *    公費の追加 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE    SPACE               TO  WRK-RECESKYKBN
      *
               IF      WRK-SEIKYU-KOHID (IDX1 IDX)    =   ZERO
                   MOVE    10                  TO  IDX
               ELSE    
      *            老人＋原爆のとき  
                   IF    ( WRK-RECE61-KOHID    NOT =   ZERO  )
                   AND   ( WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)  
                                                   =   "019" )
                       IF      WRK-RECE61-HKNNUM   =   "060"   OR  "067" 
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       END-IF
                   ELSE
                       MOVE    SPACE       TO  WRK-RECESKYKBN
                   END-IF
                   PERFORM 200121-KOHID-TBL-HENSYU-SEC
               END-IF      
           END-PERFORM
      *
      *    公費を追加しなかったとき
           IF      FLG-ADDKBN              =   ZERO
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    優先順位に並べ替え
           INITIALIZE                      WRK-RECE61-KOHI-AREA
           MOVE    ZERO                    TO  TBL-MAX
      *
      *
           IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM
               MOVE    1                   TO  IDXX
           ELSE
               IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM-200804
                   MOVE    2                   TO  IDXX
               ELSE
                   IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM-201501
                       MOVE    3                   TO  IDXX
                   ELSE
                       MOVE    4                   TO  IDXX
                   END-IF
               END-IF
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-SRT-KOHHKNNUM-MAX
                   OR      WRK-SRT-KOHHKNNUM (IDXX IDX)
                                       =   SPACE
                   OR      TBL-MAX     =   10 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                   IF      WRK-SRT-KOHHKNNUM   (IDXX IDX)
                                       =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-RECE61-KOHI-TBL (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL    (IDY)
      *****************MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    法別、適用開始日による全国公費の並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       9
                   OR      WRK-RECE61-KOHHKNNUM-O (IDX) =   SPACE
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                       OR      WRK-RECE61-KOHHKNNUM-O (IDY) =   SPACE
                   IF   ( WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =   WRK-RECE61-KOHHKNNUM-O   (IDY) )
                   AND  ( WRK-RECE61-KOHTEKSTYMD-O (IDX)
                                   >   WRK-RECE61-KOHTEKSTYMD-O (IDY) )
                       MOVE    WRK-RECE61-KOHI-TBL(IDX)    TO  WRK-KOHI
                       MOVE    WRK-RECE61-KOHI-TBL(IDY)    TO
                                             WRK-RECE61-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI                    TO
                                             WRK-RECE61-KOHI-TBL(IDY)
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時 
           IF      WRK-KOHHKNNUM-O (1)     =   SPACE
           AND     WRK-KOHHKNNUM-O (2)     =   SPACE
           AND     WRK-KOHHKNNUM-O (3)     =   SPACE
           AND     WRK-KOHHKNNUM-O (4)     =   SPACE
           AND     WRK-KOHHKNNUM-O (5)     =   SPACE
           AND     WRK-KOHHKNNUM-O (6)     =   SPACE
           AND     WRK-KOHHKNNUM-O (7)     =   SPACE
           AND     WRK-KOHHKNNUM-O (8)     =   SPACE
           AND     WRK-KOHHKNNUM-O (9)     =   SPACE
           AND     WRK-KOHHKNNUM-O (10)    =   SPACE
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           MOVE  ZERO                  TO  WRK-SYS2013-IDX
           IF      TBL-SYS2013-MAX     >   ZERO
               MOVE    WRK-RECE61-SRYYM    TO  WRK-SYS-SRYYMD (1:6)
               MOVE    "01"                TO  WRK-SYS-SRYYMD (7:2)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   TBL-SYS2013-MAX
                       OR      WRK-SYS2013-IDX
                                           >   0
                   IF    ( TBL-SYS2013-STYUKYMD (IDX)
                                               <=  WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <=  TBL-SYS2013-EDYUKYMD
                                                               (IDX) )
                       MOVE    IDX             TO    WRK-SYS2013-IDX
                   END-IF
               END-PERFORM
           END-IF
           IF     WRK-SYS2013-IDX      >   ZERO
      *        公費記載順設定情報による並び替え処理
               PERFORM 200121-KOHID-HENSYU-SYS2013-SEC
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       9
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                 IF     WRK-KOHHKNNUM-O (IDY) =   SPACE
                   CONTINUE
                 ELSE 
                   IF     WRK-KOHHKNNUM-O (IDX) =   SPACE
                       MOVE    WRK-KOHI-TBL(IDX) TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL(IDY) TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI          TO  WRK-KOHI-TBL(IDY)
                   ELSE
      *                北海道の地方公費（９１＋９０、４５＋４６、９３＋９４）
                       IF  ( ( SYS-1001-PREFNUM    =   "01"  ) 
                       AND   ( WRK-KOHHBTNUM-O (IDX)
                                                   =   "90"  OR  "91" )
                       AND   ( WRK-KOHHBTNUM-O (IDY)
                                                   =   "90"  OR  "91" ))
                       OR  ( ( SYS-1001-PREFNUM    =   "01"  ) 
                       AND   ( WRK-KOHHBTNUM-O (IDX)
                                                   =   "45"  OR  "46" )
                       AND   ( WRK-KOHHBTNUM-O (IDY)
                                                   =   "45"  OR  "46" ))
                       OR  ( ( SYS-1001-PREFNUM    =   "01"  ) 
                       AND   ( WRK-KOHHBTNUM-O (IDX)
                                                   =   "93"  OR  "94" )
                       AND   ( WRK-KOHHBTNUM-O (IDY)
                                                   =   "93"  OR  "94" ))
                           IF      WRK-KOHTEKSTYMD-O (IDX)
                                               <   WRK-KOHTEKSTYMD-O
                                                            (IDY)
                               CONTINUE
                           ELSE
                               IF      WRK-KOHTEKSTYMD-O (IDX)
                                               >   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                   MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                   MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                   MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                               ELSE
                                   IF    ( WRK-KOHHBTNUM-O (IDX)
                                                   >   WRK-KOHHBTNUM-O
                                                                (IDY) )
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                   END-IF
                               END-IF
                           END-IF
                       ELSE
      *                    徳島県の地方公費（法別４５、４８）
                           IF    ( SYS-1001-PREFNUM    =   "36"    ) 
                           AND   ( WRK-KOHHKNNUM-O (IDX) (2:2)
                                                   =   "45"    OR "48" )
                           AND   ( WRK-KOHHKNNUM-O (IDY) (2:2)
                                                   =   "45"    OR "48" )
                               IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   <   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                   CONTINUE
                               ELSE
                                   IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   >   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                   ELSE
                                       IF    ( WRK-KOHHKNNUM-O (IDX)
                                                   >   WRK-KOHHKNNUM-O
                                                                (IDY) )
                                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                           MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                       END-IF
                                   END-IF
                               END-IF
                           ELSE
      *                        山梨県の地方公費（２４１＋１４３）
                               IF    ( SYS-1001-PREFNUM    =   "19"  ) 
                               AND   ( WRK-KOHHKNNUM-O (IDX)
                                                           =   "241" )
                               AND   ( WRK-KOHHKNNUM-O (IDY)
                                                           =   "143" )
                                   CONTINUE
                               ELSE
      *                            同じ公費のとき適用開始日の古い順にする
                                   IF    ( WRK-KOHHKNNUM-O (IDX) 
                                           =   WRK-KOHHKNNUM-O (IDY) )
                                   OR    ( WRK-KOHHBTNUM-O (IDX) 
                                           =   WRK-KOHHBTNUM-O (IDY) )
      ****                             IF      WRK-KOHTEKSTYMD-O (IDX)
      ****                                        >   WRK-KOHTEKSTYMD-O
      ****                                                      (IDY)
                                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                           MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
      ****                             END-IF
                                   ELSE
                                       IF      WRK-KOHHKNNUM-O (IDX)
                                               >   WRK-KOHHKNNUM-O (IDY)
                                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                           MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF    
                   END-IF    
                 END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    法別、適用開始日による並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       9
                   OR      WRK-KOHHKNNUM-O (IDX) =   SPACE
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                       OR      WRK-KOHHKNNUM-O (IDY) =   SPACE
                   IF  (   ( WRK-KOHHKNNUM-O (IDX) 
                                       =   WRK-KOHHKNNUM-O (IDY) )
                        OR ( WRK-KOHHBTNUM-O (IDX) 
                                       =   WRK-KOHHBTNUM-O (IDY) ) )
                   AND ( WRK-KOHTEKSTYMD-O (IDX)
                                       >   WRK-KOHTEKSTYMD-O (IDY) )
                       MOVE    WRK-KOHI-TBL (IDX)  TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL (IDY)  TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI            TO  WRK-KOHI-TBL(IDY)
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   10
                   OR      TBL-MAX     =   10 
               IF      WRK-KOHID-O (IDX)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDX)  TO 
                                           WRK-RECE61-KOHI-TBL(TBL-MAX)
               END-IF    
           END-PERFORM
      *
      *    長崎の生保と被爆（０１２＋１８６）
           IF      SYS-1001-PREFNUM    =   "42"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "012"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    10                  TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "186"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    10                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    愛知の生保と特定疾患（０１２＋１９１）
           IF      SYS-1001-PREFNUM    =   "23"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "012"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    10                  TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "191"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    10                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    山梨の特定疾患と老人医療費（０５１、０９１＋１４１、２４１）
           IF      SYS-1001-PREFNUM    =   "19"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "051"   OR  "091"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    10                  TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "141"   OR  "241"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    10                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    東京都の患者票とマル都（法別８２、９３）
           IF    ( SYS-1001-PREFNUM    =   "13" ) 
               MOVE    ZERO            TO  WRK-SEIHO-IDX
                                           WRK-HIBAKU-IDX       
                                           WRK-KOH-IDX
      *
               INITIALIZE                  WRK-KOHI-AREA
               MOVE    ZERO                TO  TBL-MAX   
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX) (1:1)
                                               =   "0" OR  "9"
                       MOVE    IDX                 TO  WRK-KOH-IDX
                       ADD     1                   TO  TBL-MAX
                       MOVE    WRK-RECE61-KOHI-TBL (IDX)  TO 
                                           WRK-KOHI-TBL(TBL-MAX)
                   ELSE
                       IF      WRK-RECE61-KOHHKNNUM-O (IDX) (2:2)
                                                   =   "93"
                           MOVE    IDX             TO  WRK-SEIHO-IDX
                       END-IF
                       IF      WRK-RECE61-KOHHKNNUM-O (IDX) (2:2)
                                                   =   "82"
                           MOVE    IDX             TO  WRK-HIBAKU-IDX 
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-SEIHO-IDX       >   ZERO
                   COMPUTE WRK-SEIHO-IDX   =   WRK-SEIHO-IDX   - 
                                               WRK-KOH-IDX
               END-IF
               IF      WRK-HIBAKU-IDX      >   ZERO
                   COMPUTE WRK-HIBAKU-IDX  =   WRK-HIBAKU-IDX  - 
                                               WRK-KOH-IDX
               END-IF
               EVALUATE    WRK-SEIHO-IDX   ALSO    WRK-HIBAKU-IDX
                   WHEN    ZERO            ALSO    ZERO
                   WHEN    ZERO            ALSO    1
                   WHEN    1               ALSO    ZERO
                   WHEN    1               ALSO    2
                       CONTINUE
                   WHEN    OTHER
                       IF      WRK-SEIHO-IDX       >   ZERO
                           ADD     1                   TO  TBL-MAX
                           COMPUTE WRK-SEIHO-IDX   =   WRK-SEIHO-IDX   +
                                                       WRK-KOH-IDX
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                                   TO 
                                                   WRK-KOHI-TBL(TBL-MAX)
                           INITIALIZE              WRK-RECE61-KOHI-TBL
                                                       (WRK-SEIHO-IDX)
                       END-IF
                       IF      WRK-HIBAKU-IDX      >   ZERO
                           ADD     1                   TO  TBL-MAX
                           COMPUTE WRK-HIBAKU-IDX  =   WRK-HIBAKU-IDX  +
                                               WRK-KOH-IDX
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-HIBAKU-IDX)
                                                   TO 
                                                   WRK-KOHI-TBL(TBL-MAX)
                           INITIALIZE              WRK-RECE61-KOHI-TBL
                                                       (WRK-HIBAKU-IDX)
                       END-IF        
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX         >   10
                               OR      TBL-MAX     =   10
                           IF      IDX             >   WRK-KOH-IDX
                               IF      WRK-RECE61-KOHID-O (IDX)
                                                       >   ZERO
                                   ADD     1               TO  TBL-MAX
                                   MOVE    WRK-RECE61-KOHI-TBL (IDX)
                                                           TO 
                                                   WRK-KOHI-TBL(TBL-MAX)
                                   INITIALIZE      WRK-RECE61-KOHI-TBL
                                                               (TBL-MAX)
                               END-IF    
                           END-IF    
                       END-PERFORM
      *
                       MOVE    WRK-KOHI-AREA   TO  WRK-RECE61-KOHI-AREA
                       INITIALIZE                  WRK-KOHI-AREA
               END-EVALUATE
           END-IF
           .
       20012-KOHID-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費情報退避処理
      *****************************************************************
       200121-KOHID-TBL-HENSYU-SEC     SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               IF      WRK-KOHID-O (IDY)   =   ZERO
                   MOVE    WRK-SEIKYU-KOHID    (IDX1 IDX)
                                           TO  WRK-KOHID-O    (IDY)
                   MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                           TO  WRK-KOHHKNNUM-O(IDY)
                   MOVE    WRK-RECESKYKBN  TO  WRK-RECESKYKBN-O(IDY)
                   MOVE    WRK-SEIKYU-TEKSTYMD (IDX1 IDX)
                                           TO  WRK-KOHTEKSTYMD-O (IDY)
                   MOVE    WRK-SEIKYU-KOHHBTNUM    (IDX1 IDX)
                                           TO  WRK-KOHHBTNUM-O   (IDY)
                   MOVE    WRK-SEIKYU-KOHFTNJANUM  (IDX1 IDX)
                                           TO  WRK-KOHFTNJANUM-O (IDY)
                   MOVE    WRK-SEIKYU-KOHJKYSNUM   (IDX1 IDX)
                                           TO  WRK-KOHJKYSNUM-O  (IDY)
                   MOVE    1               TO  FLG-ADDKBN
                   MOVE    10              TO  IDY
               ELSE 
                   IF      WRK-SEIKYU-KOHID (IDX1 IDX)
                                       =   WRK-KOHID-O  (IDY)
                       MOVE    10          TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           .
       200121-KOHID-TBL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *        公費記載順設定情報による並び替え処理
      *****************************************************************
       200121-KOHID-HENSYU-SYS2013-SEC     SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       9
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                   IF     WRK-KOHHKNNUM-O (IDY)    =   SPACE
                       CONTINUE
                   ELSE 
                       IF      WRK-KOHHKNNUM-O (IDX)   =   SPACE
                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                           MOVE    WRK-KOHI        TO  WRK-KOHI-TBL(IDY)
                       ELSE
      *                    徳島県の地方公費（法別４５、４８）
                           IF  ( ( SYS-1001-PREFNUM    =   "36"    ) 
                           AND   ( WRK-KOHHKNNUM-O (IDX) (2:2)
                                               =   "45"    OR "48" )
                           AND   ( WRK-KOHHKNNUM-O (IDY) (2:2)
                                               =   "45"    OR "48" ) )
      *                    北海道の地方公費（法別９０、９１）
                           OR  ( ( SYS-1001-PREFNUM    =   "01"    ) 
                           AND   ( WRK-KOHHBTNUM-O (IDX)
                                               =   "90"    OR "91" )
                           AND   ( WRK-KOHHBTNUM-O (IDY)
                                               =   "90"    OR "91" ) )
      *                    北海道の地方公費（法別４５、４６）
                           OR  ( ( SYS-1001-PREFNUM    =   "01"    ) 
                           AND   ( WRK-KOHHBTNUM-O (IDX)
                                               =   "45"    OR "46" )
                           AND   ( WRK-KOHHBTNUM-O (IDY)
                                               =   "45"    OR "46" ) )
      *                    北海道の地方公費（法別９３、９４）
                           OR  ( ( SYS-1001-PREFNUM    =   "01"    ) 
                           AND   ( WRK-KOHHBTNUM-O (IDX)
                                               =   "93"    OR "94" )
                           AND   ( WRK-KOHHBTNUM-O (IDY)
                                               =   "93"    OR "94" ) )
                               IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   <   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                   CONTINUE
                               ELSE
                                   IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   >   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                   ELSE
                                       IF    ( WRK-KOHHBTNUM-O (IDX)
                                                   >   WRK-KOHHBTNUM-O
                                                                 (IDY) )
                                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                           MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                       END-IF
                                   END-IF
                               END-IF
                           ELSE
      *                        同じ公費のとき適用開始日の古い順にする
                               IF    ( WRK-KOHHKNNUM-O (IDX) 
                                           =   WRK-KOHHKNNUM-O (IDY) )
                               OR    ( WRK-KOHHBTNUM-O (IDX) 
                                           =   WRK-KOHHBTNUM-O (IDY) )
      ***                          IF      WRK-KOHTEKSTYMD-O (IDX)
      ***                                         >   WRK-KOHTEKSTYMD-O
      ***                                                       (IDY)
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
      ***                          END-IF
                               ELSE
                                   IF      WRK-KOHHKNNUM-O (IDX)
                                               >   WRK-KOHHKNNUM-O (IDY)
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                   END-IF
                               END-IF
                           END-IF
                       END-IF    
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    法別、適用開始日による並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       9
                   OR      WRK-KOHHKNNUM-O (IDX) =   SPACE
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                       OR      WRK-KOHHKNNUM-O (IDY) =   SPACE
                   IF  (   ( WRK-KOHHKNNUM-O (IDX) 
                                       =   WRK-KOHHKNNUM-O (IDY) )
                        OR ( WRK-KOHHBTNUM-O (IDX) 
                                       =   WRK-KOHHBTNUM-O (IDY) ) )
                   AND ( WRK-KOHTEKSTYMD-O (IDX)
                                       >   WRK-KOHTEKSTYMD-O (IDY) )
                       MOVE    WRK-KOHI-TBL (IDX)  TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL (IDY)  TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI            TO  WRK-KOHI-TBL(IDY)
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   10
                   OR      TBL-MAX     =   10 
               IF      WRK-KOHID-O (IDX)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDX)  TO 
                                           WRK-RECE61-KOHI-TBL(TBL-MAX)
               END-IF    
           END-PERFORM
      *
      *    公費記載順設定情報による並び替え
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       50
                   OR      TBL-SYS2013 (WRK-SYS2013-IDX IDX)
                                   =       SPACE
               MOVE    TBL-SYS2013 (WRK-SYS2013-IDX IDX)
                                       TO  SYS-2013-REC
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       49
                       OR      SYS-2013-KOHNUM (IDXX)
                                       =       SPACE
                   COMPUTE IDX1    =   IDXX    +   1
                   PERFORM VARYING IDYY    FROM    IDX1    BY  1
                           UNTIL   IDYY    >       50
                           OR      SYS-2013-KOHNUM (IDYY)
                                           =       SPACE
                       MOVE    ZERO            TO  IDY1
                                                   IDY2
                                                   IDY3
                       MOVE    ZERO            TO  IDZ1
                                                   IDZ2
                                                   IDZ3
                       PERFORM VARYING IDZZ    FROM    1   BY  1
                               UNTIL   IDZZ    >       10
                               OR      WRK-RECE61-KOHHKNNUM-O (IDZZ)
                                               =       SPACE
                           IF      WRK-RECE61-KOHHKNNUM-O (IDZZ)
                                               =   SYS-2013-KOHNUM(IDXX)
                               IF      IDY1            =   ZERO
                                   MOVE    IDZZ            TO  IDY1
                                                               IDY2
                                                               IDY3
                               ELSE
                                   IF      IDY2            =   IDY1
                                       MOVE    IDZZ            TO  IDY2
                                                                   IDY3
                                   ELSE
                                       MOVE    IDZZ            TO  IDY3
                                   END-IF
                               END-IF
                           END-IF
                           IF      WRK-RECE61-KOHHKNNUM-O (IDZZ)
                                               =   SYS-2013-KOHNUM(IDYY)
                               IF      IDZ1            =   ZERO
                                   MOVE    IDZZ            TO  IDZ1
                                                               IDZ2
                                                               IDZ3
                               ELSE
                                   IF      IDZ2            =   IDZ1
                                       MOVE    IDZZ            TO  IDZ2
                                                                   IDZ3
                                   ELSE
                                       MOVE    IDZZ            TO  IDZ3
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
      *
                       IF    ( IDY1            >   0    )
                       AND   ( IDZ1            >   0    )
                       AND   ( IDY1            >   IDZ1 )
                           MOVE    WRK-RECE61-KOHI-AREA
                                               TO  WRK-KOHI-AREA
                           INITIALIZE              WRK-RECE61-KOHI-AREA
                           MOVE ZERO           TO  TBL-MAX
      *
                           PERFORM VARYING IDZZ    FROM    1   BY  1
                                   UNTIL   IDZZ    >       10
                               IF      WRK-KOHHKNNUM-O (IDZZ)
                                               NOT =    SPACE
                                   EVALUATE    TRUE
                                       WHEN    IDZZ    =   IDZ1
                                         ADD     1         TO  TBL-MAX
                                         MOVE    WRK-KOHI-TBL(IDY1)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         IF      IDY2      >   IDY1
                                             ADD     1     TO  TBL-MAX
                                             MOVE    WRK-KOHI-TBL(IDY2)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         END-IF
                                         IF      IDY3    >   IDY2
                                           ADD     1       TO  TBL-MAX
                                           MOVE    WRK-KOHI-TBL(IDY3)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         END-IF
                                       WHEN    IDZZ    =   IDY1
                                         ADD     1         TO  TBL-MAX
                                         MOVE    WRK-KOHI-TBL(IDZ1)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         IF      IDZ2      >   IDZ1
                                             ADD     1     TO  TBL-MAX
                                             MOVE    WRK-KOHI-TBL(IDZ2)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         END-IF
                                         IF      IDZ3    >   IDZ2
                                           ADD     1       TO  TBL-MAX
                                           MOVE    WRK-KOHI-TBL(IDZ3)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                         END-IF
                                     WHEN    IDZZ    =   IDZ2
                                     WHEN    IDZZ    =   IDZ3
                                     WHEN    IDZZ    =   IDY2
                                     WHEN    IDZZ    =   IDY3
                                         CONTINUE
                                     WHEN    OTHER
                                         ADD     1         TO  TBL-MAX
                                         MOVE    WRK-KOHI-TBL(IDZZ)
                                                 TO  WRK-RECE61-KOHI-TBL
                                                           (TBL-MAX)
                                   END-EVALUATE
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           .
       200121-KOHID-HENSYU-SYS2013-EXT.
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
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
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
      *   
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
           MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
           MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" WRK-SYUKA-PTID
                   " SRYYM="   WRK-SYUKA-SRYYM
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    該当診療年月の医療機関情報取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
           MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
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
      *    災害該当情報
           PERFORM 500-SISETU-HENSYU-SEC
      *
      *    患者レセプト分割情報情報
           PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
      *    患者年齢計算
           PERFORM 500-AGECHK-SEC
      *
      *    同日再入院情報取得
           PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *    診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *   
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           IF      WRK-PARA-PRTKBN     =   "4" OR  "5" OR  "B" 
               MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
           ELSE
               MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
           END-IF
           MOVE    WRK-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
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
               PERFORM 20011-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院会計読み込み
           MOVE    "2"                 TO  WRK-DBKBN 
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *     
           INITIALIZE                  NYUINACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  NACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
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
               PERFORM 20012-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
           MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" WRK-SYUKA-PTID
                   " SRYYM="   WRK-SYUKA-SRYYM
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    未請求設定時は読み飛ばす
           IF    SYUKA-MISEIKYU-FLG  =   "01"
               CONTINUE
           ELSE
      *
      *        医療機関ＩＤ編集
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
                   STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                            SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO       TO      20021-KOBETU-HENSYU-EXT
               END-IF
      *
      *        患者情報取得
               MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
               MOVE    SYS-2002-PTID       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
      *        診療年月編集
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
      *        災害該当情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
      *        患者年齢計算
               PERFORM 500-AGECHK-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
               IF      FLG-END             =   ZERO
      *            同日再入院情報取得
                   PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *            診療会計読み込み
                   MOVE    "1"                 TO  WRK-DBKBN 
      *      
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  ACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   IF      WRK-PARA-PRTKBN     =   "4" OR  "5" OR  "9"
                                               OR  "8" OR  "B"
                       MOVE    "key61"         TO  WRK-SRYACCT-PATHNAME
                   ELSE
                       MOVE    "key37"         TO  WRK-SRYACCT-PATHNAME
                   END-IF
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
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
                       PERFORM 20011-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF 
      *
               IF      FLG-END             =   ZERO
      *            入院会計読み込み
                   MOVE    "2"                 TO  WRK-DBKBN 
      *
                   MOVE    LOW-VALUE           TO  KEY-AREA
      *     
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  NACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
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
                       PERFORM 20012-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計個別編集処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM 
                       MOVE    "1"             TO  WRK-ROUSAIKBN
                   ELSE                                         
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   END-IF    
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN        
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM        
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1      
                               OR      KEY-NEW     NOT =   KEY-OLD
                   PERFORM 20011-FILE-SYORI-SEC 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1      
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       20011-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       20012-HENSYU-SEC              SECTION.
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
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
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
       20012-HENSYU-EXT.
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
           STRING  "ORCR0610 "             DELIMITED   BY  SIZE
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
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE61-FILE
           CLOSE   RECE61X-FILE
      *
           IF      CNT-OUT1            =   ZERO
           AND     CNT-OUT2            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           DISPLAY "RECE61  CNT " CNT-OUT1 
           DISPLAY "RECE61X CNT " CNT-OUT2  
           DISPLAY "*** ORCR0610 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者年齢計算処理
      *****************************************************************
       500-AGECHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    SYS-1001-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID          TO  AGECHK-PTID
           MOVE    WRK-STYMD (1:6)     TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-NENREI
           END-IF
      *
           .
       500-AGECHK-EXT.
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
                   MOVE    SYS-2001-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
                   MOVE    KEY-O-PTID      TO  LNK-NYUINDAY-PTID
               WHEN    "2"
                   MOVE    SYS-2002-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
                   MOVE    SYS-2002-PTID   TO  LNK-NYUINDAY-PTID
           END-EVALUATE
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
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
      *                    医保以外読み飛ばし
                           IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM  OR
                                           WRK-CONS-KOUGAI-HKNNUM 
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                           END-IF
                       ELSE
                           IF      COMB-DELKBN         =   "1"
                               MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
      *                        医保以外読み飛ばし
                               IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               ELSE
                                   MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                               END-IF
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
      *    災害該当情報処理
      *****************************************************************
       500-SISETU-HENSYU-SEC             SECTION.
      *
           INITIALIZE                      TABLE-PTKOHINF-AREA
      *
      *    ９５９（災害該当）（平成２８年１０月以降）
           IF    ( SYS-25-DISASTER-RECEIPT
                                           =   "1"     )
           AND   ( WRK-SYUKA-SRYYM         >=   201610 )
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    WRK-SYUKA-PTID      TO  PTKOH-PTID
               MOVE    "959"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF  
               END-IF
               PERFORM     UNTIL   FLG-PTKOHINF    =   1
                           OR      TBL-PTKOHINF-MAX
                                                   >=  100
                   IF    ( PTKOH-SAKJOKBN      =   SPACE      )
                   AND   ( PTKOH-TEKSTYMD      >=  "20161001" )
                       ADD     1               TO  TBL-PTKOHINF-MAX
                       MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC
                                                   (TBL-PTKOHINF-MAX)
      *
                       DISPLAY "   TBL-PTKOHINF-MAX=" TBL-PTKOHINF-MAX
                           " KOHNUM="
                                   TBL-PTKOH-KOHNUM   (TBL-PTKOHINF-MAX)
                           " ST="  TBL-PTKOH-TEKSTYMD (TBL-PTKOHINF-MAX)
                           " ED="  TBL-PTKOH-TEKEDYMD (TBL-PTKOHINF-MAX)
                   END-IF
      *
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key4"           TO  MCP-PATHNAME
                   PERFORM  900-PTKOHINF-READ-N-SEC
               END-PERFORM
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       500-SISETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割情報処理
      *****************************************************************
       500-PT-HDIST-DAY-HENSYU-SEC     SECTION.
      *
           INITIALIZE                      TBL-PT-HDIST-DAY-T
      *
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    WRK-SYUKA-PTID      TO  PT-HDIST-DAY-PTID
           MOVE    WRK-SYUKA-SRYYM     TO  PT-HDIST-DAY-SRYYM
           MOVE    "1"                 TO  PT-HDIST-DAY-NYUGAIKBN
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
           END-IF
           PERFORM     UNTIL   FLG-PT-HDIST-DAY    =   1
                       OR      TBL-PT-HDIST-DAY-MAX
                                                   >=  10
               ADD     1                   TO  TBL-PT-HDIST-DAY-MAX
               MOVE    PT-HDIST-DAY-REC    TO  TBL-PT-HDIST-DAY-REC
                                               (TBL-PT-HDIST-DAY-MAX)
      *
               DISPLAY "   TBL-PT-HDIST-DAY=" TBL-PT-HDIST-DAY-MAX
                       " DAY=" TBL-PT-HDIST-DAY-DAY
                                               (TBL-PT-HDIST-DAY-MAX)
               DISPLAY "      1=" TBL-PT-HDIST-DAY-HKNCOMBI-OCC
                                               (TBL-PT-HDIST-DAY-MAX 1)
               DISPLAY "      2=" TBL-PT-HDIST-DAY-HKNCOMBI-OCC
                                               (TBL-PT-HDIST-DAY-MAX 2)
      *
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       500-PT-HDIST-DAY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求フラグチェック処理
      *****************************************************************
       800-MISEIKYU-CHECK-SEC          SECTION.
      *
      *    ＥＦファイル作成、連係基盤用のとき未請求設定分も作成する
      *    診療情報システムのとき未請求設定分も作成する
           IF      WRK-PARA-PRTKBN     =   "9" OR  "8" OR  "B"
               INITIALIZE                      SYUKA-REC
               GO  TO  800-MISEIKYU-CHECK-EXT
           END-IF
      *
           INITIALIZE                      SYUKA-REC
           MOVE    SYS-1001-HOSPNUM    TO  SYUKA-HOSPNUM
           MOVE    WRK-SYUKA-PTID      TO  SYUKA-PTID
           MOVE    WRK-SYUKA-SRYYM     TO  SYUKA-SRYYM
           MOVE    "1"                 TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "tbl_syuka"         TO  WRK-MCP-TABLE
           MOVE    "key15"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           .
       800-MISEIKYU-CHECK-EXT.
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
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
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
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
               MOVE    SPACE               TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
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
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
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
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-SYUKA
                   MOVE    MCPDATA-REC          TO  SYUKA-REC
               ELSE
                   MOVE    1                    TO  FLG-SYUKA
                   INITIALIZE                       SYUKA-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   INITIALIZE                      PTKOHINF-REC
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE                      PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF  
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割読込
      *****************************************************************
       900-PT-HDIST-DAY-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PT-HDIST-DAY-REC
               MOVE    ZERO                TO  FLG-PT-HDIST-DAY
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
               INITIALIZE                      PT-HDIST-DAY-REC
           END-IF
      *
           .
       900-PT-HDIST-DAY-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE61-READ-SEC             SECTION.
      *
           READ    RECE61-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61
           END-READ
           .
       900-RECE61-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療費請求書読込
      *****************************************************************
       900-RECE61X-READ-SEC            SECTION.
      *
           READ    RECE61X-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61X
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61X
           END-READ
           .
       900-RECE61X-READ-EXT.
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
               STRING  "ORCR0610 select err TABLE="
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
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
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.

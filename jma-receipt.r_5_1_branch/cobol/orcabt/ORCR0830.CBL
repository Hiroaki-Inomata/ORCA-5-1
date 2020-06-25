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
       PROGRAM-ID.             ORCR0830.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 診療実日数編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/17    NaCl−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.02    NACL-門脇    04.01.21  労災・自賠責の入院室料加算
      *                                     (09594)対応
      *  01.00.03    NACL-門脇    04.10.15  自賠責特定器材等消費税対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    04/12/17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
      *  01.01.03    NACL-門脇    05/01/26  システム管理４００１に関する修正
      *  01.01.04    NACL-門脇    05/01/27  腰・胸部固定帯加算、創外固定器
      *                                     使用加算の技術・薬剤等の選択対応
      *  01.01.05    NACL-門脇    05/03/03  食事に関する編集修正
      *  01.01.06    NACL-門脇    05/05/16  療養担当手当（北海道）対応
      *  01.01.07    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.01.08    NACL-門脇    05.10.06  診区９５９６対応
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  03.01.01    NACL-門脇    06/06/28  実日数減算コード対応
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/03/12  包括診療対応
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.01.01    NACL-門脇    07/11/01  公務災害対応
      *  04.01.02    NACL-門脇    07/12/19  入院料未算定日対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （再就労：円変更対応）
      *  04.02.02    NACL-門脇    08/05/08  点滴３３２の手技料編集
      *  04.02.03    NACL-門脇    08/06/17  入院料未算定日対応
      *
      *  04.04.01    NACL-門脇    09/01/23  入院料算定保険以外の診療分実日数算定対応
      *  04.04.02    NACL-門脇    09/06/12  療養期間の編集修正
      *  04.04.03    NACL-門脇    09/11/19  自賠責健保準拠療養担当手当対応
      *  04.04.04    NACL-門脇    10/03/23  改正対応（外泊区分５の対応）
      *  04.04.05    NACL-門脇    10/03/31  改正対応（頸部固定帯加算の対応）
      *  04.04.06    NACL-門脇    10/04/02  特別労災付添看護の対応
      *
      *  04.05.01    NACL-門脇    10/01/25  治験対応
      *  04.05.02    NACL-門脇    11/08/16  労災病衣貸与料対応
      *  04.05.03    NACL-門脇    11/11/04  救急搬送診療料算定対応
      *  04.05.04    NACL-門脇    11/11/09  同日再入院食事対応
      *  04.05.05    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     外泊区分６．７の対応
      *                                     入院略称の文字数拡張対応
      *  04.05.06    NACL-門脇    12/09/18  改正対応（外泊区分８，９の対応）
      *
      *  04.06.01    NACL-門脇    10/12/06  労災・自賠責の金額フリー入力
      *                                     コード対応
      *  04.06.02    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *  04.06.03    NACL-門脇    13/06/12  労災・自賠責の
      *                                     看護料割増加算対応
      *  04.06.04    NACL-門脇    13/06/14  救急医療管理加算算定判定修正
      *  04.06.05    NACL-門脇    13/09/09  点数マスタ月途中金額変更対応
      *                                     （０９５９１〜９４）
      *
      *  04.07.01    NACL-門脇    11/09/09  労災請求書作成対応
      *  04.07.02    NACL-門脇    12/08/28  薬剤料減点対応
      *  04.07.03    NACL-門脇    13/10/02  自賠責（第三者行為）対応
      *  04.07.04    NACL-門脇    14/02/13  労災・自賠責の施術加算対応
      *  04.07.05    NACL-門脇    14/03/01  労災マスタ追加対応
      *  04.07.06    NACL-門脇    14/03/04  平成２６年４月改正対応
      *  04.07.07    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.08    NACL-門脇    15/01/14  同日入院時のフラグ編集修正
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    16/04/18  平成２８年４月改正対応
      *  04.08.03    NACL-門脇    16/08/23  持参薬対応
      *  04.08.04    NACL-門脇    16/09/06  入院室料加算差額請求マスタと
      *                                     特別労災付添看護料差額請求
      *                                     マスタの対応
      *  04.08.05    NACL-門脇    18/02/01  第三者行為の選定入院料対応
      *  04.08.06    NACL-門脇    18/04/24  平成３０年４月改正対応
      *
      *  05.00.01    NACL-門脇    19/08/28  令和元年１０月改正（消費税）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
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
           03  STS-RECE82                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-KYUKYU                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-MOD                                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(02).
           03  IDX4                                    PIC 9(02).
           03  IDX8                                    PIC 9(02).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  IDV                                     PIC 9(02).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC X(01)
                                                       OCCURS   31.
      *    実日数減カウント用領域
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-DAY-GEN                      PIC X(01)
                                                       OCCURS   31.
      *    合計点数（診療行為）集計用領域
       01  TOTAL-ACCT.
           03  TOTALTEN-ACCT                           PIC S9(08).
           03  TOTALKIN-ACCT                           PIC  9(08).
           03  TOTALKIN2-ACCT                          PIC  9(08).
      *
      *    手技点・薬剤等点数用領域
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   31.
              05  TBL-SYUTEN-JI                        PIC S9(09).
              05  TBL-YKZTEN-JI                        PIC S9(09).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY41                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
               05  WRK-DAY7                            PIC 9(04).
               05  WRK-DAY7-ZAITEN                     PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
           03  WRK-SRYYM-5000                          PIC  X(06).
           03  WRK-SYS5000-COUNT                       PIC  9(02).
      *
           03  WRK-TAIHI-FLG                           PIC  9(01).
           03  WRK-TAIHI-SRYYMD.
               05  WRK-TAIHI-SRYYM                     PIC  9(06).
               05  WRK-TAIHI-SRYD                      PIC  9(02).
      *
           03  WRK-TEN                                 PIC  9(09).
           03  WRK-TEN-KG                              PIC  9(09).
           03  WRK-TEN-KG2                             PIC  9(09).
           03  WRK-KIN                                 PIC  9(09).
           03  WRK-KIN2                                PIC  9(09).
      *
           03  WRK-SINDAN-TUU                          PIC  9(02).
           03  WRK-SINDAN-KIN                          PIC  9(09).
           03  WRK-MEISAI-TUU                          PIC  9(02).
           03  WRK-MEISAI-KIN                          PIC  9(09).
      *
           03  WRK-KSNRATE                             PIC  9V99.
      *    消費税端数区分
           03  WRK-TAX1001-KBN                         PIC  X(01).
      *
           03  WRK-ZAIKAISU-JI                         PIC  9(03).
           03  WRK-KEISAN-JI                           PIC  9(09).
           03  WRK-TEN-JI                              PIC  9(09).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC  9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC  9(01)
                                                       OCCURS 31.
           03  WRK-SRYCD                               PIC  X(09).
           03  WRK-NYUTOKRYO                           PIC  9(09).
      *
           03  WRK-KEISAN                              PIC  9(09).
           03  WRK-KEISAN2                             PIC  9(09).
           03  WRK-KEISAN-KG                           PIC  9(09).
           03  WRK-KEISAN-KG2                          PIC  9(09).
      *
           03  WRK-STDAY                               PIC  9(02).
           03  WRK-STDAY2                              PIC  9(02).
           03  WRK-EDDAY                               PIC  9(02).
           03  WRK-EDDAY2                              PIC  9(02).
      *
           03  WRK-SYUTEN-TOTALTEN                     PIC S9(09).
           03  WRK-YKZTEN-TOTALTEN                     PIC  9(09).
      *
           03  WRK-FUTANWARI                           PIC  9V99.
      *
           03  WRK-TAIHI-DAY                           PIC  9(02).
      *
           03  WRK-S1                                  PIC  9(03).
           03  WRK-S2                                  PIC  9(03).
           03  WRK-S3                                  PIC  9V99999.
           03  WRK-S4                                  PIC  9V99.
           03  WRK-S5                                  PIC  9V99.
      *
       01  WRK-TENTEK.
           03  WRK-TENTEK-SYUTEN                       PIC  9(07).
           03  WRK-TENTEK-ZAIKAISU                     PIC  9(04).
           03  WRK-TENTEK-YKZTEN                       PIC  9(07).
           03  WRK-TENTEK-YKZTEN2                      PIC  9(07)
                                                       OCCURS   31.
           03  WRK-TENTEK-KIZAITEN                     PIC  9(07).
           03  WRK-TENTEK-KIZAITEN2                    PIC  9(07)
                                                       OCCURS   31.
           03  WRK-TENTEK-FLG                          PIC  9(01).
           03  WRK-TENTEK-FLG2                         PIC  9(01).
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ".
      *
           03  WRK-RECEERR                             PIC X(200).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    医療機関（入院基本）退避用領域
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *
      *---------------------------------------------------------------
      *    固定値
       01  CONST-AREA.
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130100".
      *
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101910010".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101910020".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101910030".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101910040".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101910070".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101910080".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101910090".
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101910100".
           03  CONS-ROUSAI21   PIC X(09)   VALUE   "101110040".
      *
           03  CONS-ROUSAI22   PIC X(09)   VALUE   "101800390".
           03  CONS-ROUSAI23   PIC X(09)   VALUE   "101800400".
           03  CONS-ROUSAI24   PIC X(09)   VALUE   "101800410".
           03  CONS-ROUSAI25   PIC X(09)   VALUE   "101800420".
           03  CONS-ROUSAI26   PIC X(09)   VALUE   "101800430".
           03  CONS-ROUSAI27   PIC X(09)   VALUE   "101800440".
           03  CONS-ROUSAI28   PIC X(09)   VALUE   "101800450".
           03  CONS-ROUSAI29   PIC X(09)   VALUE   "101800460".
      *
           03  CONS-ROUSAI30   PIC X(09)   VALUE   "101800870".
           03  CONS-ROUSAI31   PIC X(09)   VALUE   "101800470".
           03  CONS-ROUSAI32   PIC X(09)   VALUE   "101800480".
           03  CONS-ROUSAI33   PIC X(09)   VALUE   "101800490".
           03  CONS-ROUSAI34   PIC X(09)   VALUE   "101800500".
           03  CONS-ROUSAI35   PIC X(09)   VALUE   "101800510".
           03  CONS-ROUSAI36   PIC X(09)   VALUE   "101800520".
           03  CONS-ROUSAI37   PIC X(09)   VALUE   "101800530".
           03  CONS-ROUSAI38   PIC X(09)   VALUE   "101800540".
           03  CONS-ROUSAI39   PIC X(09)   VALUE   "101800960".
           03  CONS-ROUSAI40   PIC X(09)   VALUE   "101800970".
           03  CONS-ROUSAI41   PIC X(09)   VALUE   "101800980".
      *
           03  CONS-ROUSAI42   PIC X(09)   VALUE   "101800880".
      *
           03  CONS-ROUSAI43   PIC X(09)   VALUE   "101801040".
      *
           03  CONS-ROUSAI44   PIC X(09)   VALUE   "101801150".
           03  CONS-ROUSAI45   PIC X(09)   VALUE   "101801160".
           03  CONS-ROUSAI46   PIC X(09)   VALUE   "101801170".
           03  CONS-ROUSAI47   PIC X(09)   VALUE   "101801180".
      *
      *    腰部固定帯加算
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    胸部固定帯加算
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    頸部固定帯加算
           03  CONS-KEIBUKASAN PIC X(09)   VALUE   "140052610".
      *    創外固定器使用加算
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    療養担当手当（入院）
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000510".
      *---------------------------------------------------------------
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
      *    医療機関情報−基本
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
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
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    労災入院会計作成サブ
           COPY    "CPORCSROUNYUIN.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
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
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0830"          TO  JOB-PGID
               MOVE    "診療実日数編集（労災・自賠責）"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE     WRK-PARA-SRYYM     TO  WRK-SRYYM-5000
             PERFORM   120-SYS-5000-CHK-SEC
      *      医療機関減算の取得（システム管理テーブルより）
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             PERFORM 910-SYSKANRI-INV-SEC
             IF      FLG-SYSKANRI        =   ZERO
               MOVE     MCPDATA-REC      TO  SYS-5000-REC
               PERFORM  110-SYSKAN-HENSYU-SEC
               IF        WRK-SYS5000-COUNT    =     ZERO
                  CONTINUE
               ELSE
                  PERFORM  130-SYS-5000-GET-SEC
               END-IF
             ELSE
               MOVE "医療機関情報−入院基本が取得できませんでした。"
                                     TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1             TO  FLG-END
             END-IF
           ELSE
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"     TO  MCP-TABLE
                 MOVE    "key8"             TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE   MCPDATA-REC    TO  SYS-5000-REC
                   ADD     1             TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC   TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key8"           TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                 MOVE "医療機関情報−入院基本が取得できませんでした。"
                                         TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 MOVE    1               TO  FLG-END
             END-IF
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報−基本
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                 MOVE     MCPDATA-REC     TO  SYS-1001-REC
                 MOVE     SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
                 IF   WRK-TAX1001-KBN = SPACE
                    MOVE   "2"            TO  WRK-TAX1001-KBN
                 END-IF
              ELSE
                 MOVE    "医療機関情報が取得できませんでした。"
                                         TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 MOVE    1               TO  FLG-END
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM  900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
      *
           OPEN    I-O                 RECE82-FILE
      *
           PERFORM    900-RECE82-READ-SEC
      *
          .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    シスカンの編集
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
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
           .
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内に複数の医療期間情報がないか検索（医療機関減算の確認）  *
      ******************************************************************
       120-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
      *
                  MOVE    MCPDATA-REC  TO  SYS-5000-REC
                  ADD     1            TO  WRK-SYS5000-COUNT
      *
                  MOVE    "tbl_syskanri"   TO  MCP-TABLE
                  MOVE    "key11"          TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       120-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内の医療期間情報取得（医療機関減算情報）                  *
      ******************************************************************
       130-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
      *
                 MOVE   MCPDATA-REC    TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
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
                 MOVE    "tbl_syskanri"   TO   MCP-TABLE
                 MOVE    "key11"          TO   MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       130-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM    210-HENSYU-SEC
      *
           PERFORM    900-RECE82-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
      *    消費税計算の係数設定
           IF    RECE82-SRYYM >= "201910"
               MOVE      10      TO  WRK-S1
               MOVE     110      TO  WRK-S2
               MOVE     0.10     TO  WRK-S4
               MOVE     1.10     TO  WRK-S5
           ELSE
             IF  RECE82-SRYYM >= "201404"
               MOVE       8      TO  WRK-S1
               MOVE     108      TO  WRK-S2
               MOVE     0.08     TO  WRK-S4
               MOVE     1.08     TO  WRK-S5
             ELSE
               MOVE       5      TO  WRK-S1
               MOVE     105      TO  WRK-S2
               MOVE     0.05     TO  WRK-S4
               MOVE     1.05     TO  WRK-S5
             END-IF
           END-IF
           EVALUATE  WRK-TAX1001-KBN
               WHEN      "1"
                 MOVE   0.50000  TO  WRK-S3
               WHEN      "2"
                 MOVE   0.00000  TO  WRK-S3
               WHEN      "3"
                 MOVE   0.99999  TO  WRK-S3
           END-EVALUATE
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
           MOVE       SPACE             TO     TBL-JNISSU-GEN
           INITIALIZE                          TBL-JNISSU-GEN
      *
           MOVE       SPACE             TO     TBL-TENSUG-JI
           INITIALIZE                          TBL-TENSUG-JI
      *
           MOVE       SPACE             TO     TOTAL-ACCT
           INITIALIZE                          TOTAL-ACCT
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       ZERO              TO     WRK-TAIHI-FLG
      *
           MOVE       ZERO              TO     WRK-SINDAN-TUU
                                               WRK-SINDAN-KIN
                                               WRK-MEISAI-TUU
                                               WRK-MEISAI-KIN
      *
           PERFORM     220-RECE82-HENSYU-SEC
           PERFORM     230-RECE82-HENSYU2-SEC
           IF   RECE82-SRYYM  >= "200604"
             CONTINUE
           ELSE
             PERFORM   250-RECE82-HENSYU4-SEC
           END-IF
           PERFORM     260-RECE82-HENSYU9-SEC
      *
           PERFORM     270-JNISSU-SET-SEC
      *
           PERFORM     280-RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       220-RECE82-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "4"  OR  "5"  OR
                                                "7"
                       PERFORM    2201-NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE    "tbl_nyuinacct" TO      MCP-TABLE
                 MOVE    "key7"          TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
                 IF        WRK-DAY5(IDY)   =   RECE82-HKNCOMBI
                      MOVE    ZERO            TO    FLG-SHOSAISIN
                      IF  (WRK-DAY1(IDY)   NOT =    ZERO )
                      OR  (RECE82-NYUINDAY-KBN(IDY) = "1")
                               MOVE    1      TO    FLG-SHOSAISIN
                      END-IF
                      PERFORM   2202-RECE82-TENSOU11-SEC
                 END-IF
      *
           END-PERFORM
      *
      *+++同日再入院
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
                 IF   RECE82-HKNCOMBI         =
                                   RECE82-DOUJITSU-HKNCOMBI(IDY)
      ***             MOVE    SPACE           TO    RECE82-NYUINRYOFLG
                      IF   RECE82-HKNCOMBI = WRK-DAY5(IDY)
                        CONTINUE
                      ELSE
                        MOVE  SPACE           TO    RECE82-NYUINRYOFLG
                      END-IF
                      MOVE    ZERO            TO    FLG-SHOSAISIN
                      IF  (WRK-DAY1(IDY)   NOT =    ZERO )
                      OR  (RECE82-NYUINDAY-KBN(IDY) = "1")
                               MOVE    1      TO    FLG-SHOSAISIN
                      END-IF
                      PERFORM   2202-RECE82-TENSOU11-SEC
                 END-IF
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN        =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-SHOSAISIN-GEN
                                               FLG-KYUKYU
      ***               IF   ACCT-SRYKBN  =  "11"  OR
      ***                                    "13"  OR  "14"
      ***                   PERFORM   2203-SHOSAISIN-HANTEI-SEC
      ***               END-IF
                        IF   ACCT-SRYKBN  =  "95"  OR  "96"
                            CONTINUE
                        ELSE
                            IF   ACCT-SRYKBN  =  "13"  OR  "80"
                               PERFORM   2206-SHOSAISIN-HANTEI2-SEC
                            ELSE
                               MOVE    1    TO    FLG-SHOSAISIN
                            END-IF
                        END-IF
                        IF   ACCT-SRYKBN  =  "11"  OR  "80"
                            PERFORM   2204-KYUKYU-HANTEI-SEC
                        END-IF
                        PERFORM   2205-RECE82-TENSOU12-SEC
                   END-IF
                   IF   (ACCT-ZAITEN    =        ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN        =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-SHOSAISIN-GEN
                                               FLG-KYUKYU
                        IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                            PERFORM   2203-SHOSAISIN-HANTEI-SEC
                        END-IF
                        PERFORM   2205-RECE82-TENSOU12-SEC
                   END-IF
                   MOVE    "tbl_sryacct"    TO      MCP-TABLE
                   MOVE    "key22"          TO      MCP-PATHNAME
                   PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"    TO      MCP-TABLE
           MOVE    "key22"          TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       220-RECE82-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       2201-NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    CONTINUE
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                    IF  ( NACCT-SRYKBN     = "90"      )
                    OR  ( NACCT-SRYCDTOTAL = 190799410 )
                    OR  ( NACCT-SRYCDTOTAL = 190799510 )
                       MOVE   "90" TO  RECE82-NYUDAY-SRYKBN(IDY)
                    ELSE
                       MOVE   "92" TO  RECE82-NYUDAY-SRYKBN(IDY)
                    END-IF
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF   RECE82-SRYYM  >= "200604"
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
               IF     NACCT-ZAISKBKBN    =    "7"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY7(IDY) 
                    ADD    NACCT-ZAITEN     TO  WRK-DAY7-ZAITEN(IDY)
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       2201-NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2202-RECE82-TENSOU11-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
                       MOVE    "1"   TO   TBL-JNISSU-DAY(IDY)
           END-IF
           .
       2202-RECE82-TENSOU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2203-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
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
      *
      **               PERFORM  900-TENSU-READ-SEC
      *
      **               IF       FLG-TENSU      =    ZERO
      **                 IF ( (TNS-JITUDAY    =     2)    AND
      **                      (TNS-DAYCNT     =     1   OR   2) ) OR
      **                    (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
      **                                                "099999902" OR
      **                                                "099110001" OR
      **                                                "099120001" OR
      **                                                "111011810" OR
      **                                                 CONS-ROUSAI21 )
      **                     MOVE    1       TO   FLG-SHOSAISIN
      **                 END-IF
      **                 IF    SRY-SRYCD(SRYACT-IDX) =  "099409903"
      **                     MOVE    1       TO   FLG-SHOSAISIN-GEN
      **                 END-IF
      **               END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                    "099999902" OR
                                                    "099110001" OR
                                                    "099120001"
                           MOVE      1       TO   FLG-SHOSAISIN
                       END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409903"
                           MOVE      1       TO   FLG-SHOSAISIN-GEN
                       END-IF
      *
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"      TO   MCP-TABLE
           MOVE    "key9"            TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
      *
       2203-SHOSAISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2206-SHOSAISIN-HANTEI2-SEC                 SECTION.
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               IF   FLG-SRYACT = ZERO
                 IF ((SRY-SRYCD(1) =
                      CONS-ROUSAI4  OR CONS-ROUSAI5  OR
                      CONS-ROUSAI6  OR CONS-ROUSAI7  OR
                      CONS-ROUSAI8  OR CONS-ROUSAI9  OR
                      CONS-ROUSAI10 OR CONS-ROUSAI11 OR
                      CONS-ROUSAI12                       ) AND
                     (RECE82-SRYYM  <= "201306"           ))
                 OR ((SRY-SRYCD(1) =
                      CONS-ROUSAI30 OR CONS-ROUSAI31 OR
                      CONS-ROUSAI32 OR CONS-ROUSAI33 OR
                      CONS-ROUSAI34 OR CONS-ROUSAI35 OR
                      CONS-ROUSAI36 OR CONS-ROUSAI37 OR
                      CONS-ROUSAI38 OR CONS-ROUSAI39 OR
                      CONS-ROUSAI40 OR CONS-ROUSAI41 OR
                      CONS-ROUSAI43                       ) AND
                     (RECE82-SRYYM  >= "201307"           ))
                 OR ( SRY-SRYCD(1)(1:5) =
                      "09591" OR "09592" OR "09593"        )
                      CONTINUE
                 ELSE
                      MOVE    1      TO   FLG-SHOSAISIN
                 END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_sryact"      TO   MCP-TABLE
           MOVE    "key9"            TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
      *
       2206-SHOSAISIN-HANTEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    救急管理加算があるかの判定
      *****************************************************************
       2204-KYUKYU-HANTEI-SEC                 SECTION.
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)  OR
                                      (FLG-KYUKYU        =    1)
                     IF   RECE82-SRYYM    >=   "201307"
                       IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI42
                             MOVE    1      TO    FLG-KYUKYU
                       END-IF
                     ELSE
                       IF  SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI2
                             MOVE    1      TO    FLG-KYUKYU
                       END-IF
                     END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"   TO    MCP-TABLE
                   MOVE    "key9"         TO    MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
      *
       2204-KYUKYU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2205-RECE82-TENSOU12-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
              PERFORM    VARYING   IDY   FROM   1    BY   1
                           UNTIL    IDY   >      31
                  IF  ACCT-DAY(1 IDY)   NOT =    ZERO
                       MOVE   "1"  TO  TBL-JNISSU-DAY(IDY)
                  END-IF
              END-PERFORM
           END-IF
           IF   FLG-SHOSAISIN-GEN    =       1
              PERFORM    VARYING   IDY   FROM   1    BY   1
                           UNTIL    IDY   >      31
                  IF  ACCT-DAY(1 IDY)   NOT =    ZERO
                       MOVE   "1"  TO  TBL-JNISSU-DAY-GEN(IDY)
                  END-IF
              END-PERFORM
           END-IF
           IF   FLG-KYUKYU           =       1
              PERFORM    VARYING   IDY   FROM   1    BY   1
                     UNTIL  ( IDY                  >    31 )  OR
                            ( WRK-TAIHI-FLG        =     1 )
                  IF  ACCT-DAY(1 IDY)   NOT =    ZERO
                      MOVE   RECE82-SRYYM  TO  WRK-TAIHI-SRYYM
                      MOVE   IDY           TO  WRK-TAIHI-SRYD
                      MOVE   1             TO  WRK-TAIHI-FLG
                  END-IF
              END-PERFORM
           END-IF
           .
       2205-RECE82-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       230-RECE82-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPNUM      TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    "1"                 TO  LNK-SROUNYUIN-RYAKUKBN
           IF   (RECE82-HKNKBN  =  "6"    )
           AND  (RECE82-SRYYM  >= "201604")
             MOVE  "1"                 TO  LNK-SROUNYUIN-DAISANFLG
           END-IF
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
                     PERFORM   2301-KHNTEN-HENSYU-SEC
                END-IF
                PERFORM        2303-KHNTEN-HENSYU2-SEC
      *
              END-PERFORM
      *
              PERFORM   2302-RYAKUSYO-HENSYU-SEC
      *
           END-IF
      *
           .
       230-RECE82-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と外泊等の編集
      **************************************************************
       2301-KHNTEN-HENSYU-SEC           SECTION.
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDY)
             WHEN   "90"
                IF    LNK-SROUNYUIN-ZAITEN(IDY)  NOT = ZERO
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  ID1    FROM    1   BY  1
                                UNTIL   ID1                   > 20
                                  OR    RECE82-NYUKHNNISSU(ID1)
                                                              = ZERO
                                  OR    FLG-ARI               = 1
                      IF  RECE82-NYUKHNRYO(ID1)  =  
                                          LNK-SROUNYUIN-ZAITEN(IDY)
                            ADD   1   TO   RECE82-NYUKHNNISSU(ID1)
                            ADD   LNK-SROUNYUIN-ZAITEN(IDY)
                                      TO   RECE82-NYUKHNKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                  END-PERFORM  
                  IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUKHNNISSU(ID1)
                      MOVE   LNK-SROUNYUIN-ZAITEN(IDY)
                                             TO  RECE82-NYUKHNRYO(ID1)
                      ADD    LNK-SROUNYUIN-ZAITEN(IDY)
                                       TO  RECE82-NYUKHNKEI(ID1)
                  END-IF
                END-IF
      *
                COMPUTE  RECE82-NYUKHNKEI2  =
                         RECE82-NYUKHNKEI2  +
                         LNK-SROUNYUIN-ZAITEN-NACCT(IDY)
      *
                COMPUTE  RECE82-NYUKHNKEI2  =
                         RECE82-NYUKHNKEI2  +
                         LNK-SROUNYUIN-ZAITEN-ACCT(IDY)
      *
      **>       COMPUTE  RECE82-NYUTOKKEI2  =
      **>                RECE82-NYUTOKKEI2  +
      **>                LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
      *
                COMPUTE  RECE82-NYUTOKKEI2  =
                         RECE82-NYUTOKKEI2  -
                         LNK-SROUNYUIN-GENTEN(IDY)
      *
             WHEN   "92"
                COMPUTE  WRK-NYUTOKRYO                   =
                         LNK-SROUNYUIN-ZAITEN(IDY)       +
                         LNK-SROUNYUIN-ZAITEN-NACCT(IDY) +
                         LNK-SROUNYUIN-ZAITEN-ACCT(IDY)
      *
                IF    WRK-NYUTOKRYO  NOT = ZERO
                  IF     LNK-SROUNYUIN-GAIHAKU(IDY) =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )  
                          OR  ( RECE82-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE82-NYUTOKRYO-RYAKU(ID1)  =  
                                      LNK-SROUNYUIN-RYAKUSYO(IDY)
                            ADD   1   TO   RECE82-NYUTOKNISSU(ID1)
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE82-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUTOKNISSU(ID1)
                      MOVE   LNK-SROUNYUIN-RYAKUSYO(IDY)
                                             TO  RECE82-NYUTOKRYO-RYAKU
                                                                   (ID1)
      ***             略称が空白の場合の為の処置
                      MOVE   99999999        TO  RECE82-NYUTOKRYO(ID1)
      *
                      ADD    WRK-NYUTOKRYO   TO  RECE82-NYUTOKKEI(ID1)
                    END-IF
                  ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )  
                          OR  ( RECE82-NYUTOKNISSU(ID1)      = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE82-NYUTOKRYO(ID1)  =  
                                            WRK-NYUTOKRYO
                            ADD   1   TO   RECE82-NYUTOKNISSU(ID1)
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE82-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE82-NYUTOKNISSU(ID1)
      ***             点数がゼロの場合の為の処置
                      MOVE   "９９９９９９９９９９"
                                             TO  RECE82-NYUTOKRYO-RYAKU
                                                                   (ID1)
      *
                      MOVE   WRK-NYUTOKRYO   TO  RECE82-NYUTOKRYO(ID1)
                      ADD    WRK-NYUTOKRYO   TO  RECE82-NYUTOKKEI(ID1)
                    END-IF
                  END-IF
                END-IF
      *
      **>       COMPUTE  RECE82-NYUTOKKEI2  =
      **>                RECE82-NYUTOKKEI2  +
      **>                LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
      *
                COMPUTE  RECE82-NYUTOKKEI2  =
                         RECE82-NYUTOKKEI2  -
                         LNK-SROUNYUIN-GENTEN(IDY)
      *
           END-EVALUATE
      *
           IF  (LNK-SROUNYUIN-SRYKBN(IDY) =  SPACE)
           AND (RECE82-NYUINDAY-KBN(IDY)  =  "1"  )
      *
             COMPUTE       RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +
                           LNK-SROUNYUIN-ZAITEN-NACCT(IDY)
             COMPUTE       RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +  
                           LNK-SROUNYUIN-ZAITEN-ACCT(IDY)
      **>    COMPUTE       RECE82-NYUTOKKEI2  =
      **>                  RECE82-NYUTOKKEI2  +
      **>                  LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
      *
             COMPUTE       RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  -
                           LNK-SROUNYUIN-GENTEN(IDY)
      *
           END-IF
      *
           .
       2301-KHNTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    略称の編集
      *****************************************************************
       2302-RYAKUSYO-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING  IDY2    FROM    1   BY  1
                      UNTIL  (IDY2    >   20)
                       OR    (LNK-SROUNYUIN-RYAKUSYO2(IDY2) = SPACE)
      *
              MOVE   LNK-SROUNYUIN-RYAKUSYO2(IDY2)
                                 TO    RECE82-NYUSYUBETU(IDY2)
      *
           END-PERFORM
      *
      *    特別な関係にある他院
      *
           IF   (RECE82-TAINFLG          =  "1"     )
           AND  (RECE82-SRYYM           >=  "201804")
             MOVE     ZERO     TO    FLG-ARI
             PERFORM  VARYING  IDY2    FROM    1   BY  1
                       UNTIL  (IDY2                    >  20  )
                        OR    (RECE82-NYUSYUBETU(IDY2) = SPACE)
                        OR    (FLG-ARI                 =   1  )
               IF  RECE82-NYUSYUBETU(IDY2)(1:4) = "特別"
                 MOVE  1   TO   FLG-ARI
               END-IF
             END-PERFORM
             IF    FLG-ARI  =  ZERO
             AND   IDY2    <=   20
               MOVE   "特別"   TO   RECE82-NYUSYUBETU(IDY2)(1:4)
             END-IF
           END-IF
      *
           .
       2302-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と外泊等の編集２
      **************************************************************
       2303-KHNTEN-HENSYU2-SEC           SECTION.
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDY)
             WHEN   "90"
                  COMPUTE  RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +  
                           LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
             WHEN   "92"
                  COMPUTE  RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +
                           LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
           END-EVALUATE
      *
           IF   LNK-SROUNYUIN-SRYKBN(IDY) =  SPACE
             COMPUTE       RECE82-NYUTOKKEI2  =
                           RECE82-NYUTOKKEI2  +
                           LNK-SROUNYUIN-ZAITEN-ACCTETC(IDY)
           END-IF
      *
           .
       2303-KHNTEN-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       250-RECE82-HENSYU4-SEC                SECTION.
      *
      **   IF    WRK-SHOKUJIRYOYO   NOT =  ZERO
              PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >       31
      *
                    IF  WRK-DAY1(IDY) NOT =  ZERO 
                    AND WRK-DAY5(IDY)  =  RECE82-HKNCOMBI
                         IF   (WRK-DAY4(IDY)          NOT = ZERO)
                         AND  ((WRK-DAY2(IDY)         =  ZERO  ) OR
                               (WRK-DAY2(IDY)        >=   4    ))
                         AND  (WRK-SHOKUJIRYOYO(IDY)  NOT = ZERO)
                              PERFORM   2501-NYUINKHN-GET4-SEC
                         END-IF
                    END-IF
      *
              END-PERFORM
      **   END-IF
      *
           .
       250-RECE82-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称の取得
      **************************************************************
       2501-NYUINKHN-GET4-SEC           SECTION.
      *
           EVALUATE   WRK-DAY4(IDY)
             WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                  WHEN  2
                     MOVE    "197000710"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                END-EVALUATE
             WHEN  2
             WHEN  3
             WHEN  4
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  25011-NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  25011-NYUINKHN-SET4-SEC
                     END-IF
           END-EVALUATE
      *
           .
       2501-NYUINKHN-GET4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称のセット
      **************************************************************
       25011-NYUINKHN-SET4-SEC           SECTION.
      *
           MOVE    SPACE          TO   NYURYAKUSHO-REC
           INITIALIZE                  NYURYAKUSHO-REC
           MOVE    WRK-SRYCD      TO   NYURYAKUSHO-SRYCD
           MOVE    RECE82-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
           MOVE    "01"           TO   NYURYAKUSHO-YUKOSTYMD(7:2)
           MOVE    RECE82-SRYYM   TO   NYURYAKUSHO-YUKOEDYMD(1:6)
           MOVE    "01"           TO   NYURYAKUSHO-YUKOEDYMD(7:2)
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 3
                         OR    RECE82-SHOKUJISYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE82-SHOKUJISYUBETU(ID1)  
                               =  NYURYAKUSHO-RYAKUSHO
                        MOVE  1   TO   FLG-ARI
                    END-IF
                END-PERFORM  
                IF  FLG-ARI  =  ZERO  
                    MOVE   NYURYAKUSHO-RYAKUSHO
                             TO   RECE82-SHOKUJISYUBETU(ID1)
                END-IF
           END-IF                  
      *
           .
       25011-NYUINKHN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       260-RECE82-HENSYU9-SEC                SECTION.
      *
      *     点滴薬剤（器材）まとめ処理
           PERFORM    2601-TENTEK-MATOME-SEC
      *
           MOVE       ZERO              TO      WRK-TENTEK-FLG
                                                WRK-TENTEK-FLG2
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)
                     IF  ACCT-ZAIKBN        =    ZERO
                       MOVE    ZERO     TO    WRK-TEN
                                              WRK-KIN
                                              WRK-KIN2
                       PERFORM   2602-ENDATE-HANTEI-SEC
                       IF    RECE82-HKNKBN   =   "4" OR "6"
                         IF   ACCT-SRYKBN  =  "80"
                             PERFORM   2603-SINDAN-HANTEI-SEC
                         END-IF
      *
                         IF  WRK-TENTEK-FLG2  =   ZERO
                           PERFORM VARYING IDY FROM 1  BY 1
                                       UNTIL    IDY     >    31
                             COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                      TBL-YKZTEN-JI(IDY)  +
                                      WRK-TENTEK-YKZTEN2(IDY) +
                                      WRK-TENTEK-KIZAITEN2(IDY)
                           END-PERFORM
                           MOVE  1        TO   WRK-TENTEK-FLG2
                         END-IF
      *
                         PERFORM VARYING IDY FROM 1  BY 1
                                   UNTIL    IDY     >    31
                           IF  ACCT-DAY(1 IDY)   NOT =  ZERO
                                MOVE  ACCT-DAY(1 IDY)  
                                       TO  WRK-ZAIKAISU-JI
                                PERFORM  2604-SYUGITEN-HANTEI-SEC
                           END-IF
                         END-PERFORM
                       END-IF
                       PERFORM   2605-RECE82-TENSOU9-SEC
                     ELSE
                       MOVE    ZERO     TO    WRK-TEN
                                              WRK-KIN
                                              WRK-KIN2
                       IF    RECE82-HKNKBN   =   "4" OR "6"
                         IF  WRK-TENTEK-FLG2  =   ZERO
                           PERFORM VARYING IDY FROM 1  BY 1
                                       UNTIL    IDY     >    31
                             COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                      TBL-YKZTEN-JI(IDY)  +
                                      WRK-TENTEK-YKZTEN2(IDY) +
                                      WRK-TENTEK-KIZAITEN2(IDY)
                           END-PERFORM
                           MOVE  1        TO   WRK-TENTEK-FLG2
                         END-IF
      *
                         IF  ACCT-ZAIKBN   =  2
                          PERFORM VARYING IDY FROM 1  BY 1
                                    UNTIL    IDY     >    31
                           IF  ACCT-DAY(1 IDY)   NOT =  ZERO
                                MOVE  ACCT-DAY(1 IDY)  
                                       TO  WRK-ZAIKAISU-JI
                                PERFORM  2604-SYUGITEN-HANTEI-SEC
                           END-IF
                          END-PERFORM
                         END-IF
                       END-IF
                       PERFORM   2605-RECE82-TENSOU9-SEC
                     END-IF
                   END-IF
                   IF   (ACCT-ZAITEN        =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)
                     IF  ACCT-ZAIKBN        =    ZERO
                       IF    RECE82-HKNKBN   =   "4" OR "6"
                         IF   ACCT-SRYKBN  =  "80"
                             PERFORM   2603-SINDAN-HANTEI-SEC
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"    TO      MCP-TABLE
                   MOVE    "key22"          TO      MCP-PATHNAME
                   PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"    TO      MCP-TABLE
           MOVE    "key22"          TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    療養担当手当集計
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
              IF   ((WRK-DAY1(IDY)          NOT =  ZERO   ) OR
                    (RECE82-NYUINDAY-KBN(IDY)   =  "1"    ))
              AND  ( WRK-DAY5(IDY)      =  RECE82-HKNCOMBI )
                  IF  (WRK-DAY7(IDY)          NOT =  ZERO)
                  AND (WRK-DAY7-ZAITEN(IDY)   NOT =  ZERO)
                  AND ((WRK-DAY2(IDY)         =  ZERO   ) OR
                       (WRK-DAY2(IDY)        >=   4     ))
                     IF    RECE82-HKNKBN   =   "4" OR "6"
                      IF   (RECE82-HKNKBN    =  "4")
                      AND  (RECE82-JIBAIKBN  =  "1")
                        COMPUTE  TOTALKIN2-ACCT =  TOTALKIN2-ACCT
                                                +  120
                      ELSE
                        COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                 TBL-YKZTEN-JI(IDY)  +
                                 WRK-DAY7-ZAITEN(IDY)
                      END-IF
                     ELSE
                        COMPUTE  TOTALTEN-ACCT  =  TOTALTEN-ACCT
                                                +  WRK-DAY7-ZAITEN(IDY)
                     END-IF
                  END-IF
              END-IF
      *
           END-PERFORM
      *
           .
       260-RECE82-HENSYU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の薬剤（器材）点数算定
      *****************************************************************
       2601-TENTEK-MATOME-SEC                    SECTION.
      *
           INITIALIZE                  WRK-TENTEK
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE82-PTID         TO  ORCSTEN-PTID
           MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
           MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
      ***  MOVE    RECE82-HOSPNUM      TO  SPA-HOSPNUM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *    薬剤点数と器材点数
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       31
      *
              IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                 ADD  ORCSTEN-ZAITEN(IDX1)  TO  WRK-TENTEK-YKZTEN
                 ADD  ORCSTEN-ZAITEN(IDX1)  TO  WRK-TENTEK-YKZTEN2
                                                             (IDX1)
              END-IF
      *
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
                COMPUTE  WRK-TENTEK-KIZAITEN  =
                         WRK-TENTEK-KIZAITEN  +
                         ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                           WRK-TENTEK-ZAIKAISU ) 
      **
                MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                PERFORM  VARYING  IDX2    FROM    1   BY  1
                      UNTIL   IDX2     >       31
                  MOVE     ORCSTEN-KIZ-DAY(IDX1 IDX2)  
                                     TO  WRK-TENTEK-ZAIKAISU
                  COMPUTE  WRK-TENTEK-KIZAITEN2(IDX2)  =
                           WRK-TENTEK-KIZAITEN2(IDX2)  +
                         ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                           WRK-TENTEK-ZAIKAISU ) 
                END-PERFORM
      *
           END-PERFORM
      *
           .
       2601-TENTEK-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    円建て項目等であるかの判定
      *****************************************************************
       2602-ENDATE-HANTEI-SEC                 SECTION.
      *
           PERFORM 290-DAY-CHECK-SEC
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                 PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                           UNTIL    (SRYACT-IDX    >   5)  OR
                                    (SRY-SRYCD(SRYACT-IDX)
                                                       =   SPACE)
      *
                   PERFORM  900-TENSU-READ-SEC
      *
                   IF       FLG-TENSU      =    ZERO
      *
                     IF    RECE82-HKNKBN   =   "4" OR "6"
                         IF   RECE82-SRYYM    >=   "201307"
                          IF  SRY-SRYCD(SRYACT-IDX) = CONS-ROUSAI1  OR
                                                      CONS-ROUSAI42 OR
                                                      CONS-ROUSAI21
                            ADD    TNS-TEN      TO    WRK-KIN
                          END-IF
                         ELSE
                          IF  SRY-SRYCD(SRYACT-IDX) = CONS-ROUSAI1  OR
                                                      CONS-ROUSAI2  OR
                                                      CONS-ROUSAI21
                            ADD    TNS-TEN      TO    WRK-KIN
                          END-IF
                         END-IF
                         IF ((SRY-SRYCD(SRYACT-IDX) = CONS-ROUSAI3 ) AND
                             (RECE82-SRYYM          < "200804"     ))
                         OR ((SRY-SRYCD(SRYACT-IDX) =
                              CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                              CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                              CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                              CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                              CONS-ROUSAI12 OR
                              CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                              CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                              CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                              CONS-ROUSAI19 OR  CONS-ROUSAI20      ) AND
                             (RECE82-SRYYM         <= "201306"     ))
                         OR ((SRY-SRYCD(SRYACT-IDX) =
                              CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                              CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                              CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                              CONS-ROUSAI28 OR  CONS-ROUSAI29 OR
                              CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                              CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                              CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                              CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                              CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                              CONS-ROUSAI40 OR  CONS-ROUSAI41 OR
                              CONS-ROUSAI43 OR  CONS-ROUSAI44 OR
                              CONS-ROUSAI45 OR  CONS-ROUSAI46 OR
                              CONS-ROUSAI47                        ) AND
                             (RECE82-SRYYM         >= "201307"     ))
                            ADD    TNS-TEN      TO    WRK-KIN2
                         END-IF
                         IF   RECE82-SRYYM         >= "201307"
                           EVALUATE  SRY-SRYCD(SRYACT-IDX)
                             WHEN    "101800570"  THRU  "101800860"
                             WHEN    "101801010"  THRU  "101801020"
                             WHEN    "101801190"  THRU  "101801420"
                              ADD    TNS-TEN      TO    WRK-KIN2
      *
                              COMPUTE WRK-TEN-KG2 = TNS-TEN
                           END-EVALUATE
                           IF   SRY-SRYCD(SRYACT-IDX) = "101800550" OR
                                         "101800560" OR "101800990" OR
                                         "101801030"
                              COMPUTE WRK-FUTANWARI = TNS-TEN / 100
      *
                              COMPUTE WRK-KEISAN-KG2 =
                                     (WRK-TEN-KG2 * WRK-FUTANWARI)
                              ADD  WRK-KEISAN-KG2 TO    WRK-KIN2
                           END-IF
                         ELSE
                           EVALUATE  SRY-SRYCD(SRYACT-IDX)
                             WHEN    "101910110"  THRU  "101910400"
                              ADD    TNS-TEN      TO    WRK-KIN2
                           END-EVALUATE
                         END-IF
                         IF  ( ACCT-SRYKBN                =  "80"    ) 
                         AND ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09593" 
                                                          OR "09594" ) 
                          IF  SRY-SRYSYUKBN = "809"
                            ADD    SRY-JIHIMONEY(SRYACT-IDX)
                                                TO    WRK-KIN2
                          ELSE
                            ADD    TNS-TEN      TO    WRK-KIN2
                          END-IF
                         END-IF
      *                 自賠責・療養担当手当用特例処理
      *                （自賠責の場合、１０点を１２０円に読み替える）
      *                （療養担当手当：１月１回の算定）
                         IF   SRY-SRYCD(SRYACT-IDX) =  CONS-RYOYOTEATE
                          IF   (RECE82-HKNKBN    =  "4")
                          AND  (RECE82-JIBAIKBN  =  "1")
                            ADD    120          TO    WRK-KIN2
                          END-IF
                         END-IF
                     ELSE 
                         IF ( SRY-SRYCD(SRYACT-IDX) = CONS-ROUSAI1  OR
                                                      CONS-ROUSAI21 )
                         OR ((SRY-SRYCD(SRYACT-IDX) = CONS-ROUSAI3 ) AND
                             (RECE82-SRYYM          < "200804"     ))
                         OR ((SRY-SRYCD(SRYACT-IDX) =
                              CONS-ROUSAI2  OR
                              CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                              CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                              CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                              CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                              CONS-ROUSAI12 OR
                              CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                              CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                              CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                              CONS-ROUSAI19 OR  CONS-ROUSAI20      ) AND
                             (RECE82-SRYYM         <= "201306"     ))
                         OR ((SRY-SRYCD(SRYACT-IDX) =
                              CONS-ROUSAI42 OR
                              CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                              CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                              CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                              CONS-ROUSAI28 OR  CONS-ROUSAI29 OR
                              CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                              CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                              CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                              CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                              CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                              CONS-ROUSAI40 OR  CONS-ROUSAI41 OR
                              CONS-ROUSAI43 OR  CONS-ROUSAI44 OR
                              CONS-ROUSAI45 OR  CONS-ROUSAI46 OR
                              CONS-ROUSAI47                        ) AND
                             (RECE82-SRYYM         >= "201307"     ))
                            ADD    TNS-TEN      TO    WRK-KIN
                            COMPUTE  WRK-TEN  =   WRK-TEN   +
                                     ( TNS-TEN / 10)
                         END-IF
                         IF   RECE82-SRYYM         >= "201307"
                           EVALUATE  SRY-SRYCD(SRYACT-IDX)
                             WHEN    "101800570"  THRU  "101800860"
                             WHEN    "101801010"  THRU  "101801020"
                             WHEN    "101801190"  THRU  "101801420"
                              ADD    TNS-TEN      TO    WRK-KIN
                              COMPUTE  WRK-TEN  =   WRK-TEN   +
                                       ( TNS-TEN / 10)
      *
                              COMPUTE WRK-TEN-KG  = TNS-TEN / 10
                              COMPUTE WRK-TEN-KG2 = TNS-TEN
                           END-EVALUATE
                           IF   SRY-SRYCD(SRYACT-IDX) = "101800550" OR
                                         "101800560" OR "101800990" OR
                                         "101801030"
                              COMPUTE WRK-FUTANWARI = TNS-TEN / 100
      *
                              COMPUTE WRK-KEISAN-KG2 =
                                     (WRK-TEN-KG2 * WRK-FUTANWARI)
                              ADD  WRK-KEISAN-KG2 TO    WRK-KIN
      *
                              COMPUTE WRK-KEISAN-KG  =
                                     (WRK-TEN-KG  * WRK-FUTANWARI)
                                                  + 0.5
                              ADD  WRK-KEISAN-KG  TO    WRK-TEN
                           END-IF
                         ELSE
                           EVALUATE  SRY-SRYCD(SRYACT-IDX)
                             WHEN    "101910110"  THRU  "101910400"
                              ADD    TNS-TEN      TO    WRK-KIN
                              COMPUTE  WRK-TEN  =   WRK-TEN   +
                                       ( TNS-TEN / 10)
                           END-EVALUATE
                         END-IF
                         IF  ( ACCT-SRYKBN                =  "80"    ) 
                         AND ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09593" 
                                                          OR "09594" ) 
                          IF  SRY-SRYSYUKBN = "809"
                            ADD    SRY-JIHIMONEY(SRYACT-IDX)
                                                TO    WRK-KIN
                            COMPUTE  WRK-TEN  =   WRK-TEN   +
                                    ( SRY-JIHIMONEY(SRYACT-IDX)
                                               / 10)
                          ELSE
                            ADD    TNS-TEN      TO    WRK-KIN
                            COMPUTE  WRK-TEN  =   WRK-TEN   +
                                     ( TNS-TEN / 10)
                          END-IF
                         END-IF
                     END-IF
      *
                   END-IF
      *
                 END-PERFORM
      *
                 MOVE    "tbl_sryact"     TO      MCP-TABLE
                 MOVE    "key9"           TO      MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
           .
      *
       2602-ENDATE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    自賠責の診断書料・明細書料であるかの判定
      *****************************************************************
       2603-SINDAN-HANTEI-SEC                 SECTION.
      *
           PERFORM 290-DAY-CHECK-SEC
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                     PERFORM  900-TENSU-READ-SEC
      *
                     IF       FLG-TENSU      =    ZERO
      *
                        IF   SRY-SRYCD(SRYACT-IDX)(1:5) =  "09591"
      *
                          COMPUTE  WRK-SINDAN-TUU = WRK-SINDAN-TUU +
                                                    ACCT-ZAIKAISU
      ****
                          IF    RECE82-TAXKBN      =   "2"
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-KEISAN  =
                               (SRY-JIHIMONEY(SRYACT-IDX) * WRK-S5)
                                                          + WRK-S3
                            ELSE
                              COMPUTE  WRK-KEISAN  =
                               (TNS-TEN                   * WRK-S5)
                                                          + WRK-S3
                            END-IF
                            COMPUTE  WRK-KEISAN2     =  WRK-KEISAN
                                                     *  ACCT-ZAIKAISU
                            COMPUTE  WRK-SINDAN-KIN  =  WRK-SINDAN-KIN
                                                     +  WRK-KEISAN2
                          ELSE
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-SINDAN-KIN  =
                                       WRK-SINDAN-KIN  +
                                      (SRY-JIHIMONEY(SRYACT-IDX)
                                                *  ACCT-ZAIKAISU)
                            ELSE
                              COMPUTE  WRK-SINDAN-KIN  =
                                       WRK-SINDAN-KIN  +
                                      (TNS-TEN  *  ACCT-ZAIKAISU)
                            END-IF
                          END-IF
      ****
                        END-IF
      *
                        IF   SRY-SRYCD(SRYACT-IDX)(1:5) =  "09592"
      *
                          COMPUTE  WRK-MEISAI-TUU = WRK-MEISAI-TUU +
                                                    ACCT-ZAIKAISU
      ****
                          IF    RECE82-TAXKBN      =   "2"
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-KEISAN  =
                               (SRY-JIHIMONEY(SRYACT-IDX) * WRK-S5)
                                                          + WRK-S3
                            ELSE
                              COMPUTE  WRK-KEISAN  =
                               (TNS-TEN                   * WRK-S5)
                                                          + WRK-S3
                            END-IF
                            COMPUTE  WRK-KEISAN2     =  WRK-KEISAN
                                                     *  ACCT-ZAIKAISU
                            COMPUTE  WRK-MEISAI-KIN  =  WRK-MEISAI-KIN
                                                     +  WRK-KEISAN2
                          ELSE
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-MEISAI-KIN  =
                                       WRK-MEISAI-KIN  +
                                      (SRY-JIHIMONEY(SRYACT-IDX)
                                                *  ACCT-ZAIKAISU)
                            ELSE
                              COMPUTE  WRK-MEISAI-KIN  =
                                       WRK-MEISAI-KIN  +
                                      (TNS-TEN  *  ACCT-ZAIKAISU)
                            END-IF
                          END-IF
      ****
                        END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key9"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
           .
      *
       2603-SINDAN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    日単位の手技点・薬剤点等をセット
      *****************************************************************
       2604-SYUGITEN-HANTEI-SEC                 SECTION.
      *
           PERFORM 290-DAY-CHECK-SEC
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                 IF    SRY-RENNUM     =     1
      *
                   IF    ACCT-SRYKBN       =  "90"  OR  "92"  OR
                                              "95"  OR  "96"  OR  "97"
                         CONTINUE
                   ELSE
                     IF       (ACCT-SRYCDTOTAL    =    630010002)
                     OR       (ACCT-ZAIKBN        =    2        )
                      IF  ACCT-ZAIKBN  =    2
                         COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                  TBL-SYUTEN-JI(IDY)  -
                                 ( WRK-ZAIKAISU-JI * ACCT-SYUTEN1  )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  -
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                      ELSE
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  -
                                 ( WRK-ZAIKAISU-JI * ACCT-ZAITEN   )
                      END-IF
                     ELSE 
                      IF    ACCT-SRYKBN  =  "33"
                         EVALUATE  SRY-SRYSYUKBN
                            WHEN  "330"
                            WHEN  "331"
                            WHEN  "332"
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * ACCT-SYUTEN1)
                            WHEN  "340"
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * ACCT-SYUTEN1)
                              COMPUTE  WRK-KEISAN-JI = 
                                       ACCT-KIZAITEN + ACCT-YKZTEN
                              COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                       TBL-YKZTEN-JI(IDY)  +
                                      (WRK-ZAIKAISU-JI * WRK-KEISAN-JI)
                         END-EVALUATE
                      ELSE
                       IF   ACCT-SRYKBN  =  "70"
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN1
                                                  -  ACCT-SYUTEN2
                         COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                  TBL-SYUTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN2   +
                                                     ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       ELSE
                         COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                  TBL-SYUTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * ACCT-SYUTEN1  )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                  TBL-YKZTEN-JI(IDY)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       END-IF
                      END-IF
                     END-IF
                   END-IF
      *
                 END-IF
      *
                 IF   ( ACCT-SRYKBN       =  "95"  OR  "96" )
                 OR   ( ACCT-ZAIKBN       =   2             )
                   CONTINUE
                 ELSE
                   PERFORM   VARYING   SRYACT-IDX   FROM   1   BY   1
                         UNTIL    (SRYACT-IDX            >       5)  OR
                                  (SRY-SRYCD(SRYACT-IDX) =   SPACE)  
      *
                     PERFORM  900-TENSU-READ-SEC
      *
                     IF       FLG-TENSU      =    ZERO
      *
                       IF ( SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI1  OR
                                                     CONS-ROUSAI21 )
                       OR ((SRY-SRYCD(SRYACT-IDX) =  CONS-ROUSAI3 ) AND
                           (RECE82-SRYYM          <  "200804"     ))
                       OR ((SRY-SRYCD(SRYACT-IDX) =
                            CONS-ROUSAI2  OR
                            CONS-ROUSAI4  OR  CONS-ROUSAI5  OR
                            CONS-ROUSAI6  OR  CONS-ROUSAI7  OR
                            CONS-ROUSAI8  OR  CONS-ROUSAI9  OR
                            CONS-ROUSAI10 OR  CONS-ROUSAI11 OR
                            CONS-ROUSAI12 OR
                            CONS-ROUSAI13 OR  CONS-ROUSAI14 OR
                            CONS-ROUSAI15 OR  CONS-ROUSAI16 OR
                            CONS-ROUSAI17 OR  CONS-ROUSAI18 OR
                            CONS-ROUSAI19 OR  CONS-ROUSAI20       ) AND
                           (RECE82-SRYYM         <= "201306"      ))
                       OR ((SRY-SRYCD(SRYACT-IDX) =
                            CONS-ROUSAI42 OR
                            CONS-ROUSAI22 OR  CONS-ROUSAI23 OR
                            CONS-ROUSAI24 OR  CONS-ROUSAI25 OR
                            CONS-ROUSAI26 OR  CONS-ROUSAI27 OR
                            CONS-ROUSAI28 OR  CONS-ROUSAI29 OR
                            CONS-ROUSAI30 OR  CONS-ROUSAI31 OR
                            CONS-ROUSAI32 OR  CONS-ROUSAI33 OR
                            CONS-ROUSAI34 OR  CONS-ROUSAI35 OR
                            CONS-ROUSAI36 OR  CONS-ROUSAI37 OR
                            CONS-ROUSAI38 OR  CONS-ROUSAI39 OR
                            CONS-ROUSAI40 OR  CONS-ROUSAI41 OR
                            CONS-ROUSAI43 OR  CONS-ROUSAI44 OR
                            CONS-ROUSAI45 OR  CONS-ROUSAI46 OR
                            CONS-ROUSAI47                         ) AND
                           (RECE82-SRYYM         >=  "201307"     ))
      *
                            COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                              ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
      *
                       END-IF
                       IF   RECE82-SRYYM         >= "201307"
                         EVALUATE  SRY-SRYCD(SRYACT-IDX)
                           WHEN    "101800570"  THRU  "101800860"
                           WHEN    "101801010"  THRU  "101801020"
                           WHEN    "101801190"  THRU  "101801420"
                              COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  -
                                ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
      *
                              COMPUTE  WRK-TEN-KG = TNS-TEN / 10
                         END-EVALUATE
                         IF   SRY-SRYCD(SRYACT-IDX) = "101800550" OR
                                       "101800560" OR "101800990" OR
                                       "101801030"
                              COMPUTE  WRK-FUTANWARI = TNS-TEN / 100
      *
                              COMPUTE  WRK-KEISAN-KG =
                                      (WRK-TEN-KG * WRK-FUTANWARI)
                                                  + 0.5
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  -
                                ( WRK-ZAIKAISU-JI * WRK-KEISAN-KG )
                         END-IF
                       ELSE
                         EVALUATE  SRY-SRYCD(SRYACT-IDX)
                           WHEN    "101910110"  THRU  "101910400"
                              COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                              COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                       TBL-SYUTEN-JI(IDY)  -
                                ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
                         END-EVALUATE
                       END-IF
      *
                       IF  ( ACCT-SRYKBN         =  "80"    ) AND
                           ( SRY-SRYCD(SRYACT-IDX)(1:5) =  "09591" 
                                                        OR "09592" 
                                                        OR "09593" 
                                                        OR "09594")
                         IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-TEN-JI =
                                   SRY-JIHIMONEY(SRYACT-IDX) / 10
                         ELSE
                           COMPUTE WRK-TEN-JI = TNS-TEN      / 10
                         END-IF
                         COMPUTE   TBL-SYUTEN-JI(IDY)  =
                                   TBL-SYUTEN-JI(IDY)  -
                                  (WRK-ZAIKAISU-JI * WRK-TEN-JI)
                       END-IF
      *
                       IF   SRY-SRYCD(SRYACT-IDX) =  CONS-YOUBUKASAN
                                                 OR  CONS-KYOBUKASAN
                                                 OR  CONS-KEIBUKASAN
                                                 OR  CONS-SOGAIKASAN
                         IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                            COMPUTE  TBL-YKZTEN-JI(IDY)  =
                                     TBL-YKZTEN-JI(IDY)  +
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                         END-IF
                       END-IF
      *
                       IF   SRY-SRYCD(SRYACT-IDX) =  CONS-RYOYOTEATE
                            COMPUTE  TBL-SYUTEN-JI(IDY)  =
                                     TBL-SYUTEN-JI(IDY)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                            IF ((RECE82-HKNKBN     =  "4") AND
                                (RECE82-JIBAIKBN   =  "2"))
                            OR ( RECE82-HKNKBN     =  "6" )
                              COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                       TBL-YKZTEN-JI(ID1)  +
                                      ( WRK-ZAIKAISU-JI * TNS-TEN )
                            END-IF
                       END-IF
      *
                     END-IF
      *
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_sryact"     TO      MCP-TABLE
                 MOVE    "key9"           TO      MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
           .
      *
       2604-SYUGITEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2605-RECE82-TENSOU9-SEC                 SECTION.
      *
           IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                             OR "96" OR "97"
               CONTINUE
           ELSE
               IF       (ACCT-SRYCDTOTAL    =    630010002)
               OR       (ACCT-ZAIKBN        =    2        )
                 COMPUTE   TOTALTEN-ACCT =
                           TOTALTEN-ACCT -
                           (ACCT-ZAITEN  *
                            ACCT-ZAIKAISU)
               ELSE
                 IF        WRK-TENTEK-FLG  =   ZERO
                     COMPUTE   TOTALTEN-ACCT        =
                               TOTALTEN-ACCT        +
                               WRK-TENTEK-YKZTEN    +
                               WRK-TENTEK-KIZAITEN
                     MOVE  1        TO   WRK-TENTEK-FLG
                 END-IF
                 IF        ACCT-SRYKBN     =    "33"
                     PERFORM   26051-TENTEK-SYUTEN-SEC
                     COMPUTE   TOTALTEN-ACCT =  
                               TOTALTEN-ACCT +
                               WRK-TENTEK-SYUTEN
                 ELSE
                     COMPUTE   TOTALTEN-ACCT =  
                               TOTALTEN-ACCT +
                               (ACCT-ZAITEN  *  
                                ACCT-ZAIKAISU)
                     IF  ( WRK-KIN  NOT =  ZERO )
                     OR  ( WRK-KIN2 NOT =  ZERO )
                     OR  ( WRK-TEN  NOT =  ZERO )                       
                       COMPUTE  TOTALTEN-ACCT  =  TOTALTEN-ACCT  -
                                                   (WRK-TEN    *
                                                    ACCT-ZAIKAISU)
      *
                       IF    RECE82-HKNKBN   =   "4" OR "6"
                         COMPUTE  TOTALKIN-ACCT  =  TOTALKIN-ACCT  +
                                                     (WRK-KIN    *
                                                      ACCT-ZAIKAISU)
                         COMPUTE  TOTALKIN2-ACCT =  TOTALKIN2-ACCT +
                                                     (WRK-KIN2   *
                                                      ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE  TOTALKIN-ACCT  =  TOTALKIN-ACCT  +
                                                     (WRK-KIN    *
                                                      ACCT-ZAIKAISU)
                       END-IF
                     END-IF 
                 END-IF
               END-IF
           END-IF
           .
       2605-RECE82-TENSOU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の手技点数算定
      *****************************************************************
       26051-TENTEK-SYUTEN-SEC                    SECTION.
      *
           PERFORM 800-ACCT-SETACT9-SEC
      *
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
                               (ACCT-ZAITEN  * ACCT-ZAIKAISU)
                END-EVALUATE
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       26051-TENTEK-SYUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       270-JNISSU-SET-SEC                    SECTION.
      *
      *実日数から減する
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                IF  (TBL-JNISSU-DAY(IDY)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDY)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(IDY)
                END-IF
           END-PERFORM
      *
           IF   (RECE82-RYOSTYMD   =   SPACE)
           AND  (RECE82-RYOEDYMD   =   SPACE)
      *
      *      初歴が同じ履歴の範囲（９９９）で置き換え
             MOVE    ZERO   TO  FLG-OKIKAE
             MOVE    ZERO   TO  WRK-STDAY
             MOVE    ZERO   TO  WRK-STDAY2
             MOVE    ZERO   TO  WRK-EDDAY
             MOVE    ZERO   TO  WRK-EDDAY2
             MOVE    ZERO   TO  IDX4
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY     >     31
                 IF      RECE82-DAY (IDY) =  RECE82-HKNCOMBI
                     MOVE    IDY   TO   WRK-STDAY
                     MOVE    31    TO   IDY
                 END-IF
             END-PERFORM
             IF  WRK-STDAY >  1
               COMPUTE  IDX4 = WRK-STDAY - 1
               PERFORM    VARYING    IDY    FROM    1   BY   1
                           UNTIL     IDY     >    IDX4
                 IF      RECE82-DAY (IDY) =  999
                     MOVE    IDY   TO   WRK-STDAY2
                     MOVE    31    TO   IDY
                 END-IF
               END-PERFORM
               IF  WRK-STDAY2  NOT = ZERO
                 PERFORM  VARYING    IDY    FROM  WRK-STDAY2  BY   1
                           UNTIL     IDY     >    IDX4
                   IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                     MOVE     1    TO   FLG-OKIKAE
                     MOVE    31    TO   IDY
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
             PERFORM    VARYING    IDY    FROM   31   BY  -1
                         UNTIL     IDY     <      1
                 IF      RECE82-DAY (IDY) =  RECE82-HKNCOMBI
                     MOVE    IDY   TO   WRK-EDDAY
                     MOVE     1    TO   IDY
                 END-IF
             END-PERFORM
             IF  WRK-EDDAY < 31
               COMPUTE  IDX4 = WRK-EDDAY + 1
               PERFORM    VARYING    IDY    FROM   31   BY  -1
                           UNTIL     IDY     <    IDX4
                 IF      RECE82-DAY (IDY) =  999
                     MOVE    IDY   TO   WRK-EDDAY2
                     MOVE     1    TO   IDY
                 END-IF
               END-PERFORM
               IF  WRK-EDDAY2  NOT = ZERO
                 PERFORM  VARYING    IDY    FROM  WRK-EDDAY2  BY  -1
                           UNTIL     IDY     <    IDX4
                   IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                     MOVE     1    TO   FLG-OKIKAE
                     MOVE     1    TO   IDY
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
             IF     FLG-OKIKAE  =   1
               IF  WRK-STDAY2  NOT = ZERO
                  MOVE  RECE82-HKNCOMBI  TO  RECE82-DAY(WRK-STDAY2)
               END-IF
               IF  WRK-EDDAY2  NOT = ZERO
                  MOVE  RECE82-HKNCOMBI  TO  RECE82-DAY(WRK-EDDAY2)
               END-IF
             END-IF
      *
      *      同月の履歴の範囲（９９９・９９８）で置き換え
             MOVE    ZERO   TO  FLG-OKIKAE
             MOVE    ZERO   TO  WRK-STDAY
             MOVE    ZERO   TO  WRK-STDAY2
             MOVE    ZERO   TO  WRK-EDDAY
             MOVE    ZERO   TO  WRK-EDDAY2
             MOVE    ZERO   TO  IDX4
             PERFORM    VARYING    IDY    FROM    1   BY   1
                         UNTIL     IDY     >     31
                 IF      RECE82-DAY (IDY) =  RECE82-HKNCOMBI
                     MOVE    IDY   TO   WRK-STDAY
                     MOVE    31    TO   IDY
                 END-IF
             END-PERFORM
             IF  WRK-STDAY >  1
               COMPUTE  IDX4 = WRK-STDAY - 1
               PERFORM    VARYING    IDY    FROM    1   BY   1
                           UNTIL     IDY     >    IDX4
                 IF      RECE82-DAY (IDY) =  999 OR 998
                     MOVE    IDY   TO   WRK-STDAY2
                     MOVE    31    TO   IDY
                 END-IF
               END-PERFORM
               IF  WRK-STDAY2  NOT = ZERO
                 PERFORM  VARYING    IDY    FROM  WRK-STDAY2  BY   1
                           UNTIL     IDY     >    IDX4
                   IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                     MOVE     1    TO   FLG-OKIKAE
                     MOVE    31    TO   IDY
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
             PERFORM    VARYING    IDY    FROM   31   BY  -1
                         UNTIL     IDY     <      1
                 IF      RECE82-DAY (IDY) =  RECE82-HKNCOMBI
                     MOVE    IDY   TO   WRK-EDDAY
                     MOVE     1    TO   IDY
                 END-IF
             END-PERFORM
             IF  WRK-EDDAY < 31
               COMPUTE  IDX4 = WRK-EDDAY + 1
               PERFORM    VARYING    IDY    FROM   31   BY  -1
                           UNTIL     IDY     <    IDX4
                 IF      RECE82-DAY (IDY) =  999 OR 998
                     MOVE    IDY   TO   WRK-EDDAY2
                     MOVE     1    TO   IDY
                 END-IF
               END-PERFORM
               IF  WRK-EDDAY2  NOT = ZERO
                 PERFORM  VARYING    IDY    FROM  WRK-EDDAY2  BY  -1
                           UNTIL     IDY     <    IDX4
                   IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                     MOVE     1    TO   FLG-OKIKAE
                     MOVE     1    TO   IDY
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
             IF     FLG-OKIKAE  =   1
               IF  WRK-STDAY2  NOT = ZERO
                  MOVE  RECE82-HKNCOMBI  TO  RECE82-DAY(WRK-STDAY2)
               END-IF
               IF  WRK-EDDAY2  NOT = ZERO
                  MOVE  RECE82-HKNCOMBI  TO  RECE82-DAY(WRK-EDDAY2)
               END-IF
             END-IF
      *
           END-IF
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                IF    TBL-JNISSU-DAY(IDY)  NOT =  SPACE
                          ADD    1      TO    RECE82-JNISSU
                END-IF
           END-PERFORM
      *
           IF  ((RECE82-HKNKBN     =   "4") AND
                (RECE82-JIBAIKBN   =   "2"))
           OR  ((RECE82-HKNKBN     =   "5") AND
                (RECE82-KOMUKBN    =   "2"))
           OR  ( RECE82-HKNKBN     =   "6" )
             CONTINUE
           ELSE
             IF  RECE82-JNISSU     =   ZERO
                   MOVE    999    TO    RECE82-JNISSU
             END-IF
           END-IF
      *
           IF     WRK-TAIHI-FLG       =   1
                MOVE   WRK-TAIHI-FLG       TO  RECE82-KYUKYU-PRTKBN
                MOVE   WRK-TAIHI-SRYYMD    TO  RECE82-KYUKYUYMD
           END-IF
      *
           IF     WRK-SINDAN-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-SINDAN-TUU      TO  RECE82-SINDAN-TUU        
      ***>      IF    RECE82-TAXKBN      =   "2"                           
      ***>        COMPUTE  WRK-SINDAN-KIN  =
      ***>                (WRK-SINDAN-KIN  * WRK-S5) + WRK-S3
      ***>      END-IF                                                  
                MOVE   WRK-SINDAN-KIN      TO  RECE82-SINDAN-KIN        
           END-IF
      *
           IF     WRK-MEISAI-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-MEISAI-TUU      TO  RECE82-MEISAI-TUU        
      ***>      IF    RECE82-TAXKBN      =   "2"                           
      ***>        COMPUTE  WRK-MEISAI-KIN  =
      ***>                (WRK-MEISAI-KIN  * WRK-S5) + WRK-S3
      ***>      END-IF                                                  
                MOVE   WRK-MEISAI-KIN      TO  RECE82-MEISAI-KIN
           END-IF                                                       
      *
           IF    RECE82-HKNKBN   =   "4" OR "6"
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      20
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUKHNKEI(IDX)
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUTOKKEI(IDX)
              END-PERFORM
      *
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUKHNKEI2
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUTOKKEI2
      *
              MOVE     RECE82-TOTALTEN   TO   RECE82-NYUTOTALTEN
      *
              MOVE     ZERO              TO   WRK-SYUTEN-TOTALTEN
              MOVE     ZERO              TO   WRK-YKZTEN-TOTALTEN
              PERFORM    VARYING    IDY    FROM    1   BY   1              
                          UNTIL      IDY    >     31                    
      *
                  MOVE  TBL-SYUTEN-JI(IDY)  TO  RECE82-SYUTEN-JI(IDY)     
                  COMPUTE  WRK-SYUTEN-TOTALTEN = WRK-SYUTEN-TOTALTEN
                                               + TBL-SYUTEN-JI(IDY)
                  MOVE  TBL-YKZTEN-JI(IDY)  TO  RECE82-YKZTEN-JI(IDY)     
                  COMPUTE  WRK-YKZTEN-TOTALTEN = WRK-YKZTEN-TOTALTEN
                                               + TBL-YKZTEN-JI(IDY)
      *
              END-PERFORM                                                  
              ADD    WRK-SYUTEN-TOTALTEN  TO  RECE82-TOTALTEN
              ADD    WRK-YKZTEN-TOTALTEN  TO  RECE82-TOTALTEN-YAK
      *
              COMPUTE   WRK-KSNRATE     =  RECE82-KSNRATE  /  100
      *
              COMPUTE   RECE82-TOTALKIN   =  
                  (RECE82-TOTALTEN  *  RECE82-TANKA *  WRK-KSNRATE) 
                                                  +  0.5
      *
              COMPUTE   RECE82-TOTALKIN-YAK =  
                  (RECE82-TOTALTEN-YAK  *  RECE82-TANKA)  
                                                  +  0.5
      *
      ***     MOVE     TOTALKIN-ACCT   TO   RECE82-KINGAKU2-SYO
      *
      ***     MOVE     TOTALKIN2-ACCT  TO   RECE82-KINGAKU3-SYO
      *
      ***     COMPUTE  RECE82-TOTALKIN2     =  
      ***         (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5  
      *
           ELSE
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      20
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUKHNKEI(IDX)
                    COMPUTE  RECE82-TOTALTEN  = 
                             RECE82-TOTALTEN  + 
                             RECE82-NYUTOKKEI(IDX)
              END-PERFORM
      *
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUKHNKEI2
              COMPUTE  RECE82-TOTALTEN  = 
                       RECE82-TOTALTEN  + 
                       RECE82-NYUTOKKEI2
      *
              MOVE     RECE82-TOTALTEN  TO   RECE82-NYUTOTALTEN
      *
              ADD      TOTALTEN-ACCT    TO   RECE82-TOTALTEN
      *
              IF    RECE82-HKNKBN   =   "5"
              AND   RECE82-KOMUKBN  =   "2"
                COMPUTE  RECE82-TOTALKIN   =   RECE82-TOTALTEN   *
                                               RECE82-KOMUTENTANKA
              ELSE
                COMPUTE  RECE82-TOTALKIN   =   RECE82-TOTALTEN   *
                                               RECE82-TANKA
              END-IF
      *
              MOVE     TOTALKIN-ACCT    TO   RECE82-KINGAKU2-SYO
      *
           END-IF                                                       
      *
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      20
                 IF   RECE82-NYUTOKRYO(IDX)  =  99999999
                    MOVE    ZERO    TO   RECE82-NYUTOKRYO(IDX)
                 END-IF
                 IF   RECE82-NYUTOKRYO-RYAKU(IDX)  
                                             =  "９９９９９９９９９９"
                    MOVE    SPACE   TO   RECE82-NYUTOKRYO-RYAKU(IDX)
                 END-IF
           END-PERFORM
      *
           .
       270-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       280-RECE82-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE82-REC
      *
           ADD     1                   TO  CNT-MOD
      *
           .
       280-RECE82-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日チェック
      *****************************************************************
       290-DAY-CHECK-SEC              SECTION.
      *
           MOVE     ZERO         TO    WRK-TAIHI-DAY
           PERFORM   VARYING   IDX8   FROM    1    BY   1
                      UNTIL    IDX8     >    31
              IF   ACCT-DAY(1 IDX8)  NOT =   ZERO
                MOVE     IDX8    TO    WRK-TAIHI-DAY
                MOVE     31      TO    IDX8
              END-IF
           END-PERFORM
           IF   WRK-TAIHI-DAY  =  ZERO
              MOVE       01      TO    WRK-TAIHI-DAY
           END-IF
      *
           .
       290-DAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
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
           DISPLAY "RECE820 IN  :" CNT-IN
           DISPLAY "        MOD :" CNT-MOD
           DISPLAY "*** ORCR0830 END ***"
           .
       300-TERM-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
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
      *
      *****************************************************************
       800-ACCT-SETACT9-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           .
       800-ACCT-SETACT9-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE82-READ-EXT.
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
      *         医療機関ＩＤ編集
                MOVE  ZERO           TO  FLG-SET
                MOVE  RECE82-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
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
                     MOVE    SYS-1001-TAX-HASU-KBN
                                      TO    WRK-TAX1001-KBN
                     IF   WRK-TAX1001-KBN = SPACE
                        MOVE   "2"    TO    WRK-TAX1001-KBN
                     END-IF
                  END-IF
      *
                END-PERFORM
                IF     FLG-SET  =  ZERO
                  STRING
                  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
                END-IF
      *
                MOVE      RECE82-SRYYM   TO  WRK-SRYYM-5000
                PERFORM   120-SYS-5000-CHK-SEC
      *         医療機関減算の取得（システム管理テーブルより）
                INITIALIZE                 WRK-SHOKUJIRYOYO-OCC
                                           WRK-TOKUKANRIKASAN-OCC
      *
                MOVE  ZERO         TO  FLG-SET
                MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
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
                      PERFORM  110-SYSKAN-HENSYU-SEC
                      IF        WRK-SYS5000-COUNT    =     ZERO
                         CONTINUE
                      ELSE
                         PERFORM  130-SYS-5000-GET-SEC
                      END-IF
                   END-IF
      *
                END-PERFORM
                IF     FLG-SET  =  ZERO
                  STRING
             "医療機関情報−入院基本が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        RECE82-SRYYM       DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
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
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
               INITIALIZE                      NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
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
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
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
           MOVE    SPACE                  TO   TNS-TENSU-REC
           INITIALIZE                          TNS-TENSU-REC
      *
grpsys     MOVE    SRY-HOSPNUM            TO   TNS-HOSPNUM
           MOVE    SRY-SRYCD(SRYACT-IDX)  TO   TNS-SRYCD
           MOVE    ACCT-SRYYM             TO   TNS-YUKOSTYMD(1:6)
           MOVE    WRK-TAIHI-DAY          TO   TNS-YUKOSTYMD(7:2)
           MOVE    ACCT-SRYYM             TO   TNS-YUKOEDYMD(1:6)
           MOVE    WRK-TAIHI-DAY          TO   TNS-YUKOEDYMD(7:2)
      *
           MOVE    TNS-TENSU-REC          TO   MCPDATA-REC
      *
           MOVE    "tbl_tensu"            TO   MCP-TABLE
           MOVE    "key"                  TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
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
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  NYURYAKUSHO-REC
                   MOVE    ZERO            TO  FLG-NYURYAKUSHO
               ELSE
                   MOVE    1               TO  FLG-NYURYAKUSHO
                   INITIALIZE                  NYURYAKUSHO-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                      NYURYAKUSHO-REC
           END-IF
      *
           MOVE    "tbl_nyuinryakusho"     TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
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
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
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

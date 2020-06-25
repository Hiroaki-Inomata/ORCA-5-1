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
       PROGRAM-ID.             ORCR0410.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                                保険情報編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/13    MCC−門脇　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    02.09.09  自賠責対応
      *  01.00.02    NACL-門脇    02.11.21  自賠責（ポリネック等の対応）
      *  01.00.03    NACL-門脇    02.11.25  診療実日数算定のコード追加
      *  01.00.04    NACL-門脇    03.03.10  診療科１０から２０へ
      *  01.00.05    NACL-門脇    03.04.14  労災の小数点以下金額計算
      *                                     四捨五入に変更
      *  01.00.06    NACL-門脇    03.04.15  自賠責文書料消費税対応
      *  01.00.07    NACL-門脇    03.05.13  自賠責：腰部固定帯加算対応
      *  01.00.08    NACL-門脇    03.05.19  自賠責の初診日の編集修正
      *  01.00.09    NACL-門脇    03.05.20  自賠責：療養担当手当て対応
      *  01.00.10    NACL-門脇    03.06.09  消費税端数対応
      *  01.00.11    NACL-門脇    03.06.19  実日数テーブル編集の追加
      *  01.00.12    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.13    NACL-門脇    04.05.18  （特）の記載対象に麻酔追加
      *  01.00.14    NACL-門脇    04.10.15  自賠責特定器材等消費税対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    04/12/17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
      *  01.01.03    NACL-門脇    05.01.07  自賠責担当医コード、自賠責保
      *                                     険会社名の編集
      *  01.01.04    NACL-門脇    05.01.26  システム管理４００１に関する修正
      *  01.01.05    NACL-門脇    05.01.27  腰・胸部固定帯加算、創外固定器
      *                                     使用加算の技術・薬剤等の選択対応
      *  01.01.06    NACL-門脇    05.06.27  加算率３桁対応
      *  01.01.07    NACL-門脇    05.08.18  転帰事由編集修正
      *  01.01.08    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.01.09    NACL-門脇    05.09.13  （特）対象の編集修正
      *  01.01.10    NACL-門脇    05.10.06  診区９５９６対応
      *  01.01.11    NACL-門脇    05.12.01  労災レセプト回数対応
      *  02.09.01    NACL-門脇    06.04.13  ２００６年４月改正対応
      *  03.01.01    NACL-門脇    06.06.28  実日数減算コード対応
      *
      *  03.03.01    NACL-藤原    06/09/19  月途中での氏名変更対応
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/03/12  包括診療対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.02.01    NACL-門脇    08/02/26  請求区分編集
      *  04.02.02    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *                                     （再就労：円変更対応）
      *  04.03.01    NACL-門脇    09/02/19  アフターの検査日編集条件追加
      *  04.04.01    NACL-門脇    09/11/19  自賠責健保準拠療養担当手当対応
      *  04.04.02    NACL-門脇    10/03/31  改正対応（頸部固定帯加算の対応）
      *
      *  04.05.01    NACL-門脇    09/06/29  療養期間記載方法選択機能
      *                                     （休業証明請求時）
      *  04.05.02    NACL-門脇    10/01/21  治験対応
      *  04.05.03    NACL-門脇    11/10/07  療養期間（終了日）の編集修正
      *  04.05.04    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     実日数カウント対応
      *                                     労災再診（同一日２科目）対応
      *
      *  04.06.01    NACL-門脇    10/12/06  労災・自賠責の金額フリー入力
      *                                     コード対応
      *  04.06.02    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *  04.06.03    NACL-門脇    13/02/27  平成２５年４月改正対応
      *  04.06.04    NACL-門脇    13/06/14  救急医療管理加算算定判定修正
      *  04.06.05    NACL-門脇    13/09/09  点数マスタ月途中金額変更対応
      *                                     （０９５９１〜９４）
      *  04.06.06    NACL-門脇    13/09/25  労災マスタ追加対応
      *                                     （電話再診料　等）
      *
      *  04.07.01    NACL-門脇    11/09/07  労災請求書作成対応
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.03    NACL-藤原    13/03/25  リアルタイムプレビュー時は
      *                                     テスト患者にする
      *  04.07.04    NACL-門脇    13/04/16  労災レセ電対応
      *                                     労災指定医療機関区分編集
      *  04.07.05    NACL-門脇    13/10/02  自賠責（第三者行為）対応
      *  04.07.06    NACL-門脇    14/02/13  労災・自賠責の施術加算対応
      *  04.07.07    NACL-門脇    14/02/21  公務災害（労災準拠）の点数単価
      *                                     設定対応
      *  04.07.08    NACL-門脇    14/03/01  労災マスタ追加対応
      *  04.07.09    NACL-門脇    14/03/04  平成２６年４月改正対応
      *  04.07.10    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.11    NACL-藤原    14/05/07  第三者行為の給付割合の設定
      *  04.07.12    NACL-門脇    14/05/08  労災レセ「新継再別」記載修正
      *  04.07.13    NACL-門脇    14/05/27  第三者行為の給付割合の設定
      *                                     （社保前期高齢２割）
      *  04.07.14    NACL-門脇    14/07/31  第三者行為の現物給付対応
      *  04.07.15    NACL-門脇    14/08/07  労災レセ「新継再別」記載修正
      *  04.07.16    NACL-門脇    14/09/01  平成２６年１０月改正対応
      *  04.07.17    NACL-門脇    14/10/15  療養期間の記載修正
      *  04.07.18    NACL-門脇    14/12/15  平成２７年１月改正対応
      *                                     （初再診料・低妥結率対応）
      *  04.07.19    NACL-門脇    15/01/15  外来管理加算（読み替え加算）
      *                                     チェック
      *  04.07.20    NACL-門脇    15/03/16  平成２７年４月改正対応
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.04    NACL-門脇    17/02/07  労災保険登録時の労働基準監督
      *                                     署コードの編集方法変更に伴う修正
      *  04.08.05    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *  04.08.06    NACL-門脇    17/06/08  アフターケアの「前回の検査年月日」
      *                                     の編集条件修正
      *  04.08.07    NACL-門脇    18/03/27  平成３０年４月改正対応
      *  04.08.08    NACL-門脇    18/06/11  逓減点数集計に関する修正
      *
      *  05.00.01    NACL-門脇    19/05/30  アフターケアの「前回の検査年月日」
      *                                     の編集条件修正
      *  05.00.02    NACL-門脇    19/08/28  令和元年１０月改正（消費税）
      *  05.00.03    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE40-FILE     ASSIGN  RECE40PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE40-KEY
                                   FILE    STATUS  IS  STS-RECE40.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE40-FILE.
       01  RECE40-REC. 
           COPY    "CPRCF040.INC".
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
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
                                   BY         //RECE40//.
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE40                              PIC  X(02).
           03  STS-RECE41                              PIC  X(02).
           03  STS-RECEERR                             PIC  X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC  9(01).
           03  FLG-RECE40                              PIC  9(01).
           03  FLG-PTINF                               PIC  9(01).
           03  FLG-PTRSIINF                            PIC  9(01).
           03  FLG-SYSKANRI                            PIC  9(01).
           03  FLG-KYUSEIRRK                           PIC  9(01).
           03  FLG-SRYACCT                             PIC  9(01).
           03  FLG-SHOSAISIN                           PIC  9(01).
           03  FLG-SHOSAISIN2                          PIC  9(01).
           03  FLG-SHOSAISIN-GEN                       PIC  9(01).
           03  FLG-KYUKYU                              PIC  9(01).
           03  FLG-GAIRAIXX                            PIC  9(01).
           03  FLG-GAIRAI26                            PIC  9(01).
           03  FLG-GAIRAI52                            PIC  9(01).
           03  FLG-SRYACT                              PIC  9(01).
           03  FLG-TENSU                               PIC  9(01).
           03  FLG-DAY                                 PIC  9(01).
           03  FLG-KENSA                               PIC  9(01).
           03  FLG-PTNYUINRRK                          PIC  9(01).
           03  FLG-SET                                 PIC  9(01).
           03  FLG-JYURRKCHECK                         PIC  9(01).
           03  FLG-JYURRK                              PIC  9(01).
           03  FLG-PTHKNINF                            PIC  9(01).
           03  FLG-HKNJAINF                            PIC  9(01).
           03  FLG-HKNNUM                              PIC  9(01).
           03  FLG-NYUIN-ARI                           PIC  9(01).
           03  FLG-NYUIN-ARI2                          PIC  9(01).
           03  FLG-NYUINACCT                           PIC  9(01).
           03  FLG-HKNCOMBI                            PIC  9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC  9(06).
           03  CNT-IN-CANCEL                           PIC  9(06).
           03  CNT-OUT                                 PIC  9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC  9(03).
           03  IDY                                     PIC  9(03).
           03  IDZ                                     PIC  9(03).
           03  IDZ2                                    PIC  9(03).
           03  IDZ3                                    PIC  9(03).
           03  ID1                                     PIC  9(03).
           03  IDX2                                    PIC  9(02).
           03  IDX3                                    PIC  9(02).
           03  IDX4                                    PIC  9(02).
           03  IDX5                                    PIC  9(03).
           03  IDX8                                    PIC  9(02).
           03  SYSKAN-IDX                              PIC  9(03).
           03  IDX-ROU                                 PIC  9(02).
           03  IDX-ST                                  PIC  9(02).
           03  IDX-ED                                  PIC  9(02).
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
           03  WRK-KEISAN                              PIC  9(09).
           03  WRK-KEISAN2                             PIC  9(09).
           03  WRK-KEISAN-KG                           PIC  9(09).
           03  WRK-KEISAN-KG2                          PIC  9(09).
           03  WRK-TEN                                 PIC  9(09).
           03  WRK-TEN-KG                              PIC  9(09).
           03  WRK-TEN-KG2                             PIC  9(09).
           03  WRK-KIN                                 PIC  9(09).
           03  WRK-TANKA                               PIC  9(02)V99.
           03  WRK-KSNRATE                             PIC  9(03).      
           03  WRK-TAXKBN                              PIC  X(01).      
           03  WRK-KOTEIKBN                            PIC  X(01).
           03  WRK-JIBAIKBN                            PIC  X(01).
           03  WRK-4001-JIBAIKBN                       PIC  X(01).
           03  WRK-SYUTENTANKA                         PIC  9(02).
           03  WRK-YKZTENTANKA                         PIC  9(02).
           03  WRK-KOMUKBN                             PIC  X(01).
           03  WRK-KOMUTENTANKA                        PIC  9(02).
           03  WRK-KYUGYO-PRTKBN                       PIC  X(01).
           03  WRK-RSIIRYOKBN                          PIC  X(01).
           03  WRK-KOMURSITANKAKBN                     PIC  X(01).
           03  WRK-KOMURSITANKA-G.
               05  WRK-KOMURSITANKA                    PIC  99V99.
      *
           03  WRK-TAIHI-FLG                           PIC  9(01).
           03  WRK-TAIHI-SRYYMD.
               05  WRK-TAIHI-SRYYM                     PIC  9(06).
               05  WRK-TAIHI-SRYD                      PIC  9(02).
      *
           03  WRK-SINDAN-TUU                          PIC  9(02).      
           03  WRK-SINDAN-KIN                          PIC  9(09).      
           03  WRK-MEISAI-TUU                          PIC  9(02).      
           03  WRK-MEISAI-KIN                          PIC  9(09).      
      *
           03  WRK-DAY                                 PIC  9(02).
      *
           03  WRK-KENSAYMD.
               05  WRK-KENSAYYMM                       PIC  9(06).
               05  WRK-KENSADD                         PIC  9(02).
      *
           03  WRK-KENSAYMD-ZEN                        PIC  9(08).
      *
           03  WRK-ZAIKAISU                            PIC  9(03).
      *
           03  WRK-ZAIKAISU-JI                         PIC  9(03).
           03  WRK-KEISAN-JI                           PIC  9(09).
           03  WRK-TEN-JI                              PIC  9(09).
      *
      *    消費税端数区分
           03  WRK-TAX1001-KBN                         PIC  X(01).
      *
           03  WRK-STYMD                               PIC  X(08).
           03  WRK-EDYMD.
               05  WRK-EDYM                            PIC  9(06).
               05  WRK-EDDD                            PIC  9(02).
      *
           03  WRK-HKNKBN                              PIC  X(01).
      *
           03  WRK-TAIHI-DAY                           PIC  9(02).
      *
           03  WRK-FUTANWARI                           PIC  9V99.
      *
           03  WRK-S1                                  PIC  9(03).
           03  WRK-S2                                  PIC  9(03).
           03  WRK-S3                                  PIC  9V99999.
           03  WRK-S4                                  PIC  9V99.
           03  WRK-S5                                  PIC  9V99.
      *
      *    手技点・薬剤等点数用領域
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   31.
              05  TBL-SYUTEN-JI                        PIC  9(09).
              05  TBL-YKZTEN-JI                        PIC S9(09).
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC                         OCCURS  100.
               05  TBL-SYS4001                         PIC X(610).
      *
      *    保険組合せ領域
       01  TBL-HKNCOMBI.
           03  TBL-HKNCOMBI-OCC                        OCCURS  100.
               05  TBL-HKNCOMBINUM                     PIC  9(04).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC  X(01)
                                                       OCCURS   31.
      *    通院日情報カウント用領域
       01  TBL-JNISSU2.                                                 
           03  TBL-JNISSU2-DAY                         PIC  X(01)       
                                                       OCCURS   31.     
      *    実日数減カウント用領域
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-DAY-GEN                      PIC  X(01)
                                                       OCCURS   31.
      *    外来管理加算（特例：５２）用領域
       01  TBL-G52NISSU.
         02  TBL-G52NISSU-OCC                          OCCURS   9.
           03  TBL-G52NISSU-DAY                        PIC  X(01)
                                                       OCCURS   31.
      *    外来管理加算（特例：２６）用領域
       01  TBL-G26NISSU.
         02  TBL-G26NISSU-OCC                          OCCURS   9.
           03  TBL-G26NISSU-DAY                        PIC  X(01)
                                                       OCCURS   31.
      *    外来管理加算算定用領域
       01  TBL-GXXNISSU.
         02  TBL-GXXNISSU-OCC                          OCCURS   9.
           03  TBL-GXXNISSU-DAY                        PIC  X(01)
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
      *---------------------------------------------------------------
      *
      *    固定値
       01  CONST-AREA.
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101110030".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101120030".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101120040".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101120050".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101120060".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101130100".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101110040".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101120100".
      *
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101800870".
           03  CONS-ROUSAI21   PIC X(09)   VALUE   "101800470".
           03  CONS-ROUSAI22   PIC X(09)   VALUE   "101800480".
           03  CONS-ROUSAI23   PIC X(09)   VALUE   "101800490".
           03  CONS-ROUSAI24   PIC X(09)   VALUE   "101800500".
           03  CONS-ROUSAI25   PIC X(09)   VALUE   "101800510".
           03  CONS-ROUSAI26   PIC X(09)   VALUE   "101800520".
           03  CONS-ROUSAI27   PIC X(09)   VALUE   "101800530".
           03  CONS-ROUSAI28   PIC X(09)   VALUE   "101800540".
           03  CONS-ROUSAI29   PIC X(09)   VALUE   "101800960".
           03  CONS-ROUSAI30   PIC X(09)   VALUE   "101800970".
           03  CONS-ROUSAI31   PIC X(09)   VALUE   "101800980".
      *
           03  CONS-ROUSAI32   PIC X(09)   VALUE   "101800890".
           03  CONS-ROUSAI33   PIC X(09)   VALUE   "101130190".
           03  CONS-ROUSAI34   PIC X(09)   VALUE   "101120010".
      *
           03  CONS-ROUSAI35   PIC X(09)   VALUE   "101120050".
           03  CONS-ROUSAI36   PIC X(09)   VALUE   "101120060".
           03  CONS-ROUSAI37   PIC X(09)   VALUE   "101120070".
           03  CONS-ROUSAI38   PIC X(09)   VALUE   "101120080".
      *
           03  CONS-ROUSAI39   PIC X(09)   VALUE   "101801040".
      *
           03  CONS-GAIRAI1    PIC X(09)   VALUE   "101120070".
           03  CONS-GAIRAI2    PIC X(09)   VALUE   "101120080".
           03  CONS-GAIRAI3    PIC X(09)   VALUE   "101120090".
           03  CONS-GAIRAI4    PIC X(09)   VALUE   "101120020".
      *
      *    腰部固定帯加算
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    胸部固定帯加算
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    頸部固定帯加算
           03  CONS-KEIBUKASAN PIC X(09)   VALUE   "140052610".
      *    創外固定器使用加算
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    療養担当手当て
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000610".
      *
           03  WRK-CONS-SRYYM-200804
                               PIC 9(06)   VALUE   200804.
           03  WRK-CONS-SRYYM-201109
                               PIC 9(06)   VALUE   201109.
           03  WRK-CONS-SRYYM-201707
                               PIC 9(06)   VALUE   201707.
      *
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    労災患者保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報−基本
           COPY    "CPSK1001.INC".
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
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
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    労災レセプト回数取得サブ
           COPY    "CPORCSROUKAISU.INC".
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
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
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       WRK-AREA
                                       FLG-AREA
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
           MOVE    SPACE               TO  RECE40PARA
           INITIALIZE                      RECE40PARA
           MOVE    "RECE400"           TO  RECE40PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE40PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE40PARA-HOSPNUM
           MOVE    SPACE               TO  RECE41PARA
           INITIALIZE                      RECE41PARA
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE40PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE40PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE41PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0410"      TO  JOB-PGID
               MOVE    "保険情報編集（労災・自賠責）"                       
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報−基本
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO  SYS-1001-REC
                  MOVE    SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
                  IF   WRK-TAX1001-KBN = SPACE
                     MOVE   "2"           TO  WRK-TAX1001-KBN
                  END-IF
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
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
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 810-SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
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
           MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
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
               MOVE    1                   TO  FLG-END
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
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-4001-REC
                  EVALUATE  SYS-4001-TENKBN
                      WHEN  "1"
                          MOVE     11.5    TO    WRK-TANKA
                      WHEN  "2"
                          MOVE     12.0    TO    WRK-TANKA
                  END-EVALUATE
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
                  MOVE   SYS-4001-TAXKBN   TO    WRK-TAXKBN                
                  MOVE   SYS-4001-KOTEIKBN TO    WRK-KOTEIKBN
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
                      IF     SYS-4001-SYUTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-SYUTENTANKA
                      ELSE
                          MOVE SYS-4001-SYUTENTANKA
                                                 TO    WRK-SYUTENTANKA
                      END-IF
                      IF     SYS-4001-YKZTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-YKZTENTANKA
                      ELSE
                          MOVE SYS-4001-YKZTENTANKA
                                                 TO    WRK-YKZTENTANKA
                      END-IF
                  END-IF
                  MOVE   WRK-JIBAIKBN      TO  WRK-4001-JIBAIKBN
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
                  IF     SYS-4001-KYUGYO-PRTKBN   =  SPACE
                    MOVE "1"               TO    WRK-KYUGYO-PRTKBN
                  ELSE
                    MOVE SYS-4001-KYUGYO-PRTKBN
                                           TO    WRK-KYUGYO-PRTKBN
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
      *
                  DISPLAY "WRK-4001-JIBAIKBN=" WRK-4001-JIBAIKBN
              ELSE
                  MOVE    "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
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
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-4001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-4001-REC  TO  TBL-SYS4001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 810-SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "労災自賠医療機関情報が取得できませんでした"     
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           OPEN    INPUT               RECE40-FILE
           IF      STS-RECE40          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE40PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE40          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE41-FILE
           CLOSE                       RECE41-FILE
      *
           OPEN    I-O                 RECE41-FILE
      *
           PERFORM     900-RECE40-READ-SEC
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     220-DATA-WRITE-SEC
      *
           PERFORM     900-RECE40-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
      *     
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
      *    自賠責区分（患者別設定）
           IF      RECE40-SRYYM        >=  WRK-CONS-SRYYM-201707
               IF     WRK-PARA-RECEKBN     =   "8"
                   CONTINUE
               ELSE
                   DISPLAY "RECE40-JIBAIKBN=" RECE40-JIBAIKBN
                   MOVE    RECE40-JIBAIKBN     TO  WRK-JIBAIKBN
               END-IF
           END-IF
           DISPLAY "WRK-JIBAIKBN=" WRK-JIBAIKBN
      *
      *    消費税計算の係数設定
           IF    RECE40-SRYYM >= "201910"
               MOVE      10      TO  WRK-S1
               MOVE     110      TO  WRK-S2
               MOVE     0.10     TO  WRK-S4
               MOVE     1.10     TO  WRK-S5
           ELSE
             IF  RECE40-SRYYM >= "201404"
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
           MOVE    SPACE                TO  RECE41-REC
           INITIALIZE                       RECE41-REC
      *
           MOVE    RECE40-HOSPNUM       TO  RECE41-HOSPNUM
           MOVE    RECE40-SRYYM         TO  RECE41-SRYYM
           MOVE    RECE40-NYUGAIKBN     TO  RECE41-NYUGAIKBN
           MOVE    RECE40-PTID          TO  RECE41-PTID
           MOVE    RECE40-RECEKA        TO  RECE41-RECEKA
           MOVE    RECE40-HKNID         TO  RECE41-HKNID
           MOVE    RECE40-TEKSTYMD      TO  RECE41-TEKSTYMD
      *
      *    患者番号変換
           INITIALIZE                       ORCSPTIDAREA
grpsys     MOVE    RECE40-HOSPNUM       TO  SPTID-HOSPNUM
           MOVE    RECE40-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"           USING   ORCSPTIDAREA
                                                SPA-AREA
           MOVE    SPTID-PTNUM          TO  RECE41-PTNUM
      *    診療科情報のセット
           PERFORM   VARYING   IDX   FROM   1   BY   1
                     UNTIL    (IDX      >   20)    OR
                              (RECE40-SRYKA(IDX)  =   SPACE)
               MOVE  RECE40-SRYKA(IDX)    TO   RECE41-SRYKA(IDX)
           END-PERFORM
      *    患者情報の取得
           INITIALIZE                           PTINF-REC
grpsys     MOVE    RECE40-HOSPNUM       TO      PTINF-HOSPNUM
           MOVE    RECE40-PTID          TO      PTINF-PTID
           PERFORM 800-PTINF-READ-SEC
      *    リアルタイムプレビュー時はテスト患者にする
           IF      WRK-PARA-SHELLID     =   "RECEPTX"
               MOVE    "1"                  TO  PTINF-TSTPTNUMKBN
           END-IF
           MOVE   PTINF-KANANAME    TO  RECE41-KANANAME
           MOVE   PTINF-NAME        TO  RECE41-NAME
           MOVE   PTINF-SEX         TO  RECE41-SEX
           MOVE   PTINF-BIRTHDAY    TO  RECE41-BIRTHDAY
           MOVE   PTINF-DEATHKBN    TO  RECE41-DEATHKBN
      *
      *    患者年齢計算
           INITIALIZE                   ORCSAGECHKAREA
           MOVE    RECE40-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    RECE40-PTID      TO  AGECHK-PTID
           MOVE    RECE41-SRYYM     TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"             TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY   TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"         USING
                                        ORCSAGECHKAREA
                                        SPA-AREA
           IF      AGECHK-RC        =   ZERO
               MOVE  AGECHK-NENREI  TO  RECE41-AGE
           END-IF
      *
           MOVE   PTINF-TSTPTNUMKBN TO  RECE41-TSTPTNUMKBN
      *
      *    旧姓履歴の取得
           INITIALIZE                           KYUSEIRRK-REC
grpsys     MOVE    RECE40-HOSPNUM       TO      KYUSEI-HOSPNUM
           MOVE    RECE40-PTID          TO      KYUSEI-PTID
           INITIALIZE                          STS-AREA-DAY
           INITIALIZE                          LNK-DAY7-AREA
           MOVE    "71"                 TO  LNK-DAY7-IRAI
           MOVE    RECE41-SRYYM         TO  LNK-DAY7-YM
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           IF    ( RECE40-TEISEIKBN     NOT =   "1"         )
           AND   ( RECE40-TEKEDYMD-X    <   LNK-DAY7-KOYOMI )
               MOVE    RECE40-TEKEDYMD-X    TO  KYUSEI-CHGYMD
           ELSE
               MOVE    LNK-DAY7-KOYOMI      TO  KYUSEI-CHGYMD
           END-IF
           MOVE    KYUSEIRRK-REC        TO      MCPDATA-REC
           PERFORM 800-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK           =   ZERO
               MOVE    KYUSEI-NAME          TO  RECE41-NAME
               MOVE    KYUSEI-KANANAME      TO  RECE41-KANANAME
           END-IF         
      *          
      *    患者保険情報テーブルから主保険情報の取得
      *    IF  RECE40-HKNID             NOT =   ZERO
               INITIALIZE                           PTRSIINF-REC
grpsys         MOVE    RECE40-HOSPNUM       TO      PTRSI-HOSPNUM
               MOVE    RECE40-PTID          TO      PTRSI-PTID
               IF      WRK-PARA-RECEKBN     =   "8"
                   MOVE    RECE40-HKNID         TO      PTRSI-KOHID
               ELSE
                   MOVE    RECE40-HKNID         TO      PTRSI-HKNID
               END-IF
               MOVE    PTRSIINF-REC         TO      MCPDATA-REC
               MOVE    "tbl_ptrsiinf"       TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC               =       ZERO
                   MOVE    "tbl_ptrsiinf"   TO      MCP-TABLE
                   MOVE    "key"            TO      MCP-PATHNAME
                   PERFORM  800-PTRSIINF-READ-SEC
                   IF       FLG-PTRSIINF        =   ZERO
                        MOVE   PTRSI-HKNKBN     TO  RECE41-HKNKBN
                        MOVE   PTRSI-HKNNUM     TO  RECE41-HKNNUM
                        MOVE   PTRSI-KOFUNUM    TO  RECE41-KOFUNUM
                        MOVE   PTRSI-JIGYOUNAME TO  RECE41-JIGYOUNAME
                        MOVE   PTRSI-PREFNAME   TO  RECE41-PREFNAME
                        MOVE   PTRSI-PREFKBN    TO  RECE41-PREFKBN  
                        MOVE   PTRSI-CITYNAME   TO  RECE41-CITYNAME
                        MOVE   PTRSI-CITYKBN    TO  RECE41-CITYKBN
      *
                        MOVE   PTRSI-SINKEI     TO  RECE41-SINKEI
                        EVALUATE  PTRSI-HKNKBN 
                           WHEN    "1"  
                            IF   ( PTRSI-SINKEI = "1" OR "3" OR "7" )
                            AND  ( PTRSI-RYOSTYMD(1:6) 
                                                 <  RECE41-SRYYM )
                                MOVE   "5"      TO  RECE41-SINKEI
                            END-IF
                           WHEN    "2"  
                            IF   ( PTRSI-SINKEI = "1" OR "7"        )
                            AND  ( PTRSI-RYOSTYMD(1:6) 
                                                 <  RECE41-SRYYM )
                                MOVE   "5"      TO  RECE41-SINKEI
                            END-IF
                        END-EVALUATE
      *
                        MOVE   PTRSI-TENKI      TO  RECE41-TENKI
                        IF    (PTRSI-TENKI  = "1" OR "5" OR "7" OR "9")
                        AND   (RECE41-SRYYM   <   PTRSI-RYOEDYMD(1:6) )
                                MOVE   "3"      TO  RECE41-TENKI
                        END-IF
                        IF     RECE41-SRYYM   =   PTRSI-RYOEDYMD(1:6)
                          MOVE PTRSI-RYOEDYMD   TO  RECE41-TENKIYMD
                        END-IF
                        MOVE   PTRSI-SHOBYOYMD  TO  RECE41-SHOBYOYMD
                        MOVE   PTRSI-SISIKBN    TO  RECE41-SISIKBN
                        MOVE   PTRSI-RYOSTYMD   TO  RECE41-RYOSTYMD
                        MOVE   PTRSI-RYOEDYMD   TO  RECE41-RYOEDYMD
                        MOVE   PTRSI-SONSHOUKBN TO  RECE41-SONSHOUKBN
                        MOVE   PTRSI-SONSHOUKBN-EDANUM
                                         TO  RECE41-SONSHOUKBN-EDANUM 
                        MOVE   PTRSI-SINSATUYMD TO  RECE41-SINSATUYMD
                        MOVE   PTRSI-COMMENT    TO  RECE41-COMMENT
                        MOVE   PTRSI-SAIGAIKBN  TO  RECE41-SAIGAIKBN 
                        MOVE   PTRSI-RYOSTYMD   TO  RECE41-SHOSHINYMD
      *
                        MOVE   PTRSI-DRCD       TO  RECE41-DRCD
                        MOVE   PTRSI-HOKENNAME  TO  RECE41-HOKENNAME
      *
                        MOVE   PTRSI-KIJYUNYM   TO  RECE41-KIJYUNYM-KSI
                        MOVE   PTRSI-KAISU      TO  RECE41-KAISU-KSI
                        MOVE   PTRSI-JIBAISEIKBN
                                                TO  RECE41-SEIKYUKBN
                        MOVE   PTRSI-SYOCD      TO  RECE41-SYOCD
      *
                        IF     RECE41-SRYYM   >=  "201204"
      *                   現物給付対象とする
                          IF     PTRSI-DAISAN-GENBTKBN  =  "2"
                            MOVE   1   TO   RECE41-DAISAN-GENBUTU-GAITO
                          END-IF
                        END-IF
                   END-IF
               END-IF
      *    END-IF
           MOVE    "tbl_ptrsiinf"   TO      MCP-TABLE
           MOVE    "key"            TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    保険組合せ番号情報
           MOVE    RECE40-HKNCOMBI   TO  RECE41-HKNCOMBI
      *
      *    受診歴チェック
           MOVE    ZERO   TO   FLG-JYURRKCHECK
           IF      RECE41-HKNKBN    =   "4" OR "5"
              PERFORM 2109-JYURRK-CHECK-SEC
           END-IF
      *
      *    第三者行為時の医保分主保険情報
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "8"
                   MOVE    RECE40-HKNNUM-X     TO  RECE41-HKNNUM-X
                   MOVE    RECE40-HKNID-X      TO  RECE41-HKNID-X
      * 
                   INITIALIZE                      PTHKNINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                   MOVE    RECE40-PTID         TO  PTHKN-PTID
                   MOVE    RECE40-HKNID-X      TO  PTHKN-HKNID
                   MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                   PERFORM 900-PTHKNINF-READ-SEC
                   IF      FLG-PTHKNINF        =   ZERO
                       MOVE    PTHKN-HKNJANUM      TO  RECE41-HKNJANUM
                       MOVE    PTHKN-KIGO          TO  RECE41-KIGO
                       MOVE    PTHKN-NUM           TO  RECE41-NUM
                       IF      RECE41-HKNJANUM NOT =   SPACE
                           MOVE    SPACE               TO  HKNJAINF-REC
                           INITIALIZE                      HKNJAINF-REC
                           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                           MOVE    RECE41-HKNJANUM     TO
                                                       HKNJA-HKNJANUM
                           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                           PERFORM 900-HKNJAINF-READ-SEC
                           MOVE    HKNJA-HKNJANAME TO  RECE41-HKNJANAME
                       END-IF
                   END-IF
      *
                   INITIALIZE                      HKNNUM-REC
                   MOVE   RECE41-HOSPNUM       TO  HKN-HOSPNUM
                   MOVE    RECE41-SRYYM        TO  HKN-TEKSTYMD (1:6)
                   MOVE    "01"                TO  HKN-TEKSTYMD (7:2)
                   MOVE    HKN-TEKSTYMD        TO  HKN-TEKEDYMD
                   IF     RECE41-HKNNUM-X      =   "069"
                       MOVE    "067"               TO  HKN-HKNNUM
                   ELSE
                       MOVE   RECE41-HKNNUM-X      TO  HKN-HKNNUM
                   END-IF
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-READ-SEC
      *
      *            負担割合セット処理
                   PERFORM 2120-FTNRATE-SET-SEC
           END-EVALUATE
      *
      *    レセプト種別
           PERFORM 2101-RECESYUBETU-SET-SEC
      *
      *    月末日編集
           MOVE    SPACE             TO  WRK-EDYMD
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"              TO  LNK-DAY7-IRAI
           MOVE    RECE41-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO  WRK-EDYMD
      *
      *    入院履歴から入院日をセット
           PERFORM 2102-NYUINBI-SET-SEC
      *
      *    実日数等情報
           IF      RECE41-HKNKBN   NOT  =   "3"
      *
              MOVE       SPACE             TO     TBL-JNISSU
              INITIALIZE                          TBL-JNISSU
              MOVE       SPACE             TO     TBL-JNISSU2           
              INITIALIZE                          TBL-JNISSU2           
              MOVE       SPACE             TO     TBL-JNISSU-GEN
              INITIALIZE                          TBL-JNISSU-GEN
              MOVE       SPACE             TO     TBL-G52NISSU
              INITIALIZE                          TBL-G52NISSU
              MOVE       SPACE             TO     TBL-G26NISSU
              INITIALIZE                          TBL-G26NISSU
              MOVE       SPACE             TO     TBL-GXXNISSU
              INITIALIZE                          TBL-GXXNISSU
              MOVE       SPACE             TO     TBL-TENSUG-JI
              INITIALIZE                          TBL-TENSUG-JI
      *
              MOVE       ZERO              TO     WRK-TAIHI-FLG
      *
              MOVE       ZERO              TO     WRK-SINDAN-TUU        
                                                  WRK-SINDAN-KIN        
                                                  WRK-MEISAI-TUU        
                                                  WRK-MEISAI-KIN        
      *        
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE40-SRYKA(IDX)   =   SPACE)
                  PERFORM    2103-NISUU-HENSYU-SEC
              END-PERFORM
      *
              PERFORM     2104-HENSYU-SET-SEC
      ****    PERFORM     2105-HENSYU-SET2-SEC
      *
           ELSE
      *
              MOVE       SPACE             TO     TBL-JNISSU
              INITIALIZE                          TBL-JNISSU
              MOVE       SPACE             TO     TBL-JNISSU2
              INITIALIZE                          TBL-JNISSU2
              MOVE       SPACE             TO     TBL-JNISSU-GEN
              INITIALIZE                          TBL-JNISSU-GEN
              MOVE       SPACE             TO     TBL-HKNCOMBI
              INITIALIZE                          TBL-HKNCOMBI
      *
              MOVE       ZERO       TO     IDX5
      *
              MOVE       ZERO       TO     WRK-KENSAYMD-ZEN
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE40-SRYKA(IDX)   =   SPACE)
                  PERFORM    2106-NISUU-HENSYU2-SEC
              END-PERFORM
      *
              PERFORM     2107-NISUU-HENSYU3-SEC
      *
      ****    PERFORM     2105-HENSYU-SET2-SEC
              PERFORM     2108-HENSYU-SET3-SEC
      *
           END-IF 
      *
      *    点数単価
           MOVE    WRK-TANKA             TO   RECE41-TANKA
      *    公務災害（労災準拠）の点数単価
           IF   (RECE41-HKNKBN           =   "5" )
           AND  (WRK-KOMUKBN             =   "1" )
           AND  (WRK-KOMURSITANKAKBN     =   "2" )
           AND  (WRK-KOMURSITANKA-G  NOT =  SPACE)
               MOVE   WRK-KOMURSITANKA   TO   RECE41-TANKA
           END-IF
      *    自賠責加算率
           MOVE    WRK-KSNRATE           TO   RECE41-KSNRATE
      *    自賠責文書料消費税
           MOVE    WRK-TAXKBN            TO   RECE41-TAXKBN
      *    自賠責特定器材等消費税
           MOVE    SYS-4001-TAXKBN-KZI   TO   RECE41-TAXKBN-KZI
      *    自賠責固定帯加算等取扱
           MOVE    WRK-KOTEIKBN          TO   RECE41-KOTEIKBN
      *    労災レセ回数記載
           MOVE    SYS-4001-ROUKAISU     TO   RECE41-ROUKAISU
      *    自賠責区分
           MOVE    WRK-JIBAIKBN          TO   RECE41-JIBAIKBN
      *    技術点単価
           MOVE    WRK-SYUTENTANKA       TO   RECE41-SYUTENTANKA
      *    薬剤等点単価
           MOVE    WRK-YKZTENTANKA       TO   RECE41-YKZTENTANKA
      *    公務災害区分
           MOVE    WRK-KOMUKBN           TO   RECE41-KOMUKBN
      *    公務災害点単価
           MOVE    WRK-KOMUTENTANKA      TO   RECE41-KOMUTENTANKA
      *    休業証明療養期間記載
           MOVE    WRK-KYUGYO-PRTKBN     TO   RECE41-KYUGYO-PRTKBN
      *    労災指定医療機関区分
           MOVE    WRK-RSIIRYOKBN        TO   RECE41-RSIIRYOKBN
           IF      RECE41-HKNKBN     =   "4" OR "6"
               MOVE    "0"               TO   RECE41-RSIIRYOKBN
           END-IF
      *
      *    労災レセプト回数取得
           IF    ( RECE41-HKNKBN     =   "1"  OR  "2" )
             INITIALIZE                    LNK-ORCSROUKAISU-REC
             MOVE  RECE41-HOSPNUM      TO  LNK-ORCSROUKAISU-HOSPNUM
             MOVE  RECE41-NYUGAIKBN    TO  LNK-ORCSROUKAISU-NYUGAIKBN
             MOVE  RECE41-PTID         TO  LNK-ORCSROUKAISU-PTID
             MOVE  RECE41-HKNCOMBI     TO  LNK-ORCSROUKAISU-HKNCOMBI
             MOVE  RECE41-SRYYM        TO  LNK-ORCSROUKAISU-SRYYM
             MOVE  RECE41-RYOSTYMD     TO  LNK-ORCSROUKAISU-RYOSTYMD
             MOVE  RECE41-KIJYUNYM-KSI TO  LNK-ORCSROUKAISU-KIJYUNYM-KSI
             MOVE  RECE41-KAISU-KSI    TO  LNK-ORCSROUKAISU-KAISU-KSI
             MOVE  RECE41-SRYKA(1)     TO  LNK-ORCSROUKAISU-SRYKA
             MOVE  RECE41-RECEKA       TO  LNK-ORCSROUKAISU-RECESRYKA
      *+++
             MOVE  RECE41-SRYYM        TO  WRK-STYMD (1:6)
             MOVE  "01"                TO  WRK-STYMD (7:2)
      *
             IF      RECE41-RYOSTYMD   <   WRK-STYMD
                 MOVE   WRK-STYMD(7:2) TO  IDX-ST
                                           LNK-ORCSROUKAISU-KAISHIBI
             ELSE
                 MOVE   RECE41-RYOSTYMD(7:2)
                                       TO  IDX-ST
                                           LNK-ORCSROUKAISU-KAISHIBI
             END-IF
      *
             IF      RECE41-RYOEDYMD   >   WRK-EDYMD
                 MOVE   WRK-EDYMD(7:2) TO  IDX-ED
             ELSE
                 MOVE   RECE41-RYOEDYMD(7:2)
                                       TO  IDX-ED
             END-IF
      *
             PERFORM   VARYING   IDX-ROU   FROM   IDX-ST   BY  1
                        UNTIL    IDX-ROU   >      IDX-ED
                   IF  (RECE41-NYUINBITBL(IDX-ROU)  =  SPACE)  OR
                       (RECE41-JNISSUTBL (IDX-ROU)  =  "1"  )
                       MOVE    IDX-ROU   TO  LNK-ORCSROUKAISU-KAISHIBI
                       MOVE    IDX-ED    TO  IDX-ROU
                   END-IF
             END-PERFORM
      *
             CALL    "ORCSROUKAISU"    USING
                                       LNK-ORCSROUKAISU-REC
                                       SPA-AREA
      *
             IF    LNK-ORCSROUKAISU-RC   =   ZERO
              IF   LNK-ORCSROUKAISU-RECEKAISU  =  1
                MOVE   "1"               TO  RECE41-SYOKAIFLG
              END-IF
              MOVE     LNK-ORCSROUKAISU-RECEKAISU
                                         TO  RECE41-RECEKAISU
      *
              IF   (RECE41-SINKEI        =  "3"         )
              AND  (RECE41-RYOSTYMD(1:6) =  RECE41-SRYYM)
              AND  (RECE41-KIJYUNYM-KSI  =  RECE41-SRYYM)
              AND  (RECE41-KAISU-KSI     >  1           )
                IF  LNK-ORCSROUKAISU-RECEKAISU  >  RECE41-KAISU-KSI
                  MOVE     "5"           TO  RECE41-SINKEI
                END-IF
              ELSE
                IF  LNK-ORCSROUKAISU-RECEKAISU  >  1
                  MOVE     "5"           TO  RECE41-SINKEI
                END-IF
              END-IF
             END-IF
           END-IF
      *
           IF    RECE41-SYOCD            =   SPACE
           AND   PTRSI-RYOSTYMD(1:6)    <=  "201403"
             EVALUATE  RECE41-HKNKBN
               WHEN       "1"
                 MOVE    RECE41-KOFUNUM(1:5)
                                           TO  RECE41-SYOCD
               WHEN       "2"
                 INITIALIZE                    PTRSIINF-REC
                 MOVE    RECE41-HOSPNUM    TO  PTRSI-HOSPNUM
                 MOVE    RECE41-PTID       TO  PTRSI-PTID
                 MOVE    "1"               TO  PTRSI-HKNKBN
                 MOVE    RECE41-SHOBYOYMD  TO  PTRSI-SHOBYOYMD
                 MOVE    PTRSIINF-REC      TO  MCPDATA-REC
                 MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
                 MOVE    "key4"            TO  MCP-PATHNAME
                 PERFORM  900-DBSELECT-SEC
                 IF      MCP-RC            =   ZERO
                   MOVE  "tbl_ptrsiinf"    TO  MCP-TABLE
                   MOVE  "key4"            TO  MCP-PATHNAME
                   PERFORM  800-PTRSIINF-READ-SEC
                   IF    FLG-PTRSIINF      =   ZERO
                     IF    PTRSI-SYOCD  NOT = SPACE
                       MOVE  PTRSI-SYOCD         TO  RECE41-SYOCD
                     ELSE
                       MOVE  PTRSI-KOFUNUM(1:5)  TO  RECE41-SYOCD
                     END-IF
                   END-IF
                 END-IF
                 MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
                 MOVE    "key4"            TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
             END-EVALUATE
           END-IF
      *
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担割合セット処理
      *****************************************************************
       2120-FTNRATE-SET-SEC            SECTION.
      *
      *    平成２０年４月以降のみ設定
           IF      RECE41-SRYYM    >=  WRK-CONS-SRYYM-200804
               EVALUATE    RECE41-HKNNUM-X
                   WHEN    "060"
                   WHEN    "068"
                       IF       AGECHK-SYUGAKUKBN  =   1
                           EVALUATE   PTHKN-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO  RECE41-FTNRATE
                              WHEN   "1"
                                  MOVE   10        TO  RECE41-FTNRATE
                              WHEN   "4"
                                  MOVE   10        TO  RECE41-FTNRATE
                              WHEN   OTHER
                                  MOVE   20        TO  RECE41-FTNRATE
                           END-EVALUATE
                       ELSE
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                                  MOVE    ZERO     TO  RECE41-FTNRATE
                               WHEN   "1"
                                  MOVE    10       TO  RECE41-FTNRATE
                               WHEN   "2"
                                  MOVE    20       TO  RECE41-FTNRATE
                               WHEN   "3"
                                  MOVE    30       TO  RECE41-FTNRATE
                               WHEN   "4"
                                  MOVE    10       TO  RECE41-FTNRATE
                               WHEN   "5"
                                  MOVE    20       TO  RECE41-FTNRATE
                               WHEN   "6"
                                  MOVE    30       TO  RECE41-FTNRATE
                               WHEN   "8"
                                  MOVE    20       TO  RECE41-FTNRATE
                               WHEN   "9"
                                  MOVE    10       TO  RECE41-FTNRATE
                           END-EVALUATE
                       END-IF
                   WHEN    "067"
                   WHEN    "069"
                       IF      AGECHK-SYUGAKUKBN   =   1
                           MOVE    20              TO  RECE41-FTNRATE
                       ELSE
                           IF      PTHKN-HOJOKBN     NOT =  SPACE
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE41-FTNRATE
                                   WHEN   "8"
                                       MOVE    20  TO  RECE41-FTNRATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE41-FTNRATE
                               END-EVALUATE
                           ELSE
                               IF    ( RECE41-HKNJANUM =   "67330753" )
                               AND   ( RECE41-SRYYM    <=
                                               WRK-CONS-SRYYM-201109  )
                                   MOVE    20      TO  RECE41-FTNRATE
                               ELSE
                                   IF      PTHKN-HONKZKKBN =   "1"
                                       MOVE    HKN-HON-GAIKFTNRATE
                                                   TO  RECE41-FTNRATE
                                   ELSE
                                       MOVE    HKN-KZK-GAIKFTNRATE
                                                   TO  RECE41-FTNRATE
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   WHEN    SPACE
                       MOVE    ZERO            TO  RECE41-FTNRATE
                   WHEN    "039"
                   WHEN    "040"
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "1"
                               MOVE    10          TO  RECE41-FTNRATE
                           WHEN   "3"
                               MOVE    30          TO  RECE41-FTNRATE
                       END-EVALUATE
                   WHEN    OTHER
                       IF       AGECHK-SYUGAKUKBN  =   1
                           MOVE    20          TO  RECE41-FTNRATE
                       ELSE
                           IF      PTHKN-HOJOKBN       =   SPACE
                               IF      PTHKN-HONKZKKBN =   "1"
                                   MOVE   HKN-HON-GAIKFTNRATE
                                                   TO  RECE41-FTNRATE
                               ELSE
                                   MOVE   HKN-KZK-GAIKFTNRATE
                                                   TO  RECE41-FTNRATE
                               END-IF
                           ELSE
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE41-FTNRATE
                                   WHEN   "8"
                                       MOVE    20  TO  RECE41-FTNRATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE41-FTNRATE
                               END-EVALUATE
                          END-IF
                      END-IF
               END-EVALUATE
           END-IF
      *
           .
       2120-FTNRATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       2101-RECESYUBETU-SET-SEC              SECTION.
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      RECE40-TEISEIKBN     =   "1"
           OR      FLG-JYURRKCHECK      =    1
               GO  TO  2101-RECESYUBETU-SET-EXT
           END-IF
      *
           EVALUATE  RECE41-HKNKBN  ALSO  RECE41-SAIGAIKBN
               WHEN    "1"          ALSO    "1"
                   MOVE    7071            TO    RECE41-RECESYUBETU
               WHEN    "1"          ALSO    "2"
                   MOVE    7072            TO    RECE41-RECESYUBETU
               WHEN    "2"          ALSO    "1"
                   MOVE    7091            TO    RECE41-RECESYUBETU
               WHEN    "2"          ALSO    "2"
                   MOVE    7092            TO    RECE41-RECESYUBETU
               WHEN    "3"          ALSO    ANY   
                   MOVE    7021            TO    RECE41-RECESYUBETU
               WHEN    "4"          ALSO    ANY                         
               WHEN    "6"          ALSO    ANY                         
                   MOVE    7001            TO    RECE41-RECESYUBETU     
               WHEN    "5"          ALSO    ANY                         
                   MOVE    7051            TO    RECE41-RECESYUBETU     
           END-EVALUATE
           .
       2101-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴から入院日をセット
      *****************************************************************
       2102-NYUINBI-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUIN-ARI
           MOVE    ZERO                TO  FLG-NYUIN-ARI2
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
grpsys     MOVE    RECE41-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE41-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE41-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE41-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key23"             TO  MCP-PATHNAME
               PERFORM  800-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK    =   1
               IF  PTNYUINRRK-NYUINYMD (1:6)   <   RECE41-SRYYM
                   MOVE    1                         TO  IDX3
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD(7:2)  TO  IDX3
               END-IF 
               IF  PTNYUINRRK-TAIINYMD (1:6)   >   RECE41-SRYYM
                   MOVE    31                        TO  IDX4
                   IF      IDX4   >   WRK-EDDD
                      MOVE    WRK-EDDD               TO  IDX4
                   END-IF
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD(7:2)  TO  IDX4
               END-IF
      *
               PERFORM  VARYING IDX2 FROM    IDX3    BY  1
                        UNTIL   IDX2   >     IDX4
                   MOVE    "1"    TO   RECE41-NYUINBITBL(IDX2)
                   MOVE     1     TO   FLG-NYUIN-ARI
               END-PERFORM
      *                 
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key23"             TO  MCP-PATHNAME
               PERFORM  800-PTNYUINRRK-READ-SEC
           END-PERFORM        
      *                 
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ***
      *
           IF   FLG-NYUIN-ARI   =   1
             PERFORM 21021-NYUINBI-CHECK-SEC
           END-IF
      *
           .
       2102-NYUINBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日に同一保険組み合わせの入力があるかチェック
      *****************************************************************
       21021-NYUINBI-CHECK-SEC              SECTION.
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    RECE41-HOSPNUM     TO   NACCT-HOSPNUM
           MOVE    RECE41-PTID        TO   NACCT-PTID
           MOVE    RECE41-SRYYM       TO   NACCT-SRYYM
           MOVE    "5"                TO   NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC      TO   MCPDATA-REC
           MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
           MOVE    "key17"            TO   MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_nyuinacct"  TO   MCP-TABLE
             MOVE    "key17"          TO   MCP-PATHNAME
             PERFORM  800-NYUINACCT-READ-SEC
             IF    FLG-NYUINACCT  =  ZERO
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                         UNTIL   IDX2   >   31
                 IF   (RECE41-NYUINBITBL(IDX2)  =  "1"  )
                 AND  (NACCT-DAY(IDX2) = RECE41-HKNCOMBI)
                   MOVE      1        TO   FLG-NYUIN-ARI2
                 END-IF
               END-PERFORM
             END-IF
           END-IF
           MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
           MOVE    "key17"            TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           IF   FLG-NYUIN-ARI2  =  ZERO
             INITIALIZE                      SRYACCT-REC
             MOVE    RECE41-HOSPNUM     TO   ACCT-HOSPNUM
             MOVE    "1"                TO   ACCT-NYUGAIKBN
             MOVE    RECE41-PTID        TO   ACCT-PTID
             MOVE    RECE41-SRYYM       TO   ACCT-SRYYM
             MOVE    RECE41-HKNCOMBI    TO   ACCT-HKNCOMBI
             MOVE    SRYACCT-REC        TO   MCPDATA-REC
             MOVE    "tbl_sryacct"      TO   MCP-TABLE
             MOVE    "key22"            TO   MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC  =  ZERO
               MOVE    "tbl_sryacct"    TO   MCP-TABLE
               MOVE    "key22"          TO   MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL  (FLG-SRYACCT    =  1)
                         OR    (FLG-NYUIN-ARI2 =  1)
                 IF    ACCT-SRYKBN   =  "95"  OR  "96"
                   CONTINUE
                 ELSE
                   IF   (ACCT-ZAITEN    NOT =    ZERO)
                   AND  (ACCT-ZAIKAISU  NOT =    ZERO)
                   AND  (ACCT-JIHOKBN       =   SPACE)
                     MOVE      1        TO   FLG-NYUIN-ARI2
                   END-IF
                 END-IF
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key22"        TO   MCP-PATHNAME
                 PERFORM  800-SRYACCT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryacct"      TO   MCP-TABLE
             MOVE    "key22"            TO   MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           END-IF
      *
           IF   FLG-NYUIN-ARI2  =   1
             MOVE    "1"    TO    RECE41-NYUINFLG
           END-IF
      *
           .
       21021-NYUINBI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数等編集
      *****************************************************************
       2103-NISUU-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE40-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE40-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE40-PTID          TO      ACCT-PTID
           MOVE    RECE40-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE40-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE40-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL
                              FLG-SRYACCT        =    1
      *
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN    NOT = 1 AND 2)
                               MOVE    ZERO    TO      FLG-SHOSAISIN
                                                       FLG-SHOSAISIN2   
                                                       FLG-SHOSAISIN-GEN
                                                       FLG-KYUKYU
                                                       FLG-GAIRAIXX
                                                       FLG-GAIRAI26
                                                       FLG-GAIRAI52
                               MOVE    ZERO    TO      WRK-TEN
                                                       WRK-KIN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12"  OR
                                                    "13"  OR  "14"
                                   PERFORM   21031-SHOSAISIN-HANTEI-SEC
                               END-IF
                               IF   ACCT-SRYKBN  =  "11"  OR  "80"
                                   PERFORM   21032-KYUKYU-HANTEI-SEC
                               END-IF
                               IF   ACCT-SRYKBN  =  "40"  OR  "50"  OR
                                                    "60"  OR  "80"  OR
                                                    "54"  OR  "64"
                                   PERFORM   21033-GAIRAIXX-HANTEI-SEC
                               END-IF
                               IF   ACCT-SRYKBN  =  "12"   
                                   PERFORM   21034-GAIRAI-HANTEI-SEC
                               END-IF
                               PERFORM   21035-ENDATE-HANTEI-SEC 
                               IF    RECE41-HKNKBN   =   "4" OR "6"
                                 IF   ACCT-SRYKBN  =  "80"              
                                   PERFORM   21036-SINDAN-HANTEI-SEC          
                                 END-IF                                 
                               END-IF                                   
      *
                               IF    RECE41-HKNKBN   =   "4" OR "6"
                                PERFORM VARYING ID1 FROM 1  BY 1
                                           UNTIL    ID1     >    31
                                 IF  ACCT-DAY(1 ID1)   NOT =    ZERO
                                   MOVE  ACCT-DAY(1 ID1)  
                                               TO      WRK-ZAIKAISU-JI
                                   PERFORM   21037-SYUGITEN-HANTEI-SEC
                                 END-IF
                                END-PERFORM
                               END-IF
      *
                               PERFORM   21038-TENSOU-SEC
                               PERFORM   21039-TENSOU2-SEC
                   END-IF
      *
                   IF   (ACCT-ZAITEN        =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN    NOT = 1 AND 2)
                               MOVE    ZERO    TO      FLG-SHOSAISIN 
                                                       FLG-SHOSAISIN2   
                                                       FLG-SHOSAISIN-GEN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                   PERFORM   21031-SHOSAISIN-HANTEI-SEC
                               END-IF
                               IF    RECE41-HKNKBN   =   "4" OR "6"
                                 IF   ACCT-SRYKBN  =  "80"
                                   PERFORM   21036-SINDAN-HANTEI-SEC
                                 END-IF
                               END-IF
                               PERFORM   21038-TENSOU-SEC
                   END-IF
      *                 
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2103-NISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       21031-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           PERFORM      2110-DAY-CHECK-SEC
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)
      ****                            (SRY-SRYCD(IDY) =   SPACE)  OR
      ****                            (FLG-SHOSAISIN  =       1)
                       PERFORM   800-TENSU-TBL-SEC
      *
                       IF  (( TNS-JITUDAY    =     2          ) AND       
                            ( TNS-DAYCNT     =     1   OR   2 ))
                       OR  (  SRY-SRYCD(IDY) =   "112700850"
                                              OR "112003350"
                                              OR "099409901"
                                              OR "099999902"
                                              OR "099110001"
                                              OR "099120001"
      *                              同日再診
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112016550"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"
      *
                                              OR "111011810"
                                              OR "111012610"
                                              OR "113013910"
                                              OR "113014010"
                                              OR "113014110"
                                              OR "112015810"
                                              OR "112015950"
                                              OR "112016210"
                                              OR "112016410"
                                              OR "113015810"
                                              OR  CONS-ROUSAI18
                                              OR "112016850"
                                              OR "112016950"
                                              OR "112017010"
                                              OR "112017150"
                                              OR "112017450"
      *
                                              OR "113018410"
                                              OR "111012810"
                                              OR "112017610"
                                              OR "113024810"
                                              OR "113025710"
                                              OR "112023210"
                                              OR "112023450"
                                              OR "112023750"
                                              OR "112023550"
                                              OR "112023850"
                                              OR "113031310"
                                              OR "113031410"   )
                       OR  (( SRY-SRYCD(IDY) =  CONS-ROUSAI19 ) AND
                            ( RECE41-SRYYM  <=   "201306"     ))
                       OR  (( SRY-SRYCD(IDY) =  CONS-ROUSAI5
                                             OR CONS-ROUSAI35
                                             OR CONS-ROUSAI36
                                             OR CONS-ROUSAI37
                                             OR CONS-ROUSAI38 ) AND
                            ( RECE41-SRYYM  >=   "201307"     ))
      *
                          MOVE       1      TO    FLG-SHOSAISIN
      *
      **>                 IF  SRY-SRYCD(IDY) =  "099409901"
      **>                                    OR "099999902"             
      **>                    CONTINUE                                   
      **>                 ELSE                                          
                             MOVE    1      TO    FLG-SHOSAISIN2         
      **>                 END-IF                                        
      *
                       END-IF
                       IF    SRY-SRYCD(IDY)  =  "099409903"
                             MOVE    1      TO    FLG-SHOSAISIN-GEN
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM  800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21031-SHOSAISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    救急管理加算があるかの判定
      *****************************************************************
       21032-KYUKYU-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  OR
                                      (FLG-KYUKYU     =       1)
                     IF   RECE41-SRYYM    >=   "201307"
                       IF   SRY-SRYCD(IDY) =  CONS-ROUSAI32
                             MOVE    1      TO    FLG-KYUKYU
                       END-IF
                     ELSE
                       IF   SRY-SRYCD(IDY) =  CONS-ROUSAI3
                             MOVE    1      TO    FLG-KYUKYU
                       END-IF
                     END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21032-KYUKYU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    特例の外来管理加算を算定出来る剤であるかの判定
      *****************************************************************
       21033-GAIRAIXX-HANTEI-SEC                 SECTION.
      *
           PERFORM      2110-DAY-CHECK-SEC
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  OR
                                      (FLG-GAIRAIXX   =       1)
                       PERFORM   800-TENSU-TBL-SEC
                       IF      TNS-GAIKANRIKBN   =     1                
                             MOVE    1      TO    FLG-GAIRAIXX
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21033-GAIRAIXX-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    外来管理加算があるかの判定
      *****************************************************************
       21034-GAIRAI-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  OR
                                      (FLG-GAIRAI26   =       1)  OR
                                      (FLG-GAIRAI52   =       1)
                       IF ((SRY-SRYCD(IDY) =  CONS-GAIRAI1
                                          OR  CONS-GAIRAI2)  AND
                           (RECE41-SRYYM  <=  "201306"    ))
                       OR ((SRY-SRYCD(IDY) =  CONS-GAIRAI4)  AND
                           (RECE41-SRYYM  >=  "201307"    ))
                             MOVE    1      TO    FLG-GAIRAI52
                       END-IF
                       IF  (SRY-SRYCD(IDY) =  CONS-GAIRAI3)
                       AND (RECE41-SRYYM  <=  "201306"    )
                             MOVE    1      TO    FLG-GAIRAI26
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21034-GAIRAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    円建て項目等であるかの判定
      *****************************************************************
       21035-ENDATE-HANTEI-SEC                 SECTION.
      *
           PERFORM      2110-DAY-CHECK-SEC
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  
      *
                       PERFORM   800-TENSU-TBL-SEC
      *
                       IF ( SRY-SRYCD(IDY) =  CONS-ROUSAI1  OR
                                              CONS-ROUSAI18   )
                       OR ((ACCT-SRYKBN         =  "80"      )   AND
                           (SRY-SRYCD(IDY)(1:5) =  "09593"   ))
                       OR ((SRY-SRYCD(IDY) =  CONS-ROUSAI8   )   AND
                           (RECE41-SRYYM   <  "200804"       ))
                       OR ((SRY-SRYCD(IDY) =
                            CONS-ROUSAI3  OR CONS-ROUSAI4  OR
                            CONS-ROUSAI5  OR CONS-ROUSAI6  OR
                            CONS-ROUSAI7  OR CONS-ROUSAI19 OR
                            CONS-ROUSAI9  OR CONS-ROUSAI10 OR
                            CONS-ROUSAI11 OR CONS-ROUSAI12 OR
                            CONS-ROUSAI13 OR CONS-ROUSAI14 OR
                            CONS-ROUSAI15 OR CONS-ROUSAI16 OR
                            CONS-ROUSAI17                    )   AND
                           (RECE41-SRYYM  <=  "201306"       ))
                       OR ((SRY-SRYCD(IDY) =
                            CONS-ROUSAI32 OR CONS-ROUSAI33 OR
                            CONS-ROUSAI34 OR CONS-ROUSAI5  OR
                            CONS-ROUSAI20 OR CONS-ROUSAI21 OR
                            CONS-ROUSAI22 OR CONS-ROUSAI23 OR
                            CONS-ROUSAI24 OR CONS-ROUSAI25 OR
                            CONS-ROUSAI26 OR CONS-ROUSAI27 OR
                            CONS-ROUSAI28 OR CONS-ROUSAI29 OR
                            CONS-ROUSAI30 OR CONS-ROUSAI31 OR
                            CONS-ROUSAI35 OR CONS-ROUSAI36 OR
                            CONS-ROUSAI37 OR CONS-ROUSAI38 OR
                            CONS-ROUSAI39                    )   AND
                           (RECE41-SRYYM  >=  "201307"       ))
      *
                          IF  SRY-SRYSYUKBN = "809"
                            ADD  SRY-JIHIMONEY(IDY)  TO  WRK-KIN
                            COMPUTE  WRK-TEN  =  WRK-TEN   +
                                    (SRY-JIHIMONEY(IDY) / 10)
                          ELSE
                            ADD  TNS-TEN             TO  WRK-KIN
                            COMPUTE  WRK-TEN  =  WRK-TEN   +
                                    (TNS-TEN / 10)
                          END-IF
      *
                       END-IF
      *
                       IF   RECE41-SRYYM   >=  "201307"
                          EVALUATE  SRY-SRYCD(IDY)
                           WHEN    "101801010"  THRU  "101801020"
                            ADD  TNS-TEN             TO  WRK-KIN
                            COMPUTE  WRK-TEN  =  WRK-TEN   +
                                    (TNS-TEN / 10)
      *
                            COMPUTE WRK-TEN-KG  = TNS-TEN / 10
                            COMPUTE WRK-TEN-KG2 = TNS-TEN
                          END-EVALUATE
                          IF   SRY-SRYCD(IDY) = "101801030"
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
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21035-ENDATE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    自賠責の診断書料・明細書料であるかの判定
      *****************************************************************
       21036-SINDAN-HANTEI-SEC                 SECTION.
      *
           PERFORM      2110-DAY-CHECK-SEC
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                   PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  
      *
                       PERFORM   800-TENSU-TBL-SEC
      *
                       IF   SRY-SRYCD(IDY)(1:5) =  "09591"
      *
                          COMPUTE  WRK-SINDAN-TUU = WRK-SINDAN-TUU +
                                                    ACCT-ZAIKAISU
      ****
                          IF    WRK-TAXKBN      =   "2"
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-KEISAN  =
                               (SRY-JIHIMONEY(IDY) * WRK-S5) + WRK-S3
                            ELSE
                              COMPUTE  WRK-KEISAN  =
                               (TNS-TEN            * WRK-S5) + WRK-S3
                            END-IF
                            COMPUTE  WRK-KEISAN2     =  WRK-KEISAN
                                                     *  ACCT-ZAIKAISU
                            COMPUTE  WRK-SINDAN-KIN  =  WRK-SINDAN-KIN
                                                     +  WRK-KEISAN2
                          ELSE
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-SINDAN-KIN  =
                                       WRK-SINDAN-KIN  +
                                      (SRY-JIHIMONEY(IDY)
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
                       IF   SRY-SRYCD(IDY)(1:5) =  "09592"
      *
                          COMPUTE  WRK-MEISAI-TUU = WRK-MEISAI-TUU +
                                                    ACCT-ZAIKAISU
      ****
                          IF    WRK-TAXKBN      =   "2"
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-KEISAN  =
                               (SRY-JIHIMONEY(IDY) * WRK-S5) + WRK-S3
                            ELSE
                              COMPUTE  WRK-KEISAN  =
                               (TNS-TEN            * WRK-S5) + WRK-S3
                            END-IF
                            COMPUTE  WRK-KEISAN2     =  WRK-KEISAN
                                                     *  ACCT-ZAIKAISU
                            COMPUTE  WRK-MEISAI-KIN  =  WRK-MEISAI-KIN
                                                     +  WRK-KEISAN2
                          ELSE
                            IF  SRY-SRYSYUKBN = "809"
                              COMPUTE  WRK-MEISAI-KIN  =
                                       WRK-MEISAI-KIN  +
                                      (SRY-JIHIMONEY(IDY)
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
      *
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"     TO      MCP-TABLE
                   MOVE    "key2"           TO      MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21036-SINDAN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    日単位の手技点・薬剤点等をセット
      *****************************************************************
       21037-SYUGITEN-HANTEI-SEC                 SECTION.
      *
           PERFORM      2110-DAY-CHECK-SEC
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                 IF    SRY-RENNUM     =     1
      *
                   IF    ACCT-SRYKBN       =  "95"  OR  "96"       
                         CONTINUE
                   ELSE
                     IF        ACCT-SRYCDTOTAL    =    630010002
                                                  OR   630010005
                                                  OR   630010006
                                                  OR   630010007
                         COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                  TBL-YKZTEN-JI(ID1)  -
                                 ( WRK-ZAIKAISU-JI * ACCT-ZAITEN   )
                     ELSE 
                       IF   ACCT-SRYKBN  =  "70"
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN1
                                                  -  ACCT-SYUTEN2
                         COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                  TBL-SYUTEN-JI(ID1)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-SYUTEN2   +
                                                     ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                  TBL-YKZTEN-JI(ID1)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       ELSE
                         COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                  TBL-SYUTEN-JI(ID1)  +
                                 ( WRK-ZAIKAISU-JI * ACCT-SYUTEN1  )
                         COMPUTE  WRK-KEISAN-JI   =  ACCT-KIZAITEN  +
                                                     ACCT-YKZTEN
                         COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                  TBL-YKZTEN-JI(ID1)  +
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-JI )
                       END-IF
                     END-IF
                   END-IF
      *
                 END-IF
      *
                 IF    ACCT-SRYKBN       =  "95"  OR  "96"
                    CONTINUE
                 ELSE
                    PERFORM   VARYING   IDY   FROM   1   BY   1
                             UNTIL    (IDY            >       5)  OR
                                      (SRY-SRYCD(IDY) =   SPACE)  
      *
                       PERFORM   800-TENSU-TBL-SEC
      *
                       IF ( SRY-SRYCD(IDY) =  CONS-ROUSAI1  OR
                                              CONS-ROUSAI18    )
                       OR ((SRY-SRYCD(IDY) =  CONS-ROUSAI8    ) AND
                           (RECE41-SRYYM   <  "200804"        ))
                       OR ((SRY-SRYCD(IDY) =
                            CONS-ROUSAI3  OR CONS-ROUSAI4  OR
                            CONS-ROUSAI5  OR CONS-ROUSAI6  OR
                            CONS-ROUSAI7  OR CONS-ROUSAI19 OR
                            CONS-ROUSAI9  OR CONS-ROUSAI10 OR
                            CONS-ROUSAI11 OR CONS-ROUSAI12 OR
                            CONS-ROUSAI13 OR CONS-ROUSAI14 OR
                            CONS-ROUSAI15 OR CONS-ROUSAI16 OR
                            CONS-ROUSAI17                     ) AND
                           (RECE41-SRYYM  <=  "201306"        ))
                       OR ((SRY-SRYCD(IDY) =
                            CONS-ROUSAI32 OR CONS-ROUSAI33 OR
                            CONS-ROUSAI34 OR CONS-ROUSAI5  OR
                            CONS-ROUSAI20 OR CONS-ROUSAI21 OR
                            CONS-ROUSAI22 OR CONS-ROUSAI23 OR
                            CONS-ROUSAI24 OR CONS-ROUSAI25 OR
                            CONS-ROUSAI26 OR CONS-ROUSAI27 OR
                            CONS-ROUSAI28 OR CONS-ROUSAI29 OR
                            CONS-ROUSAI30 OR CONS-ROUSAI31 OR
                            CONS-ROUSAI35 OR CONS-ROUSAI36 OR
                            CONS-ROUSAI37 OR CONS-ROUSAI38 OR
                            CONS-ROUSAI39                     ) AND
                           (RECE41-SRYYM  >=  "201307"        ))
      *
                               COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                               COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                        TBL-SYUTEN-JI(ID1)  -
                                 ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
      *
                       END-IF
      *
                       IF   RECE41-SRYYM  >=  "201307"
                         EVALUATE  SRY-SRYCD(IDY)
                           WHEN    "101801010"  THRU  "101801020"
                               COMPUTE  WRK-TEN-JI   =  TNS-TEN / 10
                               COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                        TBL-SYUTEN-JI(ID1)  -
                                 ( WRK-ZAIKAISU-JI * WRK-TEN-JI )
      *
                               COMPUTE  WRK-TEN-KG   =  TNS-TEN / 10
                         END-EVALUATE
                         IF   SRY-SRYCD(IDY) = "101801030"
                               COMPUTE  WRK-FUTANWARI = TNS-TEN / 100
      *
                               COMPUTE  WRK-KEISAN-KG =
                                       (WRK-TEN-KG * WRK-FUTANWARI)
                                                   + 0.5
                               COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                        TBL-SYUTEN-JI(ID1)  -
                                 ( WRK-ZAIKAISU-JI * WRK-KEISAN-KG )
                         END-IF
                       END-IF
      *
                       IF  ( ACCT-SRYKBN         =  "80"    ) AND
                           ( SRY-SRYCD(IDY)(1:5) =  "09591" 
                                                 OR "09592" 
                                                 OR "09593" )           
                         IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-TEN-JI = SRY-JIHIMONEY(IDY) / 10
                         ELSE
                           COMPUTE WRK-TEN-JI = TNS-TEN / 10
                         END-IF
                         COMPUTE   TBL-SYUTEN-JI(ID1)  =
                                   TBL-SYUTEN-JI(ID1)  -
                                  (WRK-ZAIKAISU-JI * WRK-TEN-JI)
                       END-IF
      *
                       IF   SRY-SRYCD(IDY) =  CONS-YOUBUKASAN
                                           OR CONS-KYOBUKASAN
                                           OR CONS-KEIBUKASAN
                                           OR CONS-SOGAIKASAN
                         IF   WRK-KOTEIKBN  =  SPACE  OR  "2"
                            COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                     TBL-SYUTEN-JI(ID1)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                            COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                     TBL-YKZTEN-JI(ID1)  +
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                         END-IF
                       END-IF
      *
                       IF   SRY-SRYCD(IDY) =  CONS-RYOYOTEATE
                            COMPUTE  TBL-SYUTEN-JI(ID1)  =
                                     TBL-SYUTEN-JI(ID1)  -
                                    ( WRK-ZAIKAISU-JI * TNS-TEN )
                            IF ((RECE41-HKNKBN  =  "4") AND
                                (WRK-JIBAIKBN   =  "2"))
                            OR ( RECE41-HKNKBN  =  "6" )
                              COMPUTE  TBL-YKZTEN-JI(ID1)  =
                                       TBL-YKZTEN-JI(ID1)  +
                                      ( WRK-ZAIKAISU-JI * TNS-TEN )
                            END-IF
                       END-IF
      *
                    END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_sryact"     TO      MCP-TABLE
                 MOVE    "key2"           TO      MCP-PATHNAME
                 PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       21037-SYUGITEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       21038-TENSOU-SEC                 SECTION.
      *
      *    IF     RECE41-HKNNUM          NOT =   SPACE   
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   IDZ   FROM   1    BY   1
                                    UNTIL    IDZ     >    31
                           IF  ACCT-DAY(1 IDZ)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-JNISSU-DAY(IDZ)
                           END-IF
                    END-PERFORM
               END-IF
               IF   FLG-SHOSAISIN2       =       1                      
                    PERFORM    VARYING   IDZ   FROM   1    BY   1       
                                    UNTIL    IDZ     >    31            
                           IF  ACCT-DAY(1 IDZ)   NOT =    ZERO          
                               MOVE    "1"       TO                     
                                             TBL-JNISSU2-DAY(IDZ)       
                           END-IF                                       
                    END-PERFORM                                         
               END-IF                                                   
               IF   FLG-SHOSAISIN-GEN    =       1
                    PERFORM    VARYING   IDZ   FROM   1    BY   1
                                    UNTIL    IDZ     >    31
                           IF  ACCT-DAY(1 IDZ)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-JNISSU-DAY-GEN(IDZ)
                           END-IF
                    END-PERFORM
               END-IF
      *    END-IF
           .
       21038-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２
      *****************************************************************
       21039-TENSOU2-SEC                 SECTION.
      *
      *    IF     RECE41-HKNNUM          NOT =   SPACE   
               IF   FLG-KYUKYU           =       1
                    PERFORM    VARYING   IDZ   FROM   1    BY   1
                         UNTIL  ( IDZ                  >    31 )  OR
                                ( WRK-TAIHI-FLG        =     1 )
                           IF  ACCT-DAY(1 IDZ)   NOT =    ZERO
                               MOVE   RECE41-SRYYM    TO 
                                                 WRK-TAIHI-SRYYM
                               MOVE   IDZ             TO 
                                                 WRK-TAIHI-SRYD
                               MOVE   1               TO
                                                 WRK-TAIHI-FLG
                           END-IF
                    END-PERFORM
               END-IF
               IF   FLG-GAIRAIXX        =       1
                    PERFORM    VARYING   IDZ2   FROM   2    BY   1
                                UNTIL    IDZ2     >   10
                       COMPUTE    IDZ3  =   IDZ2  -   1
                       PERFORM    VARYING   IDZ   FROM   1    BY   1
                                   UNTIL    IDZ     >    31
                           IF  ACCT-DAY(IDZ2 IDZ)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-GXXNISSU-DAY(IDZ3 IDZ)
                           END-IF
                       END-PERFORM
                    END-PERFORM
               END-IF
               IF   FLG-GAIRAI26        =       1
                    PERFORM    VARYING   IDZ2   FROM   2    BY   1
                                UNTIL    IDZ2     >   10
                       COMPUTE    IDZ3  =   IDZ2  -   1
                       PERFORM    VARYING   IDZ   FROM   1    BY   1
                                   UNTIL    IDZ     >    31
                           IF  ACCT-DAY(IDZ2 IDZ)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-G26NISSU-DAY(IDZ3 IDZ)
                           END-IF
                       END-PERFORM
                    END-PERFORM
               END-IF
               IF   FLG-GAIRAI52        =       1
                    PERFORM    VARYING   IDZ2   FROM   2    BY   1
                                UNTIL    IDZ2     >   10
                       COMPUTE    IDZ3  =   IDZ2  -   1
                       PERFORM    VARYING   IDZ   FROM   1    BY   1
                                   UNTIL    IDZ     >    31
                           IF  ACCT-DAY(IDZ2 IDZ)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-G52NISSU-DAY(IDZ3 IDZ)
                           END-IF
                       END-PERFORM
                    END-PERFORM
               END-IF
               IF    RECE41-HKNKBN   =   "4" OR "6"
                   CONTINUE
               ELSE
                 IF        ACCT-SRYKBN        =    "95"   OR  "96"
                     CONTINUE
                 ELSE
                   IF        ACCT-SRYCDTOTAL    =    630010002
                                                OR   630010005
                                                OR   630010006
                                                OR   630010007
                     COMPUTE  RECE41-TOTALTEN =  RECE41-TOTALTEN   -
                                                 (ACCT-ZAITEN    *  
                                                  ACCT-ZAIKAISU)
                   ELSE
                     COMPUTE  RECE41-TOTALTEN =  RECE41-TOTALTEN   +
                                                 (ACCT-ZAITEN    *  
                                                  ACCT-ZAIKAISU)
                     IF  ( WRK-KIN  NOT =  ZERO )
                     OR  ( WRK-TEN  NOT =  ZERO )                       
                         COMPUTE  RECE41-TOTALTEN  =  RECE41-TOTALTEN  -
                                                     (WRK-TEN    *
                                                      ACCT-ZAIKAISU) 
      *
                         COMPUTE  RECE41-TOTALKIN2 =  RECE41-TOTALKIN2 +
                                                     (WRK-KIN    *
                                                      ACCT-ZAIKAISU)
                     END-IF 
                   END-IF
                 END-IF
               END-IF
      *    END-IF
           .
       21039-TENSOU2-EXT.
           EXIT. 
      *
      *****************************************************************
      *    実日数等の項目転送
      *****************************************************************
       2104-HENSYU-SET-SEC                    SECTION.
      *
           PERFORM    VARYING   IDZ2   FROM   1    BY   1
                       UNTIL    IDZ2     >    9
              PERFORM    VARYING    IDZ    FROM    1   BY   1
                          UNTIL     IDZ     >     31
                  IF   ( TBL-GXXNISSU-DAY(IDZ2 IDZ)
                                        NOT =  SPACE )
                  AND  ( TBL-G52NISSU-DAY(IDZ2 IDZ)
                                        NOT =  SPACE )
                       ADD    1      TO    RECE41-GAITOKU-KAISU1
                  END-IF
              END-PERFORM
           END-PERFORM
           IF    RECE41-GAITOKU-KAISU1  NOT  =  ZERO 
                  MOVE   "1"   TO     RECE41-GAITOKU-PRTKBN1
                  MOVE    52   TO     RECE41-GAITOKU-TEN1
           END-IF
      *
           PERFORM    VARYING   IDZ2   FROM   1    BY   1
                       UNTIL    IDZ2     >    9
              PERFORM    VARYING    IDZ    FROM    1   BY   1
                          UNTIL     IDZ     >     31
                  IF   ( TBL-GXXNISSU-DAY(IDZ2 IDZ)
                                        NOT =  SPACE )
                  AND  ( TBL-G26NISSU-DAY(IDZ2 IDZ)
                                        NOT =  SPACE )
                       ADD    1      TO    RECE41-GAITOKU-KAISU2
                  END-IF
              END-PERFORM
           END-PERFORM
           IF    RECE41-GAITOKU-KAISU2  NOT  =  ZERO 
                  MOVE   "1"   TO     RECE41-GAITOKU-PRTKBN2
                  MOVE    26   TO     RECE41-GAITOKU-TEN2
           END-IF
      *
      *    外来管理加算（読み替え加算）の算定チェック
      *    （パッチ適用前の診療行為入力分で、未訂正分があるか）
           IF   RECE41-SRYYM  >=  "201502"
             PERFORM    VARYING   IDZ2   FROM   1    BY   1
                         UNTIL    IDZ2     >    9
                PERFORM    VARYING    IDZ    FROM    1   BY   1
                            UNTIL     IDZ     >     31
                  IF   ( TBL-GXXNISSU-DAY(IDZ2 IDZ)
                                            =  SPACE )
                  AND  ( TBL-G52NISSU-DAY(IDZ2 IDZ)
                                        NOT =  SPACE )
                       MOVE    1     TO    RECE41-YOBIETC(1)
                       DISPLAY "<GAIKAN-CHECK> DAY=" IDZ
                  END-IF
                END-PERFORM
             END-PERFORM
           END-IF
      *
      *    実日数から減する
           PERFORM    VARYING    IDZ    FROM    1   BY   1
                          UNTIL      IDZ    >     31
                IF  (TBL-JNISSU-DAY(IDZ)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDZ)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(IDZ)
                END-IF
                IF  (TBL-JNISSU2-DAY(IDZ)     NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDZ)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU2-DAY(IDZ)
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDZ    FROM    1   BY   1
                          UNTIL      IDZ    >     31
                    IF    TBL-JNISSU-DAY(IDZ)
                                        NOT =  SPACE
                          ADD    1      TO    RECE41-JNISSU
                          MOVE  "1"     TO    RECE41-JNISSUTBL(IDZ)
                    END-IF
           END-PERFORM
      *
           IF  ((RECE41-HKNKBN   =   "4") AND
                (WRK-JIBAIKBN    =   "2"))
           OR  ((RECE41-HKNKBN   =   "5") AND
                (WRK-KOMUKBN     =   "2"))
           OR  ( RECE41-HKNKBN   =   "6" )
             CONTINUE
           ELSE
             IF       RECE41-JNISSU   =    ZERO  
               MOVE     999       TO       RECE41-JNISSU
             END-IF
           END-IF
      *
           IF     WRK-TAIHI-FLG       =   1
                MOVE   WRK-TAIHI-FLG       TO  RECE41-KYUKYU-PRTKBN
                MOVE   WRK-TAIHI-SRYYMD    TO  RECE41-KYUKYUYMD
           END-IF
      *
           PERFORM    VARYING    IDZ    FROM    1   BY   1              
                          UNTIL      IDZ    >     31                    
                    IF    TBL-JNISSU2-DAY(IDZ)                          
                                        NOT =  SPACE                    
                          MOVE  "1"     TO    RECE41-TUUIN-DAY(IDZ)     
                          ADD    1      TO    RECE41-TUUIN-KEI          
                    END-IF                                              
           END-PERFORM                                                  
      *
           IF     WRK-SINDAN-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-SINDAN-TUU      TO  RECE41-SINDAN-TUU        
      ***>      IF    WRK-TAXKBN      =   "2"                           
      ***>        COMPUTE  WRK-SINDAN-KIN  =
      ***>                (WRK-SINDAN-KIN  * WRK-S5) + WRK-S3
      ***>      END-IF                                                  
                MOVE   WRK-SINDAN-KIN      TO  RECE41-SINDAN-KIN        
           END-IF
      *
           IF     WRK-MEISAI-TUU      =   ZERO                          
                CONTINUE                                                
           ELSE                                                         
                MOVE   WRK-MEISAI-TUU      TO  RECE41-MEISAI-TUU        
      ***>      IF    WRK-TAXKBN      =   "2"                           
      ***>        COMPUTE  WRK-MEISAI-KIN  =
      ***>                (WRK-MEISAI-KIN  * WRK-S5) + WRK-S3
      ***>      END-IF                                                  
                MOVE   WRK-MEISAI-KIN      TO  RECE41-MEISAI-KIN
           END-IF                                                       
      *
           PERFORM    VARYING    IDZ    FROM    1   BY   1              
                          UNTIL      IDZ    >     31                    
      *              
                MOVE  TBL-SYUTEN-JI(IDZ)  TO  RECE41-SYUTEN-JI(IDZ)     
                MOVE  TBL-YKZTEN-JI(IDZ)  TO  RECE41-YKZTEN-JI(IDZ)     
      *
           END-PERFORM                                                  
      *
           .
       2104-HENSYU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数等の項目転送２
      *****************************************************************
       2105-HENSYU-SET2-SEC                    SECTION.
      *
           IF    RECE41-HKNKBN   =   "4" OR "6"
               CONTINUE                                                 
           ELSE                                                         
             IF    RECE41-HKNKBN   =   "5"
             AND   WRK-KOMUKBN     =   "2"
               COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                              WRK-KOMUTENTANKA
             ELSE
               IF   (RECE41-HKNKBN           =   "5" )
               AND  (WRK-KOMUKBN             =   "1" )
               AND  (WRK-KOMURSITANKAKBN     =   "2" )
               AND  (WRK-KOMURSITANKA-G  NOT =  SPACE)
                 COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                                WRK-KOMURSITANKA
               ELSE
                 COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                                WRK-TANKA
               END-IF
             END-IF
           END-IF                                                       
      *
           .
       2105-HENSYU-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数等編集２
      *****************************************************************
       2106-NISUU-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE40-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE40-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE40-PTID          TO      ACCT-PTID
           MOVE    RECE40-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE40-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE40-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL
                              FLG-SRYACCT        =    1
      *
                   MOVE   RECE40-TEKSTYMD(7:2)   TO   WRK-DAY
                   IF  ACCT-DAY(1 WRK-DAY)   NOT =    ZERO
                        MOVE     1      TO   FLG-DAY
                   ELSE
                        MOVE    ZERO    TO   FLG-DAY  
                   END-IF
      *
                   IF      FLG-DAY         =   1
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                                (ACCT-JIHOKBN       =   SPACE)    AND
                                (ACCT-ZAIKBN    NOT = 1 AND 2)
                               MOVE    ZERO    TO      FLG-SHOSAISIN
                                                       FLG-SHOSAISIN2
                                                       FLG-SHOSAISIN-GEN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12"  OR
                                                    "13"  OR  "14"
                                   PERFORM   21031-SHOSAISIN-HANTEI-SEC
                               END-IF
                               PERFORM   21062-TENSOU5-SEC
      *
                               MOVE  ACCT-DAY(1 WRK-DAY)  
                                               TO      WRK-ZAIKAISU
                               MOVE    ZERO    TO      FLG-KENSA
                               MOVE    ZERO    TO      WRK-TEN
                                                       WRK-KIN
                               IF   ACCT-SRYKBN  = "60" OR "64" OR "70"
                                   MOVE     1      TO     FLG-KENSA
                               END-IF
                               PERFORM   21035-ENDATE-HANTEI-SEC 
                               PERFORM   21061-TENSOU3-SEC
                           END-IF
      *
                           IF   (ACCT-ZAITEN        =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                                (ACCT-JIHOKBN       =   SPACE)    AND
                                (ACCT-ZAIKBN    NOT = 1 AND 2)
                               MOVE    ZERO    TO      FLG-SHOSAISIN
                                                       FLG-SHOSAISIN2
                                                       FLG-SHOSAISIN-GEN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                   PERFORM   21031-SHOSAISIN-HANTEI-SEC
                               END-IF
                               PERFORM   21062-TENSOU5-SEC
                           END-IF
                   END-IF
      *                 
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2106-NISUU-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送３
      *****************************************************************
       21061-TENSOU3-SEC                 SECTION.
      *
      *    IF     RECE41-HKNNUM          NOT =   SPACE   
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   CONTINUE
               ELSE
                 IF        ACCT-SRYCDTOTAL    =    630010002
                                              OR   630010005
                                              OR   630010006
                                              OR   630010007
                   COMPUTE  RECE41-TOTALTEN =  RECE41-TOTALTEN   -
                                               (ACCT-ZAITEN    *  
                                                WRK-ZAIKAISU)
                 ELSE
                   COMPUTE  RECE41-TOTALTEN =  RECE41-TOTALTEN   +
                                               (ACCT-ZAITEN    *  
                                                WRK-ZAIKAISU)
                   IF  ( WRK-KIN  NOT =  ZERO )
                   OR  ( WRK-TEN  NOT =  ZERO )                         
                       COMPUTE  RECE41-TOTALTEN  =  RECE41-TOTALTEN  -
                                                   (WRK-TEN    *
                                                    WRK-ZAIKAISU) 
      *
                       COMPUTE  RECE41-TOTALKIN2 =  RECE41-TOTALKIN2 +
                                                   (WRK-KIN    *
                                                    WRK-ZAIKAISU)
                   END-IF 
                 END-IF
               END-IF
               IF   FLG-KENSA    =    1
                      MOVE    "1"      TO      RECE41-KENSAKBN
               END-IF 
      *    END-IF
           .
       21061-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送５
      *****************************************************************
       21062-TENSOU5-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
              MOVE    "1"    TO   TBL-JNISSU-DAY(WRK-DAY)
           END-IF
           IF   FLG-SHOSAISIN2       =       1
              MOVE    "1"    TO   TBL-JNISSU2-DAY(WRK-DAY)
           END-IF
           IF   FLG-SHOSAISIN-GEN    =       1
              MOVE    "1"    TO   TBL-JNISSU-DAY-GEN(WRK-DAY)
           END-IF
           .
       21062-TENSOU5-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数等編集３
      *****************************************************************
       2107-NISUU-HENSYU3-SEC                SECTION.
      *
           PERFORM    21072-PTRSIINF-KENSAKU-SEC
      *
           PERFORM    VARYING    IDX5    FROM   1   BY   1
                       UNTIL    (IDX5                  >  100 )
                        OR      (TBL-HKNCOMBINUM(IDX5) =  ZERO)
             INITIALIZE                        SRYACCT-REC
             MOVE    RECE40-HOSPNUM        TO  ACCT-HOSPNUM
             MOVE    RECE40-NYUGAIKBN      TO  ACCT-NYUGAIKBN
             MOVE    RECE40-PTID           TO  ACCT-PTID
             MOVE    TBL-HKNCOMBINUM(IDX5) TO  ACCT-HKNCOMBI
             MOVE    SRYACCT-REC           TO  MCPDATA-REC
             MOVE    "tbl_sryacct"         TO  MCP-TABLE
             MOVE    "key69"               TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC  =  ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACCT  =  1
                 IF   (ACCT-ZAITEN    NOT =  ZERO   )
                 AND  (ACCT-ZAIKAISU  NOT =  ZERO   )
                 AND  (ACCT-JIHOKBN       =  SPACE  )
                 AND  (ACCT-ZAIKBN    NOT =  1 AND 2)
                   MOVE      ZERO          TO  FLG-KENSA
                   IF   ACCT-SRYKBN  = "60" OR "64" OR "70"
                     MOVE    1             TO  FLG-KENSA
                   END-IF
                   PERFORM   21071-TENSOU4-SEC
                 END-IF
                 MOVE    "tbl_sryacct"     TO  MCP-TABLE
                 MOVE    "key69"           TO  MCP-PATHNAME
                 PERFORM  800-SRYACCT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryacct"         TO  MCP-TABLE
             MOVE    "key69"               TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           END-PERFORM
      *
           .
       2107-NISUU-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送４
      *****************************************************************
       21071-TENSOU4-SEC                 SECTION.
      *
      *    IF     RECE41-HKNNUM          NOT =   SPACE   
               IF   FLG-KENSA    =    1
                    PERFORM    VARYING   IDZ   FROM   1    BY   1
                                    UNTIL    IDZ     >    31
                      IF  ACCT-DAY(1 IDZ)   NOT =    ZERO
                         MOVE   ACCT-SRYYM   TO   WRK-KENSAYYMM
                         MOVE   IDZ          TO   WRK-KENSADD
                         IF  ( WRK-KENSAYMD  <  RECE40-TEKSTYMD )
                           IF  ( WRK-KENSAYMD  >  WRK-KENSAYMD-ZEN )
                               MOVE  WRK-KENSAYMD TO
                                             WRK-KENSAYMD-ZEN
                           END-IF
                         END-IF
                      END-IF
                    END-PERFORM
               END-IF
      *    END-IF
           .
       21071-TENSOU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    アフターケア検索
      *****************************************************************
       21072-PTRSIINF-KENSAKU-SEC                 SECTION.
      *
           INITIALIZE                    PTRSIINF-REC
           MOVE    RECE40-HOSPNUM    TO  PTRSI-HOSPNUM
           MOVE    RECE40-PTID       TO  PTRSI-PTID
           MOVE    "3"               TO  PTRSI-HKNKBN
           MOVE    PTRSIINF-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
           MOVE    "key2"            TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF   MCP-RC   =   ZERO
             MOVE  "tbl_ptrsiinf"    TO  MCP-TABLE
             MOVE  "key2"            TO  MCP-PATHNAME
             PERFORM  800-PTRSIINF-READ-SEC
             PERFORM  UNTIL  FLG-PTRSIINF  =  1
               IF ((PTRSI-KOFUNUM         =  RECE41-KOFUNUM      ) AND
                   (PTRSI-SAKUJOKBN       =  SPACE               ))
               OR ((PTRSI-KOFUNUM(1:13)  IS  NUMERIC             ) AND
                   (RECE41-KOFUNUM(1:13) IS  NUMERIC             ) AND
                   (PTRSI-KOFUNUM(1:10)   =  RECE41-KOFUNUM(1:10)) AND
                   (PTRSI-KOFUNUM(11:3)   <  RECE41-KOFUNUM(11:3)) AND
                   (PTRSI-SAKUJOKBN       =  SPACE               ))
               OR ((PTRSI-KOFUNUM(1:10)  IS  NUMERIC             ) AND
                   (PTRSI-KOFUNUM(11:3)   =  SPACE               ) AND
                   (RECE41-KOFUNUM(1:13) IS  NUMERIC             ) AND
                   (PTRSI-KOFUNUM(1:10)   =  RECE41-KOFUNUM(1:10)) AND
                   (PTRSI-SAKUJOKBN       =  SPACE               ))
                 INITIALIZE                     HKNCOMBI-REC
                 MOVE     RECE40-HOSPNUM    TO  COMB-HOSPNUM
                 MOVE     RECE40-PTID       TO  COMB-PTID
                 MOVE     PTRSI-HKNID       TO  COMB-HKNID
                 MOVE     HKNCOMBI-REC      TO  MCPDATA-REC
                 PERFORM  900-HKNCOMBI-READ-SEC
                 IF       FLG-HKNCOMBI  =   ZERO
                   ADD    1                 TO  IDX5
                   MOVE   COMB-HKNCOMBINUM  TO  TBL-HKNCOMBINUM(IDX5)
                 END-IF
               END-IF
               MOVE  "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE  "key2"          TO  MCP-PATHNAME
               PERFORM  800-PTRSIINF-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
           MOVE    "key2"            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       21072-PTRSIINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数等の項目転送３
      *****************************************************************
       2108-HENSYU-SET3-SEC                    SECTION.
      *
           IF     WRK-KENSAYMD-ZEN    NOT  =    ZERO
                MOVE   WRK-KENSAYMD-ZEN    TO  RECE41-KENSAYMD-ZEN
           END-IF
      *
      *    実日数から減する
           PERFORM    VARYING    IDZ    FROM    1   BY   1
                       UNTIL     IDZ     >     31
                IF  (TBL-JNISSU-DAY(IDZ)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDZ)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(IDZ)
                END-IF
                IF  (TBL-JNISSU2-DAY(IDZ)     NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(IDZ)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU2-DAY(IDZ)
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDZ    FROM    1   BY   1
                       UNTIL     IDZ     >     31
                IF   TBL-JNISSU-DAY(IDZ)      NOT =  SPACE
                    ADD    1     TO  RECE41-JNISSU
      ***           MOVE  "1"    TO  RECE41-JNISSUTBL(IDZ)
                END-IF
           END-PERFORM
      *
           .
       2108-HENSYU-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴チェック
      *****************************************************************
       2109-JYURRK-CHECK-SEC              SECTION.
      *
      *    労災準拠の場合、受診歴に４、５のデータがない事
      *    健保準拠の場合、受診歴に１、２のデータがない事
           EVALUATE  RECE41-HKNKBN
             WHEN      "4"
               EVALUATE  WRK-JIBAIKBN
                 WHEN    "1"
                   MOVE   "4"   TO   WRK-HKNKBN
                 WHEN    "2"
                   MOVE   "2"   TO   WRK-HKNKBN
               END-EVALUATE
             WHEN      "5"
               EVALUATE  WRK-KOMUKBN
                 WHEN    "1"
                   MOVE   "5"   TO   WRK-HKNKBN
                 WHEN    "2"
                   MOVE   "1"   TO   WRK-HKNKBN
               END-EVALUATE
           END-EVALUATE
      *    受診歴の検索
           INITIALIZE                      JYURRK-REC
           MOVE    RECE41-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    RECE41-PTID         TO  JYURRK-PTID
           MOVE    RECE41-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    RECE41-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "__"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE41-HKNCOMBI     TO  JYURRK-HKNCOMBINUM
           MOVE    WRK-HKNKBN          TO  JYURRK-HKNKBN
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key39"         TO  MCP-PATHNAME
               PERFORM   800-JYURRK-READ-SEC
               IF        FLG-JYURRK    =   ZERO
                   MOVE      1         TO   FLG-JYURRKCHECK
               END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2109-JYURRK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日チェック
      *****************************************************************
       2110-DAY-CHECK-SEC              SECTION.
      *
           MOVE     ZERO           TO    WRK-TAIHI-DAY
           IF      RECE41-HKNKBN   NOT  =   "3"
             PERFORM   VARYING   IDX8   FROM    1    BY   1
                        UNTIL    IDX8     >    31
                IF   ACCT-DAY(1 IDX8)  NOT =   ZERO
                  MOVE     IDX8    TO    WRK-TAIHI-DAY
                  MOVE     31      TO    IDX8
                END-IF
             END-PERFORM
           ELSE
             MOVE     WRK-DAY      TO    WRK-TAIHI-DAY
           END-IF
           IF   WRK-TAIHI-DAY  =  ZERO
             MOVE          01      TO    WRK-TAIHI-DAY
           END-IF
      *
           .
       2110-DAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       220-DATA-WRITE-SEC                SECTION.
      *
           WRITE   RECE41-REC
      *
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       220-DATA-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE40-FILE
                   RECE41-FILE
      *
           IF      CNT-OUT           =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE40  CNT-IN  :" CNT-IN 
           DISPLAY "RECE41  CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0410 END ***"
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
      *            ジョブ管理開始処理
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
           STRING  "ORCR0410 "             DELIMITED   BY  SIZE
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
      *    患者情報読込
      *****************************************************************
       800-PTINF-READ-SEC           SECTION.
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
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災患者保険情報読込
      *****************************************************************
       800-PTRSIINF-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTRSIINF-REC
               MOVE    ZERO                TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           .
       800-PTRSIINF-READ-EXT.
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
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       800-KYUSEIRRK-READ-SEC           SECTION.
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
       800-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       800-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       800-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       800-SRYACT-READ-SEC           SECTION.
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
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       800-TENSU-TBL-SEC                    SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
grpsys     MOVE    SRY-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDY)       TO      TNS-SRYCD
           MOVE    ACCT-SRYYM           TO      TNS-YUKOSTYMD(1:6)
           MOVE    WRK-TAIHI-DAY        TO      TNS-YUKOSTYMD(7:2)
           MOVE    ACCT-SRYYM           TO      TNS-YUKOEDYMD(1:6)
           MOVE    WRK-TAIHI-DAY        TO      TNS-YUKOEDYMD(7:2)
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   MCPDATA-REC   TO  TNS-TENSU-REC
                   MOVE   ZERO          TO  FLG-TENSU
               ELSE
                   INITIALIZE               TNS-TENSU-REC
                   MOVE    1            TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                   TNS-TENSU-REC
               MOVE    1                TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込
      *****************************************************************
       800-PTNYUINRRK-READ-SEC    SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       800-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計読込
      *****************************************************************
       800-NYUINACCT-READ-SEC    SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           .
       800-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       800-JYURRK-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       800-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE40-READ-SEC            SECTION.
      *
           READ    RECE40-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
                   DISPLAY "RECE40  PTID=" RECE40-PTID
                           " SRYYM="       RECE40-SRYYM
                           " JIBAIKBN="    WRK-JIBAIKBN
      *
           END-READ
           .
       900-RECE40-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
              MOVE    WRK-4001-JIBAIKBN  TO  WRK-JIBAIKBN
           ELSE
      *       医療機関ＩＤ編集
              MOVE  ZERO           TO  FLG-SET
              MOVE  RECE40-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
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
      *
                STRING "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        RECE40-SRYYM       DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                END-STRING
      *
                PERFORM 500-ERR-HENSYU-SEC
                MOVE    1               TO  FLG-END
                GO  TO  910-SYSKANRI-HENSYU-EXT
              END-IF
      *
      *       労災自賠医療機関情報
              MOVE  ZERO           TO  FLG-SET
              MOVE  RECE40-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
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
                   EVALUATE  SYS-4001-TENKBN
                       WHEN  "1"
                           MOVE     11.5    TO    WRK-TANKA
                       WHEN  "2"
                           MOVE     12.0    TO    WRK-TANKA
                   END-EVALUATE
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
                   MOVE   SYS-4001-TAXKBN   TO    WRK-TAXKBN
                   MOVE   SYS-4001-KOTEIKBN TO    WRK-KOTEIKBN
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
                      IF     SYS-4001-SYUTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-SYUTENTANKA
                      ELSE
                          MOVE SYS-4001-SYUTENTANKA
                                                 TO    WRK-SYUTENTANKA
                      END-IF
                      IF     SYS-4001-YKZTENTANKA-X  =  SPACE
                          MOVE ZERO              TO    WRK-YKZTENTANKA
                      ELSE
                          MOVE SYS-4001-YKZTENTANKA
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
                   IF     SYS-4001-KYUGYO-PRTKBN   =  SPACE
                     MOVE "1"               TO    WRK-KYUGYO-PRTKBN
                   ELSE
                     MOVE SYS-4001-KYUGYO-PRTKBN
                                            TO    WRK-KYUGYO-PRTKBN
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
                END-IF
      *
              END-PERFORM
              IF     FLG-SET  =  ZERO
                STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        RECE40-SRYYM       DELIMITED  BY  SIZE
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
      *    保険組合せ読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC           =   ZERO
                 MOVE    ZERO          TO  FLG-HKNCOMBI
                 MOVE    MCPDATA-REC   TO  HKNCOMBI-REC
                 IF    COMB-DELKBN      =   "1"
                   MOVE    1           TO  FLG-HKNCOMBI
                 END-IF
               ELSE
                 MOVE      1           TO  FLG-HKNCOMBI
                 INITIALIZE                HKNCOMBI-REC
               END-IF
           ELSE
               MOVE        1           TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
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
               STRING  "ORCR0410 select err TABLE="
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
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
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

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
      ************************************************************
      *         システム管理マスタ     (CPSK2005.INC)            *
      *         （２００５                                       *
      *         レセプト編集区分                                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/02/26  レセ電区分、レセ電出力先の追加
      *  01.00.02    MCC-藤原     02/03/28  国保ＯＣＲ区分、国保診療報酬請求書
      *                                     区分の追加
      *  01.00.03    MCC-藤原     02/04/03  県単用項目編集プログラム名の追加
      *  01.00.04    MCC-藤原     02/05/13  病名編集区分１（診療科別）の追加
      *  01.00.05    MCC-藤原     02/06/04  転帰日編集区分の追加
      *  01.00.06    MCC-藤原     02/07/04  出力先プリンタ情報の追加
      *  01.00.07    NACL-藤原    03/01/08  通し番号編集区分の追加
      *  01.00.08    NACL-藤原    03/04/09  続紙区分（入院）の追加
      *  01.00.09    NACL-藤原    03/07/02  主病名編集フラグの追加
      *  01.00.10    NACL-藤原    03/08/21  続紙区分（入院外）・続紙出力先
      *                                     の追加
      *  01.00.11    NACL-太田    03/11/18  老人と原爆区分追加
      *  01.00.12    NACL-藤原    04/02/18  社保ＯＣＲ区分の追加
      *  01.00.13    NACL-藤原    04/04/09  レセプトコメント（症状詳記）記録区分
      *                                     ・診療科編集区分の追加
      *  01.00.14    NACL-藤原    04/07/02  レセプト入力順印刷区分・
      *                                     自動遷移区分の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *                                     「特別療養費」文字記載位置
      *                                     給付割合欄記載区分
      *                                     レセプト種別欄記載区分
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *  
      *  02.09.01    NACL-藤原    06/05/12  改定通信医１６０４９号
      *                                     レセ電日計表レコード記録条件
      *                                     の追加
      *  3.0.0
      *  03.00.01    NACL-藤原    06/05/15  入退院履歴の記載区分の追加
      *
      *  03.02.01    NACL-藤原    06/07/11  労災・自賠責のプリンタ指定の追加
      *  03.02.02    NACL-藤原    06/07/24  マル長の別総括対応
      *  03.02.03    NACL-藤原    06/07/26  自賠責の枠印刷選択機能対応
      *
      *  03.04.01    NACL-藤原    07/01/16  レセプト一括作成出力順区分
      *                                     （全体）の追加
      *
      *  03.05.01    NACL-藤原    07/06/25  自賠責の様式選択機能
      *                                     （平成１９年４月改正様式）
      *                                     入院レセの病棟番号記載区分
      *                                     印刷処理後のチェック解除の有無
      *                                     の追加 
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      ************************************************************
       01  SYS-2005-REC.
           03  SYS-2005-KEY.
               05  SYS-2005-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2005-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2005-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2005-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2005-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-2005-TBL.
      *病名編集区分
               05  SYS-2005-BYOMEIHENKBN
                                        PIC  X(01).
      *国保レセ電出力区分
               05  SYS-2005-KKHRECEKBN  PIC  X(01).
      *社保レセ電出力区分
               05  SYS-2005-SYARECEKBN  PIC  X(01).
      *レセ電データ出力先
               05  SYS-2005-RECEDENOUT  PIC  X(30).
      *国保ＯＣＲ出力区分
               05  SYS-2005-KKHOCRKBN   PIC  X(01).
      *国保診療報酬請求書区分
               05  SYS-2005-KKHPRTKBN   PIC  X(01).
      *県単用プログラム名
               05  SYS-2005-PREFPRGID-G.
                   07  SYS-2005-PREFPRGID    OCCURS  10
                                        PIC  X(15).
      *病名編集区分１
               05  SYS-2005-BYOMEIHENKBN1
                                        PIC  X(01).
      *転帰日編集区分
               05  SYS-2005-TENKIHENKBN PIC  X(01).
      *社保出力先プリンタ名
               05  SYS-2005-SYAPRTNM    PIC  X(16).
      *国保出力先プリンタ名
               05  SYS-2005-KKHPRTNM    PIC  X(16).
      *通し番号編集区分
               05  SYS-2005-RENNUMHENKBN
                                        PIC  X(01).
      *続紙区分（入院）
               05  SYS-2005-NEXTPRTKBN  PIC  X(01).
      *主病名編集フラグ
               05  SYS-2005-SYUBYOHENFLG
                                        PIC  X(01).
      *続紙区分（入院外）
               05  SYS-2005-NEXTPRTKBN-G
                                        PIC  X(01).
      *社保出力先プリンタ名（続紙）
               05  SYS-2005-SYAPRTNM-NEXT
                                        PIC  X(16).
      *国保出力先プリンタ名（続紙）
               05  SYS-2005-KKHPRTNM-NEXT
                                        PIC  X(16).
      *老人と原爆区分
               05  SYS-2005-RJNGNBKKBN  PIC  X(01).
      *自費レセ摘要欄記載区分
               05  SYS-2005-JIHITEKKSIKBN
                                        PIC  X(01).
      *社保ＯＣＲ出力区分
               05  SYS-2005-SYAOCRKBN   PIC  X(01).
      *診療科編集区分
               05  SYS-2005-SRYKAHENKBN PIC  X(01).
      *レセプトコメント（症状詳記）記録区分
               05  SYS-2005-RECECOMKBN  PIC  X(01).
      *社保出力先プリンタ名（入院）
               05  SYS-2005-SYAPRTNM-N  PIC  X(16).
      *国保出力先プリンタ名（入院）
               05  SYS-2005-KKHPRTNM-N  PIC  X(16).
      *社保出力先プリンタ名（続紙）（入院）
               05  SYS-2005-SYAPRTNM-NEXT-N
                                        PIC  X(16).
      *国保出力先プリンタ名（続紙）（入院）
               05  SYS-2005-KKHPRTNM-NEXT-N
                                        PIC  X(16).
      *自動遷移区分
               05  SYS-2005-AUTODSPKBN  PIC  X(01).
      *レセプト一括作成出力順区分（社保）
               05  SYS-2005-PRTJYUNKBN1 PIC  X(02).
      *レセプト個別作成出力順区分
               05  SYS-2005-PRTJYUNKBN2 PIC  X(02).
      *「特別療養費」文字記載位置
               05  SYS-2005-MOJI-KISAIKBN
                                        PIC  X(01).
      *給付割合欄記載区分
               05  SYS-2005-KYURATE-KISAIKBN
                                        PIC  X(01).
      *レセプト種別欄記載区分
               05  SYS-2005-RECESYUBETU-KISAIKBN
                                        PIC  X(01).
      *レセプト一括作成出力順区分（国保）
               05  SYS-2005-PRTJYUNKBN3 PIC  X(02).
      *レセ電日計表レコード記録条件
               05  SYS-2005-NI-JYOKEN-AREA.  
                   07  SYS-2005-NI-JYOKEN
                                        PIC  9(08)   OCCURS  4.
      *入退院履歴記載区分（入院外レセプトのみ）
               05  SYS-2005-NYUTAIINREKI-KSIKBN
                                        PIC  X(01).
      *労災出力先プリンタ名
               05  SYS-2005-ROUPRTNM    PIC  X(16).
      *労災出力先プリンタ名（続紙）
               05  SYS-2005-ROUPRTNM-NEXT
                                        PIC  X(16).
      *労災出力先プリンタ名（入院）
               05  SYS-2005-ROUPRTNM-N  PIC  X(16).
      *労災出力先プリンタ名（続紙）（入院）
               05  SYS-2005-ROUPRTNM-NEXT-N
                                        PIC  X(16).
      *自賠責出力先プリンタ名
               05  SYS-2005-JBAIPRTNM   PIC  X(16).
      *自賠責出力先プリンタ名（続紙）
               05  SYS-2005-JBAIPRTNM-NEXT
                                        PIC  X(16).
      *自賠責出力先プリンタ名（入院）
               05  SYS-2005-JBAIPRTNM-N PIC  X(16).
      *自賠責出力先プリンタ名（続紙）（入院）
               05  SYS-2005-JBAIPRTNM-NEXT-N
                                        PIC  X(16).
      *枠記載区分（自賠責・入院外）
               05  SYS-2005-FRAME-JIBAIPRTKBN
                                        PIC  X(01).
      *枠記載区分（自賠責・入院）
               05  SYS-2005-FRAME-JIBAIPRTKBN-N
                                        PIC  X(01).
      *診療報酬請求書マル長分作成区分（社保）
               05  SYS-2005-MARUCHO-SYAPRTKBN
                                        PIC  X(01).
      *レセプト一括作成出力順区分（全体）
               05  SYS-2005-PRTJYUNKBN4 PIC  X(02).
      *端末ＩＤ
           03  SYS-2005-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-2005-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2005-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2005-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2005-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2005-UPD-KEY.
               05  SYS-2005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2005-UPD-KANRICD     PIC X(04).
               05  SYS-2005-UPD-KBNCD       PIC X(08).
               05  SYS-2005-UPD-STYUKYMD    PIC X(08).
               05  SYS-2005-UPD-EDYUKYMD    PIC X(08).

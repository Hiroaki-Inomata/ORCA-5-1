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
      *****************************************************************
      *
      *    ＤＢサブ   処理領域     CPMCPDBPRO.INC
      *
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/11  PTINF-KEY4 追加
      *  01.00.02    MCC-多々納   01/05/21  INPUTCD-KEY3 追加
      *  01.00.03    MCC-多々納   01/06/04  TENSU-KEY41
      *                                     TENSU-KEY42 
      *                                     TENSU-KEY51 
      *                                     TENSU-KEY52 
      *                                     TENSU-KEY53  追加
      *  01.00.04    MCC-藤原     01/05/22  PTHKNINF-KEY3 追加
      *  01.00.05    MCC-藤原     01/06/15  PostgreSQL 対応  
      *  01.00.06    MCC-多々納   01/08/07  TENSU-KEY DELETE 追加
      *  01.00.07    MCC-藤原     01/08/07  PTKOHINF-KEY3      
      *  01.00.08    MCC-藤原     01/08/08  SEIKYU-KEY UPHKNJANUM 追加      
      *                                     RECEPRT-KEY  追加      
      *  01.00.09    MCC-多々納   01/08/20  PTINF-KEY5   追加
      *  01.00.10    MCC-藤原     01/08/21  SEIKYU-KEY UPHKNJANUM 削除      
      *  01.00.11    MCC-藤原     01/09/03  HKNJAINF-KEY 追加      
      *  01.00.12    MCC-藤原     01/09/04  HKNNUM-KEY3  追加      
      *  01.00.13    MCC-藤原     01/09/05  HKNNUM-KEY4  追加 
      *                                     HKNNUM-KEY5  追加
      *                                     BD001-KEY    追加
      *                                     BD002-KEY    追加      
      *  01.00.14    MCC-藤原     01/09/11  SYSKANRI-KEYの修正
      *  01.00.15    ひさなが     01/09/12  PTBYOMEI-KEY4
      *                                     CHK-KEY5
      *                                     SRYACT-KEY6 追加
      *  01.00.16    MCC-小豆沢   01/09/17  JYURRK-UPD1  追加
      *  01.00.17    MCC-藤原     01/10/01  RECEPRT-KEY2 追加
      *  01.00.18    ひさなが     01/09/15  全てのテーブルに ALL 条件追加
      *  01.00.19    MCC-多々納   01/11/12  患者ＩＤによる削除を対象の
      *                                     テーブルすべてに追加
      *  01.00.20    MCC-藤原     01/11/15  BYOMEI-KEYのかな検索のとき
      *                                     半角のときのチェック修正 
      *  01.00.21    MCC-多々納   01/11/15  請求管理の患者ＩＤによる削除
      *                                     を追加
      *  01.00.22    MCC-多々納   01/11/29  受診履歴のKEY8 を追加
      *  01.00.23    MCC-藤原     01/12/05  TENSU-KEY10K,KEY10S,KEY11K,
      *                                     KEY11Sを追加
      *  01.00.24    MCC-藤原     01/12/06  自院病名マスタの追加
      *  01.00.25    MCC-藤原     01/12/26  BYOMEI-KEY4,KEY5の追加
      *  01.00.26    MCC-多々納   01/12/27  受診履歴のKEY9 を追加
      *  01.00.27    MCC-多々納   02/01/10  JYURRK-KEY10,JYURRK-KEY11
      *                                     追加
      *  01.00.28    MCC-多々納   02/01/30  WKSRYACT-KEY4,KEY5   追加
      *  01.00.29    MCC-藤原     02/01/30  RECEPRT-KEY3,KEY4の追加
      *  01.00.30    MCC-多々納   02/02/05  JYURRK-KEY12追加
      *  01.00.31    MCC-藤原     02/02/06  レセプトコメントマスタの追加
      *                                     雛形マスタの追加
      *  01.00.32    MCC-多々納   02/02/06  RECECOM-KEY2 追加
      *                                     EXAMPLE-KEY2 追加
      *  01.00.33    MCC-多々納   02/02/07  JYURRK-KEY13 追加
      *  01.00.34    MCC-藤原     02/02/08  KYUSEIRRK-KEY3 追加
      *  01.00.35    MCC-藤原     02/02/08  KYUSEIRRK-UPD1 へ変更
      *  01.00.36    MCC-藤原     02/02/12  TENSU-KEY10K〜KEY12K1の追加、
      *                                     変更
      *  01.00.37    MCC-多々納   02/02/15  RECECOM-DEL1 追加
      *  01.00.38    MCC-藤原     02/02/15  PTKOHINF-KEY4の追加
      *  01.00.39    MCC-多々納   02/02/22  DBKANRI-KEYの追加
      *  01.00.40    MCC-藤原     02/02/25  RECEPRT-KEY5,KEY6の追加
      *  01.00.41    MCC-藤原     02/02/26  RECEKANRI-KEYの追加
      *  01.00.42    MCC-小豆沢   02/03/04  SRYACT-KEY7の追加
      *  01.00.43    MCC-藤原     02/03/05  SEIKYU-KEY5の追加
      *  01.00.44    MCC-多々納   02/03/08  SANTEI-KEY4の追加
      *  01.00.45    MCC-藤原     02/03/13  PTHKNINF-KEY4の追加
      *  01.00.46    MCC-伊藤     02/03/14  CHK-UPD1の追加
      *  01.00.47    MCC-藤原     02/03/15  SEIKYU-KEY6,KEY7の追加
      *  01.00.48    MCC-多々納   02/03/17  JYURRK-KEY14の追加
      *  01.00.49    ひさなが     02/03/19  PTBYOMEI-KEY5の追加
      *  01.00.50    MCC-藤原     02/03/22  HKNJAINF-KEY2の追加
      *  01.00.51    MCC-藤原     02/04/02  SYSKANRI-KEY6の追加
      *  01.00.52    MCC-藤原     02/04/04  USERBYOMEI-KEY6の追加
      *  01.00.53    MCC-多々納   02/04/04  ADRS-KEY3,KEY4,KEY5の追加
      *  01.00.54    MCC-小豆沢   02/04/08  PTRSIINF-KEY,KEY2,ALL,DEL1の追加
      *  01.00.55    MCC-藤原     02/04/08  USERBYOMEI-KEY7,KEY8の追加
      *  01.00.56    MCC-藤原     02/04/10  SRYACCT-KEY8,KEY9の追加
      *  01.00.57    MCC-多々納   02/04/10  JYURRK-KEY15,KEY16の追加
      *  01.00.58    MCC-多々納   02/04/18  INPUTSET-KEY3の追加
      *  01.00.59    MCC-多々納   02/04/23  INPUTSET-KEY4の追加
      *  01.00.60    MCC-多々納   02/04/23  CHK-KEY6の追加
      *  01.00.61    MCC-小豆沢   02/04/25  SRYACT-KEY8の追加
      *  01.00.62    MCC-藤原     02/04/30  RECEPRT-KEY7,KEY8の追加
      *                                     RECEKANRI-KEY3の追加
      *  01.00.63    MCC-太田     02/05/15  USERBYOMEI-KEY9の追加
      *  01.00.64    MCC-藤原     02/05/21  SEIKYU-DEL2,DEL3,DEL4の追加
      *  01.00.65    ひさなが     02/05/22  JOBKANRI-ALL02の追加
      *  01.00.66    MCC-多々納   02/05/23  SANTEI-KEY5の追加
      *  01.00.67    MCC-多々納   02/05/27  SSKIJYO,INTERACTの追加
      *  01.00.68    MCC-多々納   02/05/29  SRYACCT-KEY10の追加
      *  01.00.69    MCC-太田     02/05/31  JINMEIの追加
      *  01.00.70    MCC-多々納   02/06/06  CHK-KEYの更新・削除の修正
      *  01.00.71    MCC-多々納   02/06/17  MONTHLYNUMの追加
      *  01.00.72    MCC-森脇     02/06/19  JINMEI-KEY3の追加
      *  01.00.73    MCC-藤原     02/07/03  RECEPRT-KEY9の追加
      *  01.00.74    MCC-藤原     02/07/11  PTKOHINF-KEY5の追加
      *  01.00.75    NACL-森脇    02/07/19  YAKUJYO-KEY,ALLの追加
      *  01.00.76    NACL-多々納  02/07/31  PARA-DEL2,PARA-KEY3 の追加
      *  01.00.77    NACL-多々納  02/08/06  PARA-KEY のUPDATE修正
      *  01.00.78    NACL-多々納  02/08/08  JYURRK-KEY17,KEY-18追加
      *  01.00.79    NACL-太田    02/08/09  SYSKANRI-KEY7追加
      *  01.00.80    NACL-太田    02/08/19  SYSKANRI-KEY8追加
      *  01.00.81    NACL-藤原    02/08/19  RECECOM-KEY3追加
      *  01.00.82    NACL-多々納  02/08/20  TENSU-KEY42K,KEY-42JK8追加
      *  01.00.83    NACL-太田    02/08/22  SYSKANRI-KEY9,KEY10追加
      *  01.00.84    NACL-藤原    02/08/27  BD002-KEY2追加
      *  01.00.85    NACL-太田    02/09/03  TENSU-KEY13,14追加
      *  01.00.86    NACL-多々納  02/09/12  HKNNUM-KEY6,KEY7  追加
      *  01.00.87    NACL-太田    02/09/19  PTBYOMEI-KEY6追加
      *  01.00.88    NACL-多々納  02/09/25  SRYACCT-KEY11
      *                                     SANTEI-KEY6
      *  01.00.89    NACL-藤原    02/09/30  RECEKANRI-KEY4〜KEY7
      *                                     RECEPRT-KEY10〜KEY17
      *  01.00.90    NACL-藤原    02/10/09  SEIKYU-KEY8〜KEY13
      *  01.00.91    NACL-藤原    02/10/17  SEIKYU-KEY14
      *                                     RECEPRT-KEY10〜KEY17
      *  01.01.01    NACL-太田    02/08/05  NYUINKHN-KEY,ALLの追加
      *  01.01.02    NACL-太田    02/09/02  NYUINKHN-KEY2の追加
      *  01.01.03    NACL-太田    02/09/10  PTNYUINRRK-KEY,KEY2,KEY3追加
      *  01.01.04    NACL-太田    02/09/13  PTNYUINRRK-DEL1追加
      *  01.01.04    NACL-太田    02/09/18  PTNYUINRRK-KEY4追加
      *  01.01.05    NACL-太田    02/09/19  PTNYUINRRK-KEY5追加
      *  01.01.06    NACL-太田    02/09/24  PTTAINFUKA-KEY,ALL追加
      *  01.01.07    NACL-太田    02/09/13  PTTAINFUKA-DEL1追加
      *  01.01.08    NACL-小豆沢  02/10/07  NYUINACCT追加
      *                                     NYUINACT追加
      *  01.01.09    NACL-太田    02/10/16  PTNYUINRRK-KEY6,7,8追加
      *  01.01.10    NACL-太田    02/10/21  I001-KEY,ALL追加
      *  01.01.11    NACL-太田    02/10/23  PTTAINFUKA-DEL2追加
      *                                     PTNYUINRRK-DEL2追加
      *  01.01.12    NACL-太田    02/10/25  JYURRK-KEY19追加
      *  01.01.13    NACL-太田    02/10/30  JYURRK-KEY19追加
      *  01.01.14    NACL-太田    02/10/31  PTNYUINRRK-KEY9追加
      *              NACL-太田    02/11/01  SYUNOU-DEL2追加
      *  01.01.15    NACL-太田    02/11/18  NYUKSNCHK-KEY追加
      *  01.01.16    NACL-太田    02/11/27  TENSU-KEY15追加
      *  01.01.17    NACL-太田    02/12/10  SYUNOU-DEL2削除
      *                                     SYUNOU-KEY5追加
      *                                     SYUMEI-KEY3追加
      *                                     PTNYUINRRK-KEY10追加
      *  01.01.18    NACL-藤原    02/12/10  SEIKYU-DEL5,DEL6追加
      *  01.01.19    NACL-太田    02/12/13  PTNYUINRRK-KEY11追加
      *  01.01.20    NACL-太田    02/12/13  SYUNOU-KEY6追加
      *  01.01.21    NACL-藤原    02/12/17  PTNYUINRRK-KEY12,KEY13追加
      *  01.01.22    NACL-多々納  02/12/18  PTINF-KEY6,KEY7,KEY8,KEY9
      *  01.01.23    NACL-太田    02/12/18  NYUINRYAKUSHO-KEY,ALL追加
      *                                     SRYACT-KEY9追加
      *                                     NYUINACT-KEY9追加
      *                                     PTNYUINRRK-KEY14追加
      *                                     SYUNOU-DEL2,3,4,5追加
      *  01.01.24    NACL-太田    02/12/19  TEIKIKANRI-KEY,ALL追加
      *                                     TEIKIKANRI-DEL1追加
      *                                     TEIKIPRT-KEY,2,3,4,5,ALL追加
      *                                     TEIKIPRT-DEL1追加
      *  01.01.25    NACL-太田    02/12/20  SYUNOU-KEY7,8追加
      *                                     SYUMEI-DEL2追加
      *                                     SYUNOU-DEL6追加  
      *  01.01.26    NACL-太田    02/12/20  BD002-KEY3追加
      *  01.01.27    NACL-藤原    02/12/24  NYUINACCT-KEY13,KEY14追加
      *  01.01.28    NACL-森脇    03/01/06  SYUNOU-KEY9追加
      *  01.01.29    NACL-多々納  03/01/08  SRYACCT-KEY14 追加
      *  01.01.30    NACL-太田    03/01/17  TEIKIRRK-KEY,ALL追加        
      *                                     PTTEIKIRRK-KEY〜KEY3,ALL追加
      *  01.01.31    NACL-太田    03/01/20  PTTEIKIRRK-KEY4追加
      *                                     PTTEIKIRRK-DEL1,DEL2追加
      *  01.01.32    NACL-太田    03/01/23  SYUNOU-DEL4,5,6削除
      *  01.01.32    NACL-太田    03/01/23  SYUNOU-KEY10追加
      *  01.01.32    NACL-太田    03/01/29  PTNYUINRRK-KEY15追加
      *  01.01.33    ひさなが     03/01/30  NYUINACCT-KEY15,16の追加
      *  01.01.34    NACL-太田    03/01/31  TEIKIRRK-KEY2追加
      *  01.01.35    NACL-太田    03/02/03  SRYACCT-KEY15追加
      *  01.01.36    NACL-多々納  03/02/06  NYUINACCT-KEY17追加
      *  01.01.37    NACL-太田    03/02/07  PTNYUINRRK-KEY16,KEY17追加
      *  01.01.38    NACL-森脇    03/02/10  SRYACCT-KEY16追加
      *  01.01.39    NACL-多々納  03/02/17  SRYACCT-KEY17追加
      *  01.01.39    NACL-太田    03/02/22  PTNYUINRRK-ALL追加
      *  01.01.40    ひさなが     03/02/25  PTNYUINRRK-KEY18の追加
      *  01.01.41    NACL-藤原    03/02/28  KOHSKY-KEY,KEY2,KEY3,KEY4,
      *                                     DEL1,DEL2,DEL3追加
      *                                     SEIKYU-KEY15追加 
      *                                     RECEKANRI-DEL1,DEL2追加 
      *                                     RECEPRT-DEL1,DEL2追加 
      *  01.01.42    NACL-伊藤    03/03/03  KENSASORT-KEY追加
      *                                     KENSASORT-ALL追加 
      *  01.01.43    NACL-多々納　03/03/04  SYUNOU-KEY11,KEY12追加
      *  01.01.44    ひさなが     03/03/05  PTNYUINRRK-KEY19の追加
      *  01.01.45    NACL-森脇    03/03/05  SYUMEI-KEY4,KEY5の追加
      *  01.01.46    NACL-太田    03/03/10  SENTEICDCHG-KEY,2,3,ALL追加
      *  01.01.47    NACL-伊藤    03/03/11  INTERACT-KEY3の追加
      *                                     INTERACT-UPD1,DEL1追加 
      *  01.01.48    NACL-藤原    03/03/14  PTBYO-KEY7追加
      *  01.01.49    NACL-藤原    03/03/16  KOHSKY-KEY5,KEY6追加
      *                                     SEIKYU-KEY16,KEY17,KEY18追加
      *  01.01.50    NACL-太田    03/03/19  PTKOHFTN-KEY,KEY2,KEY3,DEL1
      *                                     UPD1,ALL追加
      *  01.01.51    NACL-多々納　03/03/19  PTHKNINF-KEY5追加
      *  01.01.52    NACL-太田    03/03/20  PTKOHFTN-KEY4追加
      *  01.01.53    NACL-太田    03/03/20  PTKOHFTN-KEY5追加
      *                                     PTKOHFTN-UPD1削除
      *  01.01.54    NACL-小豆沢  03/04/04  SRYACCT-KEY18追加
      *                                     SRYACCT-KEY19追加
      *  01.01.55    NACL-藤原    03/04/09  SEIKYU-KEY19追加
      *  01.01.56    NACL-藤原    03/04/14  SEIKYU-KEY20追加
      *  01.01.57    NACL-多々納　03/04/17  SYUNOU-KEY13追加
      *  01.01.58    ひさなが     03/04/17  NYUINACCT-KEY18の追加
      *  01.01.59    NACL-多々納　03/04/18  JYURRK-KEY20追加
      *  01.01.60    NACL-藤原    03/04/09  SEIKYU-KEY21,JEY22追加
      *  01.01.61    NACL-太田    03/04/22  SYUNOU-KEY14追加
      *  01.01.62    NACL-太田    03/04/28  SYUMEI-KEY6追加
      *  01.01.63    NACL-太田    03/04/28  PTTEIKIRRK-KEY5追加
      *  01.01.64    NACL-太田    03/04/28  SYUNOU-KEY15追加
      *  01.01.65    NACL-太田    03/04/30  SYUNOU-KEY16,17追加
      *  01.01.66    NACL-多々納　03/05/01  JYURRK-KEY21追加
      *  01.01.67    NACL-多々納　03/05/01  JYURRK-KEY22追加
      *  01.01.68    NACL-多々納　03/05/06  PTBYOMEI-KEY8追加
      *                                     JYURRK-KEY23追加
      *  01.01.69    NACL-太田    03/05/06  SYUNOU-KEY18追加
      *  01.01.70    NACL-太田    03/05/09  I001-KEY2追加
      *  01.01.71    NACL-太田    03/05/15  I001-KEY3追加
      *  01.01.72    NACL-太田    03/05/17  NYUINACCT-KEY19追加
      *  01.01.73    ひさなが     03/05/19  NYUKSNCHK-ALLの追加
      *  01.01.74    ひさなが     03/05/21  ALL2の追加
      *  01.01.75    NACL-藤原    03/05/21  SEIKYU-KEY4,KEY11,KEY12,KEY13,
      *                                     KEY14,KEY21,KEY22のとき保険者番号
      *                                     に初期値を編集
      *  01.01.76    NACL-小豆沢　03/05/28  JYURRK-KEY24追加
      *  01.01.77    NACL-太田    03/06/05  PTNYUINRRK-KEY20追加
      *  01.01.78    NACL-多々納　03/06/16  TENSU-KEY20追加
      *  01.01.79    NACL-小豆沢　03/06/20  BYOMEI-KEY6追加
      *  01.01.80    NACL-多々納　03/06/24  TENSU-KEY21追加
      *                                     PTNUM-KEY3 追加
      *  01.01.81    NACL-多々納　03/07/10  PTINF-KEY10 追加
      *  01.01.82    NACL-多々納　03/07/15  YYK-KEY7   追加 
      *  01.01.83    NACL-多々納　03/07/17  PTBYOMEI-KEY9,KEY10 追加 
      *  01.01.84    NACL-多々納　03/07/25  SANTEI-KEY7,KEY8追加
      *                                     TENSU-KEY22追加
      *  01.01.85    NACL-太田  　03/07/24  PTBYOMEI-KEY11追加 
      *  01.01.86    NACL-藤原    03/07/25  PTNYUINRRK-KEY21,KEY22,KEY23
      *                                     追加
      *  01.01.87    NACL-太田  　03/07/25  SRYACCT-KEY20追加 
      *  01.01.88    NACL-太田  　03/07/31  SRYACCT-KEY21追加 
      *  01.01.89    NACL-門脇  　03/08/01  SRYACCT-KEY22追加 
      *                                     SYUNOU-KEY19追加
      *                                     PTBYOMEI-KEY12追加
      *  01.01.90    NACL-藤原    03/08/06  RECEDENの追加
      *  01.01.91    NACL-多々納　03/08/11  CHK-KEY7追加 
      *  01.01.92    NACL-太田  　03/08/12  SRYACCT-KEY23追加 
      *  01.01.93    NACL-藤原    03/08/18  SEIKYU-KEY23,KEY24追加
      *  01.01.94    NACL-森脇    03/08/25  TENSU-KEY23S,KEY23M,KEY23K
      *  01.01.95    NACL-太田    03/08/26  PTNYUINRRK-KEY24追加
      *  01.01.96    NACL-太田    03/08/26  PTNYUINRRK-KEY25追加
      *  01.01.97    NACL-小豆沢　03/08/29  PTHKNINF-KEY6追加
      *  01.01.98    NACL-小豆沢  03/08/29  KOHPLUSの追加
      *  01.01.99    NACL-藤原    03/08/29  RECEPRT-KEY18〜KEY29追加
      *  01.01.100   NACL-太田    03/08/29  KOHPLUS-UPD1の追加
      *  01.01.101   NACL-太田    03/09/02  PTNYUINRRK-KEY26追加
      *  01.01.102   NACL-太田    03/09/08  PTCOM-KEY2追加
      *  01.01.103   NACL-門脇    03/09/24  SRYACCT-KEY24追加
      *                                     SRYACCT-KEY25追加
      *  01.01.104   NACL-太田    03/09/30  SYUNOU-KEY20追加
      *  01.01.105   NACL-藤原    03/10/03  PTBYOMEI-KEY13追加
      *  01.01.106   NACL-藤原    03/10/07  PTBYOMEI-KEYKEY14,KEY15追加
      *  01.01.107   NACL-森脇    03/10/09  BD003-KEY,KEY2,KEY3追加
      *  01.01.108   NACL-太田    03/10/10  I001-KEY4追加
      *  01.01.109   NACL-藤原    03/10/16  SYUNOU-KEY21追加
      *  01.01.110   NACL-多々納　03/11/11  RECECOM-KEY4
      *                                     PTKOHINF-KEY6追加 
      *  01.01.111   NACL-太田  　03/11/19  PTNYUINRRK-KEY27,KEY28追加
      *  01.01.112   NACL-森脇  　03/11/27  SRYACCT-KEY26追加
      *  01.01.113   NACL-太田  　03/12/10  SRYACCT-KEY27,KEY28追加
      *  01.01.114   NACL-太田  　03/12/10  NYUINACCT-KEY20追加
      *  01.01.115   NACL-太田  　03/12/11  PTBYOMEI-KEY16,KEY17追加
      *  01.01.116   NACL-門脇  　04/01/06  SYUDAY-KEY,DEL1追加
      *  01.01.117   NACL-小豆沢  04/01/14  SYUNOU-KEY22追加
      *                                     SYUNOU-KEY23追加
      *  01.01.118   NACL-門脇  　04/01/14  SRYACCT-KEY29,30,31,32,33追加
      *  01.01.119   NACL-太田    04/01/19  SYUNOU-KEY24追加
      *  01.01.120   NACL-太田    04/01/23  PTNYUINRRK-KEY29追加
      *  01.01.121   NACL-藤原    04/01/30  USERBYOMEI-DEL1,DEL2追加
      *  01.01.122   NACL-太田    04/02/05  JYURRK-KEY25追加
      *  01.01.123   NACL-森脇    04/02/05  PGKANRI-KEY,KEY2,ALL追加
      *  01.01.124   NACL-小豆沢  04/02/07  SYUNOU-KEY25追加
      *  01.01.125   NACL-藤原    04/02/24  PTBYOMEI-KEY20追加
      *  01.01.126   NACL-多々納　04/02/24  TENSYU-KEY90,KEY91,KEY92追加
      *  01.01.127   NACL-小豆沢  04/02/24  SYSKANRI-KEY11追加
      *  01.01.128   NACL-藤原    04/02/25  PTBYOMEI-KEY21,KEY22,KEY23追加
      *  01.01.129   NACL-太田    04/03/01  CHK-KEY8,CHK-KEY9,CHK-KEY10追加
      *  01.01.130   NACL-太田    04/03/02  TENSU-KEY24追加
      *  01.01.131   NACL-多々納　04/03/02  TENSUPLUS-KEY2追加
      *  01.01.132   NACL-太田    04/03/05  NYUINACCT-KEY21追加
      *  01.01.133   NACL-藤原    04/03/11  SRYACCT-KEY34,KEY35追加
      *                                     tbl_opecom.db 追加
      *  01.01.134   NACL-藤原    04/03/26  OPECOM-KEY4,KEY5追加
      *  01.01.135   NACL-太田    04/03/26  CHK-KEY11追加
      *  01.01.136   NACL-多々納  04/04/02  SRYACCT-DEL2
      *                                     SRYACT-DEL3
      *                                     RECEDEN-DEL3
      *                                     OPECOM-DEL2
      *                                     SYUDAY-DEL2 追加
      *  01.01.137   NACL-太田    04/03/29  PTTEIKIRRK-KEY6追加
      *  01.01.138   NACL-太田    04/04/08  SYUMEI-KEY7追加
      *  01.01.139   NACL-太田    04/05/06  PTNYUINRRK-KEY30,KEY31追加
      *  01.01.140   NACL-太田    04/05/17  PTTEIKIRRK-KEY7追加
      *  01.01.141   NACL-小豆沢  04/05/28  DBOPEN,DBDISCONNECT削除
      *  01.01.142   NACL-太田    04/05/28  PTNYUINRRK-KEY32追加
      *  01.01.143   NACL-藤原    04/06/26  RECEPRT-KEY30追加
      *  01.01.144   NACL-藤原    04/07/07  PTKOHINF-KEY7追加
      *  01.01.145   NACL-小豆沢  04/07/23  NYUINACCT-KEY22追加
      *  01.01.146   NACL-小豆沢  04/07/26  NYUINACCT-KEY23追加
      *  01.01.147   NACL-小豆沢  04/08/05  HKNCOMBI-KEY4追加
      *  01.01.148   NACL-太田    04/08/05  SRYKARRK-KEY3追加
      *  01.01.149   NACL-多々納  04/08/06  JYURRK-KEY26追加
      *                                     SYUTOTAL-KEY追加
      *                                     SYUTOTAL-DEL1追加
      *                                     SYUNOU-KEY26,KEY27,KEY28追加
      *  01.01.150   NACL-多々納  04/09/07  PTNYUINRRK-KEY33,KEY34追加
      *  01.01.151   NACL-森脇    04/09/08  ADRS-KEY6,KEY7追加
      *  01.01.152   NACL-門脇    04/09/08  SYUNOU-KEY29,KEY30追加
      *  01.01.153   NACL-藤原    04/09/10  SYSKANRI-KEY12追加
      *  01.01.154   NACL-多々納  04/10/12  WKSRYACT-KEY6,KEY7追加
      *  01.01.155   NACL-藤原    04/11/10  KOHSKY-KEY7追加
      *  01.01.156   NACL-多々納  04/11/19  PTBYOMEI-KEY24追加
      *  01.01.157   NACL-門脇    04/11/30  SYUNOU-KEY33追加
      *  01.01.158   NACL-小豆沢  04/12/10  NYUINACCT-KEY26追加
      *  01.01.159   NACL-多々納  04/12/20  TENSU-KEY93追加
      *  01.01.160   NACL-門脇    04/12/22  SYUNOU-KEY35追加
      *  01.01.161   NACL-多々納  05/01/26  PTBYOMEI-KEY25追加
      *                                     HELP  追加
      *  01.01.162   NACL-森脇    05/01/28  SRYACT-KEY11追加
      *  01.01.163   NACL-門脇    05/01/29  SYUNOU-DEL4,5追加
      *                                     SYUNOU-KEY39追加
      *                                     SYUDAY-DEL3,4追加
      *  01.01.164   NACL-門脇    05/02/09  SYUNOU-KEY40追加
      *                                     SYUDAY-DEL5追加
      *  01.01.165   NACL-門脇    05/02/10  HKNCOMBI-KEY5追加
      *                                     SYUNOU-KEY41追加
      *  01.01.166   NACL-太田    05/02/18  PTTEIKIRRK-DEL3追加
      *  01.01.167   NACL-多々納  05/03/09  SRYACCT-KEY39追加
      *  01.01.168   NACL-多々納  05/04/15  SRYACCT-KEY40追加
      *  01.01.169   NACL-門脇    05/05/10  SRYACT-KEY12追加
      *  01.01.170   NACL-太田    05/06/02  BTPARA-ALL追加
      *                                     CHKSND-ALL追加
      *                                     JYURRK-PRV-ALL3追加
      *                                     MEDYAKKO-ALL追加
      *                                     ORDER-ALL追加
      *                                     ORDERPRT-ALL追加
      *                                     PRVKANRI-ALL追加
      *                                     PTCOM-PRV-ALL追加
      *                                     RECEUPD-ALL追加
      *                                     SANTEI-PRV-ALL追加
      *                                     SRYACCT-PRV-ALL追加
      *                                     SRYACCTPLUS-ALL追加
      *                                     SRYACT-PRV-ALL追加
      *                                     SRYKARRK-PRV-ALL追加
      *                                     SYUDAY-ALL追加
      *                                     SYUMEI-PRV-ALL追加
      *                                     SYUNOU-PRV-ALL追加
      *                                     SYUTOTAL-PRV-ALL追加
      *                                     TOUKEIMEMO-ALL追加
      *                                     Q002-ALL追加
      *                                     BD003-ALL追加
      *  01.01.171   NACL-太田    05/09/30  HOURS-ALL追加
      *  01.01.172   NACL-小豆沢  05/12/12  NYUINACCT-KEY28追加
      *  01.01.173   NACL-太田    05/12/21  MEDYAKKO-ALL削除
      *  01.01.174   NACL-太田    06/01/19  PTTEIKIRRK-DEL2,DEL3を削除
      *  01.01.175   NACL-太田    06/02/15  TEKIOUBYOMEI-ALLを削除
      *  01.01.176   NACL-太田    06/05/22  MONFUNC対応
      *****************************************************************
      *
       PROCEDURE                   DIVISION
           USING   MCPAREA
                   ORCMCPAREA
                   MCPDATA-REC.
      *****************************************************************
       000-ENTRY               SECTION.
      *
           EVALUATE    MCP-FUNC
             WHEN     'DBOPEN'
             WHEN     'DBDISCONNECT'
             WHEN     'DBSTART'
             WHEN     'DBCOMMIT'
               MOVE    LOW-VALUE       TO  MCP-TABLE
                                           MCP-PATHNAME
             WHEN     'PUTWINDOW'
               CONTINUE
             WHEN      OTHER
               PERFORM 200-SQL
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           END-EVALUATE.
      *
           EVALUATE    MCP-FUNC
             WHEN     'DBCLOSE'
                 CONTINUE
             WHEN      OTHER
                 CALL  "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-EVALUATE.
      *
           EXIT    PROGRAM.
      *****************************************************************
       200-SQL                 SECTION.
      *---- (01.00.05) LINE ADD START --------------------------------
           EVALUATE    MCP-FUNC        ALSO    ORC-DBPATH
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY"
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY2"
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY3"
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY4"
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY5"
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-KEY6"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *********    PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    "DBUPDATE"      ALSO    "SYSKANRI-UPD1"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
      *********    PERFORM 2002-SEL-SYSKANRI-SEC
               WHEN    "DBDELETE"      ALSO    "SYSKANRI-KEY"
               WHEN    "DBDELETE"      ALSO    "SYSKANRI-KEY3"
               WHEN    "DBDELETE"      ALSO    "SYSKANRI-KEY4"
               WHEN    "DBDELETE"      ALSO    "SYSKANRI-KEY5"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
      *********    PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    "DBDELETE"      ALSO    "SYSKANRI-KEY2"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "del11 " TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY"
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY2"
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY3"
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY4"
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY5"
               WHEN    "DBUPDATE"      ALSO   "BYOMEI-KEY6"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY2"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY3"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY4"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY5"
               WHEN    "DBDELETE"      ALSO   "BYOMEI-KEY6"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "CHK-KEY"
               WHEN    "DBUPDATE"      ALSO   "CHK-KEY2"
               WHEN    "DBUPDATE"      ALSO   "CHK-KEY3"
               WHEN    "DBUPDATE"      ALSO   "CHK-KEY4"
               WHEN    "DBDELETE"      ALSO   "CHK-KEY"
               WHEN    "DBDELETE"      ALSO   "CHK-KEY2"
               WHEN    "DBDELETE"      ALSO   "CHK-KEY3"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *************PERFORM 2001-SEL-CHK-SEC
               WHEN    "DBUPDATE"      ALSO   "CHK-UPD1"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "CHK-KEY4"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "INPUTCD-KEY"
               WHEN    "DBUPDATE"      ALSO   "INPUTCD-KEY2"
               WHEN    "DBUPDATE"      ALSO   "INPUTCD-KEY3"
               WHEN    "DBUPDATE"      ALSO   "INPUTCD-KEY4"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      **********   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    "DBDELETE"      ALSO   "INPUTCD-KEY"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    "DBDELETE"      ALSO   "INPUTCD-KEY2"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "del12" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    "DBDELETE"      ALSO   "INPUTCD-KEY3"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "del21" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    "DBDELETE"      ALSO   "INPUTCD-KEY4"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "del22" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    "DBUPDATE"      ALSO   "HKNCOMBI-KEY"
               WHEN    "DBUPDATE"      ALSO   "HKNCOMBI-KEY2"
               WHEN    "DBUPDATE"      ALSO   "HKNCOMBI-KEY3"
               WHEN    "DBUPDATE"      ALSO   "HKNCOMBI-KEY4"
               WHEN    "DBDELETE"      ALSO   "HKNCOMBI-KEY"
               WHEN    "DBDELETE"      ALSO   "HKNCOMBI-KEY2"
               WHEN    "DBDELETE"      ALSO   "HKNCOMBI-KEY3"
               WHEN    "DBDELETE"      ALSO   "HKNCOMBI-KEY4"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "HKNCOMBI-DEL1"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY2"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY3"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY4"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY5"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY6"
               WHEN    "DBUPDATE"      ALSO   "HKNNUM-KEY7"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY2"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY3"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY4"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY5"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY6"
               WHEN    "DBDELETE"      ALSO   "HKNNUM-KEY7"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "INPUTSET-KEY"
               WHEN    "DBUPDATE"      ALSO   "INPUTSET-KEY2"
               WHEN    "DBUPDATE"      ALSO   "INPUTSET-KEY3"
               WHEN    "DBUPDATE"      ALSO   "INPUTSET-KEY4"
               WHEN    "DBDELETE"      ALSO   "INPUTSET-KEY"
               WHEN    "DBDELETE"      ALSO   "INPUTSET-KEY3"
               WHEN    "DBDELETE"      ALSO   "INPUTSET-KEY4"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "INPUTSET-KEY2"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY"
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY2"
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY3"
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY4"
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY5"
      *----(01.01.151)-ADD-START---
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY6"
               WHEN    "DBUPDATE"      ALSO   "ADRS-KEY7"
      *----(01.01.151)-ADD-END-----
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY"
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY2"
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY3"
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY4"
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY5"
      *----(01.01.151)-ADD-START---
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY6"
               WHEN    "DBDELETE"      ALSO   "ADRS-KEY7"
      *----(01.01.151)-ADD-END-----
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY2"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY3"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY4"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY5"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY6"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY8"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY9"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY10"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY11"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY14"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY15"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY16"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY17"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY18"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY19"
               WHEN    "DBUPDATE"      ALSO   "JYURRK-KEY20"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY2"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY3"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY4"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY5"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY6"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY7"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY8"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY9"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY10"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY11"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY14"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY15"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY16"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY17"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY18"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY19"
               WHEN    "DBDELETE"      ALSO   "JYURRK-KEY20"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "JYURRK-UPD1"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "JYURRK-DEL1"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "SRYKARRK-KEY"
               WHEN    "DBUPDATE"      ALSO   "SRYKARRK-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SRYKARRK-KEY3"
               WHEN    "DBDELETE"      ALSO   "SRYKARRK-KEY"
               WHEN    "DBDELETE"      ALSO   "SRYKARRK-KEY2"
               WHEN    "DBDELETE"      ALSO   "SRYKARRK-KEY3"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYKARRK-DEL1"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "PTBYOMEI-KEY"
               WHEN    "DBUPDATE"      ALSO   "PTBYOMEI-KEY2"
               WHEN    "DBUPDATE"      ALSO   "PTBYOMEI-KEY3"
               WHEN    "DBDELETE"      ALSO   "PTBYOMEI-KEY"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTBYOMEI-KEY2"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "del11 " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTBYOMEI-KEY3"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "del21" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "PTCOM-KEY"
               WHEN    "DBDELETE"      ALSO   "PTCOM-KEY"
               WHEN    "DBUPDATE"      ALSO   "PTCOM-KEY2"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTCOM-KEY2"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "del11 " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTCOM-DEL2"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "del12 " TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY2"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY3"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY4"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY5"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY6"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY7"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY8"
               WHEN    "DBUPDATE"      ALSO    "PTINF-KEY9"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY2"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY3"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY4"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY5"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY6"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY7"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY8"
               WHEN    "DBDELETE"      ALSO    "PTINF-KEY9"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO    "YYK-KEY"
               WHEN    "DBUPDATE"      ALSO    "YYK-KEY2"
               WHEN    "DBUPDATE"      ALSO    "YYK-KEY3"
               WHEN    "DBUPDATE"      ALSO    "YYK-KEY4"
               WHEN    "DBUPDATE"      ALSO    "YYK-KEY5"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY2"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY3"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY4"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY5"
               WHEN    "DBDELETE"      ALSO    "YYK-KEY6"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO    "PTNUM-KEY"
               WHEN    "DBUPDATE"      ALSO    "PTNUM-KEY2"
               WHEN    "DBUPDATE"      ALSO    "PTNUM-KEY3"
               WHEN    "DBDELETE"      ALSO    "PTNUM-KEY"
               WHEN    "DBDELETE"      ALSO    "PTNUM-KEY2"
               WHEN    "DBDELETE"      ALSO    "PTNUM-KEY3"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO    "PTNUM-DEL1"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      ************ PERFORM 2001-SEL-PTNUM-SEC
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY"
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY2"
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY3"
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY4"
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY5"
               WHEN    "DBUPDATE"      ALSO   "PTHKNINF-KEY6"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY2"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY3"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY4"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY5"
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-KEY6"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTHKNINF-DEL1"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "PTKOHINF-KEY"
               WHEN    "DBUPDATE"      ALSO   "PTKOHINF-KEY2"
               WHEN    "DBUPDATE"      ALSO   "PTKOHINF-KEY3"
               WHEN    "DBDELETE"      ALSO   "PTKOHINF-KEY"
               WHEN    "DBDELETE"      ALSO   "PTKOHINF-KEY2"
               WHEN    "DBDELETE"      ALSO   "PTKOHINF-KEY3"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTKOHINF-DEL1"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY3"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY4"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY5"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY6"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY7"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY8"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY9"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY10"
               WHEN    "DBUPDATE"      ALSO   "SRYACCT-KEY11"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY2"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY3"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY4"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY5"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY6"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY7"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY8"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY9"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY10"
               WHEN    "DBDELETE"      ALSO   "SRYACCT-KEY11"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYACCT-DEL1"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYACCT-DEL2"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SRYACT-KEY"
               WHEN    "DBUPDATE"      ALSO   "SRYACT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SRYACT-KEY3"
               WHEN    "DBUPDATE"      ALSO   "SRYACT-KEY4"
               WHEN    "DBUPDATE"      ALSO   "SRYACT-KEY5"
               WHEN    "DBDELETE"      ALSO   "SRYACT-KEY"
               WHEN    "DBDELETE"      ALSO   "SRYACT-KEY3"
               WHEN    "DBDELETE"      ALSO   "SRYACT-KEY4"
               WHEN    "DBDELETE"      ALSO   "SRYACT-KEY5"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYACT-KEY2"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYACT-DEL1"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "del12" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SRYACT-DEL3"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY3"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY4"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY5"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY6"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY7"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY8"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY9"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY10"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY11"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY12"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY13"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY14"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY15"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY16"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY17"
               WHEN    "DBUPDATE"      ALSO   "SYUNOU-KEY18"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY2"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY3"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY4"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY5"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY6"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY7"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY8"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY9"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY10"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY11"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY12"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY13"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY14"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY15"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY16"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY17"
               WHEN    "DBDELETE"      ALSO   "SYUNOU-KEY18"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL1"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL2"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL3"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL4"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "del4" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL5"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "del5" TO WRK-PATHNAME
      *        WHEN    "DBDELETE"      ALSO   "SYUNOU-DEL6"
      *            MOVE "tbl_syunou" TO WRK-TABLE
      *            MOVE "del6" TO WRK-PATHNAME
      *追加/*開始*01.01.45********************************************
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY"
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY3"
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY4"
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY5"
               WHEN    "DBUPDATE"      ALSO   "SYUMEI-KEY6"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY2"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY3"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY4"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY5"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY6"
               WHEN    "DBDELETE"      ALSO   "SYUMEI-KEY7"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *追加/*終了*01.01.45********************************************
               WHEN    "DBDELETE"      ALSO   "SYUMEI-DEL1"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUMEI-DEL2"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TSYRRK-KEY"
               WHEN    "DBUPDATE"      ALSO   "TSYRRK-KEY2"
               WHEN    "DBUPDATE"      ALSO   "TSYRRK-KEY3"
               WHEN    "DBDELETE"      ALSO   "TSYRRK-KEY"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "TSYRRK-KEY2"
               WHEN    "DBDELETE"      ALSO   "TSYRRK-KEY3"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TSYRRK-UPD1"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TENSU-UPD1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "TENSU-KEY"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "UKETUKE-KEY"
               WHEN    "DBUPDATE"      ALSO   "UKETUKE-KEY2"
               WHEN    "DBUPDATE"      ALSO   "UKETUKE-KEY3"
               WHEN    "DBUPDATE"      ALSO   "UKETUKE-KEY4"
               WHEN    "DBDELETE"      ALSO   "UKETUKE-KEY"
               WHEN    "DBDELETE"      ALSO   "UKETUKE-KEY2"
               WHEN    "DBDELETE"      ALSO   "UKETUKE-KEY3"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "UKETUKE-KEY4"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "UKETUKE-DEL1"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "del12" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SYKRRK-KEY"
               WHEN    "DBUPDATE"      ALSO   "SYKRRK-KEY2"
               WHEN    "DBDELETE"      ALSO   "SYKRRK-KEY"
               WHEN    "DBDELETE"      ALSO   "SYKRRK-KEY2"
                   MOVE "tbl_sykrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYKRRK-DEL1"
                   MOVE "tbl_sykrrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TNKRRK-KEY"
               WHEN    "DBUPDATE"      ALSO   "TNKRRK-KEY2"
               WHEN    "DBUPDATE"      ALSO   "TNKRRK-KEY3"
               WHEN    "DBDELETE"      ALSO   "TNKRRK-KEY"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "TNKRRK-KEY2"
               WHEN    "DBDELETE"      ALSO   "TNKRRK-KEY3"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TNKRRK-UPD1"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "WKSRYACT-KEY"
               WHEN    "DBUPDATE"      ALSO   "WKSRYACT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "WKSRYACT-KEY3"
               WHEN    "DBUPDATE"      ALSO   "WKSRYACT-KEY4"
               WHEN    "DBUPDATE"      ALSO   "WKSRYACT-KEY5"
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-KEY"
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-KEY3"
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-KEY4"
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-KEY5"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-KEY2"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "WKSRYACT-DEL1"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "del12" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "PARA-KEY"
               WHEN    "DBUPDATE"      ALSO   "PARA-KEY2"
               WHEN    "DBUPDATE"      ALSO   "PARA-KEY3"
               WHEN    "DBDELETE"      ALSO   "PARA-KEY"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *************PERFORM 2001-SEL-PARA-SEC
               WHEN    "DBDELETE"      ALSO   "PARA-KEY2"
               WHEN    "DBDELETE"      ALSO   "PARA-DEL11"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PARA-SEC
               WHEN    "DBDELETE"      ALSO   "PARA-KEY3"
               WHEN    "DBDELETE"      ALSO   "PARA-DEL2"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SANTEI-KEY"
               WHEN    "DBUPDATE"      ALSO   "SANTEI-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SANTEI-KEY4"
               WHEN    "DBUPDATE"      ALSO   "SANTEI-KEY5"
               WHEN    "DBUPDATE"      ALSO   "SANTEI-KEY6"
               WHEN    "DBDELETE"      ALSO   "SANTEI-KEY"
               WHEN    "DBDELETE"      ALSO   "SANTEI-KEY2"
               WHEN    "DBDELETE"      ALSO   "SANTEI-KEY4"
               WHEN    "DBDELETE"      ALSO   "SANTEI-KEY5"
               WHEN    "DBDELETE"      ALSO   "SANTEI-KEY6"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SANTEI-DEL1"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "TENSUPLUS-UPD1"
                   MOVE "tbl_tensuplus" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SRYCDCHG-KEY"
               WHEN    "DBUPDATE"      ALSO   "SRYCDCHG-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SRYCDCHG-KEY3"
               WHEN    "DBDELETE"      ALSO   "SRYCDCHG-KEY"
               WHEN    "DBDELETE"      ALSO   "SRYCDCHG-KEY2"
               WHEN    "DBDELETE"      ALSO   "SRYCDCHG-KEY3"
                   MOVE "tbl_srycdchg" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SEIKYU-KEY"
               WHEN    "DBUPDATE"      ALSO   "SEIKYU-KEY2"
               WHEN    "DBUPDATE"      ALSO   "SEIKYU-KEY3"
               WHEN    "DBUPDATE"      ALSO   "SEIKYU-KEY4"
               WHEN    "DBDELETE"      ALSO   "SEIKYU-KEY"
               WHEN    "DBDELETE"      ALSO   "SEIKYU-KEY2"
               WHEN    "DBDELETE"      ALSO   "SEIKYU-KEY3"
               WHEN    "DBDELETE"      ALSO   "SEIKYU-KEY4"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL1"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL2"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL3"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL4"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del4" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL5"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del5" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SEIKYU-DEL6"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "del6" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "HKNJAINF-KEY"
               WHEN    "DBDELETE"      ALSO   "HKNJAINF-KEY"
                   MOVE "tbl_hknjainf" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "USERBYOMEI-KEY"
               WHEN    "DBDELETE"      ALSO   "USERBYOMEI-KEY"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "USERBYOMEI-DEL1"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "USERBYOMEI-DEL2"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "RECECOM-KEY"
               WHEN    "DBUPDATE"      ALSO   "RECECOM-KEY1"
               WHEN    "DBUPDATE"      ALSO   "RECECOM-KEY2"
               WHEN    "DBUPDATE"      ALSO   "RECECOM-KEY3"
               WHEN    "DBUPDATE"      ALSO   "RECECOM-KEY4"
               WHEN    "DBDELETE"      ALSO   "RECECOM-KEY"
               WHEN    "DBDELETE"      ALSO   "RECECOM-KEY1"
               WHEN    "DBDELETE"      ALSO   "RECECOM-KEY2"
               WHEN    "DBDELETE"      ALSO   "RECECOM-KEY3"
               WHEN    "DBDELETE"      ALSO   "RECECOM-KEY4"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECECOM-DEL1"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "EXAMPLE-KEY"
               WHEN    "DBUPDATE"      ALSO   "EXAMPLE-KEY1"
               WHEN    "DBUPDATE"      ALSO   "EXAMPLE-KEY2"
               WHEN    "DBDELETE"      ALSO   "EXAMPLE-KEY"
               WHEN    "DBDELETE"      ALSO   "EXAMPLE-KEY1"
               WHEN    "DBDELETE"      ALSO   "EXAMPLE-KEY2"
                   MOVE "tbl_example" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "DBKANRI-KEY"
               WHEN    "DBDELETE"      ALSO   "DBKANRI-KEY"
                   MOVE "tbl_dbkanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "RECEKANRI-KEY"
               WHEN    "DBDELETE"      ALSO   "RECEKANRI-KEY"
               WHEN    "DBUPDATE"      ALSO   "RECEKANRI-KEY2"
               WHEN    "DBDELETE"      ALSO   "RECEKANRI-KEY2"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEKANRI-DEL1"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEKANRI-DEL2"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "PTRSIINF-KEY"
               WHEN    "DBUPDATE"      ALSO   "PTRSIINF-KEY2"
               WHEN    "DBUPDATE"      ALSO   "PTRSIINF-KEY3"
               WHEN    "DBDELETE"      ALSO   "PTRSIINF-KEY"
               WHEN    "DBDELETE"      ALSO   "PTRSIINF-KEY2"
               WHEN    "DBDELETE"      ALSO   "PTRSIINF-KEY3"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTRSIINF-DEL1"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *        
               WHEN    "DBUPDATE"      ALSO   "INTERACT-KEY"
               WHEN    "DBUPDATE"      ALSO   "INTERACT-KEY1"
               WHEN    "DBUPDATE"      ALSO   "INTERACT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "INTERACT-KEY3"
               WHEN    "DBDELETE"      ALSO   "INTERACT-KEY"
               WHEN    "DBDELETE"      ALSO   "INTERACT-KEY1"
               WHEN    "DBDELETE"      ALSO   "INTERACT-KEY2"
               WHEN    "DBDELETE"      ALSO   "INTERACT-KEY3"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "INTERACT-UPD1"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "INTERACT-DEL1"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "SSKIJYO-KEY"
               WHEN    "DBDELETE"      ALSO   "SSKIJYO-KEY"
                   MOVE "tbl_sskijyo" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "MONTHLYNUM-KEY"
               WHEN    "DBUPDATE"      ALSO   "MONTHLYNUM-KEY2"
               WHEN    "DBUPDATE"      ALSO   "MONTHLYNUM-KEY3"
               WHEN    "DBDELETE"      ALSO   "MONTHLYNUM-KEY"
               WHEN    "DBDELETE"      ALSO   "MONTHLYNUM-KEY2"
               WHEN    "DBDELETE"      ALSO   "MONTHLYNUM-KEY3"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "MONTHLYNUM-DEL1"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *------------------------------------
      *---- (NYUIN) LINE ADD START    --------------------------------
               WHEN    "DBDELETE"      ALSO   "PTNYUINRRK-DEL1"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTNYUINRRK-DEL2"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTTAINFUKA-DEL1"
                   MOVE "tbl_pttainfuka" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTTAINFUKA-DEL2"
                   MOVE "tbl_pttainfuka" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
      *---- (NYUIN) LINE ADD END      --------------------------------
      *---- (NYUIN.08) LINE ADD START    -----------------------------
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY3"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY4"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY5"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY6"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY7"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY8"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY9"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY10"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY11"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY12"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY13"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY14"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY15"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY16"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY17"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY18"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY19"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY20"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY21"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY22"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY23"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY26"
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-KEY28"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "NYUINACCT-DEL1"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "NYUINACCT-UPD1"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY2"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY3"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY4"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY5"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY6"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY7"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY8"
               WHEN    "DBUPDATE"      ALSO   "NYUINACT-KEY9"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "NYUINACT-DEL11"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "del11" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "NYUINACT-DEL12"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "del12" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "KOHSKY-KEY"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "KOHSKY-DEL1"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "KOHSKY-DEL2"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "KOHSKY-DEL3"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEPRT-DEL1"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEPRT-DEL2"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "PTKOHFTN-DEL1"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "RECEDEN-KEY"
               WHEN    "DBDELETE"      ALSO   "RECEDEN-KEY"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEDEN-DEL1"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEDEN-DEL2"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "RECEDEN-DEL3"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "KOHPLUS-KEY"
               WHEN    "DBDELETE"      ALSO   "KOHPLUS-KEY"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "KOHPLUS-DEL1"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBUPDATE"      ALSO   "KOHPLUS-UPD1"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
      *
               WHEN    "DBDELETE"      ALSO   "SYUDAY-DEL1"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUDAY-DEL2"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUDAY-DEL3"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "del3" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUDAY-DEL4"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "del4" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUDAY-DEL5"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "del5" TO WRK-PATHNAME
      *
               WHEN    "DBUPDATE"      ALSO   "OPECOM-KEY"
               WHEN    "DBDELETE"      ALSO   "OPECOM-KEY"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *
               WHEN    "DBDELETE"      ALSO   "OPECOM-DEL1"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "OPECOM-DEL2"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
      *
      *---- (NYUIN.08) LINE ADD END      ------------------------------
      *
               WHEN    "DBUPDATE"      ALSO   "SYUTOTAL-KEY"
                   MOVE "tbl_syutotal" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUTOTAL-KEY"
                   MOVE "tbl_syutotal" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "SYUTOTAL-DEL1"
                   MOVE "tbl_syutotal" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *
      *---- (HELP) LINE ADD START    ------------------------------
      *
               WHEN    "DBUPDATE"      ALSO   "HELP-KEY"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "HELP-KEY"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "HELP-DEL1"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN    "DBDELETE"      ALSO   "HELP-DEL2"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "del2" TO WRK-PATHNAME
      *---- (HELP) LINE ADD END      ------------------------------
      *
               WHEN    ANY             ALSO   "SYSKANRI-KEY"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY2"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY3"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY4"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY5"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY6"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYSKANRI-KEY7"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY8"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY9"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY10"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY11"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
               WHEN    ANY             ALSO   "SYSKANRI-KEY12"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SYSKANRI-ALL"
                   MOVE "tbl_syskanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SYSKANRI-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "BYOMEI-KEY"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BYOMEI-KEY2"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-BYOKEY-SEC
               WHEN    ANY             ALSO   "BYOMEI-KEY3"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-BYOKEY-SEC
               WHEN    ANY             ALSO   "BYOMEI-KEY4"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BYOMEI-KEY5"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BYOMEI-KEY6"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "BYOMEI-ALL"
                   MOVE "tbl_byomei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-BYOKEY-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "CHK-KEY"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY2"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY3"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY4"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
      *追加/*開始*01.09.12********************************************
               WHEN    ANY             ALSO   "CHK-KEY5"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY6"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY7"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY8"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY9"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY10"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
               WHEN    ANY             ALSO   "CHK-KEY11"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
      *追加/*終了*01.09.12********************************************
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "CHK-ALL"
                   MOVE "tbl_chk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-CHK-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "INPUTCD-KEY"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    ANY             ALSO   "INPUTCD-KEY2"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    ANY             ALSO   "INPUTCD-KEY3"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
               WHEN    ANY             ALSO   "INPUTCD-KEY4"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "INPUTCD-ALL"
                   MOVE "tbl_inputcd" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-INPUTCD-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "HKNCOMBI-KEY"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNCOMBI-KEY2"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNCOMBI-KEY3"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNCOMBI-KEY4"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNCOMBI-KEY5"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "HKNCOMBI-ALL"
                   MOVE "tbl_hkncombi" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "HKNNUM-KEY"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY2"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY3"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY4"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY5"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY6"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HKNNUM-KEY7"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "HKNNUM-ALL"
                   MOVE "tbl_hknnum" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "INPUTSET-KEY"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INPUTSET-KEY2"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INPUTSET-KEY3"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INPUTSET-KEY4"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "INPUTSET-ALL"
                   MOVE "tbl_inputset" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "ADRS-KEY"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "ADRS-KEY2"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "ADRS-KEY3"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-ADRS-SEC
               WHEN    ANY             ALSO   "ADRS-KEY4"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-ADRS-SEC
               WHEN    ANY             ALSO   "ADRS-KEY5"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                   PERFORM 2001-SEL-ADRS-SEC
      *----(01.01.151)-ADD-START---
               WHEN    ANY             ALSO   "ADRS-KEY6"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                   PERFORM 2001-SEL-ADRS-SEC
               WHEN    ANY             ALSO   "ADRS-KEY7"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
      *----(01.01.151)-ADD-END-----
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "ADRS-ALL"
                   MOVE "tbl_adrs" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "JYURRK-KEY"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY2"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY3"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY4"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY5"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY6"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY7"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY8"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY9"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY10"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY11"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY12"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY13"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY14"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY15"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY16"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY17"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY18"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY19"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY20"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY21"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY22"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY23"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY24"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY25"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-KEY26"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-ALL"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JYURRK-ALL2"
                   MOVE "tbl_jyurrk" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYKARRK-KEY"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYKARRK-KEY2"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "key2 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYKARRK-KEY3"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "key3 " TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SRYKARRK-ALL"
                   MOVE "tbl_srykarrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "PTBYOMEI-KEY"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY2"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key2 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY3"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key3 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY4"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key4 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY5"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key5 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY6"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key6 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY7"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key7 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY8"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key8 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY9"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key9 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY10"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key10 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY11"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key11 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY12"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key12 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY13"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key13 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY14"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY15"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY16"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY17"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY18"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY19"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY20"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY21"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY22"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY23"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY24"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-KEY25"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-ALL"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTBYOMEI-ALL2"
                   MOVE "tbl_ptbyomei" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTCOM-KEY"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTCOM-KEY2"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PTCOM-ALL"
                   MOVE "tbl_ptcom" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "PTNUM-KEY"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTNUM-SEC
               WHEN    ANY             ALSO   "PTNUM-KEY2"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "key2 " TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTNUM-SEC
               WHEN    ANY             ALSO   "PTNUM-KEY3"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "key3 " TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PTNUM-ALL"
                   MOVE "tbl_ptnum" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTNUM-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "PTHKNINF-KEY"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTHKNINF-KEY2"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key2 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTHKNINF-KEY3"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key3 " TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTHKNINF-SEC
               WHEN    ANY             ALSO   "PTHKNINF-KEY4"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTHKNINF-KEY5"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTHKNINF-KEY6"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PTHKNINF-ALL"
                   MOVE "tbl_pthkninf" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTHKNINF-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "PTKOHINF-KEY"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHINF-KEY2"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key2 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHINF-KEY3"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key3 " TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTKOHINF-SEC
               WHEN    ANY             ALSO   "PTKOHINF-KEY4"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key4 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHINF-KEY5"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key5 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHINF-KEY6"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key6 " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHINF-KEY7"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "key7 " TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PTKOHINF-ALL"
                   MOVE "tbl_ptkohinf" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTKOHINF-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "PTINF-KEY"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTINF-KEY2"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTINF-SEC
               WHEN    ANY             ALSO   "PTINF-KEY3"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTINF-SEC
               WHEN    ANY             ALSO   "PTINF-KEY5"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTINF-SEC
               WHEN    ANY             ALSO   "PTINF-KEY4"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTINF-KEY6"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTINF-SEC
               WHEN    ANY             ALSO   "PTINF-KEY7"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTINF-KEY8"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTINF-KEY9"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTINF-KEY10"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PTINF-ALL"
                   MOVE "tbl_ptinf" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PTINF-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "KYUSEIRRK-KEY"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KYUSEIRRK-KEY2"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KYUSEIRRK-KEY3"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KYUSEIRRK-UPD1"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "KYUSEIRRK-ALL"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN     "DBDELETE"     ALSO   "KYUSEIRRK-DEL1"
                   MOVE "tbl_kyuseirrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN     "DBDELETE"     ALSO   "TEIKIKANRI-DEL1"
                   MOVE "tbl_teikikanri" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN     "DBDELETE"     ALSO   "TEIKIPRT-DEL1"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
               WHEN     "DBDELETE"     ALSO   "PTTEIKIRRK-DEL1"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "del1" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SRYACCT-KEY"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY2"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY3"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY4"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY5"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY6"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY7"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY8"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY9"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY10"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY11"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY12"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY13"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY14"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY15"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY16"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY17"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY18"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY19"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY20"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY21"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY22"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY23"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY24"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY25"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
      *----(01.01.112)-ADD-START---
               WHEN    ANY             ALSO   "SRYACCT-KEY26"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
      *----(01.01.112)-ADD-END-----
               WHEN    ANY             ALSO   "SRYACCT-KEY27"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key27" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY28"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key28" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY29"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key29" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY30"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key30" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY31"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key31" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY32"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key32" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY33"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key33" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY34"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key34" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY35"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key35" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY36"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key36" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY37"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key37" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY39"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key39" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-KEY40"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "key40" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-ALL"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACCT-ALL2"
                   MOVE "tbl_sryacct" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY2"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY3"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY4"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY5"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY6"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY7"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY8"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY9"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY11"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-KEY12"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-ALL"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYACT-ALL2"
                   MOVE "tbl_sryact" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY2"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY3"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
      *追加/*開始*01.01.45********************************************
               WHEN    ANY             ALSO   "SYUMEI-ALL"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SYUMEI-KEY4"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY5"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY6"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUMEI-KEY7"
                   MOVE "tbl_syumei" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "SYUNOU-KEY"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY2"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY3"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY4"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY5"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY6"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY7"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY8"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY9"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY10"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY11"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY12"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY13"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY14"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY15"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY16"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY17"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY18"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY19"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY20"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY21"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY22"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY23"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY24"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY25"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY26"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY27"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key27" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY28"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key28" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY29"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key29" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY30"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key30" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY33"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key33" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY35"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key35" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY39"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key39" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY40"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key40" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUNOU-KEY41"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "key41" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SYUNOU-ALL"
                   MOVE "tbl_syunou" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "TSYRRK-KEY"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TSYRRK-KEY2"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TSYRRK-KEY3"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TSYRRK-UPD1"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "TSYRRK-ALL"
                   MOVE "tbl_tsyrrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "TENSU-KEY"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY22"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY2"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY3"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY4"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY5"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY6"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY7"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY41"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key41" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY42"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key42" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY42J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key42j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY42K"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key42k" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY42JK"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key42jk" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY51"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key51" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY51J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key51j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY52"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key52" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY52J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key52j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY53"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key53" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY53J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key53j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY6"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY6J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key6j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY7"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY7J"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key7j" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10S"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10s" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10K"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10k" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10M"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10m" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10S1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10s1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10K1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10k1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY10M1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key10m1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11S"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11s" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11K"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11k" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11M"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11m" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11S1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11s1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11K1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11k1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY11M1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key11m1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12S"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12s" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12K"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12k" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12M"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12m" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12S1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12s1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12K1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12k1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY12M1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key12m1" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY13"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY14"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY15"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY20"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY21"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-UPD1"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
      *----(01.01.94)--ADD-START---
               WHEN    ANY             ALSO   "TENSU-KEY23S"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key23s" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY23M"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key23m" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY23K"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key23k" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY90"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key90" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY91"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key91" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY92"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key92" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
               WHEN    ANY             ALSO   "TENSU-KEY93"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key93" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
      *----(01.01.94)--ADD-END-----
               WHEN    ANY             ALSO   "TENSU-KEY24"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
      ************ PERFORM 2001-SEL-TENSU-REC
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "TENSU-ALL"
                   MOVE "tbl_tensu" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-TENSU-REC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "UKETUKE-KEY"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "UKETUKE-KEY2"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "UKETUKE-KEY3"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "UKETUKE-KEY4"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "UKETUKE-ALL"
                   MOVE "tbl_uketuke" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "YYK-KEY"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY2"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY3"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY4"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY5"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY6"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YYK-KEY7"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "YYK-ALL"
                   MOVE "tbl_yyk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "SYKRRK-KEY"
                   MOVE "tbl_sykrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYKRRK-KEY2"
                   MOVE "tbl_sykrrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SYKRRK-ALL"
                   MOVE "tbl_sykrrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "TNKRRK-KEY"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TNKRRK-KEY2"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TNKRRK-KEY3"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TNKRRK-UPD1"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "TNKRRK-ALL"
                   MOVE "tbl_tnkrrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
      *------------------------------------
               WHEN    ANY             ALSO   "WKSRYACT-KEY"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY2"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY3"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY4"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY5"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY6"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-KEY7"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-ALL"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "WKSRYACT-ALL2"
                   MOVE "tbl_wksryact" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PARA-KEY"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PARA-SEC
               WHEN    ANY             ALSO   "PARA-KEY2"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PARA-SEC
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "PARA-ALL"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PARA-SEC
               WHEN    ANY             ALSO   "PARA-KEY3"
                   MOVE "tbl_para" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                   PERFORM 2001-SEL-PARA-SEC
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "SANTEI-KEY"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY2"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY3"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY4"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY5"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY6"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY7"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-KEY8"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-ALL"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SANTEI-ALL2"
                   MOVE "tbl_santei" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TENSUPLUS-KEY"
                   MOVE "tbl_tensuplus" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TENSUPLUS-KEY2"
                   MOVE "tbl_tensuplus" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "TENSUPLUS-ALL"
                   MOVE "tbl_tensuplus" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
               WHEN    ANY             ALSO   "SRYCDCHG-KEY"
                   MOVE "tbl_srycdchg" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYCDCHG-KEY2"
                   MOVE "tbl_srycdchg" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SRYCDCHG-KEY3"
                   MOVE "tbl_srycdchg" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SRYCDCHG-ALL"
                   MOVE "tbl_srycdchg" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
                WHEN    ANY             ALSO   "RECEPRT-KEY"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY2"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY3"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY4"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY5"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY6"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY7"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY8"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY9"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY10"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY11"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY12"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY13"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY14"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY15"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY16"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY17"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY18"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY19"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY20"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY21"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY22"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY23"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY24"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY25"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY26"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY27"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key27" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY28"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key28" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY29"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key29" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "RECEPRT-KEY30"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "key30" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "RECEPRT-ALL"
                   MOVE "tbl_receprt" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
                WHEN    ANY             ALSO   "SEIKYU-KEY"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY2"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY3"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY4"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY5"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY6"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY7"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY8"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY9"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY10"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY11"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY12"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY13"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY14"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY15"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY16"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY17"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY18"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY19"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY20"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY21"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY22"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
                   PERFORM 2001-SEL-SEIKYU-SEC
                WHEN    ANY             ALSO   "SEIKYU-KEY23"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "SEIKYU-KEY24"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "SEIKYU-ALL"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SEIKYU-ALL2"
                   MOVE "tbl_seikyu" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "HKNJAINF-KEY"
                   MOVE "tbl_hknjainf" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "HKNJAINF-KEY2"
                   MOVE "tbl_hknjainf" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "HKNJAINF-ALL"
                   MOVE "tbl_hknjainf" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
                WHEN    ANY             ALSO   "BD001-KEY"
                   MOVE "view_bd001" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
      *追加/*開始*01.09.15********************************************
               WHEN    ANY             ALSO   "BD001-ALL"
                   MOVE "view_bd001" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.09.15********************************************
                WHEN    ANY             ALSO   "BD002-KEY"
                   MOVE "view_bd002" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "BD002-KEY2"
                   MOVE "view_bd002" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
                WHEN    ANY             ALSO   "BD002-KEY3"
                   MOVE "view_bd002" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BD002-ALL"
                   MOVE "view_bd002" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BD002-ALL2"
                   MOVE "view_bd002" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY2"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY3"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY4"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY5"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY6"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY7"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-KEY8"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
                   PERFORM 2001-SEL-USERBYOMEI-SEC
               WHEN    ANY             ALSO   "USERBYOMEI-KEY9"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "USERBYOMEI-ALL"
                   MOVE "tbl_userbyomei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-KEY"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-KEY1"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key1" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-KEY2"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-KEY3"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-KEY4"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECECOM-ALL"
                   MOVE "tbl_rececom" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "EXAMPLE-KEY"
                   MOVE "tbl_example" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "EXAMPLE-KEY1"
                   MOVE "tbl_example" TO WRK-TABLE
                   MOVE "key1" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "EXAMPLE-KEY2"
                   MOVE "tbl_example" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "EXAMPLE-ALL"
                   MOVE "tbl_example" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*開始*02.02.04********************************************
               WHEN    ANY             ALSO   "JOBKANRI-ALL"
                   MOVE "tbl_jobkanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*02.02.04********************************************
      *追加/*開始*02.05.01********************************************
               WHEN    ANY             ALSO   "JOBKANRI-ALL02"
                   MOVE "tbl_jobkanri" TO WRK-TABLE
                   MOVE "all02" TO WRK-PATHNAME
      *追加/*終了*02.05.01********************************************
      *
               WHEN    ANY             ALSO   "DBKANRI-KEY"
                   MOVE "tbl_dbkanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "DBKANRI-ALL"
                   MOVE "tbl_dbkanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "RECEKANRI-KEY"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY2"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY3"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY4"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY5"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY6"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-KEY7"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEKANRI-ALL"
                   MOVE "tbl_recekanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "PTRSIINF-KEY"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTRSIINF-KEY2"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTRSIINF-ALL"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTRSIINF-KEY3"
                   MOVE "tbl_ptrsiinf" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "PRTKANRI-KEY"
                   MOVE "tbl_prtkanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PRTKANRI-KEY2"
                   MOVE "tbl_prtkanri" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PRTKANRI-ALL"
                   MOVE "tbl_prtkanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PRTDATA-KEY"
                   MOVE "tbl_prtdata" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PRTDATA-KEY2"
                   MOVE "tbl_prtdata" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PRTDATA-ALL"
                   MOVE "tbl_prtdata" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
      *---- (01.00.60) LINE ADD END   --------------------------------
               WHEN    ANY             ALSO   "INTERACT-KEY"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INTERACT-KEY1"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "key1" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INTERACT-KEY2"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INTERACT-KEY3"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "INTERACT-ALL"
                   MOVE "tbl_interact" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SSKIJYO-KEY"
                   MOVE "tbl_sskijyo" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SSKIJYO-KEY"
                   MOVE "tbl_sskijyo" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SSKIJYO-ALL"
                   MOVE "tbl_sskijyo" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JINMEI-KEY"
                   MOVE "tbl_jinmei" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JINMEI-KEY2"
                   MOVE "tbl_jinmei" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JINMEI-KEY3"
                   MOVE "tbl_jinmei" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JINMEI-UPD1"
                   MOVE "tbl_jinmei" TO WRK-TABLE
                   MOVE "upd1" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "JINMEI-ALL"
                   MOVE "tbl_jinmei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *----
               WHEN    ANY             ALSO   "MONTHLYNUM-KEY"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "MONTHLYNUM-KEY2"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "MONTHLYNUM-KEY3"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "MONTHLYNUM-ALL"
                   MOVE "tbl_monthlynum" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (01.00.05) LINE ADD END   --------------------------------
      *---- (01.00.75) LINE ADD START --------------------------------
               WHEN    ANY             ALSO   "YAKUJYO-KEY"
                   MOVE "tbl_yakujyo" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "YAKUJYO-ALL"
                   MOVE "tbl_yakujyo" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (01.00.75) LINE ADD END   --------------------------------
      *---- (NYUIN) LINE ADD START    --------------------------------
               WHEN    ANY             ALSO   "NYUINKHN-KEY"
                   MOVE "tbl_nyuinkhn" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINKHN-KEY2"
                   MOVE "tbl_nyuinkhn" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINKHN-ALL"
                   MOVE "tbl_nyuinkhn" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY2"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY3"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY4"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY5"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY6"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY7"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY8"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY9"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY10"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY11"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY12"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY13"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY14"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY15"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY16"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY17"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY18"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY19"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY20"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY21"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY22"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY23"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY24"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key24" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY25"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key25" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY26"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY27"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key27" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY28"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key28" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY29"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key29" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY30"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key30" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY31"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key31" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY32"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key32" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY33"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key33" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-KEY34"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "key34" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTNYUINRRK-ALL"
                   MOVE "tbl_ptnyuinrrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTAINFUKA-KEY"
                   MOVE "tbl_pttainfuka" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTAINFUKA-ALL"
                   MOVE "tbl_pttainfuka" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (NYUIN) LINE ADD END       --------------------------------
      *---- (NYUIN.08) LINE ADD START    -----------------------------
               WHEN    ANY             ALSO   "NYUINACCT-KEY"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY2"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY3"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY4"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY5"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY6"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY7"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY8"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY9"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY10"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key10" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY11"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key11" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY12"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key12" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY13"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key13" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY14"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key14" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY15"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key15" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY16"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key16" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY17"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key17" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY18"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key18" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY19"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key19" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY20"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key20" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY21"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key21" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY22"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key22" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY23"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key23" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY26"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key26" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-KEY28"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "key28" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-ALL"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACCT-ALL2"
                   MOVE "tbl_nyuinacct" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY2"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY3"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY4"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY5"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY6"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY7"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY8"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key8" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-KEY9"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "key9" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-ALL"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINACT-ALL2"
                   MOVE "tbl_nyuinact" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
      *---- (NYUIN.08) LINE ADD END      -----------------------------
      *---- (NYUIN.10) LINE ADD START    -----------------------------
               WHEN    ANY             ALSO   "I001-KEY"
                   MOVE "view_I001" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "I001-KEY2"
                   MOVE "view_I001" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "I001-KEY3"
                   MOVE "view_I001" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "I001-ALL"
                   MOVE "view_I001" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "I001-KEY4"
                   MOVE "view_I001" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "Q001-ALL"
                   MOVE "view_Q001" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (NYUIN.10) LINE ADD END      -----------------------------
      *---- (NYUIN.15) LINE ADD START    -----------------------------
               WHEN    ANY             ALSO   "NYUKSNCHK-KEY"
                   MOVE "tbl_nyuksnchk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-NYUKSNCHK-SEC
      *---- (NYUIN.15) LINE ADD END      -----------------------------
               WHEN    ANY             ALSO   "NYUKSNCHK-ALL"
                   MOVE "tbl_nyuksnchk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "LDMAP-ALL"
                   MOVE "tbl_ldmap" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "LOCK-ALL"
                   MOVE "tbl_lock" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "NYUINRYAKUSHO-KEY"
                   MOVE "tbl_nyuinryakusho" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
                   PERFORM 2001-SEL-NYUINRYAKUSHO-SEC
               WHEN    ANY             ALSO   "NYUINRYAKUSHO-ALL"
                   MOVE "tbl_nyuinryakusho" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIKANRI-KEY"
                   MOVE "tbl_teikikanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIKANRI-ALL"
                   MOVE "tbl_teikikanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-KEY"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-KEY2"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-KEY3"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-KEY4"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-KEY5"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIPRT-ALL"
                   MOVE "tbl_teikiprt" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIRRK-KEY"
                   MOVE "tbl_teikirrk" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIRRK-KEY2"
                   MOVE "tbl_teikirrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEIKIRRK-ALL"
                   MOVE "tbl_teikirrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key " TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY2"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY3"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY4"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY5"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY6"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-KEY7"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-ALL"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTTEIKIRRK-ALL2"
                   MOVE "tbl_ptteikirrk" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY2"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY3"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY4"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY5"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY6"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key6" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-KEY7"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "key7" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHSKY-ALL"
                   MOVE "tbl_kohsky" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (01.01.42) LINE ADD START --------------------------------
               WHEN    ANY             ALSO   "KENSASORT-KEY"
                   MOVE "tbl_kensasort" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KENSASORT-ALL"
                   MOVE "tbl_kensasort" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (01.01.42) LINE ADD END   --------------------------------
               WHEN    ANY             ALSO   "SENTEICDCHG-KEY"
                   MOVE "tbl_senteicdchg" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SENTEICDCHG-KEY2"
                   MOVE "tbl_senteicdchg" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SENTEICDCHG-KEY3"
                   MOVE "tbl_senteicdchg" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SENTEICDCHG-ALL"
                   MOVE "tbl_senteicdchg" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-KEY"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-KEY2"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-KEY3"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-KEY4"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-KEY5"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PTKOHFTN-ALL"
                   MOVE "tbl_ptkohftn" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEDEN-KEY"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEDEN-KEY2"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEDEN-ALL"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "RECEDEN-ALL2"
                   MOVE "tbl_receden" TO WRK-TABLE
                   MOVE "all2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHPLUS-KEY"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHPLUS-KEY2"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHPLUS-KEY3"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "KOHPLUS-ALL"
                   MOVE "tbl_kohplus" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *追加/*終了*01.01.107*******************************************
               WHEN    ANY             ALSO   "BD003-KEY"
                   MOVE "view_bd003" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BD003-KEY2"
                   MOVE "view_bd003" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BD003-KEY3"
                   MOVE "view_bd003" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "BD003-ALL"
                   MOVE "view_bd003" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SYUDAY-KEY"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUDAY-ALL"
                   MOVE "tbl_syuday" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *----(01.01.123)-ADD-START---
               WHEN    ANY             ALSO   "PGKANRI-KEY"
                   MOVE "tbl_pgkanri" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PGKANRI-KEY2"
                   MOVE "tbl_pgkanri" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "PGKANRI-ALL"
                   MOVE "tbl_pgkanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *----(01.01.123)-ADD-END-----
      *
               WHEN    ANY             ALSO   "OPECOM-KEY"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "OPECOM-KEY2"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "OPECOM-KEY3"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "OPECOM-KEY4"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "OPECOM-KEY5"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "OPECOM-ALL"
                   MOVE "tbl_opecom" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SYUTOTAL-KEY"
                   MOVE "tbl_syutotal" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "SYUTOTAL-ALL"
                   MOVE "tbl_syutotal" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (HELP) LINE ADD START    ------------------------------
      *
               WHEN    ANY             ALSO   "HELP-KEY"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-KEY1"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key1" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-KEY2"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key2" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-KEY3"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key3" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-KEY4"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key4" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-KEY5"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "key5" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HELP-ALL"
                   MOVE "tbl_help" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *---- (HELP) LINE ADD END      ------------------------------
      *
               WHEN    ANY             ALSO   "BTPARA-ALL"
                   MOVE "tbl_btpara" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "CHKSND-ALL"
                   MOVE "tbl_chksnd" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "JYURRK-PRV-ALL3"
                   MOVE "tbl_jyurrk_prv" TO WRK-TABLE
                   MOVE "all3" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "ORDER-ALL"
                   MOVE "tbl_order" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "ORDERPRT-ALL"
                   MOVE "tbl_orderprt" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "PRVKANRI-ALL"
                   MOVE "tbl_prvkanri" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "PTCOM-PRV-ALL"
                   MOVE "tbl_ptcom_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "RECEUPD-ALL"
                   MOVE "tbl_receupd" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SANTEI-PRV-ALL"
                   MOVE "tbl_santei_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SRYACCT-PRV-ALL"
                   MOVE "tbl_sryacct_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SRYACCTPLUS-ALL"
                   MOVE "tbl_sryacctplus" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SRYACT-PRV-ALL"
                   MOVE "tbl_sryact_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SRYKARRK-PRV-ALL"
                   MOVE "tbl_srykarrk_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SYUMEI-PRV-ALL"
                   MOVE "tbl_syumei_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SYUNOU-PRV-ALL"
                   MOVE "tbl_syunou_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "SYUTOTAL-PRV-ALL"
                   MOVE "tbl_syutotal_prv" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "TOUKEIMEMO-ALL"
                   MOVE "tbl_toukeimemo" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    ANY             ALSO   "Q002-ALL"
                   MOVE "view_Q002" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "HOURS-ALL"
                   MOVE "tbl_hours" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
               WHEN    ANY             ALSO   "TEKIOUBYOMEI-ALL"
                   MOVE "tbl_tekioubyomei" TO WRK-TABLE
                   MOVE "all" TO WRK-PATHNAME
      *
               WHEN    OTHER
                   MOVE    -9          TO  MCP-RC
           END-EVALUATE
           .
      *---- (01.00.05) LINE ADD END   --------------------------------
      *     PERFORM 210-SEND-SQL.
      *     PERFORM 220-RECV-ANS.
      *****************************************************************
      *     　　　システム管理編集検索処理
      *****************************************************************
       2001-SEL-SYSKANRI-SEC          SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-SYSKANRI-EXT
           END-IF
      *
      *    DISPLAY "2001-SEL-SYSKANRI-SEC ST ORC-DBPATH = " 
      *            "[" ORC-DBPATH "]"
           MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           MOVE    ORC-DBYMD             TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD 
      *    区分コードの文字数を確定
           MOVE    ZERO                TO  WRK-MOJISU
           IF      ORC-DBPATH            =   "SYSKANRI-KEY"
                                       OR  "SYSKANRI-KEY4"
                                       OR  "SYSKANRI-KEY9"
             IF    SYS-KBNCD           NOT =   SPACE
               PERFORM VARYING     IDX     FROM    8   BY  -1
                       UNTIL       IDX     <       1
                   IF      SYS-KBNCD(IDX:1)  NOT =   SPACE
                       MOVE    IDX     TO  WRK-MOJISU 
                       MOVE    1       TO  IDX
                   END-IF  
               END-PERFORM
             ELSE
               MOVE    1               TO  WRK-MOJISU
             END-IF
             IF  WRK-MOJISU            <   8
               COMPUTE IDX             =   WRK-MOJISU  +  1
               MOVE    "%"             TO  SYS-KBNCD(IDX:1)
             END-IF 
           END-IF
      *
      *    ユーザーＩＤの文字数を確定
           IF      ORC-DBPATH            =   "SYSKANRI-KEY3"
             IF    SYS-KANRITBL(1:16)  NOT =   SPACE
               MOVE    SYS-KANRITBL(1:16)  TO  WRK-OPID
               MOVE    ZERO                TO  WRK-MOJISU
               PERFORM VARYING     IDX     FROM    16  BY  -1
                       UNTIL       IDX     <       1
                   IF      WRK-OPID(IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-MOJISU 
                       MOVE    1                   TO  IDX
                   END-IF  
               END-PERFORM
             ELSE
               MOVE    1               TO  WRK-MOJISU
             END-IF
      *
      *      MOVE    SYS-KANRITBL(1:WRK-MOJISU)  
      *                                TO  TNS-NAME(1:WRK-MOJISU)
             COMPUTE IDX             =   WRK-MOJISU  +  1
             MOVE    "%"             TO  SYS-KANRITBL(IDX:1)
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           .
       2001-SEL-SYSKANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *     　　　システム管理編集検索処理
      *****************************************************************
       2002-SEL-SYSKANRI-SEC          SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2002-SEL-SYSKANRI-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  SYSKANRI-REC
      *    区分コードの文字数を確定
           MOVE    ZERO                TO  WRK-MOJISU
           IF    SYSUP-KBNCD           NOT =   SPACE
               PERFORM VARYING     IDX     FROM    8   BY  -1
                       UNTIL       IDX     <       1
                   IF      SYSUP-KBNCD(IDX:1)  NOT =   SPACE
                       MOVE    IDX     TO  WRK-MOJISU 
                       MOVE    1       TO  IDX
                   END-IF  
               END-PERFORM
           ELSE
               MOVE    1               TO  WRK-MOJISU
           END-IF
           IF  WRK-MOJISU            <   8
               COMPUTE IDX             =   WRK-MOJISU  +  1
               MOVE    "%"             TO  SYSUP-KBNCD(IDX:1)
           END-IF 
      *
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           .
       2002-SEL-SYSKANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *     　　　患者保険情報編集処理
      *****************************************************************
       2001-SEL-PTHKNINF-SEC          SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-PTHKNINF-EXT
           END-IF
      *
      *    DISPLAY "2001-SEL-SYSKANRI-SEC ST ORC-DBPATH = " 
      *            "[" ORC-DBPATH "]"
           MOVE    MCPDATA-REC         TO  PTHKNINF-REC
           MOVE    ORC-DBYMD           TO  PTHKN-TEKSTYMD
                                           PTHKN-TEKEDYMD 
      *
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           .
       2001-SEL-PTHKNINF-EXT.
           EXIT.
      *
      *****************************************************************
      *     　　　患者保険情報編集処理
      *****************************************************************
       2001-SEL-PTKOHINF-SEC          SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-PTKOHINF-EXT
           END-IF
      *
      *    DISPLAY "2001-SEL-SYSKANRI-SEC ST ORC-DBPATH = " 
      *            "[" ORC-DBPATH "]"
           MOVE    MCPDATA-REC         TO  PTKOHINF-REC
           MOVE    ORC-DBYMD           TO  PTKOH-TEKSTYMD
                                           PTKOH-TEKEDYMD 
      *
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           .
       2001-SEL-PTKOHINF-EXT.
           EXIT.
      *
      *****************************************************************
      *     病名マスターを漢字病名より検索処理
      *****************************************************************
       2001-SEL-BYOKEY-SEC             SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-BYOKEY-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  BYOMEI-REC
           IF      ORC-DBPATH            =   "BYOMEI-KEY2"
               MOVE    ZERO                TO  WRK-MOJISU
               PERFORM VARYING     IDX     FROM    200  BY  -1
                       UNTIL       IDX     <       1
                   IF      BYO-BYOMEI(IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-MOJISU 
                       MOVE    1                   TO  IDX
                   END-IF  
               END-PERFORM
      *
      *    検索名称編集
               PERFORM 2001B-NAME-SET-SEC
           END-IF
      *
           IF      ORC-DBPATH            =   "BYOMEI-KEY3"
               MOVE    ZERO                TO  WRK-MOJISU
               PERFORM VARYING     IDX     FROM    160  BY  -1
                       UNTIL       IDX     <       1
                   IF      BYO-BYOMEIKANA(IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-MOJISU 
                       MOVE    1                   TO  IDX
                   END-IF  
               END-PERFORM
      *
               MOVE    BYO-BYOMEIKANA(1:WRK-MOJISU)
                                    TO  BYO-BYOMEIKANA(2:WRK-MOJISU)
               MOVE    "%"                 TO  BYO-BYOMEIKANA(1:1)
               COMPUTE IDX                 =   WRK-MOJISU  +  2
               MOVE    "%"                 TO  BYO-BYOMEIKANA(IDX:1)
           END-IF
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
      *
           .
       2001-SEL-BYOKEY-EXT.
           EXIT.
      *----(01.00.01) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *     検索名称編集処理
      *****************************************************************
       2001B-NAME-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-ICHI1
           MOVE    ZERO                TO  WRK-ICHI2
           MOVE    ZERO                TO  WRK-MOJISU2
           MOVE    SPACE               TO  WRK-BYOMEI
      *
           IF      WRK-MOJISU          <   2
               CONTINUE
           ELSE                
               IF      BYO-BYOMEI(1:2)       =   "＊"
                   MOVE    1                   TO  WRK-ICHI1
               END-IF
               COMPUTE WRK-MOJISU2         =   WRK-MOJISU      -   1
               IF      BYO-BYOMEI(WRK-MOJISU2:2)     =   "＊"
                   MOVE    1                   TO  WRK-ICHI2
               END-IF
           END-IF    
      *
           EVALUATE    TRUE
      *        部分一致
               WHEN    (WRK-ICHI1           =   1    )  AND
                     (WRK-ICHI2           =   1    )
                   COMPUTE WRK-MOJISU2         =   WRK-MOJISU    -   4
                   MOVE    "%"                 TO  WRK-BYOMEI(1:1)
                   MOVE    BYO-BYOMEI(3:WRK-MOJISU2)
                                          TO  WRK-BYOMEI(2:WRK-MOJISU2)
                   COMPUTE IDX                 =   WRK-MOJISU2   +  2
                   MOVE    "%"                 TO  WRK-BYOMEI(IDX:1)
      *        後方一致
               WHEN    (WRK-ICHI1           =   1    )  AND
                       (WRK-ICHI2           =   ZERO )
                   COMPUTE WRK-MOJISU2         =   WRK-MOJISU    -   2
                   MOVE    "%"                 TO  WRK-BYOMEI(1:1)
                   MOVE    BYO-BYOMEI(3:WRK-MOJISU2)
                                          TO  WRK-BYOMEI(2:WRK-MOJISU2)
      *        前方一致
               WHEN    OTHER
                   IF      WRK-ICHI1           =   ZERO
                   AND     WRK-ICHI2           =   ZERO
                       MOVE    WRK-MOJISU          TO  WRK-MOJISU2
                   ELSE    
                       COMPUTE WRK-MOJISU2     =   WRK-MOJISU    -   2
                   END-IF    
                   MOVE    BYO-BYOMEI(1:WRK-MOJISU2)
                                          TO  WRK-BYOMEI(1:WRK-MOJISU2)
                   COMPUTE IDX                 =   WRK-MOJISU2  +  1
                   MOVE    "%"                 TO  WRK-BYOMEI(IDX:1)
           END-EVALUATE
      *
           MOVE    SPACE                  TO  BYO-BYOMEI
           MOVE    WRK-BYOMEI             TO  BYO-BYOMEI
           .
       2001B-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     チェックマスタを主キーより検索処理
      *****************************************************************
       2001-SEL-CHK-SEC          SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-CHK-EXT
           END-IF
      *
      *
           MOVE    MCPDATA-REC         TO  CHK-REC
      *
           MOVE    ORC-DBYMD           TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           .
       2001-SEL-CHK-EXT.
           EXIT.
      *
      *---- (NYUIN.15) LINE ADD START    -----------------------------
      *****************************************************************
      *     入院料加算チェックマスタを主キーより検索処理
      *****************************************************************
       2001-SEL-NYUKSNCHK-SEC    SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-NYUKSNCHK-EXT
           END-IF
      *
      *
           MOVE    MCPDATA-REC         TO  NYUKSNCHK-REC
      *
           MOVE    ORC-DBYMD           TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           .
       2001-SEL-NYUKSNCHK-EXT.
           EXIT.
      *
      *---- (NYUIN.15) LINE ADD END      -----------------------------
      *
      *****************************************************************
      *     入院略称マスタを主キーより検索処理
      *****************************************************************
       2001-SEL-NYUINRYAKUSHO-SEC    SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-NYUINRYAKUSHO-EXT
           END-IF
      *
      *
           MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
      *
           MOVE    ORC-DBYMD           TO  NYURYAKUSHO-YUKOSTYMD
                                           NYURYAKUSHO-YUKOEDYMD
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           .
       2001-SEL-NYUINRYAKUSHO-EXT.
           EXIT.
      *
      *****************************************************************
      *     　　　入力セット編集検索処理
      *****************************************************************
       2001-SEL-INPUTCD-SEC      SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-INPUTCD-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  INPUTCD-REC
      *    入力コードの文字数を確定
      *  1.
           IF      ORC-DBPATH            =   "INPUTCD-KEY"
             MOVE    ICD-INPUTCD         TO  WRK-INPUTCD
             MOVE    ZERO                TO  WRK-MOJISU
             PERFORM VARYING     IDX     FROM    20  BY  -1
                     UNTIL       IDX     <       1
                 IF      WRK-INPUTCD(IDX:1)  NOT =   SPACE
                     MOVE    IDX                 TO  WRK-MOJISU 
                     MOVE    1                   TO  IDX
                 END-IF  
             END-PERFORM
             IF      WRK-MOJISU          <   20
                 COMPUTE IDX                 =   WRK-MOJISU  +  1
                 MOVE    "%"                 TO  ICD-INPUTCD(IDX:1)
             END-IF
           END-IF
      *  2.
           IF      ORC-DBPATH            =   "INPUTCD-KEY3"
             MOVE    ICD-SRYCD           TO  WRK-INPUTCD
             MOVE    ZERO                TO  WRK-MOJISU
             PERFORM VARYING     IDX     FROM    20  BY  -1
                     UNTIL       IDX     <       1
                 IF      WRK-INPUTCD(IDX:1)  NOT =   SPACE
                     MOVE    IDX                 TO  WRK-MOJISU 
                     MOVE    1                   TO  IDX
                 END-IF  
             END-PERFORM
      *
             IF      WRK-MOJISU          <   20
                 COMPUTE IDX                 =   WRK-MOJISU  +  1
                 MOVE    "%"                 TO  ICD-SRYCD(IDX:1)
             END-IF
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           .
       2001-SEL-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *     患者番号編集処理
      *****************************************************************
       2001-SEL-PTNUM-SEC             SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-PTNUM-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  PTNUM-REC
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    20   BY  -1
                   UNTIL       IDX     <       1
               IF      PTNUM-PTNUM(IDX:1)  NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
      *
           IF  WRK-MOJISU              <   20
               COMPUTE IDX             =   WRK-MOJISU  +  1
               MOVE    "%"             TO  PTNUM-PTNUM(IDX:1)
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           .
       2001-SEL-PTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *     患者マスターを漢字氏名より検索処理
      *****************************************************************
       2001-SEL-PTINF-SEC             SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-PTINF-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  PTINF-REC
      *
           IF      ORC-DBPATH            =   "PTINF-KEY2"  OR
                                           "PTINF-KEY5"
                                       OR  "PTINF-KEY6"
               MOVE    ZERO                TO  WRK-MOJISU
               MOVE    SPACE               TO  WRK-NAME
               PERFORM VARYING     IDX     FROM    100   BY  -1
                       UNTIL       IDX     <       1
                   IF      PTINF-NAME(IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-MOJISU 
                       MOVE    1                   TO  IDX
                   END-IF  
               END-PERFORM
      *
               MOVE    PTINF-NAME(1:WRK-MOJISU)
                                           TO  WRK-NAME(1:WRK-MOJISU)
               COMPUTE IDX                 =   WRK-MOJISU  +  1
               MOVE    "%"                 TO  WRK-NAME(IDX:1)
               MOVE    WRK-NAME            TO  PTINF-NAME
           END-IF
      *
           IF      ORC-DBPATH            =   "PTINF-KEY3"  OR
                                           "PTINF-KEY5"
                                       OR  "PTINF-KEY6"
               MOVE    ZERO                TO  WRK-MOJISU
               MOVE    SPACE               TO  WRK-NAME
               PERFORM VARYING     IDX     FROM    100   BY  -1
                       UNTIL       IDX     <       1
                   IF      PTINF-KANANAME(IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-MOJISU 
                       MOVE    1                   TO  IDX
                   END-IF  
               END-PERFORM
      *
               MOVE    PTINF-KANANAME(1:WRK-MOJISU)
                                           TO  WRK-NAME(1:WRK-MOJISU)
               COMPUTE IDX                 =   WRK-MOJISU  +  1
               MOVE    "%"                 TO  WRK-NAME(IDX:1)
               MOVE    WRK-NAME            TO  PTINF-KANANAME
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           .
       2001-SEL-PTINF-EXT.
           EXIT.
      *****************************************************************
      *     　　　住所編集検索処理
      *****************************************************************
       2001-SEL-ADRS-SEC         SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-ADRS-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  ADRS-REC
      *    地方公共団体文字数を確定
           MOVE    ZERO                TO  WRK-MOJISU
           IF      ORC-DBPATH          =   "ADRS-KEY3"   OR
                                           "ADRS-KEY4"   OR
      *----(01.01.151)-ADD-START---
                                           "ADRS-KEY6"   OR
      *----(01.01.151)-ADD-END-----
                                           "ADRS-KEY5"
               IF      ADRS-LPUBCD     NOT =   SPACE
                   PERFORM VARYING     IDX     FROM    5   BY  -1
                           UNTIL       IDX     <       1
                       IF      ADRS-LPUBCD(IDX:1)  NOT =   SPACE
                           MOVE    IDX     TO  WRK-MOJISU 
                           MOVE    1       TO  IDX
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    1               TO  WRK-MOJISU
               END-IF
               IF    WRK-MOJISU            <   5
                   COMPUTE IDX             =   WRK-MOJISU  +  1
                   MOVE    "%"             TO  ADRS-LPUBCD(IDX:1)
                END-IF
           END-IF
      *    郵便番号の文字数を確定
           MOVE    ZERO                TO  WRK-MOJISU
           IF      ORC-DBPATH          =   "ADRS-KEY3"   OR
                                           "ADRS-KEY4"   OR
      *----(01.01.151)-ADD-START---
                                           "ADRS-KEY6"   OR
      *----(01.01.151)-ADD-END-----
                                           "ADRS-KEY5"
               IF      ADRS-POST     NOT =   SPACE
                   PERFORM VARYING     IDX     FROM    7   BY  -1
                           UNTIL       IDX     <       1
                       IF      ADRS-POST(IDX:1)  NOT =   SPACE
                           MOVE    IDX     TO  WRK-MOJISU 
                           MOVE    1       TO  IDX
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    1               TO  WRK-MOJISU
               END-IF
               IF    WRK-MOJISU            <   7
                   COMPUTE IDX             =   WRK-MOJISU  +  1
                   MOVE    "%"             TO  ADRS-POST(IDX:1)
                END-IF
           END-IF
      *
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           .
       2001-SEL-ADRS-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタを名称等により検索処理
      *****************************************************************
       2001-SEL-TENSU-REC          SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-TENSU-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
           MOVE    ORC-DBYMD             TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    100  BY  -1
                   UNTIL       IDX     <       1
               IF      TNS-NAME(IDX:1)  NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF  
           END-PERFORM
      *
      *    検索名称編集
           IF      TNS-NAME            NOT =   SPACE
                   PERFORM 20011-NAME-SET-SEC
           END-IF
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           .
       2001-SEL-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *     検索名称編集処理
      *****************************************************************
       20011-NAME-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-ICHI1
           MOVE    ZERO                TO  WRK-ICHI2
           MOVE    ZERO                TO  WRK-MOJISU2
      *
           IF      TNS-NAME(1:2)       =   "＊"
               MOVE    1                   TO  WRK-ICHI1
           END-IF
           IF      WRK-MOJISU          =   1
               CONTINUE
           ELSE
               COMPUTE WRK-MOJISU2         =   WRK-MOJISU      -   1
               IF      TNS-NAME(WRK-MOJISU2:2)     =   "＊"
                   MOVE    1                   TO  WRK-ICHI2
               END-IF
           END-IF
      *
           EVALUATE    TRUE
      *        部分一致
               WHEN    (WRK-ICHI1           =   1    )  AND
                       (WRK-ICHI2           =   1    )
                   COMPUTE WRK-MOJISU2         =   WRK-MOJISU    -   4
                   MOVE    "%"                 TO  TNS-NAME(1:1)
                   MOVE    TNS-NAME(3:WRK-MOJISU2)
                                               TO  TNS-NAME
                                                   (2:WRK-MOJISU2)
                   COMPUTE IDX                 =   WRK-MOJISU2   +  2
                   MOVE    "%"                 TO  TNS-NAME(IDX:1)
      *--- TAKE ADD ST 
                   COMPUTE IDX                 =   IDX  +  1 
                   MOVE    SPACE               TO  TNS-NAME(IDX:2)
      *--- TAKE ADD ED 
      *        後方一致
               WHEN    (WRK-ICHI1           =   1    )  AND
                       (WRK-ICHI2           =   ZERO )
                   COMPUTE WRK-MOJISU2         =   WRK-MOJISU    -   2
                   MOVE    "%"                 TO  TNS-NAME(1:1)
                   MOVE    TNS-NAME(3:WRK-MOJISU2)
                                           TO  TNS-NAME(2:WRK-MOJISU2)
      *--- TAKE ADD ST 
      *            COMPUTE IDX             =   200 - (WRK-MOJISU2  +  2)
                   COMPUTE WRK-MOJISU2     =   WRK-MOJISU2  +  2 
      *            MOVE    SPACE           TO TNS-NAME(WRK-MOJISU2:IDX)
                   MOVE    SPACE           TO  TNS-NAME(WRK-MOJISU2:2)
      *--- TAKE ADD ED 
      *        前方一致
               WHEN    OTHER
                   MOVE    TNS-NAME(1:WRK-MOJISU)
                                              TO TNS-NAME(1:WRK-MOJISU)
                   COMPUTE IDX                 =   WRK-MOJISU  +  1
                   MOVE    "%"                 TO  TNS-NAME(IDX:1)
      *--- TAKE ADD ST 
                   COMPUTE IDX                 =   IDX  +  1 
                   MOVE    SPACE               TO  TNS-NAME(IDX:2)
      *--- TAKE ADD ED 
           END-EVALUATE
           MOVE    TNS-NAME                    TO  TNS-KANANAME
           .
       20011-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタテーブル編集処理
      *****************************************************************
       2001-SEL-PARA-SEC            SECTION.
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-PARA-EXT
           END-IF
      *
      *    ファイル名の文字数を確定
           MOVE    MCPDATA-REC         TO  PARA-REC
           MOVE    PARA-FILEMEI        TO  WRK-FILEMEI
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    20  BY  -1
                   UNTIL       IDX     <       1
               IF      WRK-FILEMEI(IDX:1)  NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF  
           END-PERFORM
      *
           COMPUTE IDX                 =   WRK-MOJISU  +  1
           MOVE    "%"                 TO  PARA-FILEMEI(IDX:1)
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
      *
           .
       2001-SEL-PARA-EXT.
           EXIT. 
      *
      *****************************************************************
      *     自院病名マスタを病名より検索処理
      *****************************************************************
       2001-SEL-USERBYOMEI-SEC         SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-USERBYOMEI-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  USERBYOMEI-REC
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    160  BY  -1
                   UNTIL       IDX     <       1
               IF      USBYO-BYOMEI(IDX:1)  NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF  
           END-PERFORM
      *
      *    検索名称編集
           MOVE    SPACE               TO  WRK-BYOMEI
           MOVE    "%"                 TO  WRK-BYOMEI(1:1)
           MOVE    USBYO-BYOMEI(1:WRK-MOJISU)
                                       TO  WRK-BYOMEI(2:WRK-MOJISU)
           COMPUTE IDX                 =   WRK-MOJISU   +  2
           MOVE    "%"                 TO  WRK-BYOMEI(IDX:1)
           MOVE    SPACE               TO  USBYO-BYOMEI
           MOVE    WRK-BYOMEI          TO  USBYO-BYOMEI
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
      *
           .
       2001-SEL-USERBYOMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     請求管理マスタの保険者番号が初期値のときの編集処理
      *****************************************************************
       2001-SEL-SEIKYU-SEC             SECTION.
      *
           IF      MCP-FUNC            =   "DBFETCH" OR
                                           "DBCLOSECURSOR" OR
                                           "DBINSERT"
                   GO                  TO  2001-SEL-SEIKYU-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  RECE10-REC
           IF      RECE10-XXEDHKNJANUM =   SPACE
               MOVE    ALL "9"         TO  RECE10-XXEDHKNJANUM
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
      *
           .
       2001-SEL-SEIKYU-EXT.
           EXIT.

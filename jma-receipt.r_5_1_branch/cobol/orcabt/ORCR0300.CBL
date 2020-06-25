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
       PROGRAM-ID.             ORCR0300.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト電算ファイル作成（ＣＳＶ形式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/18    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03/07/18  主病名編集フラグによって
      *                                     主傷病名コードを変更する
      *  01.00.02    NACL-藤原    03/08/01  コメントの編集の訂正
      *  01.00.03    NACL-藤原    03/08/06  用法は出力しないようにする
      *                                     証明書番号の省略値の変更
      *                                     画像診断の点数の変更
      *                                     薬剤・器材の剤点数が０のとき
      *                                     摘要欄に印刷しないように修正
      *  01.00.04    NACL-伊藤    03/08/13  指導料・在宅料摘要欄編集
      *                                     加算点数でも告示区分１により
      *                                     基本点数剤とする
      *  01.00.05    NACL-藤原    03/08/17  廃止年月日の取扱
      *  01.00.06    NACL-藤原    03/08/20  レセプト番号の編集を止める
      *  01.00.07    NACL-藤原    03/08/22  画像診断の乳幼児加算の点数の修正
      *                                     薬剤料逓減を出力する
      *                                     未コード化傷病名コードのとき
      *                                     修飾語コードの編集をしない
      *  01.00.08    NACL-多々納  03/08/25  コメントの値を全角編集へ修正
      *  01.00.09    NACL-藤原    03/09/03  公費の負担者番号・受給者番号
      *                                     の全角チェック追加
      *  01.00.10    NACL-藤原    03/09/22  再診のときの時間外の編集
      *  01.00.11    NACL-藤原    03/09/30  酸素の単価の編集
      *  01.00.12    NACL-藤原    03/10/10  コメントの名称編集の修正
      *  01.00.13    NACL-藤原    03/10/15  診療年月の編集の修正
      *  01.00.14    NACL-藤原    03/10/22  摘要情報の編集の修正
      *  01.00.15    NACL-藤原    03/10/27  テスト患者を対象としない
      *  01.00.16    NACL-藤原    03/11/12  時間加算の追加
      *  01.00.17    NACL-藤原    03/11/17  傷病名データファイル名の変更
      *  01.00.18    NACL-藤原    03/11/19  老人外来金額の項目変更
      *  01.00.19    NACL-藤原    03/11/20  給付割合の編集の変更
      *                                     その他材料の単価の編集
      *                                     老人減免区分コードの編集
      *                                     医薬品・特定器材の出力順
      *  01.00.20    NACL-藤原    03/11/26  生体検査の加算について
      *  01.00.21    NACL-藤原    03/12/11  診療識別が指導（１３）
      *                                     在宅（１４）のときコメントは
      *                                     最後に記載する
      *  01.00.22    NACL-藤原    03/12/17  特定器材の単位コード編集
      *  01.00.23    NACL-藤原    03/12/19  病床数を一般病床数から編集
      *                                     山形の地方公費対応（国保）
      *  01.00.24    NACL-藤原    04/01/19  病床数の記載の修正
      *  01.00.25    NACL-藤原    04/01/20  老人減免区分コードの修正
      *  01.00.26    NACL-藤原    04/02/01  病名コード２１に変更
      *  01.00.27    NACL-藤原    04/02/05  特記事項のカスタマイズ項目
      *                                     の編集
      *  01.00.28    NACL-藤原    04/03/02  出力対象のコメントを追加
      *  01.00.29    NACL-藤原    04/03/18  画像診断の出力順の修正
      *  01.00.30    NACL-藤原    04/03/19  ５１公費のとき実施機関番号「６０２」
      *                                     のときのみ公費負担額を記録する
      *  01.00.31    NACL-藤原    04/04/05  病名コード９９９９９９９読み飛ばす
      *  01.00.32    NACL-藤原    04/04/06  保険履歴に保険者番号を追加
      *
      *  ２００４年４月仕様変更
      *  01.01.01    NACL-藤原    04/04/06  手術歴の記載
      *                                     診療科コード（レセプト共通）・
      *                                     商品名及び規格名及びサイズ
      *                                     （特定器材）の追加
      *  01.01.02    NACL-藤原    04/04/22  画像診断の加算が複数ある場合の
      *                                     出力の修正
      *  01.01.03    NACL-藤原    04/04/26  乳幼児の時間加算の追加
      *  01.01.04    NACL-藤原    04/04/27  ユーザーコメントの追加
      *  01.01.05    NACL-藤原    04/05/11  診療識別が画像診断（７０）の
      *                                     ときコメントは先頭に記載する
      *  01.01.06    NACL-藤原    03/08/13  指導料・在宅料摘要欄編集
      *                                     加算点数でも告示区分１により
      *                                     剤回数を編集する
      *  01.01.07    NACL-小豆沢  04/05/19  ＤＢ書き込み時のコミット追加
      *  01.01.08    NACL-藤原    04/05/21  指導料・在宅料のとき
      *                                     加算点数で告示区分１が７の
      *                                     ときの点数・回数の編集
      *  01.01.09    NACL-太田    04/05/21  処理高速化対応（保険組合せ）
      *  01.01.10    NACL-太田    04/05/25  処理高速化対応（点数マスタ）
      *
      *  01.02.01    NACL-藤原    04/05/26  RECE05レイアウト変更
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.02.02    NACL-藤原    04/06/17  症状詳記をデータにも編集する
      *                                     01:保険履歴、手術歴
      *                                     99:レセコメント
      *  01.02.03    NACL-藤原    04/08/05  神奈川対応
      *                                     地方公費のとき、負担者番号・
      *                                     受給者番号が空白のとき公費
      *                                     レコードを出力しない
      *  01.02.04    NACL-藤原    04/08/27  一部負担金・食事療養費標準負
      *                                     担額区分の編集を追加
      *  01.02.05    NACL-藤原    04/09/29  福岡対応
      *                                     地方公費のとき、４１老人以外
      *                                     はレセ電データを作成しない
      *  01.02.06    NACL-藤原    04/10/18  氏名欄編集方法の変更
      *                                     職務上の事由の編集条件に
      *                                     共済組合の下船３ヶ月を追加
      *                                     医療保険一部負担金額の編集追加 
      *                                     パラメタの追加
      *  01.02.07    NACL-藤原    04/11/16  特定器材レコードのフィルムと
      *                                     器材の点数・回数を別々に記載
      *  01.02.08    NACL-太田    04/12/08  点数フラグの初期化追加
      *  01.02.09    NACL-藤原    04/12/20  職務上の事由の編集方法の修正
      *  01.02.10    NACL-藤原    05/01/17  愛知県対応
      *                                     地方公費のとき、法別番号を判
      *                                     定して受給者番号にゼロを編集
      *                                     する
      *  01.02.11    NACL-藤原    05/01/18  特定器材レコードの点数の編集
      *                                     方法を修正
      *  01.02.12    NACL-藤原    05/01/25  酸素の計算式を対象としないよ
      *                                     うにする
      *  01.02.13    NACL-小豆沢  05/02/10  負担区分の判定に公費ＩＤを追加
      *  01.02.14    NACL-藤原    05/02/23  注加算による加算項目の記載
      *                                     （手術・麻酔・検査・処置・その他）
      *                                     加算項目であるが基本項目となって
      *                                     いる項目がある場合の記載順
      *                                     超音波内視鏡加算を逓減の後に記載
      *  01.02.15    NACL-藤原    05/02/05  公費の負担者番号・受給者番号
      *                                     のカスタマイズ項目の編集
      *  01.02.16    NACL-藤原    05/03/18  レセプト特記事項編集情報によ
      *                                     るレセプト編集
      *                                     公費の負担者番号・受給者番号
      *                                     のカスタマイズ項目を中止
      *  01.02.17    NACL-藤原    05/04/04  コメント編集行を変更
      *  01.02.18    NACL-藤原    05/04/04   01.02.03の神奈川対応
      *                                     レセプト特記事項編集情報対応
      *                                     により中止する
      *  01.02.19    NACL-小豆沢  05/04/20  手術の５０％併施加算を告示識別
      *                                     区分を手技の扱いに置き換え
      *  01.02.20    NACL-藤原    05/05/09  改定通信医１６０３９号
      *                                     「主」の記録についてより
      *                                    「0000998」を「0000999」に変更
      *
      *  02.06.01    NACL-藤原    05/06/27  症状詳記の編集方法の訂正
      *  02.06.02    NACL-藤原    05/07/20  レセコメントのコメントの長さ
      *                                     変更による対応
      *  02.06.03    NACL-藤原    05/07/28  レセプト特記事項編集情報の
      *                                     負担者番号に条件を追加
      *  02.06.04    NACL-藤原    05/08/22  検体検査コメントの追加
      *  02.06.05    NACL-小豆沢  05/09/05  福岡老人公費２割の保険番号変更
      *  02.06.06    NACL-藤原    05/09/12  一部負担金のカスタマイズ項目
      *                                     の編集
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/12/12  保険履歴を年月日記載へ変更
      *  02.07.03    NACL-藤原    06/01/05  山形県のレセプト様式変更対応
      *                                     平成１８年１月分からレセ電に
      *                                     記載
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  ２００６年４月改正対応
      *  02.09.01    NACL-藤原    06/04/20  外来迅速検体検査加算対応
      *                                     検査等実施判断グループの追加
      *                                     再診のときの時間外の追加
      *  02.09.02    NACL-藤原    06/04/25  主科のときの症状詳記データの
      *                                     キーの設定を修正
      *  02.09.03    NACL-藤原    06/05/08  改定通信医１６０４９号
      *                                     日計表、症状詳記レコード対応
      *  02.09.04    NACL-藤原    06/05/15  加算等がある場合の
      *                                     外来迅速検体検査加算対応
      *  02.09.05    NACL-藤原    06/05/16  改定通信医１６０４９号
      *                                     症状詳記レコード対応
      *                                     レセ電データを２５００に変更
      *                                     改行までを１レコードとする
      *  3.0.0
      *  03.00.01    NACL-藤原    06/05/15  入退院履歴記載対応
      *  03.00.02    NACL-藤原    06/06/13  レセコメントの症状詳記区分に
      *                                     よる記載対応
      *  03.00.03    NACL-藤原    06/06/14  外来迅速検体検査加算を算定した
      *                                     検査にコメントがある場合の記載
      *  03.00.04    NACL-藤原    06/07/10  外来迅速検体検査加算を算定した
      *                                     検査にあるユーザーコメントを
      *                                     フリーコメントに置き換える
      *  03.00.05    NACL-藤原    06/07/11  前期高齢者の一部負担金の記載
      *  03.00.06    NACL-藤原    06/07/18  外来迅速検体検査加算にコメント
      *                                     がある場合の記載
      *
      *  03.02.01    NACL-藤原    06/07/31  国保主科対応
      *  03.02.02    NACL-藤原    06/08/23  船員保険のとき保険限定がある
      *                                     病名の記載の修正
      *  03.02.03    NACL-藤原    06/09/04  公費レコードの負担金額の編集
      *                                     方法の修正
      *  03.02.04    NACL-藤原    06/10/02  全角空白のみのコメントを出力
      *                                     しないようにする
      *
      *  03.03.01    NACL-藤原    06/09/29  検査の判断料の記載方法の変更
      *                                     検査等実施判断の追加
      *  03.03.02    NACL-藤原    06/10/03  酸素ボンベ併用時の補正率の記
      *                                     載方法の修正
      *  03.03.03    NACL-藤原    06/10/04  同日に複数の外来迅速検体検査
      *                                     加算を算定したときの記載
      *  03.03.04    NACL-藤原    06/10/10  ２００６年１０月改正対応
      *                                     一部負担金・食事療養費・生活
      *                                     療養日標準負担額区分コード
      *                                     職務上の事由の判定条件追加
      *                                     徴収猶予及び減免の記録
      *  03.03.05    NACL-藤原    06/10/18  分点数が生じる場合の公費レコ
      *                                     ードの一部負担金の記録対応
      *  03.03.06    NACL-藤原    06/10/23  外来迅速検体検査加算の検査日
      *                                     の記録をやめる
      *  03.03.07    NACL-藤原    06/11/08  きざみ値による数量の記録の条件
      *                                     の修正
      *  03.03.08    NACL-藤原    06/11/14  老人減免対応
      *  03.03.11    NACL-藤原    06/11/17  福岡県対応
      *                                     ２００６年１０月以降全て対象
      *                                     とする
      *  03.03.12    NACL-藤原    06/11/17  主科未設定の場合でも科別印刷
      *                                     設定のときはレセ電を記録する
      *  03.03.13    NACL-藤原    06/12/19  基準不適合低減区分による記載
      *                                     順の修正
      *  03.04.01    NACL-門脇    07/03/22  公費給付対象一部負担金編集修正
      *  03.04.02    NACL-藤原    07/03/27  特定器材の記載方法の変更
      *  ２００７年４月改正
      *  03.04.03    NACL-門脇    07/04/01  現物給付対応
      *  03.04.04    NACL-藤原    07/04/13  症状詳記区分コード追加に伴う対応
      *  03.04.05    NACL-藤原    07/04/26  注加算による加算項目の記載
      *                                     注射（その他）
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  03.05.03    NACL-藤原    07/06/28  空のコメントコードの記載をやめる
      *  03.05.04    NACL-藤原    07/07/10  特定器材再編集用の器材点数追加
      *  03.05.05    NACL-藤原    07/09/04  宮崎県「公費９０」対応
      *                                     ２００７年９月以降のみ対象 
      *  03.05.06    NACL-藤原    07/09/11  新潟県中越沖地震対応
      *                                     ２００７年９月〜１２月対象
      *
      *  04.01.01    NACL-藤原    07/11/26  特定器材の商品名及び規格又は
      *                                     サイズ記載対応
      *  04.01.02    NACL-藤原    08/01/10  レセプト特記事項編集情報
      *                                     適用欄コメント対応
      *  04.01.03    NACL-藤原    08/02/12  宮崎県「公費９０」対応
      *                                     通常の記載方法に戻す
      *
      *  04.02.01    NACL-藤原    08/02/20  三種公費併用特例時の負担区分
      *                                     対応
      *  04.02.02    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.03    NACL-藤原    08/03/10  特定器材名称の記録方法の修正
      *  04.02.04    NACL-門脇    08/03/17  宮崎県地方公費（１９０）の対応
      *  04.02.05    NACL-藤原    08/03/25  再診（夜間・早朝等）加算の追加
      *                                     広域連合対応
      *  04.02.06    NACL-藤原    08/04/10  平成２０年４月改正に伴う記録
      *                                     条件仕様の変更
      *  04.02.07    NACL-藤原    08/04/24  続紙の変更
      *                                     ７０歳代前半の特例措置対象者
      *                                     対応 
      *                                     国保のみ２割の記載
      *  04.02.08    NACL-門脇    08/06/12  愛媛県地方公費対応
      *                                     （一部負担金再掲欄編集）
      *  04.02.09    NACL-門脇    08/06/25  全国公費２種の一部負担金等記載対応
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *  04.03.02    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.03    NACL-藤原    08/08/05  患者登録特記事項入力対応
      *  04.03.04    NACL-藤原    08/07/23  宮城県、岩手県地震対応
      *                                     ２００８年６月〜対象 
      *  04.03.05    NACL-門脇    08/12/01  在宅（院外分）編集修正
      *  04.03.06    NACL-藤原    09/02/05  院外処方の直後の症状詳記の記
      *                                     録の修正
      *
      *  04.04.01    NACL-門脇    09/03/03  地方公費のレセ記載対応（レセ電含む）
      *  04.04.02    NACL-藤原    09/04/21  注加算による加算項目の記載
      *                                     基本項目が複数ある場合は基本
      *                                     項目毎に加算項目の記載を行なう
      *                                     告示等識別区分２、通則加算対
      *                                     象外区分の追加
      *  04.04.03    NACL-藤原    09/05/29  コメントが基本項目より先にある
      *                                     場合の記載の修正
      *  04.04.04    NACL-藤原    09/05/22  通則加算項目の加減算コードの
      *                                     基本項目単位の記録
      *  04.04.05    NACL-藤原    09/06/10  １３医学管理、１４在宅の点数、
      *                                     回数の記載の修正
      *  04.04.06    NACL-藤原    09/08/17  「平成２１年７月中国・九州
      *                                     北部豪雨」対応
      *                                     ２００９年７月〜対象 
      *  04.04.07    NACL-藤原    09/10/14  １３医学管理・１４在宅で
      *                                     告示区分１が基本項目以外のとき
      *                                     点数・回数の記載を行う
      *  04.04.08    NACL-藤原    09/12/07  注加算による加算項目の記載
      *                                     時間外加算区分による加算項目
      *                                     の記載の有無
      *                                     時間外加算区分の追加
      *  04.04.09    NACL-藤原    10/01/08  処置で基本項目の時間加算区分
      *                                     が算定不可の場合、時間加算の
      *                                     通則加算項目の記載の判定
      *  04.04.10    NACL-門脇    10/04/30  再掲欄の記録条件修正
      *
      *  04.05.01    NACL-藤原    09/06/10  摘要欄下部へのコメント記載対応
      *                                     診療種別区分の追加
      *  04.05.02    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.03    NACL-藤原    09/09/18  「平成２１年台風第９号」対応
      *                                     兵庫県・岡山県
      *                                     ２００９年８月〜２０１０年１月対象 
      *  04.05.04    NACL-藤原    09/10/30  拡張漢字対応
      *  04.05.05    NACL-藤原    09/11/27  レセ電コメント拡張対応
      *                                     コメント  
      *  04.05.06    NACL-藤原    09/11/25  レセ電コメント拡張対応
      *                                     傷病名レコードの補足コメント
      *  04.05.07    NACL-門脇    10/02/08  地方公費の全国公費併用分請求点
      *                                     集計対応
      *  04.05.08    NACL-藤原    10/02/18  レセ電コメント拡張対応
      *                                     傷病名レコードの補足コメント
      *                                     を中止
      *  04.05.09    NACL-藤原    10/03/03  平成２２年４月改正対応
      *                                     平成２２年４月分以降の処理を
      *                                     行わない
      *  04.05.10    NACL-藤原    10/03/23  平成２２年４月改正対応
      *                                     請求に係わる記録条件仕様の
      *                                     変更に伴う修正
      *                                     平成２２年４月分以降の処理を
      *                                     行うようにする
      *  04.05.11    NACL-藤原    10/04/12  特定器材レコードの使用量、
      *                                     単位コードの記録方法を修正
      *  04.05.12    NACL-藤原    10/04/14  平成２２年４月改正対応
      *                                     請求に係わる記録条件仕様の
      *                                     変更に伴う修正
      *                                     診療科２、３を追加
      *  04.05.13    NACL-藤原    10/04/15  レセ電通信医２２００５号
      *                                     アナログ撮影とデジタル撮影を
      *                                     同時に行った場合の記録方法
      *  04.05.14    NACL-藤原    10/04/23  画像診断の加算項目の記録方法
      *                                     の修正
      *  04.05.15    NACL-藤原    10/05/10  注加算による加算項目の記載
      *                                     画像診断
      *                                     告示等識別区分２の条件追加 
      *  04.05.16    NACL-藤原    10/05/19  公費の組合せによる負担区分
      *                                     対応
      *  04.05.17    NACL-藤原    10/08/11  「平成２２年７月中国地方豪雨」対応
      *                                     広島県・山口県
      *                                     ２０１０年７月〜２０１０年１２月対象
      *  04.05.18    NACL-藤原    10/11/16  「鹿児島県奄美市等における局地的大雨」対応
      *                                     鹿児島県
      *                                     ２０１０年１０月〜２０１１年３月対象 
      *  04.05.19    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.20    NACL-藤原    11/04/20  災害対応
      *                                     災害該当レセプトのレセ電デー
      *                                     タ作成対応
      *  04.05.21    NACL-藤原    11/07/12  愛知県対応
      *                                     法別番号を判定して受給者番号
      *                                     にゼロを編集するのを中止
      *  04.05.22    NACL-藤原    11/08/26  レセ電データの記号番号区切り
      *                                     文字設定対応
      *                                     対象文字コード追加
      *  04.05.23    NACL-藤原    11/08/25  公費単独対応
      *                                     保険履歴を最大１００に変更
      *  04.05.24    NACL-門脇    11/12/13  災害に関するコメント修正
      *  04.05.25    NACL-藤原    11/12/16  東大阪市国保の減免対応
      *  04.05.26    NACL-藤原    12/02/08  算定日情報に係る記録条件仕様
      *                                     の変更
      *  04.05.27    NACL-門脇    12/04/19  一部負担金・食事療養費標準負担
      *                                     額区分の記録変更
      *  04.05.28    NACL-藤原    12/06/25  複数の基本項目をまとめて記録す
      *                                     る場合、途中にコメントがあった
      *                                     ときの注加算の記録順の修正 
      *
      *  04.06.01    NACL-藤原    10/10/04  レセプトコメント文字数増対応
      *  04.06.02    NACL-藤原    10/11/04  補足コメント対応
      *  04.06.03    NACL-藤原    10/11/20  日付情報の記録を行う
      *  04.06.04    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.05    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *  04.06.06    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.07    NACL-藤原    11/06/02  再診（時間外）の記録方法の修正
      *  04.06.08    NACL-藤原    11/06/10  基本項目でデータ区分２の記録
      *  04.06.09    NACL-藤原    11/09/29  基本項目でデータ区分２のデー
      *                                     タのみの診療区分の記録
      *  04.06.10    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.06.11    NACL-藤原    13/04/05  コメントの記録方法の変更
      *  04.06.12    NACL-藤原    13/05/02  氏名の外字チェックに条件追加
      *  04.06.13    NACL-藤原    13/06/28  ００８２で始まるコメントの
      *                                     記録方法の修正
      *
      *  04.07.01    NACL-藤原    11/06/02  入退院履歴記載対応
      *                                     「2自費分は含まない」を追加
      *  04.07.02    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *  04.07.03    NACL-門脇    13/09/11  自賠責（第三者行為）対応
      *  04.07.04    NACL-門脇    13/12/09  医療情報連携対応
      *  04.07.05    NACL-門脇    14/02/03  第三者行為対応（院外処方のみの
      *                                     保険組み合わせの病名記載対応）
      *  04.07.06    NACL-藤原    14/03/30  平成２６年４月改正対応
      *                                     使用量、単価の小数部の記録方
      *                                     法の変更
      *                                     漢字モードの継続するスペース
      *                                     の記録方法の変更
      *  04.07.07    NACL-門脇    14/09/01  平成２６年１０月改正対応
      *  04.07.08    NACL-藤原    14/09/24  全角の負担者番号、受給者番号の
      *                                     記録対応
      *  04.07.09    NACL-門脇    15/02/09  第三者行為のコメント編集条件修正
      *  04.07.10    NACL-藤原    15/02/12  告示等識別区分による記録順を修正
      *                                    （通則加算対象外）
      *  04.07.11    NACL-藤原    15/02/18  医療情報連携対応
      *                                     自費レセプトを対象とする
      *  04.07.12    NACL-門脇    15/03/17  請求点数０点公費レセ記載対応
      *  04.07.13    NACL-門脇    15/03/16  平成２７年４月改正対応
      *  04.07.14    NACL-藤原    15/03/23  施設基準不適合減算の対応
      *                                     通則加算項目（加減算コード）
      *                                     のときの記録方法
      *  04.07.17    NACL-藤原    15/04/16  注加算コードが１４１９の加算
      *                                     項目の記録方法の修正
      *  04.07.18    NACL-藤原    15/06/10  保険請求対象外傷病名コード対応
      *
      *  04.08.01    NACL-藤原    14/06/02  請求情報への更新日設定
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-伊藤    15/06/23  医療情報連携
      *                                     後発品変更不可（処方単位）
      *  04.08.04    NACL-藤原    15/09/01  保険履歴の記載対応
      *  04.08.05    NACL-門脇    16/02/08  RECE02の予備項目使用分について、
      *                                     個々に項目を設ける事に伴う修正。
      *                                     第３第４公費の括弧再掲記載対応
      *  04.08.06    NACL-藤原    16/04/01  平成２８年４月改正対応
      *  04.08.07    NACL-藤原    16/06/20  診療行為データ用テーブル
      *                                    （加算項目扱いの基本項目）を増やす
      *  04.08.08    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.09    NACL-門脇    16/08/18  法別５１の一部負担金の記録修正
      *  04.08.10    NACL-門脇    16/09/20  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.11    NACL-門脇    17/03/22  医療情報連携
      *                                     後発品変更可（処方単位）
      *  04.08.12    NACL-門脇    17/03/27  請求金額０円レセプト作成対応
      *  04.08.13    NACL-門脇    17/07/03  （災１）等の記録を括弧なしで
      *                                     記録するよう変更
      *  04.08.14    NACL-門脇    17/08/28  平成２９年１０月改正
      *                                     （入院時生活療養の見直し）
      *  04.08.15    NACL-藤原    18/04/06  平成３０年４月改正対応
      *                                     カタカナ（氏名）、患者状態の追加
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-伊藤    17/12/20  診療情報システム対応
      *  05.00.03    NACL-藤原    19/03/25  元号改正対応
      *  05.00.04    NACL-藤原    20/04/01  記録条件仕様変更に伴う対応
      *                                     年月（年月日）の西暦記録 
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *
      *    レセプト明細書（病名）  
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *    レセプト０４データ
           SELECT  RECE041-FILE    ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE041-KEY
                                   FILE    STATUS  IS  STS-RECE04.
      *    レセプト０５データ
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
      *    関連コメントデータ
           SELECT  RECE055-FILE    ASSIGN  RECE055PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE055-KEY
                                   FILE    STATUS  IS  STS-RECE055.
      *
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    レセプト（県単用項目）データ
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
      *    入退院履歴情報
           SELECT  RECE12-FILE     ASSIGN  RECE12PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE12-KEY
                                   FILE    STATUS  IS  STS-RECE12.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
      *    レセプト明細書（病名）  
       FD  RECE03-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
           03  RECE031-SYUBYOFLG-X PIC X(01).
      *
      *    レセプト０４データ
       FD  RECE041-FILE.
       01  RECE041-REC.
           COPY    "CPRCF0041.INC".
      *
      *    レセプト０５データ
       FD  RECE05-FILE.
       01  RECE05-REC.
           COPY    "CPRCF005.INC".
      *
      *    関連コメントデータ
       FD  RECE055-FILE.
       01  RECE055-REC.
           COPY    "CPRCF0055.INC".
      *
      *    レセプト０９データ
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
      *    レセプト（県単用項目）データ
       FD  RECE11-FILE.
       01  RECE11-REC. 
           COPY    "CPRCF011.INC".
      *
       FD  RECE12-FILE.
       01  RECE12-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE68//
                                   BY         //RECE12//.
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE02PARA//.
      *
      *     レセプト明細ファイル（病名） 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE03PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE04PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE05PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE055PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE09PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE11PARA//.
      *
      *    入退院履歴情報ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE12PARA//.
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
           03  STS-RECE02              PIC X(02).
           03  STS-RECE03              PIC X(02).
           03  STS-RECE04              PIC X(02).
           03  STS-RECE05              PIC X(02).
           03  STS-RECE055             PIC X(02).
           03  STS-RECE09              PIC X(02).
           03  STS-RECE11              PIC X(02).
           03  STS-RECE12              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01). 
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE02              PIC 9(01).
           03  FLG-RECE03              PIC 9(01).
           03  FLG-RECE04              PIC 9(01).
           03  FLG-RECE05              PIC 9(01).
           03  FLG-RECE055             PIC 9(01).
           03  FLG-RECE09              PIC 9(01).
           03  FLG-RECE11              PIC 9(01).
           03  FLG-RECE12              PIC 9(01).
           03  FLG-OPECOM              PIC 9(01).
      *
           03  FLG-READ                PIC 9(01).
      *
           03  FLG-TEIGEN              PIC 9(01).
           03  FLG-YKZ20               PIC 9(01).
           03  FLG-TOTALTEN            PIC 9(01).
           03  FLG-KOKUJISKBKBN1       PIC 9(03).
           03  FLG-SRYKBN70-TEIGEN     PIC 9(01).
           03  FLG-KENSA-KASAN         PIC 9(01).
           03  FLG-KENSA-TEIGEN        PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-OPECOM-CHK          PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-KOUI                PIC 9(01).
           03  FLG-TOYAKUFLG           PIC 9(01).
           03  FLG-SUUJI               PIC 9(01).
           03  FLG-SISETUKIJYUN-GENZAN PIC 9(01).
           03  FLG-HENKOFUKA           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECE04              PIC 9(06).
           03  CNT-RECE09              PIC 9(06).
           03  CNT-COMMIT              PIC 9(03). 
      *
       01  INDEX-AREA.
           03  IDV                     PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(04). 
           03  IDY                     PIC 9(03). 
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDY3                    PIC 9(03).
           03  IDY4                    PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDXY                    PIC 9(03).
           03  IDXZ                    PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(04).
           03  IDW3                    PIC 9(04).
           03  IDW4                    PIC 9(04).
           03  IDW5                    PIC 9(04).
           03  IDZ1                    PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
           03  IDZ5                    PIC 9(03).
           03  IDZ6                    PIC 9(03).
           03  IDYY                    PIC 9(03).
           03  IDZZ                    PIC 9(03).
           03  IDWW                    PIC 9(03).
           03  IDVV                    PIC 9(03).
           03  IDV1                    PIC 9(03).
      *
           03  SYSKAN-IDX              PIC 9(03).
           03  IDX0                    PIC 9(03).
      *
       01  KEY-AREA                                VALUE   LOW-VALUE.
           03  KEY-NEW.
      *****    05  KEY-N-TEISYUTUSAKI  PIC 9(01).
               05  KEY-N-SRYKA         PIC X(02).
           03  KEY-OLD.
      *****    05  KEY-O-TEISYUTUSAKI  PIC 9(01).
               05  KEY-O-SRYKA         PIC X(02).      
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU         PIC 9(06).
           03  SUM-TOTALTEN         PIC 9(10).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  100.
               05  TBL-SYS1001         PIC  X(610).
     *
      *    一時領域
       01  WRK-AREA.
      *    パラメタエリア
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
               05  WRK-PARA-KOUIKIRECEKBN
                                       PIC X(01).
               05  WRK-PARA-CREYMD     PIC X(08).
               05  WRK-PARA-CREHMS     PIC X(08).
               05  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-OPECOM-MCP-TABLE
                                   PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME
                                   PIC X(64).
      *
           03  WRK-KIZAI-SRYCD         PIC X(09).
      *     右づめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.99999.
               05  WRK-NUM-R           REDEFINES   WRK-NUM.
                   07  WRK-NUM-R-1     PIC X(10).
                   07  FILLER          PIC X(01).
                   07  WRK-NUM-R-2     PIC X(05).
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
               05  WRK-MOJI-CHK        PIC 9(01).
      *
           03  WRK-CO-COMMENT      PIC X(76).
           03  WRK-CO-SRYCD        PIC X(09).
      *    傷病名レコード編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEICD        PIC X(07).
               05  WRK-RECECD-G.
                   07  WRK-RECECD      PIC X(04)   OCCURS  20.
      *    マルチボリューム識別情報     
           03  WRK-VOLNUM              PIC 9(06).
      *    給付割合 
           03  WRK-KYURATE             PIC 9(03).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).        
      *       
           03  WRK-RENNUM              PIC 9(04).
           03  WRK-COM-RENNUM          PIC 9(04).
      *
           03  WRK-RECE041-SRYKBN      PIC X(02).
           03  WRK-COM-FTNKBN          PIC X(01).
           03  WRK-TAIHI-FTNKBN        PIC X(01).
           03  WRK-SJ-KBN              PIC X(02).
           03  WRK-RECECOM-SJKBN       PIC X(02).
           03  WRK-SJ-CHK              PIC 9(01).
      *
           03  WRK-CHUKSNCD            PIC X(04).
           03  WRK-DISASTER            PIC 9(02).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU            PIC X(03).
      *    生保 
           03  SEHIDX                  PIC 9(01).
      *    精神 
           03  SESIDX                  PIC 9(01).
      *    結核 
           03  KEKIDX                  PIC 9(01).
      *
           03  IDX1                    PIC 9(01).     
           03  IDX2                    PIC 9(01).     
           03  IDX3                    PIC 9(01).
      *
      *    摘要レコード編集用
           03  WRK-ZAIWMAX             PIC 9(03).
           03  WRK-TBLMAX1             PIC 9(03).
           03  WRK-TBLMAX6             PIC 9(03).
           03  WRK-TBLMAX7             PIC 9(03).
           03  WRK-TBLMAX7-SRT         PIC 9(03).
           03  WRK-TBLMAX8             PIC 9(03).
           03  WRK-SURYO               PIC 9(08).
           03  WRK-TENSU               PIC 9(07).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-TANKA               PIC 9(08)V9(02).
           03  WRK-SRYSURYO            PIC 9(05)V9(05).
           03  WRK-FTNKBN-AREA.                            
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  4.
               05  WRK-SEHFLG          PIC 9(01).
           03  WRK-SRYCD1-MAX          PIC 9(03).           
           03  WRK-SRYCD71-MAX         PIC 9(03).           
           03  WRK-SRYCD72-MAX         PIC 9(03).           
           03  WRK-KOKUJISKBKBN1-KOUI-TEN
                                       PIC 9(09)V9(02). 
           03  WRK-RECE04-ZAIKAISU     PIC 9(04).    
           03  WRK-KENSASU             PIC 9(04).    
      *    時間外加算区分（基本項目）
           03  WRK-KIHON-TIMEKSNKBN    PIC 9(01).
      *    処置の加算項目の有無フラグ
           03  WRK-SRYKBN40-KOKUJISKBKBN1
                                       PIC 9(01).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-CNT                 PIC 9(10).
           03  WRK-CNT-X               REDEFINES   WRK-CNT
                                       PIC X(10).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-ACCTP-SYUTEN        PIC 9(07).
      *
           03 WRK-KOHFTNJANUM          PIC X(08).
           03 WRK-KOHJKYSNUM           PIC X(20).
      *
      *    災害用コメント
           03  WRK-DISASTER-COMMENT    PIC X(76).
      ***  03  WRK-FUTAN               PIC ZZZ,ZZZ,ZZ9. 
           03  WRK-FUTAN               PIC ZZZZZZZZ9.
           03  WRK-FUTAN2              PIC ZZZZZZZ9.
           03  WRK-FUTAN3              PIC ZZ,ZZZ,ZZ9.
           03  WRK-FUTAN4              PIC ZZZ,ZZZ,ZZ9.
           03  WRK-ADRS.
               05  WRK-ADRS-R      PIC X(76)  OCCURS  4.
      *
      *    区切り文字設定
           03  WRK-DELIMITER-AREA.
               05  WRK-DELIMITER-FLG   PIC X(01).
               05  WRK-DELIMITER-HKNJANUM-FLG
                                       PIC X(01).
               05  WRK-DELIMITER-MAX   PIC 9(02).
               05  WRK-DELIMITER-IN.
                   07  WRK-DELIMITER-IN-O
                                       PIC X(02)   OCCURS 30.
               05  WRK-DELIMITER-OUT.
                   07  WRK-DELIMITER-OUT-O
                                       PIC X(02)   OCCURS 30.
      *                 
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  100.  
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(04).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD.
                               13  WRK-HKN-TEKEDYY
                                               PIC X(08).
                               13  WRK-HKN-TEKEDMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(03).                
           03  WRK-HKN-RIREKI-X                PIC X(60).
           03  WRK-HKN-HKNJANUM                PIC X(16).
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).    
           03  WRK-HKN-FIL                     PIC X(02).
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).   
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  48.
           03  WRK-COMMENT-R               REDEFINES   WRK-COMMENT-G.
               05  WRK-COMMENT1                PIC X(100)
                                               OCCURS  24.
           03  WRK-COMM                        PIC X(2400).
           03  WRK-COMM1                       PIC X(2400).
           03  WRK-COL                         PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
      *
      *    手術歴編集
           03  WRK-OPECOM-HEN      PIC X(50).
      *                 
      *    入退院履歴編集
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-MSG              PIC X(12).
               05  WRK-NYUTAI-NYUINYMD         PIC X(20).
               05  WRK-NYUTAI-FL1              PIC X(02).
               05  WRK-NYUTAI-TAIINYMD         PIC X(20).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
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
      *    関連コメントテーブル
       01  TBL-RECE055-COMMENT-AREA.
           03  TBL-RECE055-OCC                 OCCURS  100.
               05  TBL-RECE055-SRYCD           PIC X(09).
               05  TBL-RECE055-NAME            PIC X(200).
           03  TBL-RECE055-MAX                 PIC 9(03).
      *
      *    医薬品・特定器材用テーブル（１医薬品 ２特定器材 ３特定器材並び変え用）
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-TABLE          OCCURS  3.
               05  TBL-ZAIW-REC        OCCURS 100.
                   07  TBL-ZAIW-KEY.
                       09  TBL-ZAIW-HOSPNUM         PIC 9(2).
                       09  TBL-ZAIW-SRYYM           PIC  9(06).
                       09  TBL-ZAIW-NYUGAIKBN       PIC  X(01).
                       09  TBL-ZAIW-PTID            PIC  9(10).
                       09  TBL-ZAIW-RECEKA          PIC  X(02).
                       09  TBL-ZAIW-TEISYUTUSAKI    PIC  9(01).
                       09  TBL-ZAIW-RECESYUBETU     PIC  9(04).
                       09  TBL-ZAIW-HKNJANUM        PIC  X(08).
                       09  TBL-ZAIW-HOJOKBN         PIC  X(01).
                       09  TBL-ZAIW-KOHID           PIC  9(10).
                       09  TBL-ZAIW-TEKSTYMD        PIC  X(08).
                       09  TBL-ZAIW-RECEIPT-KBN     PIC  X(10).                    
                       09  TBL-ZAIW-ZAINUM          PIC  9(08).
                       09  TBL-ZAIW-RENNUM          PIC  9(02).
                   07  TBL-ZAIW-SHORI-RENNUM        PIC  9(01).
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
      *                診療種別区分
                       09  TBL-ZAIW-SRYSYUKBN       PIC  X(03).
      *                数量
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
      *                回数
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード
                       09  TBL-ZAIW-TANICD          PIC  9(03).
      *                単位名称有効桁数
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
      *                単位名称
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
      *                点数
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
      *                明細請求フラグ
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
      *                名称入力番号
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
      *                点数欄集計先識別
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *                データ区分
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
      *                包括対象検査
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *                検査等実施判断
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
      *                検査等実施判断グループ
                       09  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
      *                各種番号等
                       09  TBL-ZAIW-CDKBNINF.
                           11  TBL-ZAIW-CDKBN-KBN   PIC  X(01).
                           11  TBL-ZAIW-CDKBN-SYO   PIC  9(01).
                           11  TBL-ZAIW-CDKBN-BU    PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                           11  TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                           11  TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
      *                告示等識別区分１
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
      *                告示等識別区分２
                       09  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
      *                通則加算対象外区分
                       09  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
      *                時間外加算区分
                       09  TBL-ZAIW-TIMEKSNKBN      PIC  9(01).
      *                きざみ値計算識別
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
      *                注加算コード
                       09  TBL-ZAIW-CHUKSNINF.
      *                    コード
                           11  TBL-ZAIW-CHUKSNCD    PIC  X(04).
      *                    通番
                           11  TBL-ZAIW-CHUKSNTUBAN PIC  X(01).
      *                検体検査コメント
                       09  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
      *                入力コメント
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(140).
      *            名称情報
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数
                       09  TBL-ZAIW-YUKOKETA        PIC  9(03).
      *                漢字名称
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *            データ識別(１：フィルム ２：特定機材)
                   07  TBL-ZAIW-DATSKB              PIC  9(01).
      *            特定器材種別１
                   07  TBL-ZAIW-TOKUKIZAISBT1       PIC  9(01).
      *            酸素等区分  
                   07  TBL-ZAIW-SANSOKBN            PIC  9(01).
      *            レセ電用特定器材商品名
                   07  TBL-ZAIW-RECEDENNAME         PIC  X(300).
      *
      *    診療行為・コメント用テーブル
       01  TBL-KOUI-AREA.
           03  TBL-KOUI-REC                     OCCURS  100.
               05  TBL-KOUI-KEY.
                   07  TBL-KOUI-HOSPNUM         PIC 9(2).
                   07  TBL-KOUI-SRYYM           PIC  9(06).
                   07  TBL-KOUI-NYUGAIKBN       PIC  X(01).
                   07  TBL-KOUI-PTID            PIC  9(10).
                   07  TBL-KOUI-RECEKA          PIC  X(02).
                   07  TBL-KOUI-TEISYUTUSAKI    PIC  9(01).
                   07  TBL-KOUI-RECESYUBETU     PIC  9(04).
                   07  TBL-KOUI-HKNJANUM        PIC  X(08).
                   07  TBL-KOUI-HOJOKBN         PIC  X(01).
                   07  TBL-KOUI-KOHID           PIC  9(10).
                   07  TBL-KOUI-TEKSTYMD        PIC  X(08). 
                   07  TBL-KOUI-RECEIPT-KBN     PIC  X(10).                    
                   07  TBL-KOUI-ZAINUM          PIC  9(08).
                   07  TBL-KOUI-RENNUM          PIC  9(02).
               05  TBL-KOUI-SHORI-RENNUM        PIC  9(01).
               05  TBL-KOUI-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                   07  TBL-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KOUI-CDKBNINF.
                       09  TBL-KOUI-CDKBN-KBN   PIC  X(01).
                       09  TBL-KOUI-CDKBN-SYO   PIC  9(01).
                       09  TBL-KOUI-CDKBN-BU    PIC  9(02).
                       09  TBL-KOUI-CDKBN-KBNNUM     PIC  9(03).
                       09  TBL-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  TBL-KOUI-CDKBN-KOUBAN     PIC  9(02).
                       09  TBL-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                   07  TBL-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KOUI-CHUKSNINF.
      *                コード
                       09  TBL-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KOUI-INPUTCOMENT     PIC  X(140). 
      *        名称情報
               05  TBL-KOUI-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KOUI-NAME            PIC  X(200).
      *
      *    コメントデータ用テーブル
       01  TBL-COMMENT-AREA.
           03  TBL-COMMENT-REC                  OCCURS  100.
               05  TBL-COMMENT-SRYCD            PIC  X(09).
               05  TBL-COMMENT-NAME             PIC  X(200).
      *
      *    診療行為データ用テーブル（通則加算項目の加減算コード）
       01  WRK-KOUI-KASAN-TAIHI-AREA.
           03  WRK-KOUI-KASAN-TAIHI-TABLE       OCCURS  10.
               05  WRK-KOUI-KASAN-TAIHI-REC     PIC X(600).
      *        時間外加算区分
               05  WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN
                                                PIC  9(01).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-TAIHI-AREA.
           03  WRK-KOUI-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-TAIHI-REC           PIC X(600).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-SRY-TAIHI-AREA.
           03  WRK-KOUI-SY-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-SRY-TAIHI-REC          PIC X(600).
      *        診療コード
               05  WRK-KOUI-SY-TAIHI-SRYCD         PIC  X(09).
      *        告示等識別区分１
               05  WRK-KOUI-SY-TAIHI-KOKUJISKBKBN1 PIC  9(01).
      *        告示等識別区分２
               05  WRK-KOUI-SY-TAIHI-KOKUJISKBKBN2 PIC  9(01).
      *        通則加算対象外区分
               05  WRK-KOUI-SY-TAIHI-TUSOKUGAIKBN  PIC  9(01).
      *        時間外加算区分
               05  WRK-KOUI-SY-TAIHI-TIMEKSNKBN    PIC  9(01).
      *        注加算コード
               05  WRK-KOUI-SY-TAIHI-CHUKSNCD      PIC  X(04).
      *        点数識別区分
               05  WRK-KOUI-SY-TAIHI-TENSIKIBETU   PIC  9(01).
      *
      *   
       01  WRK-KOUI-AREA.
           03  WRK-KOUI-TABLE                   OCCURS  5.
               05  WRK-KOUI-TBLMAX              PIC 9(03).
               05  WRK-KOUI-REC                     OCCURS  100.
                   07  WRK-KOUI-KEY.
                       09  WRK-KOUI-HOSPNUM        PIC 9(2).
                       09  WRK-KOUI-SRYYM          PIC 9(06).
                       09  WRK-KOUI-NYUGAIKBN      PIC X(01).
                       09  WRK-KOUI-PTID           PIC 9(10).
                       09  WRK-KOUI-RECEKA         PIC X(02).
                       09  WRK-KOUI-TEISYUTUSAKI   PIC 9(01).
                       09  WRK-KOUI-RECESYUBETU    PIC 9(04).
                       09  WRK-KOUI-HKNJANUM       PIC X(08).
                       09  WRK-KOUI-HOJOKBN        PIC X(01).
                       09  WRK-KOUI-KOHID          PIC 9(10).
                       09  WRK-KOUI-TEKSTYMD       PIC X(08). 
                       09  WRK-KOUI-RECEIPT-KBN    PIC X(10).                    
                       09  WRK-KOUI-ZAINUM         PIC 9(08).
                       09  WRK-KOUI-RENNUM         PIC 9(02).
                   07  WRK-KOUI-SHORI-RENNUM       PIC 9(01).
      *
                   07  WRK-KOUI-SRYACTINF.
      *            診療コード
                       09  WRK-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                       09  WRK-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                       09  WRK-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                       09  WRK-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                       09  WRK-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                       09  WRK-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                       09  WRK-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                       09  WRK-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                       09  WRK-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                       09  WRK-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                       09  WRK-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                       09  WRK-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                       09  WRK-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                       09  WRK-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                       09  WRK-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                       09  WRK-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                       09  WRK-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                       09  WRK-KOUI-CDKBNINF.
                           11  WRK-KOUI-CDKBN-KBN   PIC  X(01).
                           11  WRK-KOUI-CDKBN-SYO   PIC  9(01).
                           11  WRK-KOUI-CDKBN-BU    PIC  9(02).
                           11  WRK-KOUI-CDKBN-KBNNUM     PIC  9(03).
                           11  WRK-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  WRK-KOUI-CDKBN-KOUBAN     PIC  9(02).
                           11  WRK-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                       09  WRK-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                       09  WRK-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                       09  WRK-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                       09  WRK-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                       09  WRK-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                       09  WRK-KOUI-CHUKSNINF.
      *                コード
                           11  WRK-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                           11  WRK-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                       09  WRK-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                       09  WRK-KOUI-INPUTCOMENT     PIC  X(140). 
      *        名称情報
                   07  WRK-KOUI-NAMEINF.
      *            漢字有効桁数
                       09  WRK-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                       09  WRK-KOUI-NAME            PIC  X(200).
      *
      *    並び替え用ワーク
       01  TBL-KOUI-SORT                        PIC X(600).
      *
      *    診療行為データ用テーブル（加算項目扱いの基本項目）
       01  TBL-KOUI-SRYKBN12-AREA.
           03  TBL-KOUI-SRYKBN12-MAX                 PIC 9(02).
      *
           03  TBL-KOUI-SRYKBN12-REC                 OCCURS  20.
               05  TBL-KOUI-SRYKBN12-KEY.
                   07  TBL-KOUI-SRYKBN12-HOSPNUM     PIC 9(2).
                   07  TBL-KOUI-SRYKBN12-SRYYM       PIC 9(06).
                   07  TBL-KOUI-SRYKBN12-NYUGAIKBN   PIC X(01).
                   07  TBL-KOUI-SRYKBN12-PTID        PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-RECEKA      PIC X(02).
                   07  TBL-KOUI-SRYKBN12-TEISYUTUSAKI
                                                     PIC 9(01).
                   07  TBL-KOUI-SRYKBN12-RECESYUBETU PIC 9(04).
                   07  TBL-KOUI-SRYKBN12-HKNJANUM    PIC X(08).
                   07  TBL-KOUI-SRYKBN12-HOJOKBN     PIC X(01).
                   07  TBL-KOUI-SRYKBN12-KOHID       PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-TEKSTYMD    PIC X(08).
                   07  TBL-KOUI-SRYKBN12-RECEIPT-KBN PIC X(10).
                   07  TBL-KOUI-SRYKBN12-ZAINUM      PIC 9(08).
                   07  TBL-KOUI-SRYKBN12-RENNUM      PIC 9(02).
               05  TBL-KOUI-SRYKBN12-SHORI-RENNUM    PIC 9(01).
               05  TBL-KOUI-SRYKBN12-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-SRYCD PIC X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-KAISU PIC 9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-TEN   PIC 9(09)V9(02).
      *            注加算コード
                   07  TBL-KOUI-SRYKBN12-CHUKSNINF.
      *            コード
                       09  TBL-KOUI-SRYKBN12-CHUKSNCD
                                               PIC X(04).
      *            通番
                       09  TBL-KOUI-SRYKBN12-CHUKSNTUBAN
                                               PIC X(01).
      *    点数合計
           03  TBL-KOUI-SRYKBN12-TEN-G         PIC 9(09)V9(02).
      *
      *    診療行為データ用テーブル（加算項目扱いの基本項目と同じ注換算）
       01  TBL-KOUI-SRYKBN12-CHU-AREA.
           03  TBL-KOUI-SRYKBN12-CHU-MAX                 PIC 9(02).
      *
           03  TBL-KOUI-SRYKBN12-CHU-REC                 OCCURS  5.
               05  TBL-KOUI-SRYKBN12-CHU-KEY.
                   07  TBL-KOUI-SRYKBN12-CHU-HOSPNUM     PIC 9(2).
                   07  TBL-KOUI-SRYKBN12-CHU-SRYYM       PIC 9(06).
                   07  TBL-KOUI-SRYKBN12-CHU-NYUGAIKBN   PIC X(01).
                   07  TBL-KOUI-SRYKBN12-CHU-PTID        PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-CHU-RECEKA      PIC X(02).
                   07  TBL-KOUI-SRYKBN12-CHU-TEISYUTU    PIC 9(01).
                   07  TBL-KOUI-SRYKBN12-CHU-RECESYU     PIC 9(04).
                   07  TBL-KOUI-SRYKBN12-CHU-HKNJANUM    PIC X(08).
                   07  TBL-KOUI-SRYKBN12-CHU-HOJOKBN     PIC X(01).
                   07  TBL-KOUI-SRYKBN12-CHU-KOHID       PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-CHU-TEKSTYMD    PIC X(08).
                   07  TBL-KOUI-SRYKBN12-CHU-RECEIPT-K   PIC X(10).
                   07  TBL-KOUI-SRYKBN12-CHU-ZAINUM      PIC 9(08).
                   07  TBL-KOUI-SRYKBN12-CHU-RENNUM      PIC 9(02).
               05  TBL-KOUI-SRYKBN12-CHU-SHORI-NUM       PIC 9(01).
               05  TBL-KOUI-SRYKBN12-CHU-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-CHU-SRYCD PIC X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-CHU-KAISU PIC 9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-CHU-TEN   PIC 9(09)V9(02).
      *            注加算コード
                   07  TBL-KOUI-SRYKBN12-CHU-CHUKSNINF.
      *            コード
                       09  TBL-KOUI-SRYKBN12-CHU-CHUKSNCD
                                               PIC X(04).
      *            通番
                       09  TBL-KOUI-SRYKBN12-CHU-CHUKSNTU
                                               PIC X(01).
      *    点数合計
           03  TBL-KOUI-SRYKBN12-CHU-TEN-G     PIC 9(09)V9(02).
      *
       01  WRK-RECE055-REC.
           COPY    "CPRCF0055.INC" REPLACING  //RECE055//
                                   BY         //WRK-RECE055//.
      *
       01  WRK-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-RECE05//.
      *
       01  WRK-TAIHI-RECE041-REC.
           COPY    "CPRCF0041.INC" REPLACING  //RECE041//
                                   BY         //WRK-TAIHI-R4//.
       01  WRK-TAIHI-R41G10-REC.
           COPY    "CPRCF0041.INC" REPLACING  //RECE041//
                                   BY         //WRK-R41G10//.
       01  WRK-TAIHI-R41G11-REC.
           COPY    "CPRCF0041.INC" REPLACING  //RECE041//
                                   BY         //WRK-R41G11//.
      *
      *    画像診断用退避エリア
      *    撮影料退避
       01  FLG-SRYSYUKBN721        PIC 9(01).
       01  WRK-721-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-721-RECE05//.
       01  WRK-700-RECE05-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE05-REC                        OCCURS  5.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-700-RECE05//.
      *    撮影料退避（同時撮影）
       01  FLG-SRYSYUKBN721-DOUJI             PIC 9(01).
       01  WRK-721-DOUJI-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-721-DOU05//.
       01  WRK-700-DOUJI-RECE05-AREA.
           02  WRK-SRYSYUKBN700-DOUJI-MAX     PIC 9(01).
           02  FLG-SRYSYUKBN700-DOUJI         PIC 9(01)  OCCURS  5.
           02  WRK-700-DOUJI-RECE05-REC                  OCCURS  5.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-700-DOU05//.
      *    間接撮影の５０％逓減
       01  WRK-SRYKBN70-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-SRY70-RE05//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-KASAN-RE05//.
      *
      *    検査（判断料）用退避エリア
       01  TBL-KENSA-RECE04-AREA.
         02  TBL-KENSA-RECE04-REC  OCCURS  100.
      *
           COPY    "CPRCF0041.INC"  REPLACING  //RECE041//
                                    BY         //TBL-KENSA-R4//.
      *
           03  TBL-KENSA-RECE05-REC        OCCURS  50.
               05  TBL-KENSA-RE05-KEY.
                   07  TBL-KENSA-RE05-HOSPNUM         PIC 9(2).
                   07  TBL-KENSA-RE05-SRYYM           PIC 9(06).
                   07  TBL-KENSA-RE05-NYUGAIKBN       PIC X(01).
                   07  TBL-KENSA-RE05-PTID            PIC 9(10).
                   07  TBL-KENSA-RECE05-RECEKA        PIC X(02).
                   07  TBL-KENSA-RE05-TEISYUTUSAKI    PIC 9(01).
                   07  TBL-KENSA-RECE05-RECESYUBETU   PIC 9(04).
                   07  TBL-KENSA-RE05-HKNJANUM        PIC X(08).
                   07  TBL-KENSA-RE05-HOJOKBN         PIC X(01).
                   07  TBL-KENSA-RE05-KOHID           PIC 9(10).
                   07  TBL-KENSA-RE05-TEKSTYMD        PIC X(08). 
                   07  TBL-KENSA-RE05-RECEIPT-KBN     PIC X(10).                    
                   07  TBL-KENSA-RE05-ZAINUM          PIC 9(08).
                   07  TBL-KENSA-RE05-RENNUM          PIC 9(02).
              05  TBL-KENSA-RE05-SHORI-RENNUM         PIC 9(01).
      *
              05  TBL-KENSA-RE05-SRYACTINF.
      *            診療コード
                   07  TBL-KENSA-RE05-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KENSA-RE05-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KENSA-RE05-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                   07  TBL-KENSA-RE05-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KENSA-RE05-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KENSA-RE05-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KENSA-RE05-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KENSA-RE05-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KENSA-RE05-TEN             PIC 9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KENSA-RE05-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KENSA-RE05-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KENSA-RE05-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KENSA-RE05-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KENSA-RE05-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KENSA-RE05-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KENSA-RE05-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KENSA-RE05-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KENSA-RE05-CDKBNINF.
                       09  TBL-KENSA-RE05-CDKBN-KBN   PIC  X(01).
                       09  TBL-KENSA-RE05-CDKBN-SYO   PIC  9(01).
                       09  TBL-KENSA-RE05-CDKBN-BU    PIC  9(02).
                       09  TBL-KENSA-RE05-CDKBN-KBNNUM     PIC 9(03).
                       09  TBL-KENSA-RE05-CDKBN-KBNNUM-EDA PIC 9(02).
                       09  TBL-KENSA-RE05-CDKBN-KOUBAN     PIC 9(02).
                       09  TBL-KENSA-RE05-KOUHYOJYUNNUM    PIC 9(09).
      *            告示等識別区分１
                   07  TBL-KENSA-RE05-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KENSA-RE05-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KENSA-RE05-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KENSA-RE05-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KENSA-RE05-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KENSA-RE05-CHUKSNINF.
      *                コード
                       09  TBL-KENSA-RE05-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KENSA-RE05-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KENSA-RE05-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KENSA-RE05-INPUTCOMENT     PIC  X(140). 
      *        名称情報
               05  TBL-KENSA-RE05-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KENSA-RE05-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KENSA-RE05-NAME            PIC  X(200).
      *
           03  TBL-KENSA-SRYCD-MAX
                                  PIC 9(02).
      *
         02  TBL-KENSA-RECE04-MAX PIC 9(03).
         02  TBL-KENSA-RECE05-MAX PIC 9(03).
      *
      *    全角変換領域
       01  TBL-SUUJI-AREA.
           03  FILLER              PIC X(02)   VALUE   "１".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "２".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "３".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "４".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "５".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "６".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "７".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "８".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "９".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "０".
           03  FILLER              PIC X(01)   VALUE   "0".
       01  TBL-SUUJI-AREA-R        REDEFINES   TBL-SUUJI-AREA.
           03  TBL-SUUJI-G         OCCURS  10.   
               05  TBL-SUUJI-X     PIC X(02).
               05  TBL-SUUJI-9     PIC X(01).
      *
       01  WRK-CONS-AREA.
      *    日計表レコード記載の条件点数
           03  WRK-CONS-JYOKEN-TENSU   PIC 9(08)   VALUE   350000.
      *    検査日　日
           03  WRK-CONS-COMMENT        PIC X(09)   VALUE   "840000099".
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *    退職者保険番号
      *        組合
           03  WRK-CONS-HKNNUM1       PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-CONS-HKNNUM2       PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-CONS-HKNNUM3       PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-CONS-HKNNUM4       PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-CONS-HKNNUM5       PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-CONS-HKNNUM6       PIC X(03)   VALUE  "067".
      *    薬剤料逓減
           03  WRK-CONS-TEIGEN        PIC X(09)   VALUE   "630010002".
      *    薬剤料逓減（８０／１００）（向精神薬多剤投与）
           03  WRK-CONS-TEIGEN2       PIC X(09)   VALUE   "630010005".
      *    薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
           03  WRK-CONS-TEIGEN3       PIC X(09)   VALUE   "630010006".
      *    薬剤料減点（湿布薬薬剤料上限超）
           03  WRK-CONS-TEIGEN4       PIC X(09)   VALUE   "630010007".
      *
      *    再診の時間加算（診療区分１２、データ区分２、告示識別区分１）
           03  WRK-CONS-JIKANGAI-TABLE. 
      *    一般
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112001110".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112001210".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112001310".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112001410".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006670".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006770".
      *    老人
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112701010".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112701110".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112701210".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112701310".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701710".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701810".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701910".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112702010".
      *    乳幼児
      *        夜間（再診）
               05  FILLER             PIC X(09)    VALUE   "112014170".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112014270".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112014370".
      *        夜間（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014670".
      *        再診（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112014770".
      *        再診（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112014870".
      *        再診（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112014970".
      *        再診（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015070".
      *        外来診療科（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112015170".
      *        外来診療科（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112015270".
      *        外来診療科（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112015370".
      *        外来診療科（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015470".
      *
      *        再診（夜間・早朝等）加算
               05  FILLER             PIC X(09)    VALUE   "112015570".
      *
      *    妊婦
      *        妊婦時間外加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021370".
      *        妊婦休日加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021470".
      *        妊婦深夜加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021570".
      *        妊婦時間外特例医療機関加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021670".
      *        妊婦夜間加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022170".
      *        妊婦休日加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022270".
      *        妊婦深夜加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022370".
      *        妊婦時間外加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022470".
      *        妊婦休日加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022570".
      *        妊婦深夜加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022670".
      *        妊婦時間外特例医療機関加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022770".
      *        妊婦夜間加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022970".
      *        妊婦休日加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112023070".
      *        妊婦深夜加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112023170".
      *
      *        薬剤適正使用連携加算（再診）
               05  FILLER             PIC X(09)    VALUE   "112021970".
      *
           03  WRK-CONS-JIKANGAI-TABLE-R  REDEFINES   
                                      WRK-CONS-JIKANGAI-TABLE.
               05  WRK-CONS-JIKANGAI-SRYCD-G       OCCURS  46.
                   07  WRK-CONS-JIKANGAI-SRYCD
                                      PIC X(09).
           03  WRK-CONS-JIKANGAI-TEN-TABLE.
               05  WRK-CONS-JIKANGAI-TEN-G         OCCURS  46.
                   07  WRK-CONS-JIKANGAI-TEN
                                      PIC 9(09).
           03  WRK-CONS-JIKANGAI-MAX  PIC 9(02)    VALUE   46.
      *
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）
           03  WRK-CONS-SANSO1        PIC X(09)   VALUE   "739210000".
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）（離島）
           03  WRK-CONS-SANSO2        PIC X(09)   VALUE   "739250000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）
           03  WRK-CONS-SANSO3        PIC X(09)   VALUE   "739200000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）（離島）
           03  WRK-CONS-SANSO4        PIC X(09)   VALUE   "739240000".
      *    酸素ボンベ・大型
           03  WRK-CONS-SANSO5        PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ・大型（離島）
           03  WRK-CONS-SANSO6        PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ・小型
           03  WRK-CONS-SANSO7        PIC X(09)   VALUE   "739230000".
      *    酸素ボンベ・小型（離島）
           03  WRK-CONS-SANSO8        PIC X(09)   VALUE   "739270000".
      *
      *    間接撮影の５０％逓減
           03  WRK-CONS-SRYKBN70-TEIGEN
                                      PIC X(09)   VALUE   "170020570".
      *
      *    施設基準不適合（検査）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-KENSA
                                      PIC X(09)   VALUE   "160174470".
      *
      *    施設基準不適合（画像）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-GAZOU
                                      PIC X(09)   VALUE   "170025170".
      *
      *    施設基準不適合（放射線）（１００分の７０）
           03  WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                      PIC X(09)   VALUE   "180025270".
      *
      *    施設基準不適合減算（手術）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-GENZAN
                                      PIC X(09)   VALUE   "150382490".
      *
      *    酸素補正率１．３（１気圧）
           03  WRK-CONS-SANSO-HOSEI   PIC X(09)   VALUE   "770020070".
      *
      *    デジタル撮影とアナログ撮影の同時撮影
           03  WRK-CONS-DOUJI-SATUEI-AREA.
      *        単純撮影（デジタルと同時撮影）（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032050".
      *        単純撮影（アナログと同時撮影）（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032150".
      *        単純撮影（デジタルと同時撮影）（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032450".
      *        単純撮影（アナログと同時撮影）（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032550".
      *        造影剤使用撮影（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032250".
      *        造影剤使用撮影（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032350".
      *        造影剤使用撮影（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032650".
      *        造影剤使用撮影（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032750".
           03  WRK-CONS-DOUJI-SATIEI-R     REDEFINES
                                       WRK-CONS-DOUJI-SATUEI-AREA.
               05  WRK-CONS-DOUJI-SATUEI
                                       PIC X(09)   OCCURS  8.
      *
      *    出力対象のコメント
           03  WRK-CONS-COMMENT-G.
      *        複数診療科受診
               05  WRK-CONS-COMMENT1  PIC X(09)   VALUE   "099999901".
      *        他保険にて診察料算定済
               05  WRK-CONS-COMMENT2  PIC X(09)   VALUE   "099999902".
      *********（粉砕）
      *********05  WRK-CONS-COMMENT3  PIC X(09)   VALUE   "099209901".
      *       （残量廃棄）
               05  WRK-CONS-COMMENT4  PIC X(09)   VALUE   "099309901".
      *
           03  WRK-CONS-SRYYM-200604   PIC 9(06)   VALUE   200604. 
           03  WRK-CONS-SRYYM-200704   PIC 9(06)   VALUE   200704.
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.
           03  WRK-CONS-SRYYM-201204   PIC 9(06)   VALUE   201204.
           03  WRK-CONS-SRYYM-201804   PIC 9(06)   VALUE   201804.
      *    レセプトコメント
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
      *    レセプトコメント長さ
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
      *    保険組み合わせ退避ワーク
       01  THKNCOMBI-AREA.
           02      TCOMB-MAX        PIC 9(05).
           02      TCOMB-OCC        OCCURS     30.
           COPY    "CPHKNCOMBI.INC" REPLACING   //COMB-//
                                    BY          //TCOMB-//.
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
           COPY    "CPSK2009.INC".
      *
           COPY    "CPSK2011.INC"  REPLACING  //SYS-2011//
                                   BY         //S21//.
      *
      *    医療機関情報
           COPY   "CPSK1001.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    レセ器材再編集サブ
           COPY    "CPORCSRECESORT.INC"
                                   REPLACING  //-IN-KEY-TEISYUTUSAKI//
                                   BY         //-IN-KEY-TEISYUTU//.
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
                                       WRK-AREA
                                       FLG-AREA
                                       TTNS-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-CREYMD
                                               WRK-PARA-CREHMS
                                               WRK-PARA-PRTKBN
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "レセ電データ作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCR0300"      TO  JOB-PGID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE039"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE049"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE051"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE090"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    "RECE110"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    "RECE120"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           MOVE    "RECE055"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (9)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE02PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE03PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE04PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE05PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE09PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE11PARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE12PARA
           MOVE    SGETTEMP-FULLNAMES (9)
                                       TO  RECE055PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           OPEN    I-O                 RECE02-FILE  
           IF      STS-RECE02          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE02PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE02          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE03-FILE  
           IF      STS-RECE03          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE03PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE041-FILE  
           IF      STS-RECE04          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE04PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE04          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE05-FILE  
           IF      STS-RECE05          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE05PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE05          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE09-FILE
           IF      STS-RECE09          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE09PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE09          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE11-FILE
           IF      STS-RECE11          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE11PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE11          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE12-FILE
           IF      STS-RECE12          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE12PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE12          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE055-FILE
           IF      STS-RECE055         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE055PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE055         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      * 
           IF      FLG-END             =   ZERO
      *        ファイル読み込み
               PERFORM 900-RECE02-READ-SEC
      *
      *        時間外加算点数取得
               INITIALIZE                  WRK-CONS-JIKANGAI-TEN-TABLE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-JIKANGAI-MAX
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    WRK-CONS-JIKANGAI-SRYCD (IDX)
                                           TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-TEN         TO
                                           WRK-CONS-JIKANGAI-TEN (IDX)
               END-PERFORM
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
grpsys         MOVE    SPA-HOSPNUM        TO  SYS-1001-HOSPNUM
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
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
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
               IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
               IF      SYS-2005-NI-JYOKEN (1)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(1)
               END-IF
               IF      SYS-2005-NI-JYOKEN (2)  NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                               TO  SYS-2005-NI-JYOKEN(2)
               END-IF
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
               INITIALIZE                   SYS-2005-REC
               MOVE    WRK-CONS-JYOKEN-TENSU
                                        TO  SYS-2005-NI-JYOKEN(1)
                                            SYS-2005-NI-JYOKEN(2)
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           IF      SYS-2005-RECECOMKBN      =   SPACE
               MOVE    ZERO             TO  SYS-2005-RECECOMKBN
           END-IF
           IF      SYS-2005-NYUTAIINREKI-KSIKBN
                                            =   SPACE
               MOVE    "0"              TO  SYS-2005-NYUTAIINREKI-KSIKBN
           END-IF
      *         
           MOVE    SPACE               TO  SYS-25-REC
           INITIALIZE                      SYS-25-REC
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-25-REC
               IF      SYS-25-NI-JYOKEN (1)      NOT NUMERIC
                   MOVE    WRK-CONS-JYOKEN-TENSU
                                           TO  SYS-25-NI-JYOKEN(1)
               END-IF
           ELSE    
               MOVE    SPACE           TO  SYS-25-REC
               INITIALIZE                  SYS-25-REC
               MOVE    WRK-CONS-JYOKEN-TENSU
                                       TO  SYS-25-NI-JYOKEN(1)
           END-IF
           IF      SYS-25-HKN-RIREKI-KBN
                                        =   SPACE
               MOVE    "0"              TO  SYS-25-HKN-RIREKI-KBN
           END-IF
           IF      SYS-25-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    ZERO        TO  SYS-25-KOUIKIRECEKBN
           END-IF
           MOVE    SYS-25-KOUIKIRECEKBN
                                   TO  WRK-PARA-KOUIKIRECEKBN 
      *
           DISPLAY "2005 NI-JYOKEN (1)=" SYS-2005-NI-JYOKEN(1)
                                 " (2)=" SYS-2005-NI-JYOKEN(2)
                   " 200501 NI-JYOKEN (1)=" SYS-25-NI-JYOKEN(1)
           DISPLAY "200501 HKN-RIREKI-KBN="
                                         SYS-25-HKN-RIREKI-KBN
           DISPLAY "KOUIKIRECEKBN=" WRK-PARA-KOUIKIRECEKBN
      *
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYS-2009-REC
           INITIALIZE                  SYS-2009-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-2009-HOSPNUM
           MOVE    "2009"          TO  SYS-2009-KANRICD
           MOVE    "*"             TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD
                                   TO  SYS-2009-EDYUKYMD
           MOVE    SYS-2009-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE    
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOUIKI
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
           END-IF
      *         
      *    記号番号区切り文字設定情報
           MOVE    SPACE           TO  S21-REC
           INITIALIZE                  S21-REC
           MOVE    SPA-HOSPNUM     TO  S21-HOSPNUM
           MOVE    "2011"          TO  S21-KANRICD
           MOVE    "*"             TO  S21-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  S21-STYUKYMD (1:6)
           MOVE    "01"            TO  S21-STYUKYMD (7:2)
           MOVE    S21-STYUKYMD    TO  S21-EDYUKYMD
           MOVE    S21-REC         TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  S21-REC
           ELSE    
               INITIALIZE                  S21-REC
           END-IF
           IF      S21-KIGO-DELIMITER-FLG
                                       =   SPACE
               MOVE    "0"             TO  S21-KIGO-DELIMITER-FLG
           END-IF 
           IF      S21-KIGO-DELIMITER-HKNJANUM-FLG
                                       =   SPACE
               MOVE    "0"             TO
                                   S21-KIGO-DELIMITER-HKNJANUM-FLG
           END-IF 
           IF      S21-NUM-DELIMITER-FLG
                                       =   SPACE
               MOVE    "0"             TO  S21-NUM-DELIMITER-FLG
           END-IF 
           IF      S21-NUM-DELIMITER-HKNJANUM-FLG
                                       =   SPACE
               MOVE    "0"             TO
                                   S21-NUM-DELIMITER-HKNJANUM-FLG
           END-IF 
      *
           DISPLAY "SYS-2011-KIGO-DELIMITER-FLG=" 
                                   S21-KIGO-DELIMITER-FLG
                   " SYS-2011-KIGO-DELIMITER-HKNJANUM-FLG="
                                   S21-KIGO-DELIMITER-HKNJANUM-FLG
           DISPLAY "SYS-2011-NUM-DELIMITER-FLG ="
                                   S21-NUM-DELIMITER-FLG
                   " SYS-2011-NUM-DELIMITER-HKNJANUM-FLG ="
                                   S21-NUM-DELIMITER-HKNJANUM-FLG
      *    
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-VOLNUM
                                       WRK-RECENUM
      *
      *    審査支払機関・診療科別処理              
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *
      *        県単用項目ファイル読込処理
               PERFORM 301-RECE11-HENSYU-SEC
      *
               MOVE    ZERO               TO  WRK-RENNUM
      *
               MOVE    ZERO               TO  WRK-COM-RENNUM
                                              FLG-COMMENT
               MOVE    SPACE              TO  WRK-COM-SRYKBN
                                              WRK-COM-FTNKBN
      *
               INITIALIZE                 WRK-RECE041-SRYKBN
                                          TBL-KOUI-SRYKBN12-AREA
                                          TBL-KOUI-SRYKBN12-CHU-AREA
      *********INITIALIZE                 TBL-KENTAI-RECE04-AREA
               INITIALIZE                 TBL-KENSA-RECE04-AREA
      *
               MOVE    ZERO               TO  FLG-TOYAKUFLG
      *  
      *        患者保険組み合わせ取得処理
               PERFORM     2001-HKNCOMBI-GET-SEC
      *
      *        保険履歴情報編集処理
               PERFORM     2001-RIREKI-HEN-SEC 
      *
      *        三種公費併用特例の判定
               MOVE    SPACE               TO  WRK-3SYU-KBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX                 >    4 
                       OR      RECE02-KOHID (IDX)  =   ZERO
      *            生保
                   IF  RECE02-KOHNUM (IDX) =   "012"
                       MOVE    "1"         TO  WRK-3SYU (1:1)
                   END-IF
      *            結核
                   IF  RECE02-KOHNUM (IDX) =   "010"  OR  "011"
                       MOVE    "1"         TO  WRK-3SYU (2:1)
                   END-IF
      *
                   IF      RECE02-SRYYM    <   WRK-CONS-SRYYM-200604
      *                精神
                       IF  RECE02-KOHNUM (IDX) =   "020"  OR  "021"
                           MOVE    "1"         TO  WRK-3SYU (3:1)
                       END-IF
                   ELSE
      *                自立支援法
                       IF  RECE02-KOHNUM (IDX) =   "021"  OR
                                                   "015"  OR  "016"  OR
                                                   "024"
                           MOVE    "1"         TO  WRK-3SYU (3:1)
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        実日数／合計点数等のＩＮＤＥＸ
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
      *
      *        総件数／総合計点数の集計
               MOVE    ZERO                TO  SUM-TOTALTEN
                                               FLG-TOTALTEN
               IF      RECE02-SRYYM        <   WRK-CONS-SRYYM-200804       
                  IF      RECE02-RJNHKNNUM    =   SPACE           
                   AND     RECE02-HKNNUM       =   SPACE
                       IF      RECE02-KOHNUM (3)   NOT =   SPACE
                           MOVE    RECE02-TOTALTEN (4) TO  SUM-TOTALTEN
                       ELSE
                           MOVE    RECE02-TOTALTEN (2) TO  SUM-TOTALTEN
                       END-IF
                   ELSE
                       MOVE     RECE02-TOTALTEN (1) TO  SUM-TOTALTEN
                   END-IF
               ELSE
                   IF      RECE02-HKNNUM       =   SPACE
                       MOVE    RECE02-TOTALTEN (2) TO  SUM-TOTALTEN
                   ELSE
                       MOVE    RECE02-TOTALTEN (1) TO  SUM-TOTALTEN
                   END-IF
               END-IF
      *         
      *        レセプト共通レコード
               PERFORM     2002-HENSYU-SEC
      *        保険者レコード
               PERFORM     2003-HENSYU-SEC
      *        老人レコード
               PERFORM     2004-HENSYU-SEC
      *        公費レコード
               PERFORM     2005-HENSYU-SEC
      *        傷病名レコード
               PERFORM     2006-HENSYU-SEC
      *        適用レコード
               PERFORM     2007-HENSYU-SEC
      *        症状詳記（適用欄下段コメント・レセコメント）
               MOVE    ZERO        TO  FLG-TOYAKUFLG
               PERFORM     2009-COMMENT-HEN-99-SEC
      *
               ADD     1           TO  CNT-COMMIT
               IF  CNT-COMMIT      =    300
                   MOVE  ZERO      TO      CNT-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
               PERFORM     900-RECE02-READ-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理
      *****************************************************************
       2002-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RE,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    レセプト番号
           PERFORM 5005-NUM-HENSYU-SEC
      *    レセプト種別
           MOVE    RECE02-RECESYUBETU  TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    RECE02-SRYYM        TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    氏名
           INITIALIZE                      ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    "1"                 TO  KANACHK2-SYORIKBN 
           MOVE    RECE02-NAME         TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING   ORCSKANACHK2AREA
           IF      KANACHK2-RC4        =   ZERO
               MOVE    SPACE               TO  RECE02-NAME
               MOVE    KANACHK2-OUT-INPUT (1:KANACHK2-MAX)
                                           TO  RECE02-NAME
      *
               PERFORM VARYING IDX FROM    1   BY  2
                       UNTIL   IDX >       49
      *                                            F4A5 F4A6
                   IF      RECE02-NAME (IDX:2) =   "凜" 
                                               OR  "熙"
                       MOVE    "　"                TO  RECE02-NAME
                       MOVE    49                  TO  IDX
                   END-IF
               END-PERFORM 
               IF      RECE02-NAME (1:2)   =   "　"
                   IF      RECE02-KANANAME (1:2)   =   "　"
                       PERFORM VARYING IDX FROM    1   BY  2
                               UNTIL   IDX >       49
                           IF      RECE02-KANANAME (IDX:2) =   "　"
                               CONTINUE
                           ELSE     
                               MOVE    RECE02-KANANAME (IDX:)
                                                           TO  WRK-DATA
                               MOVE    49                  TO  IDX
                           END-IF
                       END-PERFORM 
                   ELSE
                       MOVE    RECE02-KANANAME     TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    RECE02-NAME         TO  WRK-DATA
               END-IF
           ELSE
               IF      RECE02-KANANAME (1:2)   =   "　"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       49
                       IF      RECE02-KANANAME (IDX:2)   =   "　"
                           CONTINUE
                       ELSE     
                           MOVE    RECE02-KANANAME (IDX:) TO  WRK-DATA
                           MOVE    49                     TO  IDX
                       END-IF
                   END-PERFORM 
               ELSE
                   MOVE    RECE02-KANANAME     TO  WRK-DATA
               END-IF
           END-IF 
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    RECE02-SEX          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    RECE02-BIRTHDAY     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    給付割合（国保OR退職者のみ）
           IF    ( RECE02-TEISYUTUSAKI =   "2"     AND
                   RECE02-RJNHKNNUM    =   SPACE                )
      *        ７０歳代前半の特例措置対象者のみ２割の記載
               IF      RECE02-RJNGAIKOHFTN (2)     =   1
                   MOVE    20              TO  RECE02-KYURATE
               END-IF
      *
               COMPUTE WRK-KYURATE =  (100 -   RECE02-KYURATE) 
               MOVE   WRK-KYURATE          TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF    
      *    レセプト特記事項編集情報
           IF      RECE02-2006-RECEDEN-KYURATE
                                       >   ZERO
               MOVE   RECE02-2006-RECEDEN-KYURATE
                                           TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF
      *
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院年月日
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病棟区分
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    一部負担金・食事療養費標準負担額区分
           IF      RECE02-SRYYM  >=  "201710"
             IF    ( RECE02-TEISYOTOKU   NOT =   SPACE )
             AND   ( RECE02-ZAITAKU          =   "1"   )
               EVALUATE    RECE02-TEISYOTOKU
                   WHEN    "1"
                   WHEN    "4"
                       MOVE    "3"             TO  WRK-DATA
                   WHEN    "2"
                       MOVE    "1"             TO  WRK-DATA
               END-EVALUATE
             END-IF
           ELSE
             IF    ( RECE02-TEISYOTOKU   NOT =   SPACE )
             AND   ( RECE02-ZAITAKU          =   "1"   )
               EVALUATE    RECE02-TEISYOTOKU
                   WHEN    "1"
                       MOVE    "3"             TO  WRK-DATA
                   WHEN    "2"
                       MOVE    "1"             TO  WRK-DATA
                   WHEN    "4"
                       MOVE    "4"             TO  WRK-DATA
               END-EVALUATE
             END-IF
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *
      *    特記事項
           IF      RECE02-PTTOKKI-AREA     NOT =   SPACE
      *        患者特記事項情報
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   IF      RECE02-PTTOKKI-CD (IDX)
                                           NOT =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       5
                           IF      RECE02-TOKKI (IDZ)  =   SPACE
                               MOVE    RECE02-PTTOKKI-CD (IDX)
                                               TO  RECE02-TOKKI (IDZ)
                           END-IF
                           IF      RECE02-PTTOKKI-CD (IDX)
                                               =   RECE02-TOKKI (IDZ)      
                               MOVE    5       TO  IDZ
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      RECE02-2006-RECEDEN-TOKKIINF
                                   NOT =   SPACE
      *        レセプト特記事項編集情報
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   IF      RECE02-2006-RECEDEN-TOKKI (IDX)
                                           NOT =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       5
                           IF      RECE02-TOKKI (IDZ)  =   SPACE
                               MOVE    RECE02-2006-RECEDEN-TOKKI (IDX)
                                               TO  RECE02-TOKKI (IDZ)
                           END-IF
                           IF      RECE02-2006-RECEDEN-TOKKI (IDX)
                                               =   RECE02-TOKKI (IDZ)      
                               MOVE    5       TO  IDZ
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-RECE11          =   ZERO
      *        レセプトカスタマイズ情報
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   IF      RECE11-TOKKI (IDX)  NOT =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       5
                           IF      RECE02-TOKKI (IDZ)  =   SPACE
                               MOVE    RECE11-TOKKI (IDX)
                                               TO  RECE02-TOKKI (IDZ)
                           END-IF
                           IF      RECE11-TOKKI (IDX)
                                               =   RECE02-TOKKI (IDZ)      
                               MOVE    5       TO  IDZ
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF 
           MOVE    1               TO  IDZ                  
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02-TOKKI (IDX)  TO  WRK-DATA (IDZ:2)
                   ADD     2               TO  IDZ
           END-PERFORM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE02-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-NUM
                   MOVE    4                   TO  WRK-NAGASA
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-NUM
                       MOVE    4                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC         
      *    カルテ番号等
           MOVE    RECE02-PTNUM        TO  WRK-DATA     
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    割引点数単価
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    旧診療科コード
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    検索番号
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    記録条件仕様年月情報
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    請求情報（作成日時、災害区分）
           MOVE    RECE02-RJNGAIKOHFTN (1)
                                       TO  WRK-DISASTER
           STRING  WRK-PARA-CREYMD     DELIMITED  BY SIZE
                   WRK-PARA-CREHMS (1:6)
                                       DELIMITED  BY SIZE
                   WRK-DISASTER        DELIMITED  BY SIZE
                                       INTO  WRK-DATA
           END-STRING
           MOVE    16                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科名１
           IF      RECE02-SYUKA    NOT =   SPACE
               MOVE    RECE02-RECESYUKA    TO  WRK-DATA
           END-IF
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科名２
           IF      RECE02-SYUKA2   NOT =   SPACE
               MOVE    RECE02-RECESYUKA2   TO  WRK-DATA
           END-IF
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科名３
           IF      RECE02-SYUKA3   NOT =   SPACE
               MOVE    RECE02-RECESYUKA3   TO  WRK-DATA
           END-IF
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病３
           PERFORM 5004-MOJI-HENSYU-SEC
      *    カタカナ（氏名）
           IF      RECE02-SRYYM        >=  WRK-CONS-SRYYM-201804
               MOVE    1                   TO  IDY
               PERFORM VARYING IDX FROM    1   BY  2
                       UNTIL   IDX >       49
                       OR      RECE02-KANANAME (IDX:1) =   SPACE
                   IF      RECE02-KANANAME (IDX:2)
                                               >=  "ァ"  AND
                                               <=  "ヶ"
                       EVALUATE    RECE02-KANANAME (IDX:2)
                           WHEN    "ヮ"
                               MOVE    "ワ"        TO  WRK-DATA (IDY:2)
                           WHEN    "ヵ"
                               MOVE    "カ"        TO  WRK-DATA (IDY:2)
                           WHEN    "ヶ"
                               MOVE    "ケ"        TO  WRK-DATA (IDY:2)
                           WHEN    OTHER
                               MOVE    RECE02-KANANAME (IDX:2)
                                                   TO  WRK-DATA (IDY:2)
                       END-EVALUATE
                       ADD     2               TO  IDY
                   ELSE
                       EVALUATE    RECE02-KANANAME (IDX:2)
      *                    A8 A1
                           WHEN    "─"
      *                    A1 DD
                           WHEN    "−"
      *                    A1 BE
                           WHEN    "‐"
      *                    A1 BD
                           WHEN    "―"
      *                        A1 BC
                               MOVE    "ー"        TO  WRK-DATA (IDY:2)
                               ADD     2           TO  IDY
      *                        A1 BC
                           WHEN    "ー"
                               MOVE    RECE02-KANANAME (IDX:2)
                                                   TO  WRK-DATA (IDY:2)
                               ADD     2               TO  IDY
                       END-EVALUATE
                   END-IF
               END-PERFORM
           END-IF
           MOVE    80                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者状態 
           IF      RECE02-NINPUFLG >   ZERO
               MOVE    "001"           TO  WRK-DATA
           END-IF
           MOVE    60                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-TOTALTEN
           PERFORM 5001-FILE-WRITE-SEC
           .
       2002-HENSYU-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    保険者レコード編集処理
      *****************************************************************
       2003-HENSYU-SEC              SECTION.
      *
           IF      RECE02-HKNNUM           =   SPACE
           OR      RECE02-RJNHKNNUM    NOT =   SPACE 
               GO  TO  2003-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "HO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    保険者番号
           IF  RECE02-TEISYUTUSAKI    =   "2"
               IF      RECE02-HKNJANUM (1:2)  =   "67"
                   MOVE    RECE02-HKNJANUM  TO  WRK-HENKAN
               ELSE
      *        国保の場合（６桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:6)
                                            TO  WRK-HENKAN
               END-IF
           ELSE
               IF  RECE02-HKNNUM      =   "001"
      *            政府管掌の場合（４桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:4)
                                         TO  WRK-HENKAN
               ELSE
      *            他の保険の場合（８桁）
                   MOVE    RECE02-HKNJANUM
                                         TO  WRK-HENKAN
               END-IF
           END-IF
      *    災害対応
           IF    ( RECE02-RJNGAIKOHFTN (1)
                                         >   0     )
           AND   ( RECE02-HKNJANUM       =   SPACE )
               MOVE    "99999999"            TO  WRK-HENKAN
           END-IF
           MOVE    8                     TO  WRK-LENGTH 
           PERFORM 5003-DATA-RIGHT-HENSYU-SEC
           MOVE    WRK-HENKAN            TO  WRK-DATA
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           IF      RECE02-TEISYUTUSAKI =   "2"
               IF    ( S21-KIGO-DELIMITER-FLG
                                           =   "0" )
               OR    ( RECE02-PREFKBN      =   "2" )
                   MOVE    RECE02-KIGO         TO  WRK-DATA
               ELSE
                   INITIALIZE                      WRK-DELIMITER-AREA
                   MOVE    RECE02-KIGO         TO  WRK-DELIMITER-IN
                   MOVE    S21-KIGO-DELIMITER-FLG
                                               TO  WRK-DELIMITER-FLG
                   MOVE    S21-KIGO-DELIMITER-HKNJANUM-FLG
                                               TO
                                           WRK-DELIMITER-HKNJANUM-FLG
                   PERFORM 20031-DELIMITER-FLG-HENSYU-SEC
                   MOVE    WRK-DELIMITER-OUT   TO  WRK-DATA
               END-IF
           ELSE
               MOVE    RECE02-KIGO         TO  WRK-DATA
           END-IF
           MOVE    38                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           IF      RECE02-TEISYUTUSAKI =   "2"
               IF    ( S21-NUM-DELIMITER-FLG
                                           =   "0" )
               OR    ( RECE02-PREFKBN      =   "2" )
                   MOVE    RECE02-NUM          TO  WRK-DATA
               ELSE
                   INITIALIZE                      WRK-DELIMITER-AREA
                   MOVE    RECE02-NUM          TO  WRK-DELIMITER-IN
                   MOVE    S21-NUM-DELIMITER-FLG
                                               TO  WRK-DELIMITER-FLG
                   MOVE    S21-NUM-DELIMITER-HKNJANUM-FLG
                                               TO
                                           WRK-DELIMITER-HKNJANUM-FLG
                   PERFORM 20031-DELIMITER-FLG-HENSYU-SEC
                   MOVE    WRK-DELIMITER-OUT   TO  WRK-DATA
               END-IF
           ELSE
               MOVE    RECE02-NUM          TO  WRK-DATA
           END-IF
      *    災害対応
           IF    ( RECE02-RJNGAIKOHFTN (1)
                                         >   0     )
           AND   ( RECE02-NUM            =   SPACE )
               MOVE    "999999999"           TO  WRK-DATA
           END-IF
           MOVE    38                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX1)   TO  WRK-NUM
           MOVE    2                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX1) TO  WRK-NUM
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    予備
           MOVE    ZERO                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養費（日数）
           PERFORM 5005-NUM-HENSYU-SEC   
      *    食事療養費（合計金額）
           PERFORM 5005-NUM-HENSYU-SEC     
      *    職務上の事由
           IF      RECE02-HKNNUM       =   "002"
               EVALUATE    RECE02-HOJOKBN
                   WHEN    "1"
                   WHEN    "A"
                   WHEN    "D"
                   WHEN    "G"
                       MOVE    "1"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "E"
                   WHEN    "H"
                       MOVE    "2"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
                   WHEN    "3" 
                   WHEN    "C"
                   WHEN    "F"
                   WHEN    "I"
                       MOVE    "3"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
               END-EVALUATE
           END-IF    
           IF      RECE02-HKNNUM (1:2) =   "03"
               EVALUATE    RECE02-HOJOKBN
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "E"
                   WHEN    "H"
                       MOVE    "2"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
               END-EVALUATE
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    証明書番号（東大阪市国保減免のとき）
           IF      RECE02-GENMEN-COM   >   0
               MOVE    RECE02-GENMEN-COM   TO  WRK-NUM
               MOVE    1                   TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC
      *    医療保険一部負担金額
           IF      RECE02-FTNMONEY(IDX1)
                                       >   ZERO
               IF    RECE02-SRYYM   >=   WRK-CONS-SRYYM-200704
                 IF    ( RECE02-ZAITAKU      =   "1" OR  "2" )
                   MOVE    RECE02-FTNMONEY(IDX1) TO  WRK-NUM
                   MOVE    9                     TO  WRK-NAGASA
                 END-IF
               ELSE
                 IF    ( RECE02-RECESYUBETU (4:1)
                                             =   "8" OR  "0" )
                 AND   ( RECE02-RJNHKNNUM    =   SPACE       )
                   MOVE    RECE02-FTNMONEY(IDX1) TO  WRK-NUM
                   MOVE    9                     TO  WRK-NAGASA
                 END-IF
               END-IF
      ***      IF    ( RECE02-HKNNUM       =   "002" )
      ***      AND   ( RECE02-HOJOKBN      =   "3"   )
               IF      RECE02-SENIN-TUUSAI =   "1"
                   MOVE    RECE02-FTNMONEY(IDX1) TO  WRK-NUM
                   MOVE    9                     TO  WRK-NAGASA
               END-IF
           ELSE
               IF      RECE02-ZAITAKU   =   "1" OR "2"
                   MOVE    RECE02-FTNMONEY (IDX1)
                                               TO  WRK-NUM
                   MOVE    9                   TO  WRK-NAGASA
               END-IF
           END-IF    
      *
      *    県単用項目
           IF      FLG-RECE11          =   ZERO
               IF      RECE11-RECEDEN-FTNMONEY(IDX1)
                                           =   999999999
                   MOVE    ZERO                TO  WRK-NUM
                   MOVE    9                   TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    減免区分
           IF      RECE02-GENMENKBN    >   ZERO
               MOVE    RECE02-GENMENKBN    TO  WRK-NUM
               MOVE    1                   TO  WRK-NAGASA
           END-IF                                 
           PERFORM 5005-NUM-HENSYU-SEC     
      *    減額割合 
           IF      RECE02-GENGAKURATE  >   ZERO
               MOVE    RECE02-GENGAKURATE  TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF                                 
           PERFORM 5005-NUM-HENSYU-SEC  
      *    減額金額 
           MOVE    1                   TO  WRK-END 
           IF      RECE02-GENGAKU      >   ZERO
               MOVE    RECE02-GENGAKU      TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF                                 
           PERFORM 5005-NUM-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2003-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    記号番号区切り文字設定情報による編集処理
      *****************************************************************
       20031-DELIMITER-FLG-HENSYU-SEC      SECTION.
      *
           MOVE    WRK-DELIMITER-IN    TO  WRK-DELIMITER-OUT
      *
           IF      WRK-DELIMITER-IN    =   SPACE
               GO  TO  20031-DELIMITER-FLG-HENSYU-EXT
           END-IF
      *
      *    設定保険者によるチェック
           EVALUATE    WRK-DELIMITER-HKNJANUM-FLG
               WHEN    "1"
      *            県内保険者
                   IF    ( RECE02-PREFKBN      =   "1"              )
                   AND   ( RECE02-HKNJANUM-PREFNUM
                                               =   SYS-1001-PREFNUM )
                       CONTINUE
                   ELSE
                       GO  TO  20031-DELIMITER-FLG-HENSYU-EXT
                   END-IF
               WHEN    "2"
      *            県内保険者と県内扱い保険者
                   IF    ( RECE02-PREFKBN      =   "1"              )
                       CONTINUE
                   ELSE
                       GO  TO  20031-DELIMITER-FLG-HENSYU-EXT
                   END-IF
           END-EVALUATE
      *
      *    全角ハイフン、全角マイナス、長音、ダッシュを設定情報より変換
           MOVE    SPACE               TO  WRK-DELIMITER-OUT
           MOVE    ZERO                TO  WRK-DELIMITER-MAX
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       20
               IF      WRK-DELIMITER-IN-O (IDXX)   =
      *                                            A8 A1
                                                       "─"
      *                                            A1 DD
                                                   OR  "−"
      *                                            A1 BC
                                                   OR  "ー"
      *                                            A1 BE
                                                   OR  "‐"
      *                                            A1 BD
                                                   OR  "―"
                   EVALUATE    WRK-DELIMITER-FLG
                       WHEN    "1"
      *                    全角ハイフンを設定
                           ADD     1           TO  WRK-DELIMITER-MAX
                           MOVE    "‐"        TO  WRK-DELIMITER-OUT-O
                                                   (WRK-DELIMITER-MAX) 
                       WHEN    "2"
      *                    全角マイナスを設定
                           ADD     1           TO  WRK-DELIMITER-MAX
                           MOVE    "−"        TO  WRK-DELIMITER-OUT-O
                                                   (WRK-DELIMITER-MAX) 
                       WHEN    "9"
      *                    削除
                           CONTINUE
                       WHEN    OTHER
                           ADD     1           TO  WRK-DELIMITER-MAX
                           MOVE    WRK-DELIMITER-IN-O (IDXX)
                                               TO  WRK-DELIMITER-OUT-O
                                                   (WRK-DELIMITER-MAX) 
                   END-EVALUATE
               ELSE
                   ADD     1           TO  WRK-DELIMITER-MAX
                   MOVE    WRK-DELIMITER-IN-O (IDXX)
                                       TO  WRK-DELIMITER-OUT-O
                                                   (WRK-DELIMITER-MAX) 
               END-IF    
           END-PERFORM
      *
           .
       20031-DELIMITER-FLG-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    老人レコード編集処理
      *****************************************************************
       2004-HENSYU-SEC              SECTION.
      *
           IF      RECE02-SRYYM    >=  WRK-CONS-SRYYM-200804
               GO  TO  2004-HENSYU-EXT
           END-IF
      * 
           IF      RECE02-RJNHKNNUM    =   SPACE
               GO  TO  2004-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    保険者番号
           IF  RECE02-TEISYUTUSAKI    =   "2"
               IF      RECE02-HKNJANUM (1:2)  =   "67"
                   MOVE    RECE02-HKNJANUM  TO  WRK-HENKAN
               ELSE
      *        国保の場合（６桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:6)
                                            TO  WRK-HENKAN
               END-IF
           ELSE
               IF  RECE02-HKNNUM      =   "001"
      *            政府管掌の場合（４桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:4)
                                         TO  WRK-HENKAN
               ELSE
      *            他の保険の場合（８桁）
                   MOVE    RECE02-HKNJANUM
                                         TO  WRK-HENKAN
               END-IF
           END-IF
           MOVE    8                   TO  WRK-LENGTH
           PERFORM 5003-DATA-RIGHT-HENSYU-SEC
           MOVE    WRK-HENKAN          TO  WRK-DATA
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           MOVE    RECE02-KIGO           TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           MOVE    RECE02-NUM            TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX1)   TO  WRK-NUM
           MOVE    2                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX1) TO  WRK-NUM
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC
      *    食事療養費（日数）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJINISSU(IDX1)
                                             TO  WRK-NUM
               MOVE    2                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC   
      *    食事療養費（合計金額）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJIRYOYOHI(IDX1)
                                             TO  WRK-NUM
               MOVE    8                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（市町村番号）
           MOVE    RECE02-RJNFTNJANUM  TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    老人医療（受給者番号）
           MOVE    RECE02-RJNJKYSNUM   TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担金額（老人減免区分）
      *****IF    ( RECE02-TEISYUTUSAKI     =   "2"        )
           IF    ( RECE02-RJNHKNNUM    NOT =   SPACE )
           AND   (RECE02-ETC-KOHNUM  (1)   =   "019" ) 
               MOVE    4                   TO  WRK-NUM
               MOVE    1                   TO  WRK-NAGASA
           ELSE
               IF      RECE02-GENMENKBN    =   1   OR  2
                   MOVE    RECE02-GENMENKBN    TO  WRK-NUM
                   MOVE    1                   TO  WRK-NAGASA
               END-IF                                 
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（老人外来金額）
           IF    ( RECE02-NYUGAIKBN    =   "2"  )
           AND   ( RECE02-FTNMONEY (IDX1)
                                       >   ZERO )
               MOVE    RECE02-FTNMONEY (IDX1)
                                           TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF   
           IF      RECE02-FTNMONEY (IDX1)
                                       =   ZERO
               IF  (   RECE02-ZAITAKU      =  "1"  OR  "2"     )
               OR  ( ( RECE02-RJNNYUFTN    =   1         ) AND
                     ( RECE02-GENMENKBN    =   1   OR  2 )     )   
                   MOVE    RECE02-FTNMONEY (IDX1)
                                               TO  WRK-NUM
                   MOVE    6                   TO  WRK-NAGASA
               END-IF
           END-IF    
      *
      *    県単用項目
           IF      FLG-RECE11          =   ZERO
               IF      RECE11-RECEDEN-FTNMONEY(IDX1)
                                           =   999999999
                   MOVE    ZERO                TO  WRK-NUM
                   MOVE    6                   TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（老人入院金額）
           IF    ( RECE02-NYUGAIKBN    =   "1"  )
           AND   ( RECE02-FTNMONEY (IDX1)
                                       >   ZERO )
               MOVE    RECE02-FTNMONEY (IDX1)
                                           TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF    
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC     
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2004-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    公費レコード編集処理
      *****************************************************************
       2005-HENSYU-SEC              SECTION.
      *
           IF      RECE02-KOHNUM (1)   =   SPACE
               GO  TO  2005-HENSYU-EXT
           END-IF
      *
      *    公費併用のとき
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
                   OR      RECE02-KOHNUM (IDY) =   SPACE
               COMPUTE IDX     =   IDY     +   1
      *        レセプト特記事項編集情報
               IF      RECE02-2006-RECEDEN-KORECKBN (IDY)
                                               =   "1"
                   CONTINUE
               ELSE 
                   PERFORM 20051-KOH-HENSYU-SEC
               END-IF
           END-PERFORM                      
           .
       2005-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    公費レコード編集処理
      *****************************************************************
       20051-KOH-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "KO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    負担者番号
           IF      RECE02-KOHFTNJANUM (IDY)
                                   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHFTNJANUM (IDY)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-KOHFTNJANUM (IDY)
                                               TO  WRK-DATA
                   ELSE
                       IF      KANACHK-SYUBETU =   "2"
                           MOVE    SPACE           TO  WRK-KOHFTNJANUM
                           MOVE    ZERO            TO  IDXY 
                           PERFORM VARYING IDYY     FROM    1   BY  2
                                   UNTIL   IDYY     >       8
                                   OR      RECE02-KOHFTNJANUM
                                                           (IDY)(IDYY:2)
                                                   =   SPACE
                               MOVE    ZERO            TO  FLG-SUUJI
                               PERFORM VARYING IDXX    FROM    1   BY  1
                                       UNTIL   IDXX    >       10
                                   IF      RECE02-KOHFTNJANUM
                                                           (IDY)(IDYY:2)
                                                   =   TBL-SUUJI-X(IDXX)
                                       MOVE    1           TO  FLG-SUUJI
                                       ADD     1           TO  IDXY
                                       MOVE    TBL-SUUJI-9(IDXX)
                                                           TO
                                               WRK-KOHFTNJANUM (IDXY:1)
                                       MOVE    10          TO  IDXX
                                   END-IF
                               END-PERFORM
                               IF    ( FLG-SUUJI   =   ZERO )
                                   MOVE    SPACE   TO  WRK-KOHFTNJANUM
                                   MOVE    8       TO  IDYY
                               END-IF
                           END-PERFORM
                           IF      WRK-KOHFTNJANUM  NOT =   SPACE
                               MOVE    WRK-KOHFTNJANUM TO  WRK-DATA
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           EVALUATE    RECE02-2006-RECEDEN-FTNJANUMKBN  (IDY)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  WRK-DATA
               WHEN    "02"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-RECEDEN-FTNJANUM (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-RECEDEN-FTNJANUM (IDY)
                                               TO  WRK-DATA
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHFTNJANUM (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHFTNJANUM (IDY) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  WRK-DATA
                       END-IF
                   END-IF
           END-EVALUATE
      *    災害対応
           IF    ( RECE02-RJNGAIKOHFTN (1)
                                         >   0     )
           AND   ( RECE02-KOHFTNJANUM (IDY)
                                         =   SPACE )
               MOVE    SPACE                 TO  WRK-DATA
               STRING  RECE02-KOHHBTNUM (IDY)    DELIMITED BY  SIZE
                       "888888"                  DELIMITED BY  SIZE  
                                                 INTO    WRK-DATA
               END-STRING
           END-IF
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    受給者番号
           IF      RECE02-KOHJKYSNUM  (IDY)
                                       NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHJKYSNUM  (IDY)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHJKYSNUM  (IDY) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (9:7)
                                               TO  WRK-DATA
                       END-IF
                   ELSE
                       IF      KANACHK-SYUBETU =   "2"
                           MOVE    SPACE           TO  WRK-KOHJKYSNUM
                           MOVE    ZERO            TO  IDXY 
                           PERFORM VARYING IDYY     FROM    1   BY  2
                                   UNTIL   IDYY     >       20
                                   OR      RECE02-KOHJKYSNUM
                                                           (IDY)(IDYY:2)
                                                   =   SPACE
                               MOVE    ZERO            TO  FLG-SUUJI
                               PERFORM VARYING IDXX    FROM    1   BY  1
                                       UNTIL   IDXX    >       10
                                   IF      RECE02-KOHJKYSNUM
                                                           (IDY)(IDYY:2)
                                                   =   TBL-SUUJI-X(IDXX)
                                       MOVE    1           TO  FLG-SUUJI
                                       ADD     1           TO  IDXY
                                       MOVE    TBL-SUUJI-9(IDXX)
                                                           TO
                                               WRK-KOHJKYSNUM (IDXY:1)
                                       MOVE    10          TO  IDXX
                                   END-IF
                               END-PERFORM
                               IF    ( FLG-SUUJI   =   ZERO )
                               OR    ( IDXY        >   7    )
                                   MOVE    SPACE   TO  WRK-KOHJKYSNUM
                                   MOVE    20      TO  IDYY
                               END-IF
                           END-PERFORM
                           IF      WRK-KOHJKYSNUM  NOT =   SPACE
                               INITIALIZE                   ORCSNUMAREA
                               MOVE    WRK-KOHJKYSNUM   TO  SNUM-INX
                               CALL    "ORCSNUM"        USING
                                                        ORCSNUMAREA
                               IF      SNUM-RC          =   ZERO
                                   MOVE   SNUM-OUTEDT (9:7)
                                                       TO  WRK-DATA
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           EVALUATE    RECE02-2006-RECEDEN-JKYSNUMKBN  (IDY)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  WRK-DATA
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-RECEDEN-JKYSNUM (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-2006-RECEDEN-JKYSNUM (IDY)
                                                    (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (9:7)
                                               TO  WRK-DATA
                       END-IF
                   END-IF
           END-EVALUATE
      *    災害対応
           IF    ( RECE02-RJNGAIKOHFTN (1)
                                         >   0     )
           AND   ( RECE02-KOHJKYSNUM  (IDY)
                                         =   SPACE )
               MOVE    "9999999"             TO   WRK-DATA
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    任意給付区分                             
           IF    ( RECE02-TEISYUTUSAKI =   "2"     AND
                   RECE02-RJNHKNNUM    =   SPACE                ) 
           OR    ( RECE02-HKNNUM       =   WRK-CONS-HKNNUM1  OR
                                           WRK-CONS-HKNNUM2  OR
                                           WRK-CONS-HKNNUM3  OR
                                           WRK-CONS-HKNNUM4  OR
                                           WRK-CONS-HKNNUM5  OR
                                           WRK-CONS-HKNNUM6     )
               IF      RECE02-KOHNINIKYUKBN (IDY) NOT =   SPACE
                   MOVE    "1"             TO  WRK-DATA
                   MOVE    1               TO  WRK-NAGASA 
               END-IF         
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX)     TO  WRK-NUM
           MOVE    2                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX)   TO  WRK-NUM
           MOVE    8                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（公費）
      ***  IF     RECE02-KOHFTNJANUM (IDY) (1:2)
      ***                                  =   "51"
      ***      IF      RECE02-KOHFTNJANUM (IDY) (5:3)
      ***                                      =   "602"
      ***          MOVE    RECE02-FTNMONEY(IDX)   TO  WRK-NUM
      ***          MOVE    8                      TO  WRK-NAGASA
      ***      END-IF
      ***  ELSE
             IF  ((RECE02-PREFNUM     =   "45"      ) AND
                  (RECE02-KOHNUM(IDY) =   "190"     ) AND
                  (RECE02-SRYYM      <=  "200803"   ))
             OR  ( RECE02-EKF-FTNKEIFLG(IDX)  =  "2" )
               CONTINUE
             ELSE
               IF    ( RECE02-FTNMONEY(IDX)      >   ZERO  )
               OR    ((RECE02-FTNMONEY(IDX)      =   ZERO ) AND
                      (RECE02-ZEROKISAIFLG(IDX)  =   "1"  ))
                   MOVE    RECE02-FTNMONEY(IDX)   TO  WRK-NUM
                   MOVE    8                      TO  WRK-NAGASA
               END-IF
             END-IF
      ***  END-IF
      *    県単用項目
           IF      FLG-RECE11          =   ZERO
               IF      RECE11-RECEDEN-FTNMONEY(IDX)
                                           =   999999999
                   MOVE    ZERO                TO  WRK-NUM
                   MOVE    8                   TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（公費給付対象外来一部負担金）
           IF  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI1       =   1               ) AND
                (RECE02-ZAITAKU-SAIKEI   NOT = 999999999         ) AND
                (IDY                 =   1                       ))
           OR  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI1       =  ZERO             ) AND
                (RECE02-ZAITAKU-SAIKEI   NOT = 999999999 AND ZERO) AND
                (IDY                 =   1                       ))
           OR  ((RECE02-SRYYM       <   WRK-CONS-SRYYM-200804    ) AND
                (RECE02-ZAITAKU-SAIKEI      >  ZERO              ) AND
                (IDY                 =   1                       ))
               MOVE   RECE02-ZAITAKU-SAIKEI    TO  WRK-NUM
               MOVE    6                       TO  WRK-NAGASA
           END-IF
           IF  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI2       =   1               ) AND
                (RECE02-ZAITAKU-SAIKEI2  NOT = 999999999         ) AND
                (IDY                 =   2                       ))
           OR  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI2       =  ZERO             ) AND
                (RECE02-ZAITAKU-SAIKEI2  NOT = 999999999 AND ZERO) AND
                (IDY                 =   2                       ))
               MOVE   RECE02-ZAITAKU-SAIKEI2   TO  WRK-NUM
               MOVE    6                       TO  WRK-NAGASA
           END-IF
           IF  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI3       =   1               ) AND
                (RECE02-ZAITAKU-SAIKEI3  NOT = 999999999         ) AND
                (IDY                 =   3                       ))
               MOVE   RECE02-ZAITAKU-SAIKEI3   TO  WRK-NUM
               MOVE    6                       TO  WRK-NAGASA
           END-IF
           IF  ((RECE02-SRYYM       >=  WRK-CONS-SRYYM-200804    ) AND
                (RECE02-SAIKEIZEROKSI4       =   1               ) AND
                (RECE02-ZAITAKU-SAIKEI4  NOT = 999999999         ) AND
                (IDY                 =   4                       ))
               MOVE   RECE02-ZAITAKU-SAIKEI4   TO  WRK-NUM
               MOVE    6                       TO  WRK-NAGASA
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（公費給付対象入院一部負担金）
           PERFORM 5005-NUM-HENSYU-SEC
      *    予備
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養費（日数）
           PERFORM 5005-NUM-HENSYU-SEC    
      *    食事療養費（合計金額）
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20051-KOH-HENSYU-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    傷病名レコード編集処理
      *****************************************************************
       2006-HENSYU-SEC              SECTION.
      *     
      *    傷病名情報
           INITIALIZE                      RECE031-REC
grpsys     MOVE    RECE02-KEY          TO  RECE031-KEY1
      *
           START   RECE03-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE03
               NOT INVALID
                   PERFORM 990-RECE03-NEXT-SEC
           END-START
      *     
           PERFORM UNTIL  ( FLG-RECE03   =   1                    )
grpsys             OR     ( RECE02-KEY
                                     NOT =   RECE031-KEY1         )
      *        転帰区分:8(移行)以外 処理対象
               IF      RECE031-TENKIKBN     =   "8"
                   CONTINUE
               ELSE    
                   PERFORM 20061-BYOMEI-HENSYU-SEC
               END-IF
      *             
               PERFORM 990-RECE03-NEXT-SEC
           END-PERFORM       
           .
       2006-HENSYU-EXT.
           EXIT. 
      *            
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20061-BYOMEI-HENSYU-SEC         SECTION.
      *     
      *    廃止年月日が設定されているとき廃止年月日＜診療開始日なら
      *    未傷病名コードにする
      *    保険請求対象外のときは未傷病名コードにする
      *    保険請求対象外の傷病名コードのみのときはそのまま記録する
      *
           IF      RECE031-BYOMEIHENFLG =   "1"
                CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE031-BYOMEICD(IDX)   =   SPACE
                   IF      RECE031-BYOMEICD(IDX)   =   ALL "9"
                       CONTINUE
                   ELSE  
                       IF      RECE031-BYOMEICD(IDX)(1:3)
                                                   NOT =   "ZZZ"
                           MOVE    RECE031-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HKNSKYKBN   =   1
                               IF      RECE031-BYOMEICD (2)
                                                   NOT =   SPACE
                                   MOVE    "1" TO  RECE031-BYOMEIHENFLG
                               END-IF
                               MOVE    21      TO  IDX
                           ELSE
                               IF      BYO-HAISIYMD    NOT =   SPACE
                                   IF      BYO-HAISIYMD    <
                                                       RECE031-SRYYMD
                                       MOVE    "1"     TO
                                                   RECE031-BYOMEIHENFLG
                                       MOVE    21      TO  IDX
                                   END-IF 
                               END-IF
                           END-IF    
                       END-IF    
                   END-IF       
               END-PERFORM
           END-IF    
      *
      *    疑いフラグより病名を編集
           IF      RECE031-UTAGAIFLG   NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    RECE031-UTAGAIFLG   TO  SMKBYO-UTAGAIFLG
               MOVE    RECE031-IN-BYOMEI   TO  SMKBYO-BYOMEI
               MOVE    RECE031-BYOMEIHENFLG
                                           TO  SMKBYO-BYOMEIHENFLG
               MOVE    RECE031-BYOMEICD-G  TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
               MOVE    SMKBYO-BYOMEI       TO  RECE031-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  RECE031-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  RECE031-BYOMEICD-G
           ELSE
               MOVE    RECE031-IN-BYOMEI   TO  RECE031-BYOMEI 
           END-IF
      *
      *    傷病名コード／修飾語コードの取得
           MOVE    SPACE           TO  WRK-BYOMEI-AREA
      *
           IF      RECE031-BYOMEIHENFLG =   "1"
      *        編集病名のとき未傷病名コードにする
               MOVE    "0000999"           TO  WRK-BYOMEICD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE031-BYOMEICD(IDX)   =   SPACE
                 IF      RECE031-BYOMEICD(IDX) =   ALL "9"
                     CONTINUE
                 ELSE
                   IF      RECE031-BYOMEICD(IDX)(1:3)  NOT =   "ZZZ"
                       IF      WRK-BYOMEICD                =   SPACE
                           MOVE    RECE031-BYOMEICD(IDX)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   ELSE
                       ADD     1           TO  IDY
                       MOVE    RECE031-BYOMEICD(IDX)(4:)
                                           TO  WRK-RECECD(IDY)
                   END-IF
                 END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "SY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    傷病名コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    診療開始日
           MOVE    RECE031-SRYYMD      TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    転記区分
           EVALUATE    RECE031-TENKIKBN
               WHEN    "1"
                   MOVE    "2"         TO  WRK-DATA
               WHEN    "2"
                   MOVE    "3"         TO  WRK-DATA
               WHEN    "3"
                   MOVE    "4"         TO  WRK-DATA
               WHEN    SPACE
                   MOVE    "1"         TO  WRK-DATA
           END-EVALUATE        
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    修飾語コード
           EVALUATE    WRK-BYOMEICD 
               WHEN    "0000998"
               WHEN    "0000999"
                   CONTINUE
               WHEN    OTHER
                   MOVE    WRK-RECECD-G        TO  WRK-DATA
                   MOVE    80                  TO  WRK-NAGASA
           END-EVALUATE
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    傷病名称
           IF      WRK-BYOMEICD        =   "0000999"   OR
                                           "0000998"
               MOVE    RECE031-BYOMEI      TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    主傷病
           IF      RECE031-SYUBYOFLG-X         =   "1" 
           AND     SYS-2005-SYUBYOHENFLG   NOT =   ZERO
               MOVE    "01"                TO  WRK-DATA
           END-IF 
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    補足コメント
           MOVE    RECE031-HOSOKU-COMMENT
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20061-BYOMEI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    摘要レコード編集処理
      *****************************************************************
       2007-HENSYU-SEC              SECTION.
      *                                        
           IF      WRK-PARA-PRTKBN     =   "9"
               PERFORM 8900-SYSTEM-RESERVE-98-SEC
           END-IF
      *    摘要情報
           INITIALIZE                      RECE041-REC
grpsys     MOVE    RECE02-KEY          TO  RECE041-KEY1
      *
           START   RECE041-FILE
               KEY IS    >=   RECE041-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE041-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE04           =   1                   )
grpsys         OR     ( RECE02-KEY       NOT =   RECE041-KEY1        )
      *
      *        負担区分編集処理
               PERFORM 20070-FTNKBN-HENSYU-SEC
      *
               IF      FLG-COMMENT         =   ZERO
                   MOVE    1                   TO  FLG-COMMENT
      *            災害対応
                   IF      RECE02-RJNGAIKOHFTN (1) =   3   OR  ZERO
                       CONTINUE
                   ELSE
                       PERFORM 20070-DISASTER-HENSYU-SEC
                   END-IF
      *
      *            第三者行為
                   PERFORM 20073-DAISAN-HENSYU-SEC
      *
      *            症状詳記（適用欄上段コメント・入退院履歴・手術歴・保険履歴）
                   MOVE    WRK-FTNKBN          TO  WRK-COM-FTNKBN
                   MOVE    ZERO                TO  FLG-TOYAKUFLG
                   PERFORM 2008-COMMENT-HEN-01-SEC
               END-IF
      *
      *        検査に判断料がある場合の検査の編集
               IF    ( RECE041-SRYKBN      >   "60"     )
               AND   ( TBL-KENSA-RECE04-MAX
                                           >   0        )
                   MOVE    RECE041-REC         TO  WRK-TAIHI-RECE041-REC
                   MOVE    WRK-FTNKBN          TO  WRK-TAIHI-FTNKBN
      *
                   PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
                   MOVE    WRK-TAIHI-RECE041-REC
                                               TO  RECE041-REC
                   MOVE    WRK-TAIHI-FTNKBN    TO  WRK-FTNKBN
      *
                   INITIALIZE                  TBL-KENSA-RECE04-AREA
               END-IF
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *
      *        仮レセデータフラグ設定
               IF      RECE041-TOYAKUFLG =   SPACE
                   MOVE    ZERO              TO  FLG-TOYAKUFLG
               ELSE
                   MOVE    1                 TO  FLG-TOYAKUFLG
               END-IF 
      *
      *       摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   IF     (WRK-PARA-PRTKBN     =   "9")  AND
                          (RECE041-SRYKBN      =   "21"
                                               OR  "22"
                                               OR  "23")
                       PERFORM 8901-YKZ-HENSYU-SEC
                   ELSE
                       PERFORM 20072-ZAI-HENSYU-SEC
                   END-IF
      * 
                   PERFORM 990-RECE041-NEXT-SEC
               END-IF     
           END-PERFORM
      *
      *    検査に判断料がある場合の検査の編集
           IF    ( TBL-KENSA-RECE04-MAX
                                       >   0        )
               PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
               INITIALIZE                  TBL-KENSA-RECE04-AREA
           END-IF
      *
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    負担区分編集処理
      *****************************************************************
       20070-FTNKBN-HENSYU-SEC    SECTION.
      *      
      *    主保険＋３種公費のとき該当の負担区分がないので
      *    ＋２種公費のときと同じ負担区分を編集する      
      *   
           INITIALIZE                 WRK-FTNKBN-AREA
      *
           MOVE    ZERO               TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1  BY 1
                   UNTIL ( IDX0    >  TCOMB-MAX )
                    OR   ( FLG-HIT =  1 )
grpsys          IF    ( RECE041-HOSPNUM    =   TCOMB-HOSPNUM(IDX0))
                AND  ( RECE041-PTID        =   TCOMB-PTID   (IDX0))
                AND  ( RECE041-HKNCOMBI    =   TCOMB-HKNCOMBINUM
                                                            (IDX0))
                    MOVE   TCOMB-OCC (IDX0)
                                       TO  HKNCOMBI-REC
                    MOVE   1           TO  FLG-HIT
               END-IF
           END-PERFORM
      * 
           MOVE    ZERO            TO  FLG-HKNCOMBI
           IF    ( FLG-HIT         =  ZERO )
      *        保険組合せ読込み
               INITIALIZE                 HKNCOMBI-REC
grpsys         MOVE    RECE041-HOSPNUM TO  COMB-HOSPNUM
               MOVE    RECE041-PTID    TO  COMB-PTID
               MOVE    RECE041-HKNCOMBI
                                      TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC   TO  MCPDATA-REC
               PERFORM 910-HKNCOMBI-INV-SEC
               IF      FLG-HKNCOMBI   =   1
                   DISPLAY "HKNCONBI INV"
                   GO  TO  20070-FTNKBN-HENSYU-EXT
               END-IF
           END-IF
      *
      *    平成２０年４月改正以降の負担区分設定
           IF      RECE02-SRYYM        >=  WRK-CONS-SRYYM-200804
               PERFORM 200701-FTNKBN-HENSYU-200804-SEC
               GO  TO  20070-FTNKBN-HENSYU-EXT
           END-IF
      *                                                      
      *    主保険のみ      
           IF      COMB-HKNNUM    NOT =   SPACE
           AND     COMB-KOH1HKNNUM    =   SPACE
               MOVE    "1"            TO  WRK-FTNKBN
           END-IF
      *         
      *    老人保険のみ     
           IF      COMB-HKNNUM    NOT =   SPACE
           AND     COMB-KOH1HKNNUM    =   "027"
           AND     COMB-KOH2HKNNUM    =   SPACE
               MOVE    "1"            TO  WRK-FTNKBN
           END-IF
      *     
      *    生保のみ（三種公費のとき）      
           IF      WRK-3SYU-KBN       =   ALL "1"  
           AND     RECE02-HKNNUM      =   SPACE            
               IF      COMB-KOH1HKNNUM    =   "012"
               AND     COMB-KOH2HKNNUM    =   SPACE
                   MOVE    "1"            TO  WRK-FTNKBN
               END-IF
           END-IF    
      *???
      **     DISPLAY "HKNNUM=" COMB-HKNNUM  "KOH1=" COMB-KOH1HKNNUM "##"
      **             "KOH2="   COMB-KOH2HKNNUM "##" WRK-FTNKBN        
      *???              
      *
           IF      WRK-FTNKBN     NOT =   SPACE
               GO  TO  20070-FTNKBN-HENSYU-EXT
           END-IF
      *
      *    公費の編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
                   OR      COMB-KOHHKNNUM  (IDX)   =   SPACE
               IF      COMB-KOHHKNNUM  (IDX)   =   "027"
                   CONTINUE
               ELSE        
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       3
                           OR      RECE02-KOHNUM(IDY)  =   SPACE
                       IF     (COMB-KOHHKNNUM(IDX) =
                                              RECE02-KOHNUM(IDY)) AND
                              (COMB-KOHID(IDX) =
                                              RECE02-KOHID(IDY))
                           MOVE    1           TO  WRK-KOHNUM (IDY)
                           MOVE    3           TO  IDY                
                       END-IF
                   END-PERFORM
               END-IF    
           END-PERFORM
      *???
      **     DISPLAY "RECE02(1)=" RECE02-KOHNUM (1) "(2)="
      **                          RECE02-KOHNUM (2)
      **             "(3)=" RECE02-KOHNUM (3) "##"
      **     DISPLAY "(1)=" WRK-KOHNUM (1) "(2)=" WRK-KOHNUM (2)
      **             "(3)=" WRK-KOHNUM (3) "##"
      *???                  
      *                                                      
      *    主保険があるとき      
           IF      COMB-HKNNUM        NOT =   SPACE
               IF      COMB-KOH1HKNNUM    NOT =   "027"
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "2"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "4"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    ZERO
                           MOVE    "1"            TO  WRK-FTNKBN
                   END-EVALUATE
               ELSE
      *    老人保険があるとき     
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "8"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "A"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    ZERO
                           MOVE    "1"            TO  WRK-FTNKBN
                   END-EVALUATE
               END-IF    
           ELSE
      *     
      *        公費単独（三種公費）のとき      
               IF      WRK-3SYU-KBN       =   ALL "1"  
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                                                  ALSO    WRK-KOHNUM (3)
                       WHEN    1          ALSO    ZERO     ALSO    ZERO
                           MOVE    "5"            TO  WRK-FTNKBN
                       WHEN    ZERO       ALSO    1        ALSO    ZERO
                           MOVE    "6"            TO  WRK-FTNKBN
                       WHEN    1          ALSO    1        ALSO    ZERO
                           MOVE    "7"            TO  WRK-FTNKBN
                       WHEN    1          ALSO    ZERO     ALSO    1
                           MOVE    "2"            TO  WRK-FTNKBN
                       WHEN    ZERO       ALSO    1        ALSO    1
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    1          ALSO    1        ALSO    1
                           MOVE    "4"            TO  WRK-FTNKBN
                   END-EVALUATE
               ELSE    
      *                                                      
      *        公費単独（三種公費以外）のとき      
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "5"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "6"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "7"            TO  WRK-FTNKBN
                   END-EVALUATE
               END-IF
           END-IF
      *???  
           IF      WRK-FTNKBN    =   SPACE
               DISPLAY "FTNKBN ERR " RECE02-PTNUM "##"
           END-IF
      *???              
           .
       20070-FTNKBN-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    平成２０年４月改正以降の負担区分設定処理
      *****************************************************************
       200701-FTNKBN-HENSYU-200804-SEC    SECTION.
      *
      *    公費の編集
      *    レセ電用負担区分編集フラグより編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE02-HKNCOMBI (IDX)   =   ZERO
               IF      COMB-HKNCOMBINUM    =   RECE02-HKNCOMBI (IDX)
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       4
                       IF    ( RECE02-KOHNUMHC  (IDX IDY)
                                                   NOT =   SPACE )
                       AND   ( RECE02-FTNKBNFLG (IDX IDY)
                                                       =   ZERO  )
                           PERFORM VARYING IDX0    FROM    1   BY  1
                                   UNTIL   IDX0    >       4
                                   OR      RECE02-KOHNUM (IDX0)
                                                   =       SPACE
                               IF    ( RECE02-KOHNUMHC  (IDX IDY) =
                                                  RECE02-KOHNUM (IDX0) )
                               AND   ( RECE02-KOHIDHC   (IDX IDY) =
                                                  RECE02-KOHID  (IDX0) )
                                   MOVE    1       TO  WRK-KOHNUM (IDX0)
                                   MOVE    4       TO  IDX0
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
                   MOVE    10                  TO  IDX     
               END-IF
           END-PERFORM
      *???
      **     DISPLAY "RECE02(1)=" RECE02-KOHNUM (1) "(2)="
      **                          RECE02-KOHNUM (2)
      **             "(3)=" RECE02-KOHNUM (3) "##"
      **     DISPLAY "(1)=" WRK-KOHNUM (1) "(2)=" WRK-KOHNUM (2)
      **             "(3)=" WRK-KOHNUM (3) "##"
      *???
           IF      COMB-HKNNUM         NOT =   SPACE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者 
                   WHEN    0   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "1"            TO  WRK-FTNKBN
      *            ２者 
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "2"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "3"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "E"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "G"            TO  WRK-FTNKBN
      *            ３者 
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "4"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "M"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "N"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "O"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "P"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "Q"            TO  WRK-FTNKBN
      *            ４者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "V"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "W"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "X"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Y"            TO  WRK-FTNKBN
      *            ５者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "9"            TO  WRK-FTNKBN
               END-EVALUATE
           ELSE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者 
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "5"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "6"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "B"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "C"            TO  WRK-FTNKBN
      *            ２者 
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "7"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "H"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "I"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "J"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "K"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "L"            TO  WRK-FTNKBN
      *            ３者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "R"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "S"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "T"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "U"            TO  WRK-FTNKBN
      *            ４者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Z"            TO  WRK-FTNKBN
               END-EVALUATE
           END-IF
      *???  
           IF      WRK-FTNKBN    =   SPACE
               DISPLAY "FTNKBN ERR " RECE02-PTNUM "##"
           END-IF
      *???              
           .
       200701-FTNKBN-HENSYU-200804-EXT.
           EXIT. 
      *
      *****************************************************************
      *    摘要データ（災害コメント）編集処理
      *****************************************************************
       20070-DISASTER-HENSYU-SEC       SECTION.
      *
           MOVE    "01"                TO  WRK-SRYKBN 
      *
           MOVE    SPACE               TO  WRK-DISASTER-COMMENT
           EVALUATE    RECE02-RJNGAIKOHFTN (1)
               WHEN    1
               WHEN    8
               WHEN    10
               WHEN    11
                   MOVE    "不詳"      TO  WRK-DISASTER-COMMENT
               WHEN    2
               WHEN    7
                   MOVE    "災１"      TO  WRK-DISASTER-COMMENT
               WHEN    4
                   MOVE    "災２"      TO  WRK-DISASTER-COMMENT
               WHEN    5
                   MOVE    "不詳・災１"
                                       TO  WRK-DISASTER-COMMENT
           END-EVALUATE
           PERFORM 200701-DISASTER-COMMENT-HEN-SEC
      *
      *    震災以前一部負担金の記載（災２）
           MOVE    SPACE               TO  WRK-DISASTER-COMMENT
           IF      RECE02-RJNGAIKOHFTN (1) =   4
               MOVE    RECE02-GENMEN-NASI-KIN
                                       TO  WRK-FUTAN
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-FUTAN       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                 IF   RECE02-SRYYM   =  "201103"
                   STRING  "【震災以前一部負担金】　　　　"
                                               DELIMITED  BY  SIZE
                           KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                           "円"                DELIMITED  BY  SIZE
                                           INTO    WRK-DISASTER-COMMENT
                   END-STRING
                 ELSE
                   STRING  "【非減免対象分扱い一部負担金】"
                                               DELIMITED  BY  SIZE
                           KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                           "円"                DELIMITED  BY  SIZE
                                           INTO    WRK-DISASTER-COMMENT
                   END-STRING
                 END-IF
               END-IF
               PERFORM 200701-DISASTER-COMMENT-HEN-SEC
           END-IF
      *                   
      *    災害対応（連絡先記載）
           MOVE    SPACE               TO  WRK-DISASTER-COMMENT
           EVALUATE    RECE02-RJNGAIKOHFTN (1)
               WHEN    6
               WHEN    7
               WHEN    8
               WHEN    9
               WHEN    11
                   IF    ( RECE02-HOME-POST1
                                           =   SPACE )
                   AND   ( RECE02-HOME-ADRS1
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-POST1
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-NAME
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-ADRS1
                                           =   SPACE )
                   AND   ( RECE02-TEL      =   SPACE )
                       CONTINUE
                   ELSE
      *                自宅住所記載
                       IF   ( RECE02-HOME-ADRS1      =   SPACE )
                       AND  ( RECE02-HOME-POST1      =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "【住所】"  TO  WRK-DISASTER-COMMENT
                           PERFORM 200701-DISASTER-COMMENT-HEN-SEC
      *                    自宅住所記載
                           MOVE    SPACE               TO  WRK-ADRS
                           IF      RECE02-HOME-POST1   =   SPACE
                               MOVE    RECE02-HOME-ADRS1   TO  WRK-ADRS
                           ELSE
                               STRING   "〒"       DELIMITED  BY  SIZE
                                    RECE02-HOME-POST1
                                                   DELIMITED  BY  SPACE
                                    RECE02-HOME-ADRS1
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-ADRS
                               END-STRING
                           END-IF
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       4
                                   OR      WRK-ADRS-R (IDY)
                                                   =       SPACE
                              MOVE    WRK-ADRS-R (IDY)    TO
                                                   WRK-DISASTER-COMMENT
                              PERFORM 200701-DISASTER-COMMENT-HEN-SEC
                          END-PERFORM
                       END-IF
      *                連絡先記載
                       IF    ( RECE02-RENRAKU-POST1
                                               =   SPACE )
                       AND   ( RECE02-RENRAKU-NAME
                                               =   SPACE )
                       AND   ( RECE02-RENRAKU-ADRS1
                                               =   SPACE )
                       AND   ( RECE02-TEL      =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "【連絡先】"
                                               TO  WRK-DISASTER-COMMENT
                           PERFORM 200701-DISASTER-COMMENT-HEN-SEC
      *                    事業所名記載
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       2
                                   OR      RECE02-RENRAKU-NAME-R (IDY)
                                                   =       SPACE
                              MOVE    RECE02-RENRAKU-NAME-R (IDY)
                                               TO  WRK-DISASTER-COMMENT
                              PERFORM 200701-DISASTER-COMMENT-HEN-SEC
                          END-PERFORM
      *                    連絡先住所記載
                           MOVE    SPACE               TO  WRK-ADRS
                           IF      RECE02-RENRAKU-POST1
                                                       =   SPACE
                               MOVE    RECE02-RENRAKU-ADRS1
                                                           TO  WRK-ADRS
                           ELSE
                               STRING   "〒"       DELIMITED  BY  SIZE
                                    RECE02-RENRAKU-POST1
                                                   DELIMITED  BY  SPACE
                                    RECE02-RENRAKU-ADRS1
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-ADRS
                               END-STRING
                           END-IF
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       4
                                   OR      WRK-ADRS-R (IDY)
                                                   =       SPACE
                              MOVE    WRK-ADRS-R (IDY)    TO
                                                   WRK-DISASTER-COMMENT
                              PERFORM 200701-DISASTER-COMMENT-HEN-SEC
                           END-PERFORM
      *                    連絡先記載
                           IF     RECE02-TEL       =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPACE       TO
                                                   WRK-DISASTER-COMMENT
                               STRING  "ＴＥＬ："  DELIMITED  BY  SIZE
                                       RECE02-TEL  DELIMITED  BY  SIZE
                                                   INTO
                                                   WRK-DISASTER-COMMENT
                               END-STRING
                              PERFORM 200701-DISASTER-COMMENT-HEN-SEC
                          END-IF
                      END-IF
                  END-IF
           END-EVALUATE

           .
       20070-DISASTER-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（災害用コメント）編集処理
      *****************************************************************
       200701-DISASTER-COMMENT-HEN-SEC  SECTION.
      *
           IF      WRK-DISASTER-COMMENT         =   SPACE
               GO  TO  200701-DISASTER-COMMENT-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    "810000001"         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    WRK-DISASTER-COMMENT         TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    コメントのクリア
           MOVE    SPACE                TO  WRK-DISASTER-COMMENT
           .
       200701-DISASTER-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（第三者行為コメント）編集処理
      *****************************************************************
       20073-DAISAN-HENSYU-SEC       SECTION.
      *
           MOVE    "01"                TO  WRK-SRYKBN
      *
           MOVE    SPACE               TO  WRK-DISASTER-COMMENT
           IF  (RECE02-DAISAN-GAITO   =   1  OR  2  OR  4)
           AND (RECE02-DAISAN-TOKKI   =   ZERO           )
               MOVE    RECE02-DAISAN-TOTALTEN  TO  WRK-FUTAN3
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-FUTAN3      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                 STRING  "（第三者行為）請求点　　　"
                                         DELIMITED  BY  SIZE
                          KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         DELIMITED  BY  SIZE
                          "点"           DELIMITED  BY  SIZE
                                         INTO    WRK-DISASTER-COMMENT
                 END-STRING
               END-IF
               PERFORM 200701-DISASTER-COMMENT-HEN-SEC
      *
               MOVE    SPACE           TO  WRK-DISASTER-COMMENT
               MOVE    RECE02-DAISAN-FTNMONEY  TO  WRK-FUTAN4
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-FUTAN4      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                 STRING  "（第三者行為）一部負担額"
                                         DELIMITED  BY  SIZE
                          KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         DELIMITED  BY  SIZE
                          "円"           DELIMITED  BY  SIZE
                                         INTO    WRK-DISASTER-COMMENT
                 END-STRING
               END-IF
               PERFORM 200701-DISASTER-COMMENT-HEN-SEC
           END-IF
      *
           .
       20073-DAISAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       20071-ZAI-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
                                           TBL-KOUI-AREA
                                           TBL-KOUI-SRYKBN12-AREA
                                           TBL-KOUI-SRYKBN12-CHU-AREA
                                           TBL-COMMENT-AREA
           INITIALIZE                      WRK-RECE05-REC
                                           WRK-KASAN-RECE05-REC
                                           WRK-700-RECE05-AREA
                                           WRK-700-DOUJI-RECE05-AREA
                                           WRK-721-RECE05-REC
                                           WRK-721-DOUJI-RECE05-REC
                                           WRK-SRYKBN70-RECE05-REC
           INITIALIZE                      TBL-RECE055-COMMENT-AREA
      *
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX1
                                           WRK-TBLMAX6
                                           WRK-TBLMAX7
                                           WRK-TBLMAX7-SRT
                                           WRK-TBLMAX8
                                           FLG-TEIGEN
                                           FLG-YKZ20
                                           FLG-KOKUJISKBKBN1
                                           FLG-SRYSYUKBN721
                                           FLG-SRYSYUKBN721-DOUJI 
                                           FLG-SRYKBN70-TEIGEN
                                           FLG-KENSA-KASAN
                                           FLG-KENSA-TEIGEN
                                           WRK-SRYCD1-MAX
                                           WRK-SRYCD71-MAX
                                           WRK-SRYCD72-MAX
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
      *                                     
           MOVE    RECE041-SRYKBN      TO  WRK-SRYKBN
      *
      *    検査（判断料）の退避
           IF     ( RECE041-KNSJISKBN   =   2 )
               ADD     1               TO  TBL-KENSA-RECE04-MAX
               MOVE    RECE041-REC     TO  TBL-KENSA-RECE04-REC
                                           (TBL-KENSA-RECE04-MAX)
           END-IF
           MOVE    ZERO                 TO  TBL-KENSA-RECE05-MAX
      *
           INITIALIZE                       RECE05-REC
grpsys     MOVE    RECE041-KEY1         TO  RECE05-KEY1
           MOVE    RECE041-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END                =   1                   )
               OR     ( FLG-RECE05             =   1                   )
grpsys         OR     ( RECE041-KEY1       NOT =   RECE05-KEY1         )
               OR     ( RECE041-ZAINUM     NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                         OR      ( FLG-END           =   1     )
                 IF    ( RECE05-RECEDENFLG(IDX) =   SPACE )
                 AND   ( RECE05-MEISKYFLG (IDX)
                                            NOT =   "2"   )
                   MOVE    SPACE                TO  WRK-KIZAI-SRYCD
                   IF      RECE05-SRYCD (IDX) (1:3) = "059"
                       MOVE    RECE05-SRYCD (IDX)
                                                TO  WRK-KIZAI-SRYCD
                       MOVE    "777770000"      TO  RECE05-SRYCD (IDX)
                   END-IF
      *             IF      RECE05-INGAIFLG(IDX) =   "1"
      *                 MOVE    "699990001"      TO  RECE05-SRYCD (IDX)
      *             END-IF
      *
                   EVALUATE    RECE05-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
      *                    空白または全角スペースのみのフリーコメントは記載しない
                           MOVE    ZERO                TO  WRK-SJ-CHK
                           IF      RECE05-SRYCD (IDX)  =   "810000001"
                               IF      RECE05-NAME  (IDX)  =   SPACE
                                   CONTINUE
                               ELSE
                                   PERFORM VARYING IDY FROM    1   BY  2
                                           UNTIL   IDY >       200
                                           OR  RECE05-NAME(IDX) (IDY:2)
                                                           =   SPACE
                                           OR  WRK-SJ-CHK  =   1
                                       IF       RECE05-NAME(IDX) (IDY:2)
                                                       NOT =   "　"
                                           MOVE    1   TO  WRK-SJ-CHK
                                       END-IF
                                   END-PERFORM
                               END-IF
                               IF      WRK-SJ-CHK      =   1
      *                            後ろの全角空白を削除する
                                   INITIALIZE          ORCSKANACHKAREA
                                   MOVE    "1"     TO  KANACHK-SYORI
                                   MOVE    "1"     TO  KANACHK-SYORIKBN 
                                   MOVE    RECE05-NAME(IDX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"
                                               USING   ORCSKANACHKAREA
                                   IF    ( KANACHK-RC  =   ZERO )
                                   AND   ( KANACHK-MAX >   ZERO )
                                       MOVE    SPACE
                                                   TO  RECE05-NAME(IDX)
                                       MOVE    KANACHK-OUT-INPUT
                                                       (1:KANACHK-MAX)
                                                   TO  RECE05-NAME(IDX)
                                   END-IF
                               END-IF
                           ELSE
                               MOVE    1               TO  WRK-SJ-CHK
                           END-IF
                           IF      WRK-SJ-CHK          =   ZERO
                               CONTINUE
                           ELSE                      
      *                        検査（判断料）の場合の退避
                               IF      RECE041-KNSJISKBN   =   2
                                   PERFORM
                                       20013-KENSA-HANDANRYO-TAIHI-SEC
                               ELSE
                                   IF      RECE041-SRYKBN  =   "11"   OR
                                                               "12"   OR
                                                               "13"   OR
                                                               "14"   OR
                                                               "70"
                                     PERFORM
                                      200711-SRYKBN13-COM-TAIHI-SEC
                                   ELSE
                                      IF    ( RECE041-SRYKBN
                                                           =   "23"  )
                                      AND   ( RECE041-SIPPUFLG
                                                       NOT =   SPACE )
                                        IF      RECE05-SRYCD (IDX)
                                                           = "820000047"
                                          PERFORM
                                              200711-COMMENT-TAIHI-SEC
                                        ELSE
                                          PERFORM
                                           200711-SRYKBN13-COM-TAIHI-SEC
                                        END-IF
                                      ELSE
                                        PERFORM 200711-COMMENT-TAIHI-SEC
                                      END-IF
                                   END-IF
                               END-IF
                           END-IF
                       WHEN    "1"
      *                    検査（判断料）の場合の退避
                           IF     ( RECE041-KNSJISKBN   =   2 )
                               PERFORM 20013-KENSA-HANDANRYO-TAIHI-SEC
                           ELSE
                               PERFORM 200711-KOUI-TAIHI-SEC
                           END-IF
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                           IF      RECE041-ZAIREQFLG       =   1
                               PERFORM 200711-KIZAI-ACCT-TAIHI-SEC
                           END-IF
                   END-EVALUATE
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE05-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX 
               MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX 
                       MOVE    WRK-700-RECE05-REC (IDXY)  TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
           IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
               MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
               MOVE    WRK-721-DOUJI-RECE05-REC    TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                       MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                       MOVE    WRK-700-DOUJI-RECE05-REC (IDXY)
                                                          TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
      *
           .
       20071-ZAI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-COMMENT-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE05-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE05-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE05-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-COMMENT-TAIHI-EXT
               END-IF
           END-IF    
      *     
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0300 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    RECE05-SRYCD (IDX)  TO
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
               MOVE    RECE05-NAME  (IDX)  TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
      *
               IF      RECE05-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE05-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
                   END-IF
      *            コメント
                   IF    ( RECE05-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE05-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE05-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE05-INPUTCOMENT (IDX)     TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE05-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
               END-IF
           END-IF
           .
       200711-COMMENT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-SRYKBN13-COM-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE05-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE05-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE05-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-SRYKBN13-COM-TAIHI-EXT
               END-IF
           END-IF    
      *               
           IF      WRK-TBLMAX8         >=  100
               MOVE    "*** ORCR0300 COMMENT TABLE  OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX8
      *
               MOVE    RECE05-SRYCD (IDX)  TO
                                   TBL-COMMENT-SRYCD (WRK-TBLMAX8)
               MOVE    RECE05-NAME  (IDX)  TO
                                   TBL-COMMENT-NAME  (WRK-TBLMAX8)
      *
               IF      RECE05-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE05-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-COMMENT-SRYCD (WRK-TBLMAX8)
                   END-IF
      *            コメント
                   IF    ( RECE05-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE05-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE05-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE05-INPUTCOMENT (IDX)     TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE05-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
               END-IF
           END-IF
          .
       200711-SRYKBN13-COM-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ退避処理
      *****************************************************************
       200711-KOUI-TAIHI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-ACCTP-SYUTEN
      *
      *    再診の時間外のとき
           IF      RECE041-SRYKBN      =   "12"
               PERFORM VARYING IDV1    FROM    1   BY  1
                       UNTIL   IDV1    >       WRK-CONS-JIKANGAI-MAX
                   IF      RECE05-SRYCD (IDX)
                                   =   WRK-CONS-JIKANGAI-SRYCD (IDV1)
                       MOVE    WRK-CONS-JIKANGAI-TEN (IDV1)
                                               TO  WRK-ACCTP-SYUTEN
                       MOVE    WRK-CONS-JIKANGAI-MAX
                                               TO  IDV1
                   END-IF
               END-PERFORM
               IF      WRK-ACCTP-SYUTEN        >   ZERO 
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE05-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE041-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE05-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
                   ADD     WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN-G
      *    
                   MOVE    ZERO          TO  WRK-ACCTP-SYUTEN
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
           END-IF
      *
           IF    ( RECE041-SRYKBN      =   "11"   OR  "12"     )
           AND   ( RECE041-ZAIREQFLG   =   "1"                 )
           AND   ( RECE05-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
      *        基本項目のとき診療会計マスタ付加情報から点数を取得
               PERFORM VARYING IDV1    FROM    1   BY  1
                       UNTIL   IDV1    >       5
                       OR      RECE041-ACCTP-SRYCD (IDV1)
                                       =   SPACE
                   IF      RECE05-SRYCD (IDX)  =
                                           RECE041-ACCTP-SRYCD (IDV1)
                       MOVE    RECE041-ACCTP-SYUTEN (IDV1)
                                               TO  WRK-ACCTP-SYUTEN
                       MOVE    5               TO  IDV1
                   END-IF
               END-PERFORM
      *
      *        加算項目だが基本項目扱いのとき剤を別にする
               IF    ( RECE05-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
               AND   ( RECE05-KOKUJISKBKBN2 (IDX)
                                       =   "7" OR  "9"         )
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE05-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE041-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE05-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
      *    
                   MOVE    ZERO          TO  WRK-ACCTP-SYUTEN
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
      *
      *        基本項目のとき付加情報の点数を設定
               IF      RECE041-SRYKBN      =   "11"
                   MOVE    WRK-ACCTP-SYUTEN  TO  RECE041-SYUTEN1
               END-IF
           END-IF
      *
           IF    ( RECE041-SRYKBN      =   "13"    OR  "14"    )
      *****AND   ( RECE041-ZAIREQFLG   =   "1"                 )
           AND   ( RECE05-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
      *        加算項目だが基本項目扱いのとき剤を別にする
               IF    ( RECE05-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
               AND   ( RECE05-DATAKBN (IDX)
                                       =   "2"                 )
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE05-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE041-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE05-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    RECE05-CHUKSNCD (IDX)
                                           TO
                                           TBL-KOUI-SRYKBN12-CHUKSNCD
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE05-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
                   ADD     RECE05-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN-G
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
           END-IF
      *
           IF    ( RECE041-SRYKBN      =   "13"    OR  "14" )
           AND   ( RECE05-KOKUJISKBKBN1 (IDX)
                                       =   "7" OR  "9"      )
           AND   ( RECE05-CHUKSNCD      (IDX)
                                       =   "1419"           )
      *        注加算コードが１４１９の加算項目のとき剤を別にする
               ADD     1               TO  TBL-KOUI-SRYKBN12-CHU-MAX
               MOVE    RECE05-KEY      TO  TBL-KOUI-SRYKBN12-CHU-KEY
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    IDX             TO
                                   TBL-KOUI-SRYKBN12-CHU-SHORI-NUM
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE041-ZAIKAISU
                                       TO  TBL-KOUI-SRYKBN12-CHU-KAISU
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE05-SRYCD (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-SRYCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE05-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE05-CHUKSNCD (IDX)
                                       TO
                                       TBL-KOUI-SRYKBN12-CHU-CHUKSNCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               ADD     RECE05-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN-G
               GO  TO  200711-KOUI-TAIHI-EXT
           END-IF
      *
      *    画像診断の撮影料のとき順番を変える（同時撮影以外）
           IF     RECE05-SRYSYUKBN-X (IDX)
                                       =   "721"
             MOVE    RECE05-REC          TO  WRK-RECE05-REC
             MOVE    IDX                 TO  IDW1
      * 
             IF     RECE05-SRYCD (IDX)
                                   NOT =   WRK-CONS-DOUJI-SATUEI (1)
                                       AND WRK-CONS-DOUJI-SATUEI (2) 
                                       AND WRK-CONS-DOUJI-SATUEI (3) 
                                       AND WRK-CONS-DOUJI-SATUEI (4) 
                                       AND WRK-CONS-DOUJI-SATUEI (5) 
                                       AND WRK-CONS-DOUJI-SATUEI (6) 
                                       AND WRK-CONS-DOUJI-SATUEI (7) 
                                       AND WRK-CONS-DOUJI-SATUEI (8)
      *
               IF      FLG-SRYSYUKBN721    >   ZERO
                   MOVE     FLG-SRYSYUKBN721   TO  IDX 
                   MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE05-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE05-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX 
                           MOVE    WRK-700-RECE05-REC (IDXY)
                                                           TO RECE05-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE05-AREA
                   END-IF
               END-IF
               IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
                   MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
                   MOVE    WRK-721-DOUJI-RECE05-REC    TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO               TO  FLG-SRYSYUKBN721-DOUJI
                   INITIALIZE                   WRK-721-DOUJI-RECE05-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                                   TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE05-REC
                                                   TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                           MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                           MOVE    WRK-700-DOUJI-RECE05-REC (IDXY)
                                                          TO  RECE05-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
                       INITIALIZE              WRK-700-DOUJI-RECE05-AREA
                   END-IF
               END-IF
             END-IF  
      *
             MOVE    WRK-RECE05-REC      TO  RECE05-REC
             MOVE    IDW1                TO  IDX
      *
             IF     RECE05-SRYCD (IDX)
                                   NOT =   WRK-CONS-DOUJI-SATUEI (1)
                                       AND WRK-CONS-DOUJI-SATUEI (2) 
                                       AND WRK-CONS-DOUJI-SATUEI (3) 
                                       AND WRK-CONS-DOUJI-SATUEI (4) 
                                       AND WRK-CONS-DOUJI-SATUEI (5) 
                                       AND WRK-CONS-DOUJI-SATUEI (6) 
                                       AND WRK-CONS-DOUJI-SATUEI (7) 
                                       AND WRK-CONS-DOUJI-SATUEI (8)
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE05-REC          TO WRK-721-RECE05-REC
             ELSE
               MOVE    IDX                 TO  FLG-SRYSYUKBN721-DOUJI
               MOVE    RECE05-REC          TO  WRK-721-DOUJI-RECE05-REC
             END-IF
           ELSE
               IF    ( RECE05-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE05-REC  TO  WRK-SRYKBN70-RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
                   IF    ( RECE05-SRYSYUKBN-X (IDX)
                                               =   "700"  )
                   AND   ( RECE05-DATAKBN   (IDX)
                                               =   "2"    )
                   AND   (  (  FLG-SRYSYUKBN721-DOUJI
                                               >   ZERO ) 
                                   OR
                            ( FLG-SRYSYUKBN721 >   ZERO ) )
                       IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO 
                           ADD     1           TO
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                           MOVE    IDX         TO
                                           FLG-SRYSYUKBN700-DOUJI
                                            (WRK-SRYSYUKBN700-DOUJI-MAX) 
                           MOVE    RECE05-REC  TO
                                           WRK-700-DOUJI-RECE05-REC
                                            (WRK-SRYSYUKBN700-DOUJI-MAX) 
                       ELSE
                           ADD     1           TO  WRK-SRYSYUKBN700-MAX
                           MOVE    IDX         TO  FLG-SRYSYUKBN700
                                                  (WRK-SRYSYUKBN700-MAX) 
                           MOVE    RECE05-REC  TO WRK-700-RECE05-REC
                                                  (WRK-SRYSYUKBN700-MAX)
                       END-IF
                   ELSE
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-IF    
               END-IF     
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）退避編集処理
      *****************************************************************
       2007111-KOUI-TAIHI-HENSYU-SEC SECTION.
      *
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0300 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE05-KEY          TO  TBL-KOUI-KEY
                                                        (WRK-TBLMAX1)
               MOVE    IDX                 TO  TBL-KOUI-SHORI-RENNUM
                                                        (WRK-TBLMAX1)
               MOVE    RECE05-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE05-NAMEINF (IDX)    TO
                                     TBL-KOUI-NAMEINF   (WRK-TBLMAX1)
           END-IF
           .
       2007111-KOUI-TAIHI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査（判断料）の場合の退避処理
      *****************************************************************
       20013-KENSA-HANDANRYO-TAIHI-SEC           SECTION.
      *
           ADD     1               TO  TBL-KENSA-RECE05-MAX
           MOVE    RECE05-KEY      TO  TBL-KENSA-RE05-KEY
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
           MOVE    IDX             TO  TBL-KENSA-RE05-SHORI-RENNUM
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
           MOVE    RECE05-SRYACTINF (IDX)
                                   TO  TBL-KENSA-RE05-SRYACTINF
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
           MOVE    RECE05-NAMEINF (IDX)
                                   TO  TBL-KENSA-RE05-NAMEINF
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
           IF      RECE05-SRYCD (IDX) (1:1)  =   "1"
               MOVE    TBL-KENSA-RECE05-MAX 
                                       TO  TBL-KENSA-SRYCD-MAX
                                                (TBL-KENSA-RECE04-MAX)
           ELSE
               IF      RECE05-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE05-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-KENSA-RE05-SRYCD
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
                   END-IF
      *            コメント
                   IF    ( RECE05-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE05-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE05-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE05-INPUTCOMENT (IDX)     TO
                                       TBL-KENSA-RE05-NAME
                           (TBL-KENSA-RECE04-MAX TBL-KENSA-RECE05-MAX)
                   END-IF
               END-IF
           END-IF
      *
           .
       20013-KENSA-HANDANRYO-TAIHI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    摘要データ（医薬品）退避処理
      *****************************************************************
       200711-YKZ-TAIHI-SEC          SECTION.
      *
           IF      RECE05-SRYCD (IDX)  =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
               CONTINUE
           ELSE
               IF    ( RECE041-YKZTEN       =   ZERO  )
               AND   ( RECE041-TOYAKUFLG    =   SPACE )
               AND   ( RECE041-YKZKISAIFLG  =   SPACE )
                   GO  TO  200711-YKZ-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE05-SRYCD (IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *     
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-KEY          TO
                                   TBL-ZAIW-KEY        (1 WRK-TBLMAX6)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                       (1 WRK-TBLMAX6)
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE05-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE05-YUKOKETA   (IDX)  TO
                                   TBL-ZAIW-YUKOKETA   (1 WRK-TBLMAX6)
               IF     (WRK-PARA-PRTKBN         =   "9")  AND
                      (RECE05-SRYKBN           =   "21"
                                               OR  "22"
                                               OR  "23") AND
                      (RECE05-INGAIFLG (IDX)   =   "1")
                   MOVE    1                   TO
                               TBL-ZAIW-KOKUJISKBKBN2 (1 WRK-TBLMAX6)
               END-IF
           END-IF
           .
       200711-YKZ-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（その他）退避処理
      *****************************************************************
       200711-KIZAI-TAIHI-SEC          SECTION.
      *
           IF      RECE041-SRYKBN    NOT =   "70"
               IF    ( RECE041-KIZAITEN  =   ZERO  )
               AND   ( RECE041-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-TAIHI-EXT
               END-IF
           END-IF
      *
      *    酸素補正率１．３（１気圧）のとき
           IF     RECE05-SRYCD (IDX)    =   WRK-CONS-SANSO-HOSEI
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7
      *     
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-KEY          TO
                                   TBL-ZAIW-KEY        (2 WRK-TBLMAX7)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM(2 WRK-TBLMAX7)
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE05-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
               MOVE    RECE05-RECEDENNAME(IDX)  TO
                                   TBL-ZAIW-RECEDENNAME(2 WRK-TBLMAX7)
      *
               IF    ( RECE05-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE041-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
               ELSE
                   IF       RECE05-SRYCD (IDX)
                                              =   "799990070"
                       MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
                   ELSE
                       MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX 
                   END-IF
               END-IF
           END-IF
      *
      *    酸素のとき補正率を編集
           IF    ( RECE05-SRYCD (IDX)  =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               ADD     1                   TO  WRK-TBLMAX7
      *     
               IF      WRK-TBLMAX7         >   100
                   MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    WRK-CONS-SANSO-HOSEI TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
                   MOVE    1                   TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
                   MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
               END-IF
           END-IF
           .
       200711-KIZAI-TAIHI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    摘要データ（その他）特定器材退避処理
      *****************************************************************
       200711-KIZAI-ACCT-TAIHI-SEC          SECTION.
      *
           IF      RECE041-SRYKBN    NOT =   "70"
               IF    ( RECE041-KIZAITEN  =   ZERO  )
               AND   ( RECE041-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-ACCT-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7-SRT
      *     
           IF      WRK-TBLMAX7-SRT     >   100
               MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-KEY          TO
                                   TBL-ZAIW-KEY      (3 WRK-TBLMAX7-SRT)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD    (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD   (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN      (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-TOKUKIZAISBT1 (IDX)  TO
                                   TBL-ZAIW-TOKUKIZAISBT1
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-SANSOKBN  (IDX)  TO
                                   TBL-ZAIW-SANSOKBN (3 WRK-TBLMAX7-SRT)
               MOVE    RECE05-RECEDENNAME  (IDX)   TO
                                   TBL-ZAIW-RECEDENNAME
                                                     (3 WRK-TBLMAX7-SRT)
           END-IF
           .
       200711-KIZAI-ACCT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）並び替え処理
      *****************************************************************
       20074-ZAI-KOUI-SORT-SEC          SECTION.
      *
           INITIALIZE                   WRK-KOUI-AREA
                                        WRK-KOUI-TAIHI-AREA
                                        WRK-KOUI-KASAN-TAIHI-AREA
                                        WRK-KOUI-SRY-TAIHI-AREA
                                        WRK-SRYKBN40-KOKUJISKBKBN1
      *
           IF      RECE041-SRYKBN       =   "50"    OR  "54"    OR
                                            "60"    OR  "40"    OR
                                            "80"    OR  "33"    OR
                                            "70"    OR  "11"    OR
                                            "12"    OR  "13"    OR
                                            "14"
               CONTINUE
           ELSE
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           IF      WRK-TBLMAX1          =   1
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDY
                                           IDWW
                                           FLG-SISETUKIJYUN-GENZAN
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
      *
      *        加減算コード（通則加算項目）があるか
               IF    ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                           =   "9"         )
               AND   ( TBL-KOUI-TENSIKIBETU   (IDX)
                                           =   "5" OR  "6" )
      *            施設基準不適合減算（手術）（１００分の８０）のとき
                   IF      TBL-KOUI-SRYCD (IDX)    =
                                           WRK-CONS-SISETUKIJYUN-GENZAN
                       IF      FLG-SISETUKIJYUN-GENZAN =   ZERO
                           ADD     1                   TO  IDWW
                           MOVE    TBL-KOUI-REC  (IDX) TO
                                       WRK-KOUI-KASAN-TAIHI-REC (IDWW)
                           MOVE    TBL-KOUI-TIMEKSNKBN    (IDX)
                                                   TO
                                       WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN
                                                                (IDWW)
                           MOVE    1               TO
                                               FLG-SISETUKIJYUN-GENZAN
                       END-IF
                   ELSE
                       ADD     1                   TO  IDWW
                       MOVE    TBL-KOUI-REC  (IDX) TO
                                       WRK-KOUI-KASAN-TAIHI-REC (IDWW)
                       MOVE    TBL-KOUI-TIMEKSNKBN    (IDX)
                                                   TO
                                       WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN
                                                                (IDWW)
                   END-IF
               END-IF
      *
      *        処置の加算項目の有無                
               IF    ( RECE041-SRYKBN      =   "40" )
               AND   ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                           =   "7"  )
                   MOVE    1                   TO
                                   WRK-SRYKBN40-KOKUJISKBKBN1
               END-IF
      *
      *        コメントを最初に編集（手術のとき）
               IF      RECE041-SRYKBN      =   "50"
                   EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                       WHEN    SPACE
                           CONTINUE
                       WHEN    "0"
                       WHEN    "8" 
                           ADD     1                           TO  IDY
                           MOVE    TBL-KOUI-REC          (IDX) TO
                                           WRK-KOUI-TAIHI-REC    (IDY)
                           INITIALIZE      TBL-KOUI-REC (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    基本項目毎に並び替えをする
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >   WRK-TBLMAX1
             MOVE    ZERO                TO  IDYY
             INITIALIZE                  WRK-KOUI-SRY-TAIHI-AREA
             PERFORM UNTIL   IDX >   WRK-TBLMAX1
               MOVE    SPACE               TO  WRK-CHUKSNCD
               MOVE    ZERO                TO  FLG-KOUI
               PERFORM UNTIL   IDX >   WRK-TBLMAX1
                 IF    ( TBL-KOUI-SRYCD (IDX)  =   SPACE       )
                 OR    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                               =   "0" OR  "8" )
                   COMPUTE IDX     =   IDX     +   1
                 ELSE
                   EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                             ALSO    TBL-KOUI-KOKUJISKBKBN1 (IDX)
                     WHEN    SPACE   ALSO    ANY
                     WHEN    "0"     ALSO    ANY
                     WHEN    "8"     ALSO    ANY
                         CONTINUE
                     WHEN    ANY     ALSO    "1"
                     WHEN    ANY     ALSO    "3"
                     WHEN    ANY     ALSO    "5"
                       IF      WRK-CHUKSNCD        =   SPACE
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-CHUKSNCD
                           ADD     1                           TO  IDYY
                           MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-REC     (IDYY)
                           MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-SY-TAIHI-SRYCD   (IDYY)
                           MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-SY-TAIHI-TENSIKIBETU
                                                                  (IDYY)
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-SY-TAIHI-CHUKSNCD(IDYY)
                           MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-SY-TAIHI-KOKUJISKBKBN1
                                                                  (IDYY)
                           MOVE    TBL-KOUI-TUSOKUGAIKBN(IDX) TO
                                       WRK-KOUI-SY-TAIHI-TUSOKUGAIKBN
                                                                  (IDYY)
                           MOVE    TBL-KOUI-TIMEKSNKBN(IDX) TO
                                       WRK-KOUI-SY-TAIHI-TIMEKSNKBN
                                                                  (IDYY)
                           INITIALIZE      TBL-KOUI-REC (IDX)
                       ELSE
                           MOVE    1               TO  FLG-KOUI
                       END-IF    
                     WHEN    OTHER
                       ADD     1                           TO  IDYY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-REC     (IDYY)
                       MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-SY-TAIHI-SRYCD   (IDYY)
                       MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-SY-TAIHI-TENSIKIBETU
                                                                  (IDYY)
                       MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-SY-TAIHI-CHUKSNCD(IDYY)
                       MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-SY-TAIHI-KOKUJISKBKBN1
                                                                  (IDYY)
                       MOVE    TBL-KOUI-TUSOKUGAIKBN(IDX) TO
                                       WRK-KOUI-SY-TAIHI-TUSOKUGAIKBN
                                                                  (IDYY)
                       MOVE    TBL-KOUI-TIMEKSNKBN(IDX) TO
                                       WRK-KOUI-SY-TAIHI-TIMEKSNKBN
                                                                  (IDYY)
                       INITIALIZE      TBL-KOUI-REC (IDX)
                   END-EVALUATE
      *    
                   COMPUTE IDX     =   IDX     +   1
                 END-IF
      *    
                 IF    ( IDX       <=  WRK-TBLMAX1 )
                 AND   ( IDYY      >   ZERO        )
                     IF    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                   =   "1"                 )
                     AND   ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                   =   "1" OR  "3" OR  "5" )
                     AND   ( TBL-KOUI-KOKUJISKBKBN2 (IDX)
                                   =   "1" OR  "3" OR  "5" )
                         ADD     WRK-TBLMAX1       TO  IDX
                         IF      FLG-KOUI          =   ZERO
                             MOVE    9                 TO  FLG-KOUI
                         END-IF 
                     END-IF
                 END-IF
               END-PERFORM
      *
      *        出力対象となっていないデータがある 
               IF      FLG-KOUI            =   1
                   MOVE    1                   TO  IDX
               END-IF
             END-PERFORM
      *      摘要データ（診療行為）基本項目毎の並び替え処理
             PERFORM 200741-KIHON-HENSYU-SEC
      *      データが残っているとき 
             IF      FLG-KOUI            =   9
                 MOVE    1                   TO  IDX
             END-IF
           END-PERFORM
      *
      *    コメントを最後に編集
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "0"
                   WHEN    "8"
                       ADD     1                           TO  IDY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                               WRK-KOUI-TAIHI-REC (IDY)
                 END-EVALUATE
            END-PERFORM           
      *
           INITIALIZE                      TBL-KOUI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX1
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       100
                   OR      WRK-KOUI-TAIHI-REC (IDX)  =   SPACE
               ADD     1           TO  WRK-TBLMAX1   
               MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                   TO  TBL-KOUI-REC(WRK-TBLMAX1)
           END-PERFORM
           .
       20074-ZAI-KOUI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）基本項目毎の並び替え処理
      *****************************************************************
       200741-KIHON-HENSYU-SEC          SECTION.
      *
           MOVE    1                   TO  IDZZ
           PERFORM UNTIL   IDZZ    >=  100
                   OR      WRK-KOUI-SY-TAIHI-SRYCD (IDZZ)  =   SPACE
               EVALUATE    WRK-KOUI-SY-TAIHI-SRYCD (IDZZ) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
                           PERFORM 200741-KASAN-HENSYU-SEC
                       END-IF
                       ADD     1           TO  IDY   
                       MOVE    WRK-KOUI-SRY-TAIHI-REC (IDZZ)
                                           TO  WRK-KOUI-TAIHI-REC (IDY)
                       COMPUTE IDZZ    =   IDZZ    +   1
                   WHEN    OTHER
      *                基本項目・加算項目・通則加算項目に分ける
                       PERFORM UNTIL   IDZZ    >=      100
                               OR      WRK-KOUI-SY-TAIHI-SRYCD (IDZZ)
                                                       =   SPACE
                               OR      WRK-KOUI-SY-TAIHI-SRYCD (IDZZ)
                                                                  (1:1)
                                                   NOT =   "1"
                           MOVE    ZERO            TO  IDZ
                           EVALUATE    WRK-KOUI-SY-TAIHI-KOKUJISKBKBN1
                                                                (IDZZ)
                               WHEN    "1"
                               WHEN    "3" 
                               WHEN    "5"
                                   IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
                                       PERFORM 200741-KASAN-HENSYU-SEC
                                   END-IF
                                   MOVE    1               TO  IDZ  
                               WHEN    "7"
                                   IF  WRK-KOUI-SY-TAIHI-TUSOKUGAIKBN
                                                               (IDZZ)
                                                           =   "1"
                                     MOVE    4               TO  IDZ  
                                   ELSE
                                     IF      WRK-KOUI-SY-TAIHI-SRYCD
                                                                 (IDZZ)
                                                           =
                                         "150268970"                  OR
                                         WRK-CONS-SISETUKIJYUN-KENSA  OR
                                         WRK-CONS-SISETUKIJYUN-GAZOU  OR
                                         WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                       MOVE    2               TO  IDZ  
                                     ELSE
                                      IF
                                        ( WRK-KOUI-SY-TAIHI-TENSIKIBETU 
                                                               (IDZZ)
                                                         =   5   OR  6 )
                                      AND
                                        ( WRK-KOUI-SY-TAIHI-CHUKSNCD
                                                                 (IDZZ)
                                                         =   "0000"    )
                                      AND   ( RECE041-SRYKBN
                                                     NOT =   "54"      ) 
                                       MOVE    3               TO  IDZ
                                      ELSE
                                       MOVE    2               TO  IDZ
                                      END-IF
                                     END-IF
                                   END-IF   
                               WHEN    "9" 
                                   IF   ( WRK-KOUI-SY-TAIHI-TENSIKIBETU 
                                                               (IDZZ)
                                           =   "5" OR  "6" )
                                       MOVE    ZERO        TO  IDZ
                                   ELSE
                                       MOVE    5           TO  IDZ
                                   END-IF  
                           END-EVALUATE
                           IF      IDZ         >   ZERO
                               ADD     1           TO
                                               WRK-KOUI-TBLMAX (IDZ)
                               MOVE    WRK-KOUI-TBLMAX (IDZ)
                                                   TO  IDYY
                               MOVE    WRK-KOUI-SRY-TAIHI-REC (IDZZ)
                                                   TO
                                               WRK-KOUI-REC (IDZ IDYY)
                           END-IF
                           COMPUTE IDZZ    =   IDZZ    +   1
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
      *               
           IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
               PERFORM 200741-KASAN-HENSYU-SEC
           END-IF
           .
       200741-KIHON-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       200741-KASAN-HENSYU-SEC          SECTION.
      *
      *    注加算項目の記録順
           IF      WRK-KOUI-TBLMAX (2)         >   ZERO
               COMPUTE IDYY    =   WRK-KOUI-TBLMAX (2) -   1
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       IDYY
                   COMPUTE IDZ     =   IDY1    +   1
                   PERFORM VARYING IDY2    FROM   IDZ  BY  1
                           UNTIL   IDY2    >   WRK-KOUI-TBLMAX (2)
                       IF      WRK-KOUI-CHUKSNINF (2 IDY1)
                                       > WRK-KOUI-CHUKSNINF (2 IDY2)
                           MOVE    WRK-KOUI-REC (2 IDY1)  TO
                                                 TBL-KOUI-SORT
                           MOVE    WRK-KOUI-REC (2 IDY2)  TO
                                                 WRK-KOUI-REC (2 IDY1)
                           MOVE    TBL-KOUI-SORT         TO
                                                 WRK-KOUI-REC (2 IDY2)
                       END-IF
                   END-PERFORM                              
               END-PERFORM
           END-IF    
      *
      *    基本項目・加算項目・通則加算項目の順に編集
           MOVE    ZERO            TO  WRK-KIHON-TIMEKSNKBN
      *
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       5
               MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDYY
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       IDYY
      *            基本項目の時間外加算区分を退避
                   IF    ( IDY1            =   1 ) 
                   AND   ( IDY2            =   1 )
                       MOVE    WRK-KOUI-TIMEKSNKBN  (IDY1 IDY2)
                                           TO  WRK-KIHON-TIMEKSNKBN
                   END-IF
      * 
                   ADD     1               TO  IDY
                   MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                   MOVE    IDY             TO  WRK-SRYCD1-MAX
               END-PERFORM
           END-PERFORM
      *
      *    加減算コードの通則加算項目の編集
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       IDWW
      *        時間外加算区分による編集有無チェック
               IF    ( WRK-KIHON-TIMEKSNKBN    =   ZERO )
               AND   ( WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN (IDY1)
                                           NOT =   ZERO )
      *            時間外区分算定外のとき加算項目があれば編集（処置）
                   IF      WRK-SRYKBN40-KOKUJISKBKBN1  =   1
                       ADD     1               TO  IDY
                       MOVE    WRK-KOUI-KASAN-TAIHI-REC (IDY1)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                       MOVE    IDY         TO  WRK-SRYCD1-MAX
                   END-IF
               ELSE 
                   ADD     1               TO  IDY
                   MOVE    WRK-KOUI-KASAN-TAIHI-REC (IDY1)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                   MOVE    IDY             TO  WRK-SRYCD1-MAX
               END-IF    
           END-PERFORM
      *
           INITIALIZE                   WRK-KOUI-AREA
           .
       200741-KASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       20072-ZAI-HENSYU-SEC          SECTION.
      *
      *    コメントデータの編集（診療区分７０）
           IF      RECE041-SRYKBN      =   "70"                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *     
      *    診療行為データ・コメントデータの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX1
      *
               INITIALIZE                  TBL-RECE055-COMMENT-AREA
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       MOVE    "CO,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF    
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                負担区分
                       MOVE    WRK-FTNKBN          TO  WRK-DATA
                       MOVE    1                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ
                       MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-DATA
                       MOVE    76                  TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                       MOVE    1                   TO  WRK-END 
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *
                       IF      TBL-KOUI-NAME(IDX) (77:)
                                               NOT =   SPACE
                           PERFORM 5001-FILE-WRITE-SEC
      *
                           MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-CO-SRYCD
                           MOVE    TBL-KOUI-NAME  (IDX) (77:)
                                                   TO  WRK-CO-COMMENT
                           PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
                       END-IF
                   WHEN    OTHER
      *                関連コメント退避処理
                       IF      RECE041-SETCOMFLG   =   "1"
                           INITIALIZE               WRK-RECE055-REC
                           MOVE    TBL-KOUI-KEY (IDX)
                                                TO  WRK-RECE055-KEY1
                           MOVE    TBL-KOUI-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE055-SHORI-RENNUM
                           MOVE    ZERO         TO  WRK-RECE055-SEQNO
                           PERFORM 200799-KANREN-COM-SEC
                       END-IF
      *  
                       MOVE    "SI,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                負担区分
                       MOVE    WRK-FTNKBN          TO  WRK-DATA
                       MOVE    1                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                診療行為コード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                数量
                       IF      TBL-KOUI-SRYSURYO (IDX) >   ZERO
                               IF    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                                       =   "1"        )
                               AND   ( TBL-KOUI-KZMCOMPSIKIBETU (IDX)
                                                   NOT =   "1"        )
                                   CONTINUE
                               ELSE
                                   MOVE    TBL-KOUI-SRYSURYO (IDX)
                                                       TO  WRK-SURYO
                                   MOVE    WRK-SURYO   TO  WRK-NUM
                                   MOVE    8           TO  WRK-NAGASA
                               END-IF
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                点数
                       IF    ( IDX                 =   WRK-SRYCD1-MAX )
                       AND   ( RECE041-SYUTEN1     >   ZERO           )
                           EVALUATE    RECE041-SRYKBN
                               WHEN    "70"
                                   COMPUTE WRK-TENSU   =
                                               RECE041-SYUTEN1 -
                                               RECE041-SYUTEN2
                               WHEN    "12"
                               WHEN    "13"
                               WHEN    "14"
                                   COMPUTE WRK-TENSU   =
                                           RECE041-SYUTEN1 -
                                           TBL-KOUI-SRYKBN12-TEN-G
                                                           -
                                           TBL-KOUI-SRYKBN12-CHU-TEN-G
                               WHEN    OTHER  
                                   MOVE    RECE041-SYUTEN1
                                                       TO  WRK-TENSU
                           END-EVALUATE
                           MOVE    WRK-TENSU         TO  WRK-NUM
                           MOVE    7                 TO  WRK-NAGASA
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                回数
      *****            IF    ( IDX             =   WRK-SRYCD1-MAX     )
      *****            AND   ( RECE041-ZAIKAISU
      *****                                    >   ZERO               )
                           MOVE    RECE041-ZAIKAISU    TO  WRK-KAISU
                           MOVE    WRK-KAISU           TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
      *****            END-IF
                       PERFORM 5005-NUM-HENSYU-SEC
      *                コメントコード１
                       IF      TBL-RECE055-MAX >   ZERO
                           MOVE    TBL-RECE055-SRYCD (1)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ１
                       IF      TBL-RECE055-MAX >   ZERO
                           MOVE    TBL-RECE055-NAME  (1)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード２
                       IF      TBL-RECE055-MAX >   1
                           MOVE    TBL-RECE055-SRYCD (2)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ２
                       IF      TBL-RECE055-MAX >   1
                           MOVE    TBL-RECE055-NAME  (2)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード３
                       IF      TBL-RECE055-MAX >   2
                           MOVE    TBL-RECE055-SRYCD (3)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ３
                       IF      TBL-RECE055-MAX >   2
                           MOVE    TBL-RECE055-NAME  (3)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                日付情報
                       PERFORM VARYING IDV     FROM    1   BY  1
                               UNTIL   IDV     >       31
                           IF      RECE041-KAISU (1 IDV)  >   ZERO
                               MOVE    RECE041-KAISU (1 IDV)
                                                       TO  WRK-NUM
                               MOVE    3               TO  WRK-NAGASA
                           END-IF
                           IF      IDV                 =   31
                               MOVE    1                   TO  WRK-END 
                           END-IF     
                           PERFORM 5005-NUM-HENSYU-SEC
                       END-PERFORM  
               END-EVALUATE  
      *
               PERFORM 5001-FILE-WRITE-SEC
      *
      *        関連コメント編集処理
               PERFORM 200799-KANREN-COMMENT-HEN-SEC
           END-PERFORM
      *
      *    加算項目扱いの基本項目を編集
           IF      TBL-KOUI-SRYKBN12-MAX   >   ZERO
               PERFORM 20079-JIKANGAI-HENSYU-SEC
           END-IF
      *
      *    コメントデータの編集（診療区分１１〜１４）
           IF    ( RECE041-SRYKBN      =   "11"    OR  "12"    OR
                                           "13"    OR  "14" ) 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *
      *    医薬品データの編集                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX6
               PERFORM 200722-YKZ-HENSYU-SEC
           END-PERFORM            
      *    日計表データの編集
           IF      RECE02-SRYYM        <   WRK-CONS-SRYYM-201204
             IF    ( WRK-TBLMAX6         >   ZERO                      )
             AND   ( RECE041-SRYKBN (1:1)
                                       =   "2" OR  "3"  OR  "4" OR  "5")
               IF    ( RECE02-TEISYUTUSAKI   =   "1"                )
               AND   ( RECE02-RJNGAIFTN  >=  SYS-2005-NI-JYOKEN (1) )
                       PERFORM 200725-NIKKEI-HENSYU-SEC
               END-IF
               IF    ( RECE02-TEISYUTUSAKI   =   "2"                )
               AND   ( RECE02-RJNGAIFTN  >=  SYS-2005-NI-JYOKEN (2) )
                       PERFORM 200725-NIKKEI-HENSYU-SEC
               END-IF
               IF    ( RECE02-TEISYUTUSAKI   =   "6"                  )
               AND   ( RECE02-RJNGAIFTN  >=  SYS-25-NI-JYOKEN (1)     )
                       PERFORM 200725-NIKKEI-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
      *    コメントデータの編集（診療区分１１〜１４）
           IF    ( RECE041-SRYKBN      =   "23"  )
           AND   ( RECE041-SIPPUFLG
                                   NOT =   SPACE )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM
           END-IF
      *
      *    特定器材データの編集
           IF      RECE041-ZAIREQFLG       =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX7
                       IF      IDXX        =   TBL-ZAIW-DATSKB (2 IDX)
                           PERFORM 200723-KIZAI-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           ELSE
               INITIALIZE                      ORCSRECESORTAREA
grpsys         MOVE    RECE02-HOSPNUM      TO  ORCSRECESORT-HOSPNUM
               MOVE    RECE02-NYUGAIKBN    TO  ORCSRECESORT-NYUGAIKBN
               MOVE    RECE02-PTID         TO  ORCSRECESORT-PTID
               MOVE    RECE02-SRYYM        TO  ORCSRECESORT-SRYYM
               MOVE    RECE041-SRYKBN      TO  ORCSRECESORT-SRYKBN
               MOVE    RECE041-ZAINUM      TO  ORCSRECESORT-ZAINUM
               MOVE    RECE041-KIZAITEN    TO  ORCSRECESORT-KIZAITEN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX7-SRT
                   MOVE    TBL-ZAIW-KEY           (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY           (IDX)
                   MOVE    TBL-ZAIW-SHORI-RENNUM  (3 IDX)  TO
                                   ORCSRECESORT-IN-SHORI-RENNUM  (IDX)
                   MOVE    TBL-ZAIW-SRYCD         (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYCD         (IDX)
                   MOVE    TBL-ZAIW-DATAKBN       (3 IDX)  TO
                                   ORCSRECESORT-IN-DATAKBN       (IDX)
                   MOVE    TBL-ZAIW-SRYSURYO      (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYSURYO      (IDX)
                   MOVE    TBL-ZAIW-SRYKAISU      (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYKAISU      (IDX)
                   MOVE    TBL-ZAIW-TANICD        (3 IDX)  TO
                                   ORCSRECESORT-IN-TANICD        (IDX)
                   MOVE    TBL-ZAIW-TEN           (3 IDX)  TO
                                   ORCSRECESORT-IN-TEN           (IDX)
                   MOVE    TBL-ZAIW-TENSIKIBETU   (3 IDX)  TO
                                   ORCSRECESORT-IN-TENSIKIBETU   (IDX)
                   MOVE    TBL-ZAIW-NAME          (3 IDX)  TO
                                   ORCSRECESORT-IN-NAME          (IDX)
                   MOVE    TBL-ZAIW-TOKUKIZAISBT1 (3 IDX)  TO
                                   ORCSRECESORT-IN-TOKUKIZAISBT1 (IDX)
                   MOVE    TBL-ZAIW-SANSOKBN      (3 IDX)  TO
                                   ORCSRECESORT-IN-SANSOKBN      (IDX)
                   MOVE    TBL-ZAIW-RECEDENNAME   (3 IDX)  TO
                                   ORCSRECESORT-IN-RECEDENNAME   (IDX)
               END-PERFORM
               CALL    "ORCSRECESORT"      USING   ORCSRECESORTAREA
                                                   SPA-AREA
               IF      ORCSRECESORT-ERRCD  =   0
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       8
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >       50
                               OR      ORCSRECESORT-SRYCD (IDXX IDX)
                                                       =   SPACE
                           PERFORM 200723-KIZAI-SORT-HENSYU-SEC
                       END-PERFORM
                   END-PERFORM
               ELSE
                   DISPLAY "ORCSRECESORT-ERRCD=" ORCSRECESORT-ERRCD
      *
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       2
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-TBLMAX7
                           IF      IDXX    =   TBL-ZAIW-DATSKB (2 IDX)
                               PERFORM 200723-KIZAI-HENSYU-SEC
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF    
           END-IF
           .
       20072-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）編集処理（診療区分１３・１４）
      *****************************************************************
       200724-COMMENT-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    TBL-COMMENT-SRYCD (IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    TBL-COMMENT-NAME(IDX)
                                       TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           IF      TBL-COMMENT-NAME(IDX) (77:)
                                   NOT =   SPACE
               PERFORM 5001-FILE-WRITE-SEC
      *
               MOVE    TBL-COMMENT-SRYCD (IDX)
                                           TO  WRK-CO-SRYCD
               MOVE    TBL-COMMENT-NAME  (IDX) (77:)
                                           TO  WRK-CO-COMMENT
               PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
            END-IF
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       200724-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）編集処理
      *****************************************************************
       200722-YKZ-HENSYU-SEC         SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE055-COMMENT-AREA
           IF      RECE041-SETCOMFLG   =   "1"
               INITIALIZE               WRK-RECE055-REC
               MOVE    TBL-ZAIW-KEY (1 IDX)
                                           TO  WRK-RECE055-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (1 IDX)
                                           TO  WRK-RECE055-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE055-SEQNO
               PERFORM 200799-KANREN-COM-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "IY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    医薬品コード
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
               CONTINUE
           ELSE
               IF      TBL-ZAIW-SRYSURYO (1 IDX) >   ZERO
                   MOVE    TBL-ZAIW-SRYSURYO (1 IDX) TO  WRK-SRYSURYO
                   MOVE    WRK-SRYSURYO              TO  WRK-NUM
                   MOVE    11                        TO  WRK-NAGASA
                   MOVE    5                         TO  WRK-SYOSUU     
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF      IDX                 =   WRK-TBLMAX6
               IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
                   MOVE    RECE041-ZAITEN      TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               ELSE
                   IF      RECE041-YKZTEN          >   ZERO
                   OR      RECE041-TOYAKUFLG   NOT =   SPACE
                   OR      RECE041-YKZKISAIFLG     =   "1"
                       MOVE    RECE041-YKZTEN      TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
      *****IF      IDX                 =   WRK-TBLMAX6
      *****    IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
      *****    END-IF
      *****END-IF        
           PERFORM 5005-NUM-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE041-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE041-KAISU (1 IDV)   TO  WRK-NUM
                   MOVE    3                       TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200799-KANREN-COMMENT-HEN-SEC
           .
       200722-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（日計表）編集処理
      *****************************************************************
       200725-NIKKEI-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "NI,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
               IF      RECE041-KAISU (1 IDXX)  >   ZERO
                   MOVE    RECE041-KAISU (1 IDXX)    TO  WRK-NUM
                   MOVE    3                         TO  WRK-NAGASA
               END-IF
               IF      IDXX                =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       200725-NIKKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（特定機材）編集処理
      *****************************************************************
       200723-KIZAI-HENSYU-SEC         SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE055-COMMENT-AREA
           IF      RECE041-SETCOMFLG   =   "1"
               INITIALIZE               WRK-RECE055-REC
               MOVE    TBL-ZAIW-KEY (2 IDX)
                                           TO  WRK-RECE055-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (2 IDX)
                                           TO  WRK-RECE055-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE055-SEQNO
               PERFORM 200799-KANREN-COM-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "TO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    特定器材コード
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF    ( TBL-ZAIW-SRYSURYO (2 IDX)   >   ZERO )
           AND   ( TBL-ZAIW-TENSIKIBETU (2 IDX)
                                           NOT =   "5"  )    
               MOVE    TBL-ZAIW-SRYSURYO (2 IDX)   TO  WRK-SRYSURYO
               MOVE    WRK-SRYSURYO                TO  WRK-NUM
               MOVE    9                           TO  WRK-NAGASA
               MOVE    3                           TO  WRK-SYOSUU
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF    ( IDXX                =   1               )
           AND   ( IDX                 =   WRK-SRYCD71-MAX )
           AND   ( RECE041-SRYKBN      =   "70"            )
               MOVE    RECE041-SYUTEN2     TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           IF    ( IDXX                =   2               )
           AND   ( IDX                 =   WRK-SRYCD72-MAX )
               MOVE    RECE041-KIZAITEN    TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
      *****IF    ( IDXX                =   1               )
      *****AND   ( IDX                 =   WRK-SRYCD71-MAX )
      *****AND   ( RECE041-SRYKBN      =   "70"            )
      *****    IF      RECE041-ZAIKAISU    >   ZERO
      *****        MOVE    RECE041-ZAIKAISU    TO  WRK-NUM
      *****        MOVE    3                   TO  WRK-NAGASA
      *****    END-IF
      *****END-IF        
      *****IF    ( IDXX                =   2               )
      *****AND   ( IDX                 =   WRK-SRYCD72-MAX )
      *****    IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
      *****    END-IF
      *****END-IF        
           PERFORM 5005-NUM-HENSYU-SEC
      *    単位コード
           IF      TBL-ZAIW-TANICD (2 IDX)     >   ZERO
               MOVE    TBL-ZAIW-TANICD (2 IDX) TO  WRK-DATA
               MOVE    3                       TO  WRK-NAGASA
           END-IF        
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    単価
           IF    ( TBL-ZAIW-TENSIKIBETU (2 IDX)    =   2               )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   "777770000"     )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   WRK-CONS-SANSO1
                                                   OR  WRK-CONS-SANSO2
                                                   OR  WRK-CONS-SANSO3
                                                   OR  WRK-CONS-SANSO4
                                                   OR  WRK-CONS-SANSO5
                                                   OR  WRK-CONS-SANSO6
                                                   OR  WRK-CONS-SANSO7
                                                   OR  WRK-CONS-SANSO8 )
          
               IF      TBL-ZAIW-TEN (2 IDX)  >   ZERO
                   MOVE    TBL-ZAIW-TEN (2 IDX)  TO  WRK-TANKA
                   MOVE    WRK-TANKA             TO  WRK-NUM
                   MOVE    10                    TO  WRK-NAGASA
                   MOVE    2                     TO  WRK-SYOSUU
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    特定器材名称
           IF      TBL-ZAIW-SRYCD (2 IDX)        =   "777770000"
               MOVE    TBL-ZAIW-NAME (2 IDX)     TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    商品名及び規格名及びサイズ
           IF      TBL-ZAIW-RECEDENNAME (2 IDX)
                                       NOT =   SPACE
               MOVE    TBL-ZAIW-RECEDENNAME (2 IDX)
                                                 TO  WRK-DATA
           END-IF
           MOVE    300                 TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE041-KAISU (1 IDV)  >    ZERO
                   MOVE    RECE041-KAISU (1 IDV)   TO  WRK-NUM
                   MOVE    3                       TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200799-KANREN-COMMENT-HEN-SEC
           .
       200723-KIZAI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    摘要データ（再編集特定機材）編集処理
      *****************************************************************
       200723-KIZAI-SORT-HENSYU-SEC    SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE055-COMMENT-AREA
           IF      RECE041-SETCOMFLG   =   "1"
               INITIALIZE               WRK-RECE055-REC
               MOVE    ORCSRECESORT-KEY (IDXX IDX)
                                           TO  WRK-RECE055-KEY1
               MOVE    ORCSRECESORT-SHORI-RENNUM (IDXX IDX)
                                           TO  WRK-RECE055-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE055-SEQNO
               PERFORM 200799-KANREN-COM-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "TO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    特定器材コード
           MOVE    ORCSRECESORT-SRYCD (IDXX IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF    ( ORCSRECESORT-SRYSURYO (IDXX IDX)
                                       >   ZERO         )
           AND   ( ORCSRECESORT-TENSIKIBETU (IDXX IDX)
                                           NOT =   "5"  )    
               MOVE    ORCSRECESORT-SRYSURYO (IDXX IDX)
                                           TO  WRK-SRYSURYO
               MOVE    WRK-SRYSURYO        TO  WRK-NUM
               MOVE    9                   TO  WRK-NAGASA
               MOVE    3                   TO  WRK-SYOSUU
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF      ORCSRECESORT-TBLMAX (IDXX)
                                       =   IDX
               MOVE    ORCSRECESORT-ZAITEN (IDXX)
                                           TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
      *****IF      ORCSRECESORT-TBLMAX (IDXX)
      *****                            =   IDX
      *****    IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
      *****    END-IF
      *****END-IF        
           PERFORM 5005-NUM-HENSYU-SEC
      *    単位コード
           IF      ORCSRECESORT-TANICD (IDXX IDX)
                                       >   ZERO
               MOVE    ORCSRECESORT-TANICD (IDXX IDX)
                                               TO  WRK-DATA
               MOVE    3                       TO  WRK-NAGASA
           END-IF        
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    単価
           IF    ( ORCSRECESORT-TENSIKIBETU (IDXX IDX)
                                       =   2               )
           OR    ( ORCSRECESORT-SRYCD       (IDXX IDX)
                                       =   "777770000"     )
           OR    ( ORCSRECESORT-SRYCD       (IDXX IDX)
                                       =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               IF      ORCSRECESORT-TEN (IDXX IDX)  >   ZERO
                   MOVE    ORCSRECESORT-TEN (IDXX IDX)
                                                 TO  WRK-TANKA
                   MOVE    WRK-TANKA             TO  WRK-NUM
                   MOVE    10                    TO  WRK-NAGASA
                   MOVE    2                     TO  WRK-SYOSUU
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    特定器材名称
           IF      ORCSRECESORT-SRYCD (IDXX IDX)
                                       =   "777770000"
               MOVE    ORCSRECESORT-NAME (IDXX IDX)
                                           TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    商品名及び規格名及びサイズ
           IF      ORCSRECESORT-RECEDENNAME (IDXX IDX)
                                       NOT =   SPACE
               MOVE    ORCSRECESORT-RECEDENNAME (IDXX IDX)
                                               TO  WRK-DATA
           END-IF
           MOVE    300                 TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE055-MAX >   ZERO
               MOVE    TBL-RECE055-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE055-MAX >   1
               MOVE    TBL-RECE055-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE055-MAX >   2
               MOVE    TBL-RECE055-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE041-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE041-KAISU (1 IDV)    TO  WRK-NUM
                   MOVE    3                        TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200799-KANREN-COMMENT-HEN-SEC
           .
       200723-KIZAI-SORT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査に判断料がある場合の検査の編集処理
      *****************************************************************
       20078-KENSA-HANDANRYO-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDZ3 FROM    1   BY  1
                   UNTIL   IDZ3 >   TBL-KENSA-RECE04-MAX
               IF      TBL-KENSA-R4-ZAIKAISU (IDZ3)
                                           >   0
      *            負担区分編集処理
                   MOVE    TBL-KENSA-RECE04-REC (IDZ3)
                                               TO  RECE041-REC
                   PERFORM 20070-FTNKBN-HENSYU-SEC
                   MOVE    "60"                TO  WRK-SRYKBN
      *
      *            仮レセデータフラグ設定
                   IF      RECE041-TOYAKUFLG =   SPACE
                       MOVE    ZERO              TO  FLG-TOYAKUFLG
                   ELSE
                       MOVE    1                 TO  FLG-TOYAKUFLG
                   END-IF 
      *
                   PERFORM VARYING IDZ4    FROM    1   BY  1
                           UNTIL   IDZ4    >       50
                           OR      TBL-KENSA-RECE05-REC  (IDZ3 IDZ4)
                                                   =   SPACE
                       MOVE    TBL-KENSA-SRYCD-MAX (IDZ3) 
                                           TO  IDZ5
                       INITIALIZE      WRK-RECE05-REC
                       MOVE    TBL-KENSA-RE05-KEY        (IDZ3 IDZ4)
                                           TO  WRK-RECE05-KEY
      *???             MOVE    TBL-KENSA-RE05-SHORI-RENNUM
      *???                                                 (IDZ3 IDZ4)
      *???                                 TO  IDX
                       MOVE    TBL-KENSA-RE05-SRYACTINF  (IDZ3 IDZ4)
                                           TO  WRK-RECE05-SRYACTINF (1)
                       MOVE    TBL-KENSA-RE05-NAMEINF    (IDZ3 IDZ4)
                                           TO  WRK-RECE05-NAMEINF   (1)
                       PERFORM 200781-KENSA-HENSYU-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20078-KENSA-HANDANRYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査に判断料がある場合の検査の編集処理
      *****************************************************************
       200781-KENSA-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  TBL-RECE055-COMMENT-AREA
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           EVALUATE    WRK-RECE05-SRYCD (1) (1:1)
               WHEN    "0"
               WHEN    "8"
                   MOVE    "CO,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      WRK-SRYKBN      NOT =   SPACE
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF    
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            負担区分
                   MOVE    WRK-FTNKBN          TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード
                   MOVE    WRK-RECE05-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ
                   MOVE    WRK-RECE05-NAME (1) TO  WRK-DATA
                   MOVE    76                  TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-MOJI-CHK
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *
                   IF      WRK-RECE05-NAME (1)  (77:)
                                           NOT =   SPACE
                       PERFORM 5001-FILE-WRITE-SEC
      *
                       MOVE    WRK-RECE05-SRYCD (1)
                                                   TO  WRK-CO-SRYCD
                       MOVE    WRK-RECE05-NAME  (1) (77:)
                                                   TO  WRK-CO-COMMENT
                       PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
                   END-IF
               WHEN    OTHER     
      *            関連コメント退避処理
                   IF      RECE041-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE055-REC
                       MOVE    TBL-KENSA-RE05-KEY (IDZ3 IDZ4)
                                            TO  WRK-RECE055-KEY1
                       MOVE    TBL-KENSA-RE05-SHORI-RENNUM
                                                    (IDZ3 IDZ4)
                                            TO  WRK-RECE055-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE055-SEQNO
                       PERFORM 200799-KANREN-COM-SEC
                   END-IF
      *
                   MOVE    "SI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      WRK-SRYKBN      NOT =   SPACE
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            負担区分
                   MOVE    WRK-FTNKBN          TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    WRK-RECE05-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   IF      WRK-RECE05-SRYSURYO (1)
                                           >   ZERO
                       IF    ( WRK-RECE05-SRYCD (1) (1:1)
                                               =   "1"        )
                       AND   ( WRK-RECE05-KZMCOMPSIKIBETU (1)
                                           NOT =   "1"        )
                           CONTINUE
                       ELSE
                           MOVE    WRK-RECE05-SRYSURYO (1)
                                               TO  WRK-SURYO
                           MOVE    WRK-SURYO   TO  WRK-NUM
                           MOVE    8           TO  WRK-NAGASA
                       END-IF
                   END-IF
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   IF    ( IDZ4            =   IDZ5 )
                   AND   ( RECE041-SYUTEN1 >   ZERO )
                       MOVE    RECE041-SYUTEN1     TO  WRK-TENSU
                       MOVE    WRK-TENSU           TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF    
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
      *****        IF    ( IDZ4            =   IDZ5 )
      *****        AND   ( RECE041-ZAIKAISU
      *****                                >   ZERO )
                       MOVE    RECE041-ZAIKAISU    TO  WRK-KAISU
                       MOVE    WRK-KAISU           TO  WRK-NUM
                       MOVE    3                   TO  WRK-NAGASA
      *****        END-IF
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE041-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE041-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
           END-EVALUATE  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200799-KANREN-COMMENT-HEN-SEC
           .
       200781-KENSA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算項目扱いの基本項目の編集処理
      *****************************************************************
       20079-JIKANGAI-HENSYU-SEC   SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       TBL-KOUI-SRYKBN12-MAX
               IF      TBL-KOUI-SRYKBN12-KAISU (IDX)   >   ZERO
      *            関連コメント退避処理
                   INITIALIZE              TBL-RECE055-COMMENT-AREA
                   IF      RECE041-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE055-REC
                       MOVE    TBL-KOUI-SRYKBN12-KEY (IDX)
                                                TO  WRK-RECE055-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE055-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE055-SEQNO
                       PERFORM 200799-KANREN-COM-SEC
                   END-IF
      *
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
                   MOVE    "SI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
      *************MOVE    "12"                TO  WRK-DATA
      *************MOVE    2                   TO  WRK-NAGASA
                   IF    ( IDX                 =   1     )
                   AND   ( WRK-SRYKBN      NOT =   SPACE )
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            負担区分
                   MOVE    WRK-FTNKBN          TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    TBL-KOUI-SRYKBN12-SRYCD (IDX)
                                                   TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   MOVE    TBL-KOUI-SRYKBN12-TEN (IDX)
                                               TO  WRK-TENSU
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    TBL-KOUI-SRYKBN12-KAISU (IDX)
                                               TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE041-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE041-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
      *
                   PERFORM 5001-FILE-WRITE-SEC
      *
      *            関連コメント編集処理
                   PERFORM 200799-KANREN-COMMENT-HEN-SEC
      *
      *            注加算コードが１４１９の加算項目を編集
                   IF    ( TBL-KOUI-SRYKBN12-CHUKSNCD (IDX)
                                                   =   "1419" )
                   AND   ( TBL-KOUI-SRYKBN12-CHU-MAX
                                                   >   ZERO   )
                       PERFORM 200791-SRYKBN12-CHUKSN-HEN-SEC
                       MOVE    ZERO        TO  TBL-KOUI-SRYKBN12-CHU-MAX
                   END-IF
               END-IF
           END-PERFORM
           .
       20079-JIKANGAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    注加算コードが１４１９の加算項目を編集処理
      *****************************************************************
       200791-SRYKBN12-CHUKSN-HEN-SEC      SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDY4    FROM    1   BY  1
                   UNTIL   IDY4    >       TBL-KOUI-SRYKBN12-CHU-MAX
               IF      TBL-KOUI-SRYKBN12-CHU-KAISU (IDY4)
                                       >   ZERO
      *            関連コメント退避処理
                   INITIALIZE              TBL-RECE055-COMMENT-AREA
                   IF      RECE041-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE055-REC
                       MOVE    TBL-KOUI-SRYKBN12-CHU-KEY (IDY4)
                                            TO  WRK-RECE055-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-CHU-SHORI-NUM (IDY4)
                                            TO  WRK-RECE055-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE055-SEQNO
                       PERFORM 200799-KANREN-COM-SEC
                   END-IF
      *
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *
                   MOVE    "SI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF    ( IDY4                 =   1    )
                   AND   ( WRK-SRYKBN      NOT =   SPACE )
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            負担区分
                   MOVE    WRK-FTNKBN          TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    TBL-KOUI-SRYKBN12-CHU-SRYCD (IDY4)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
      *            数量
                   PERFORM 5005-NUM-HENSYU-SEC
      *            点数
                   MOVE    TBL-KOUI-SRYKBN12-CHU-TEN (IDY4)
                                               TO  WRK-TENSU
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    TBL-KOUI-SRYKBN12-CHU-KAISU (IDY4)
                                               TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-SRYCD (1)
                                               TO  WRK-DATA
                       MOVE    9               TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE055-MAX >   ZERO
                       MOVE    TBL-RECE055-NAME  (1)
                                               TO  WRK-DATA
                       MOVE    100             TO  WRK-NAGASA
                       MOVE    1               TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-SRYCD (2)
                                               TO  WRK-DATA
                       MOVE    9               TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE055-MAX >   1
                       MOVE    TBL-RECE055-NAME  (2)
                                               TO  WRK-DATA
                       MOVE    100             TO  WRK-NAGASA
                       MOVE    1               TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-SRYCD (3)
                                               TO  WRK-DATA
                       MOVE    9               TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE055-MAX >   2
                       MOVE    TBL-RECE055-NAME  (3)
                                               TO  WRK-DATA
                       MOVE    100             TO  WRK-NAGASA
                       MOVE    1               TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE041-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE041-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
      *
                   PERFORM 5001-FILE-WRITE-SEC
      *
      *            関連コメント編集処理
                   PERFORM 200799-KANREN-COMMENT-HEN-SEC
               END-IF
           END-PERFORM
           .
       200791-SRYKBN12-CHUKSN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメント退避処理
      *****************************************************************
       200799-KANREN-COM-SEC           SECTION.
      *
           INITIALIZE                  TBL-RECE055-COMMENT-AREA
      *
           MOVE    WRK-RECE055-REC     TO  RECE055-REC
           START   RECE055-FILE
               KEY IS    >=   RECE055-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE055
               NOT INVALID
                   PERFORM 900-RECE055-NEXT-SEC
           END-START
           PERFORM UNTIL  ( FLG-END        =   1                    )
                   OR     ( FLG-RECE055    =   1                    )
                   OR     ( WRK-RECE055-KEY1 
                                       NOT =   RECE055-KEY1         )
                   OR     ( WRK-RECE055-SHORI-RENNUM
                                       NOT =   RECE055-SHORI-RENNUM )
               PERFORM 2007991-KANREN-COM-TAIHI-SEC
      *
               PERFORM 900-RECE055-NEXT-SEC
           END-PERFORM
      *
           .
       200799-KANREN-COM-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメントチェック退避処理
      *****************************************************************
       2007991-KANREN-COM-TAIHI-SEC    SECTION.
      *
      *    診療行為コード＝'０'のとき対象外は出力しない
           IF      RECE055-SRYCD (1:1) =   "0"
               IF    ( RECE055-SRYCD (1:3) =   "002"             )
               OR    ( RECE055-SRYCD (1:3) =   "008"             )
               OR    ( RECE055-SRYCD       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE055-SRYCD       =   WRK-CONS-COMMENT2 ) 
      *****    OR    ( RECE055-SRYCD       =   WRK-CONS-COMMENT3 ) 
               OR    ( RECE055-SRYCD       =   WRK-CONS-COMMENT4 )
                   CONTINUE
               ELSE
                   GO  TO  2007991-KANREN-COM-TAIHI-EXT
               END-IF     
           END-IF
      *
           ADD     1                TO  TBL-RECE055-MAX
           MOVE    RECE055-SRYCD    TO  TBL-RECE055-SRYCD
                                               (TBL-RECE055-MAX)         
           MOVE    RECE055-NAME     TO  TBL-RECE055-NAME
                                               (TBL-RECE055-MAX) 
           IF     (WRK-PARA-PRTKBN     =   "9")    AND
                  (RECE041-SRYKBN      =   "21"
                                       OR  "22"
                                       OR  "23")
               IF     (TBL-ZAIW-KOKUJISKBKBN2 (1 IDX)  =   1)
               AND    (TBL-ZAIW-SRYCD (1 IDX)      NOT = "699990001")
                   MOVE    SPACE            TO  TBL-RECE055-SRYCD
                                                       (TBL-RECE055-MAX)
                   MOVE    SPACE            TO  TBL-RECE055-NAME
                                                       (TBL-RECE055-MAX)
               END-IF
           END-IF
      *
           IF      RECE055-SRYCD    =   "810000001"
               CONTINUE
           ELSE
      *        部位／薬剤／ユーザーコメント
               IF      RECE055-SRYCD (1:1) =   "0"
                   MOVE    "810000001"     TO  
                                           TBL-RECE055-SRYCD
                                               (TBL-RECE055-MAX)   
               END-IF
      *        コメント
               IF    ( RECE055-SRYCD (1:1) =   "8"   )
               OR    ( RECE055-SRYCD (1:3) =   "008"  AND
                       RECE055-SRYCD (4:1)
                                       NOT =   "2"   )
                   MOVE    RECE055-INPUTCOMENT TO
                                           TBL-RECE055-NAME
                                               (TBL-RECE055-MAX)
               END-IF
           END-IF
      *    コメントパターン２０のコメントコード
           IF      RECE055-SRYCD (1:2) =   "82"
               MOVE    SPACE              TO
                                           TBL-RECE055-NAME
                                               (TBL-RECE055-MAX)
           END-IF
      *
           .
       2007991-KANREN-COM-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメント編集処理
      *****************************************************************
       200799-KANREN-COMMENT-HEN-SEC    SECTION.
      *
           GO  TO  200799-KANREN-COMMENT-HEN-EXT
      *
      *    関連コメントが３対を超える場合
           IF      TBL-RECE055-MAX     <=  3
               GO  TO  200799-KANREN-COMMENT-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDVV    FROM    4   BY  1
                   UNTIL   IDVV    >       TBL-RECE055-MAX
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               MOVE    "CO,"               TO  WRK-REC
               MOVE    3                   TO  WRK-REC-LENGTH
      *        診療識別
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        負担区分
               MOVE    WRK-FTNKBN          TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        コメントコード
               MOVE    TBL-RECE055-SRYCD (IDVV)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        文字データ
               MOVE    TBL-RECE055-NAME (IDVV)
                                           TO  WRK-DATA
               MOVE    76                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
               MOVE    1                   TO  WRK-END 
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               IF      TBL-RECE055-NAME (IDVV) (77:)
                                       NOT =   SPACE
                   PERFORM 5001-FILE-WRITE-SEC
      *
                   MOVE    TBL-RECE055-SRYCD (IDVV)
                                               TO  WRK-CO-SRYCD
                   MOVE    TBL-RECE055-NAME  (IDVV) (77:)
                                               TO  WRK-CO-COMMENT
                   PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
               END-IF
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-PERFORM
      *
           INITIALIZE                  TBL-RECE055-COMMENT-AREA
      *
           .
       200799-KANREN-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単用項目ファイル読込処理
      *****************************************************************
       301-RECE11-HENSYU-SEC           SECTION.
      *       
           INITIALIZE                      RECE11-REC
           MOVE    RECE02-KEY          TO  RECE11-KEY
           PERFORM 990-RECE11-READ-SEC
           .
       301-RECE11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       2001-HKNCOMBI-GET-SEC    SECTION.
      *
           INITIALIZE                 THKNCOMBI-AREA
      *
           MOVE    ZERO            TO FLG-HKNCOMBI
           INITIALIZE                 HKNCOMBI-REC
grpsys     MOVE    RECE02-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    RECE02-PTID     TO  COMB-PTID
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-KEY2-SEL-SEC
           IF    ( FLG-HKNCOMBI    =   ZERO )
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    =   1 )
                    OR   ( TCOMB-MAX      >=   30 )
      *
               COMPUTE TCOMB-MAX   =   TCOMB-MAX   +   1
               MOVE    HKNCOMBI-REC    TO  TCOMB-OCC (TCOMB-MAX)
      *
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *    
           .
       2001-HKNCOMBI-GET-EXT.
           EXIT. 
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE02-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE09
      *     
           INITIALIZE                      RECE09-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE09-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE09-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE09-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE09-PTID
           MOVE    RECE02-RECEKA       TO  RECE09-RECEKA
      *
           START   RECE09-FILE
               KEY IS    >=   RECE09-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE09
               NOT INVALID
                   PERFORM 990-RECE09-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE09           =   1                )
grpsys             OR    ( RECE02-HOSPNUM   NOT =   RECE09-HOSPNUM   )
                   OR    ( RECE02-SRYYM     NOT =   RECE09-SRYYM     )
                   OR    ( RECE02-NYUGAIKBN NOT =   RECE09-NYUGAIKBN )
                   OR    ( RECE02-PTID      NOT =   RECE09-PTID      )
                   OR    ( RECE02-RECEKA    NOT =   RECE09-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE09
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   100
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "1"
                       IF      RECE09-HKNID   >   ZERO
                           MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                       ELSE
                           MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
                       END-IF
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
               END-EVALUATE          
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "6"
                       MOVE    SPACE      TO  WRK-HKN-FIL
                   WHEN    OTHER
                       MOVE    "−"       TO  WRK-HKN-FIL
               END-EVALUATE
      *
               IF  ( ( RECE09-NUM         =   SPACE )
               AND   ( RECE09-KIGO        =   SPACE )
               AND   ( RECE09-HKNJANUM    =   SPACE ) )
               OR  (   SYS-25-HKN-RIREKI-KBN
                                           =   "1"    )
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  WRK-HKN-HKNJANUM
                   IF      RECE09-HKNJANUM     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-HKNJANUM TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-HKNJANUM
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-KIGO
                   IF      RECE09-KIGO     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-NUM
                   IF      RECE09-NUM      NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF
                   MOVE    SPACE                  TO  WRK-HKN-RIREKI-X
                   IF      RECE09-HKNID   >   ZERO
                       STRING
                           WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-HKNJANUM       DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           WRK-HKN-FIL            DELIMITED  BY  SPACE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                                              INTO    WRK-HKN-RIREKI-X
                       END-STRING
                   ELSE
                       STRING
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           WRK-HKN-FIL            DELIMITED  BY  SPACE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                                              INTO    WRK-HKN-RIREKI-X
                       END-STRING
                   END-IF
                   IF      WRK-HKN-RIREKI-X (53:) NOT =   SPACE           
                       STRING  WRK-HKN-TEISYUTUSAKI
                                                  DELIMITED  BY  SPACE
                               RECE09-TANSEIDONAME
                                                  DELIMITED  BY  SPACE
                               "　"               DELIMITED  BY  SIZE
                               WRK-HKN-HKNJANUM   DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                       END-STRING
                       ADD     1                   TO  WRK-RIREKI-MAX
                       IF      WRK-RIREKI-MAX     >   100
                           MOVE
                               "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           GO  TO  2001-RIREKI-HEN-EXT
                       END-IF    
                       STRING  WRK-HKN-KIGO       DELIMITED  BY  SPACE
                               WRK-HKN-FIL        DELIMITED  BY  SPACE
                               WRK-HKN-NUM        DELIMITED  BY  SPACE
                               INTO
                                   WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                       END-STRING
                   ELSE
                       MOVE    WRK-HKN-RIREKI-X   TO
                                       WRK-HKN-RIREKI (WRK-RIREKI-MAX)  
                   END-IF   
               END-IF                   
      *********IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
               IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                                          NOT =   SPACE
                   ADD     1                   TO  WRK-RIREKI-MAX
                   IF      WRK-RIREKI-MAX     >   100
                       MOVE    "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF    
               END-IF                             
      *
               MOVE    RECE09-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
               MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
      *  
               IF      RECE09-TEKEDYMDX(1:6)  =   RECE02-SRYYM                      
                   MOVE    RECE09-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(3:)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)                                     
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX) 
               END-IF                                    
      *
               PERFORM 990-RECE09-NEXT-SEC
           END-PERFORM
      *
           IF    ( CNT-RECE09             =   1    )
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF          
      *
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（適用欄上段コメント・入退院履歴・手術歴・保険履歴）
      *****************************************************************
       2008-COMMENT-HEN-01-SEC      SECTION.
      *
           MOVE    "01"                TO  WRK-COM-SRYKBN
      *
      *    適用欄上段コメント編集
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      RECE02-2006-TEKIYOU-COMMENT1KBN (IDXX)
                                           =   "2"
                   ADD     1                   TO  WRK-COM-RENNUM
      *                キー編集処理
                   PERFORM 50011-KEY-HENSYU-SEC
                   MOVE    "2"                 TO  RECEDEN-RECKBN
                   MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                   MOVE    RECE02-2006-TEKIYOU-COMMENT1 (IDXX)
                                               TO  RECEDEN-RECEDATA
      *
                   PERFORM 50012-DBINSERT-SEC
      *
      *            症状詳記（コメント）編集処理
                   PERFORM 20099-COMMENT-HENSYU-SEC
               END-IF
           END-PERFORM
      *
      *    入退院履歴情報
           IF    ( SYS-2005-NYUTAIINREKI-KSIKBN
                               NOT =   "0" )               
           AND   ( RECE02-NYUINRRK-FLG
                                   =   "1" )               
               PERFORM 20082-NYUTAI-RIREKI-HEN-SEC
           END-IF
      *
      *    手術歴編集
           PERFORM 20081-OPECOM-HEN-SEC
      *
      *    保険履歴情報
      *    公費単独以外、心身喪失単独は保険履歴の記載をしない
           IF    ( RECE02-HKNNUM       =   SPACE )
           AND   ( RECE02-KOHNUM (1)
                                   NOT =   SPACE )
           AND   ( RECE02-KOHNUM (2)
                                   NOT =   SPACE )
               CONTINUE
           ELSE
               IF    ( RECE02-HKNNUM       =   SPACE )
               AND   ( RECE02-KOHNUM (1)   =   "030" )
               AND   ( RECE02-KOHNUM (2)   =   SPACE )
                  CONTINUE
               ELSE
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       WRK-RIREKI-MAX  
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
      *                キー編集処理
                       PERFORM 50011-KEY-HENSYU-SEC
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       INSPECT WRK-HKN-RIREKI-TBL  (IDXX)
                                                   REPLACING   ALL "  "
                                                               BY  "　"
                       MOVE    WRK-HKN-RIREKI-TBL  (IDXX)
                                                   TO  RECEDEN-RECEDATA
      *
                       PERFORM 50012-DBINSERT-SEC
      *
      *                症状詳記（コメント）編集処理
                       PERFORM 20099-COMMENT-HENSYU-SEC
                   END-PERFORM
               END-IF
           END-IF
           .
       2008-COMMENT-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）編集処理
      *****************************************************************
       20081-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE02-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE02-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE02-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                                   MOVE    1       TO  FLG-OPECOM-CHK
                                   MOVE    4       TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE02-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE02-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE02-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                                   MOVE    1       TO  FLG-OPECOM-CHK
                                   MOVE    4       TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20081-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）コメントの改行コード削除処理
      *****************************************************************
       200811-OPECOM-HENSYU-SEC    SECTION.
      *
           ADD     1                   TO  WRK-COM-RENNUM
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
           INSPECT WRK-OPECOM-HEN      REPLACING   ALL "  "
                                                   BY  "　"
           MOVE    WRK-OPECOM-HEN      TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
      *    症状詳記（コメント）編集処理
           PERFORM 20099-COMMENT-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       200811-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       20082-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
      *    最初の行のコメントのセット
           MOVE    SPACE               TO  WRK-NYUTAI-RIREKI-AREA
           MOVE    "入退院履歴"        TO  WRK-NYUTAI-MSG
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
      *          
           INITIALIZE                      RECE12-REC
grpsys     MOVE    RECE02-KEY          TO  RECE12-KEY1
      *
           START   RECE12-FILE
               KEY IS    >=   RECE12-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE12
               NOT INVALID
                   PERFORM 900-RECE12-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE12           =  1                   )
grpsys             OR    ( RECE02-KEY       NOT =  RECE12-KEY1         )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE12-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-NYUINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD (3:)
                   MOVE    "〜"                TO  WRK-NYUTAI-FL1
               END-IF
      *                          
               IF      RECE12-TAIINYMD (1:6)   
                                           >   RECE02-SRYYM
                   MOVE    SPACE               TO  WRK-NYUTAI-TAIINYMD
                   CONTINUE
               ELSE
                   MOVE    RECE12-TAIINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-TAIINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD (3:)
               END-IF
      *
               IF      RECE12-NYUINCHUKBN  =   ZERO
                   PERFORM 200821-NYUTAI-RIREKI-HENSYU-SEC  
                   MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
               END-IF
      *
               MOVE    RECE12-NYUINCHUKBN  TO  WRK-NYUINCHUKBN    
      *
               PERFORM 900-RECE12-NEXT-SEC
           END-PERFORM
      *
      *    最後の履歴が入院中のときに編集する
           IF      WRK-NYUTAI-RIREKI-AREA
                                       NOT =   SPACE                   
               PERFORM 200821-NYUTAI-RIREKI-HENSYU-SEC  
               MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
           END-IF
      *
           .
       20082-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       200821-NYUTAI-RIREKI-HENSYU-SEC    SECTION.
      *
           ADD     1                   TO  WRK-COM-RENNUM
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
           INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                       BY  "　"
           MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
      *
      *    症状詳記（コメント）編集処理
           PERFORM 20099-COMMENT-HENSYU-SEC
           .
       200821-NYUTAI-RIREKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（適用欄下段コメント・レセコメント）
      *****************************************************************
       2009-COMMENT-HEN-99-SEC      SECTION.
      *
           MOVE    "99"                TO  WRK-COM-SRYKBN
           MOVE   SPACE                TO  WRK-SJ-KBN
                                           WRK-RECECOM-SJKBN
      *
      *    適用欄下段コメント編集
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      RECE02-2006-TEKIYOU-COMMENT2KBN (IDXX)
                                           =   "2"
                   ADD     1                   TO  WRK-COM-RENNUM
      *            キー編集処理
                   PERFORM 50011-KEY-HENSYU-SEC
                   MOVE    "2"                 TO  RECEDEN-RECKBN
                   MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                   MOVE    RECE02-2006-TEKIYOU-COMMENT2 (IDXX)
                                               TO  RECEDEN-RECEDATA
      *
                   PERFORM 50012-DBINSERT-SEC
      *
      *            症状詳記（コメント）編集処理
                   PERFORM 20099-COMMENT-HENSYU-SEC
               END-IF 
           END-PERFORM
      *     
      *    レセコメント編集処理
      *
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１、５２、９０）       
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key23"             TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
           .
       2009-COMMENT-HEN-99-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       2009-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE02-PTID         TO  RECECOM-PTID
           MOVE    RECE02-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE02-HKNCOMBI (IDY) =   ZERO
                   IF      RECECOM-HKNCOMBI    =   RECE02-HKNCOMBI(IDY)
                       IF      RECECOM-SJKBN   =   "99"
                           PERFORM 20091-RECECOM-TEKIYOU-HEN-SEC 
                       ELSE      
                           PERFORM 20092-RECECOM-SJ-HENSYU-SEC                 
                       END-IF                 
                       MOVE    10                  TO  IDY
                   END-IF
               END-PERFORM
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2009-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄コメント（レセコメント）の改行コード削除処理
      *****************************************************************
       20091-RECECOM-TEKIYOU-HEN-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1    =   IDX     -   1
                               COMPUTE IDY2    =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                               TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                               TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                               TO  IDX
                           END-IF
                       END-PERFORM
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G       NOT =   SPACE
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       WRK-CONS-RECECOM-MAX
                             OR      WRK-COMMENT(IDW)  =   SPACE
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
      *                キー編集処理
                       PERFORM 50011-KEY-HENSYU-SEC
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       MOVE    WRK-COMMENT (IDW)   TO  RECEDEN-RECEDATA
                       MOVE    "99"                TO  RECEDEN-SJKBN
      *
                       PERFORM 50012-DBINSERT-SEC
      *
      *                適用欄コメント編集処理
                       PERFORM 20099-COMMENT-HENSYU-SEC
                     END-PERFORM
                   END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       20091-RECECOM-TEKIYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記コメント（レセコメント）の改行コード削除処理
      *****************************************************************
       20092-RECECOM-SJ-HENSYU-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN
           IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
                                               WRK-SJ-KBN
           END-IF
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1     =   IDX     -   1
                               COMPUTE IDY2     =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                                TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                                TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                                TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G       NOT =   SPACE
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       24
                             OR      WRK-COMMENT1(IDW)   =   SPACE
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
      *                キー編集処理
                       PERFORM 50011-KEY-HENSYU-SEC
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       MOVE    WRK-COMMENT1 (IDW)  TO  RECEDEN-RECEDATA
                       MOVE    RECECOM-SJKBN       TO  WRK-SJKBN-X
                       MOVE    WRK-SJKBN           TO  RECEDEN-SJKBN
      *
                       PERFORM 50012-DBINSERT-SEC
      *
                     END-PERFORM
      *
      *              症状詳記レコード編集処理
                     PERFORM 200921-SJ-HENSYU-SEC
                   END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       20092-RECECOM-SJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記レコード編集処理
      *****************************************************************
       200921-SJ-HENSYU-SEC       SECTION.
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    WRK-COMMENT-G       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF       KANACHK-RC2        =   2
               GO  TO  200921-SJ-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  200921-SJ-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  WRK-COMMENT-G
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-COMMENT-G
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "SJ,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    症状詳記区分
           IF      WRK-SJ-KBN      NOT =   SPACE     
               MOVE    WRK-SJ-KBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
           END-IF
           MOVE    SPACE               TO  WRK-SJ-KBN
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    症状詳記データ
           MOVE    WRK-COMMENT-G       TO  WRK-DATA
           MOVE    2400                TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       200921-SJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（コメント）編集処理
      *****************************************************************
       20099-COMMENT-HENSYU-SEC       SECTION.
      *
           IF      SYS-2005-RECECOMKBN =   "1"
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    RECEDEN-RECEDATA    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC2         =   2
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  RECEDEN-RECEDATA
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  RECEDEN-RECEDATA
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-COM-SRYKBN      NOT =   SPACE     
               MOVE    WRK-COM-SRYKBN      TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-COM-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-COM-FTNKBN      TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    "810000001"         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    RECEDEN-RECEDATA    TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       20099-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE02-FILE
                   RECE03-FILE
                   RECE041-FILE
                   RECE05-FILE
                   RECE09-FILE
                   RECE11-FILE
                   RECE055-FILE
                   RECE12-FILE
      *
      *    ＰＵＳＨ通知処理
           PERFORM 3001-PUSH-SEC
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           IF      WRK-PARA-PRTKBN =   "9"
               MOVE    CNT-OUT         TO  JOB-UPDCNT
           END-IF
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE02  CNT-IN  :" CNT-IN  
           DISPLAY "RECE04  CNT-IN  :" CNT-RECE04  
           DISPLAY "DB      CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR0300 END ***"
      *
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＳＨ通知処理
      *****************************************************************
       3001-PUSH-SEC        SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-KARTE-UID
                               NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-KARTE-UID   TO  PUSHBA01API-KARTE-UID
               MOVE    JOB-ORCA-UID    TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    "receipt_make"  TO  PUSHBA01-EVENT
               IF      WRK-PARA-SRYYM  =   ZERO
                   MOVE    "9999-12"       TO  PUSHBA01-PERFORM-MONTH
               ELSE
                   MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
               END-IF
               MOVE    JOB-UPDCNT1     TO  WRK-CNT
               MOVE    WRK-CNT-X       TO  PUSHBA01-OUTPUT-COUNT
      *??
             display "push=" PUSHBA01-REC "#"
      *??
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       3001-PUSH-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
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
           STRING  "ORCR0300 "             DELIMITED   BY  SIZE
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
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING        
      *
           ADD     1                   TO  WRK-RENNUM
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    仮レセ（院外処方含む）かどうかの区分設定
           IF      FLG-TOYAKUFLG       =   ZERO
               MOVE    "1"                 TO  RECEDEN-RECKBN
           ELSE
               MOVE    "9"                 TO  RECEDEN-RECKBN
           END-IF
           MOVE    WRK-RENNUM          TO  RECEDEN-RENNUM
           IF      FLG-TOTALTEN        =   1
               MOVE    SUM-TOTALTEN        TO  RECEDEN-TOTALTEN
           END-IF
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           MOVE    ZERO                TO  FLG-TOTALTEN
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー編集処理
      *****************************************************************
       50011-KEY-HENSYU-SEC        SECTION.
      *          
           INITIALIZE                      RECEDEN-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECEDEN-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECEDEN-PTID
           IF    ( SYS-2009-SYUKA-KBN  =   "1" )
               IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   MOVE    RECE02-SYUKA        TO  RECEDEN-RECEKA
               ELSE
                   IF      RECE02-RECEKA       =   SPACE
                       MOVE    ZERO            TO  RECEDEN-RECEKA
                   ELSE
                       MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
                   END-IF
               END-IF
           ELSE
               IF      RECE02-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECEDEN-RECEKA
               ELSE
                    MOVE    RECE02-RECEKA  TO  RECEDEN-RECEKA
               END-IF
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECEDEN-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
           END-IF   
           MOVE    RECE02-KOHID-KEY    TO  RECEDEN-KOHID-KEY
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECEDEN-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
           END-IF
           MOVE    RECE02-RECEIPT-KBN  TO  RECEDEN-RECEIPT-KBN
           .
       50011-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       50012-DBINSERT-SEC          SECTION.
      *
           MOVE    WRK-PARA-CREYMD     TO  RECEDEN-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECEDEN-CREHMS
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           IF      WRK-PARA-PRTKBN =   "9"
               MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           ELSE
               IF      WRK-PARA-PRTKBN =   "B"
                   MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
               ELSE
                   MOVE    "tbl_receden"       TO  MCP-TABLE
               END-IF
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT
           .
       50012-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    右詰め編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               IF      WRK-MOJI-CHK    =   1
      *            後ろの全角空白を削除する
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    "1"                 TO  KANACHK-SYORIKBN
                   MOVE    WRK-NAGASA          TO  KANACHK-MAX-CNT
                   MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   ZERO )
                   AND   ( KANACHK-MAX         >   ZERO )
                       MOVE    SPACE               TO  WRK-DATA
                       MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-DATA
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        小数ありのとき、小数部がゼロの場合
               IF      WRK-SYOSUU          >   ZERO
                   IF      WRK-NUM-R-2         =   "00000"
                      MOVE    ZERO                TO  WRK-SYOSUU
                   ELSE
                       PERFORM VARYING IDW2    FROM    WRK-SYOSUU  BY -1
                               UNTIL   IDW2    <       1
                           IF      WRK-NUM-R-2 (IDW2:1)    NOT =   "0"
                               MOVE   IDW2             TO  WRK-SYOSUU
                               MOVE    1               TO  IDW2
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW2
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントレコード記録処理（７６バイトを超えるとき）
      *****************************************************************
       5010-CO-RECORD-NEXT-HENSYU-SEC  SECTION.
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    WRK-CO-COMMENT      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF       KANACHK-RC2        =   2
               GO  TO  5010-CO-RECORD-NEXT-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  5010-CO-RECORD-NEXT-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  WRK-CO-COMMENT
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-CO-COMMENT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    WRK-CO-SRYCD        TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    WRK-CO-COMMENT      TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           INITIALIZE                      WRK-CO-SRYCD
                                           WRK-CO-COMMENT
           .
       5010-CO-RECORD-NEXT-HENSYU-EXT.
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
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYMD (1:4)      TO  WRK-HEN-YY
               MOVE    WRK-SYMD (5:2)      TO  WRK-HEN-MM
               MOVE    WRK-SYMD (7:2)      TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発変更不可対応（該当レコードを退避）処理
      *    後発変更可対応  （該当レコードを退避）処理
      *****************************************************************
       8900-SYSTEM-RESERVE-98-SEC  SECTION.
      *                                        
           INITIALIZE                      WRK-TAIHI-R41G10-REC
           INITIALIZE                      WRK-TAIHI-R41G11-REC
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-KEY          TO  RECE041-KEY1
           MOVE    "98"                TO  RECE041-SRYKBN
      *
           START   RECE041-FILE
               KEY IS    >=   RECE041-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE041-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1            )
               OR     ( FLG-RECE04           =   1            )
               OR     ( RECE02-KEY       NOT =   RECE041-KEY1 )
      *
      *        98コメントに診療コード計に該当するものを退避
               IF    ( RECE041-SRYKBN      =   "98"     )  AND
                     ( RECE041-SRYCDTOTAL  =   99209910 )
                   MOVE    RECE041-REC TO  WRK-TAIHI-R41G10-REC
               END-IF
               IF    ( RECE041-SRYKBN      =   "98"     )  AND
                     ( RECE041-SRYCDTOTAL  =   99209911 )
                   MOVE    RECE041-REC TO  WRK-TAIHI-R41G11-REC
               END-IF
      *
               PERFORM 990-RECE041-NEXT-SEC
           END-PERFORM
           .
       8900-SYSTEM-RESERVE-98-EXT.
           EXIT. 
      *
      *****************************************************************
      *    後発変更不可対応（編集）処理
      *    後発変更可対応  （編集）処理
      *****************************************************************
       8901-YKZ-HENSYU-SEC         SECTION.
      *
      *    099209910と099209911の入力なし（通常処理）
           IF      WRK-R41G10-SRYCDTOTAL   =   ZERO
           AND     WRK-R41G11-SRYCDTOTAL   =   ZERO
               PERFORM 89012-YKZ-HENSYU-SEC
               PERFORM 20072-ZAI-HENSYU-SEC
               GO  TO  8901-YKZ-HENSYU-EXT
           END-IF
      *
      *    日別に調べる
           MOVE    ZERO                TO  FLG-HENKOFUKA
           PERFORM 89011-YKZ-HENSYU-SEC
      *            1:全て該当
      *            2:部分的に該当
      *            3:該当なし
      *
      *    全て該当の場合
           IF      FLG-HENKOFUKA       =   1
               PERFORM 20072-ZAI-HENSYU-SEC
               GO  TO  8901-YKZ-HENSYU-EXT
           END-IF
      *
      *    該当なしの場合
           IF      FLG-HENKOFUKA       =   3
               PERFORM 89012-YKZ-HENSYU-SEC
               PERFORM 20072-ZAI-HENSYU-SEC
               GO  TO  8901-YKZ-HENSYU-EXT
           END-IF
      *
      *    部分的に該当の場合
      *    銘柄名
           MOVE    RECE041-REC         TO  WRK-TAIHI-RECE041-REC
           PERFORM 89014-YKZ-HENSYU-SEC
           IF      RECE041-ZAIKAISU    >   ZERO
               PERFORM 20072-ZAI-HENSYU-SEC
           END-IF
           MOVE    WRK-TAIHI-RECE041-REC   TO  RECE041-REC
      *    一般名
           MOVE    RECE041-SRYKBN      TO  WRK-SRYKBN
           PERFORM 89013-YKZ-HENSYU-SEC
           IF      RECE041-ZAIKAISU    >   ZERO
               PERFORM 89012-YKZ-HENSYU-SEC
               PERFORM 20072-ZAI-HENSYU-SEC
           END-IF
           MOVE    WRK-TAIHI-RECE041-REC   TO  RECE041-REC
           .
       8901-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発変更不可対応（日別該当チェック）処理
      *    後発変更可対応  （日別該当チェック）処理
      *****************************************************************
       89011-YKZ-HENSYU-SEC        SECTION.
      *
      *    全部該当？
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE041-KAISU (1 IDV)   >   ZERO
                   IF      WRK-R41G10-KAISU (1 IDV)    >   ZERO
                   OR      WRK-R41G11-KAISU (1 IDV)    >   ZERO
                       CONTINUE
                   ELSE
                       MOVE    9                   TO  FLG-HENKOFUKA
                       MOVE    31                  TO  IDV
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-HENKOFUKA       =   ZERO
               MOVE    1                   TO  FLG-HENKOFUKA
               GO  TO  89011-YKZ-HENSYU-EXT
           END-IF
      *    該当なし？
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE041-KAISU (1 IDV)   >   ZERO
                   IF      WRK-R41G10-KAISU (1 IDV)    =   ZERO
                   AND     WRK-R41G11-KAISU (1 IDV)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    9                   TO  FLG-HENKOFUKA
                       MOVE    31                  TO  IDV
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-HENKOFUKA       =   ZERO
               MOVE    3                   TO  FLG-HENKOFUKA
           ELSE
               MOVE    2                   TO  FLG-HENKOFUKA
           END-IF
           .  
       89011-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発変更不可対応（一般名の場合診療コードを戻す）処理
      *****************************************************************
       89012-YKZ-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDV FROM    1   BY  1
                   UNTIL   IDV >   WRK-TBLMAX6
               IF      TBL-ZAIW-KOKUJISKBKBN2 (1 IDV)  =   1
                   MOVE    "699990001"     TO  TBL-ZAIW-SRYCD (1 IDV)
               END-IF
           END-PERFORM
           .
       89012-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発変更不可対応（一般名の回数）処理
      *    後発変更可対応  （一般名の回数）処理
      *****************************************************************
       89013-YKZ-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
             IF    WRK-R41G10-SRYCDTOTAL  NOT =  ZERO
               IF     (RECE041-KAISU (1 IDV)     >   ZERO)   AND
                      (WRK-R41G10-KAISU (1 IDV)  >   ZERO)
                   COMPUTE RECE041-ZAIKAISU    =
                           RECE041-ZAIKAISU    -   RECE041-KAISU (1 IDV)
                   MOVE    ZERO                TO  RECE041-KAISU (1 IDV)
               END-IF
             END-IF
             IF    WRK-R41G11-SRYCDTOTAL  NOT =  ZERO
               IF     (RECE041-KAISU (1 IDV)     >   ZERO)   AND
                      (WRK-R41G11-KAISU (1 IDV)  =   ZERO)
                   COMPUTE RECE041-ZAIKAISU    =
                           RECE041-ZAIKAISU    -   RECE041-KAISU (1 IDV)
                   MOVE    ZERO                TO  RECE041-KAISU (1 IDV)
               END-IF
             END-IF
           END-PERFORM
           .  
       89013-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    後発変更不可対応（銘柄名の回数）処理
      *    後発変更可対応  （銘柄名の回数）処理
      *****************************************************************
       89014-YKZ-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
             IF    WRK-R41G10-SRYCDTOTAL  NOT =  ZERO
               IF     (RECE041-KAISU (1 IDV)     >   ZERO)   AND
                      (WRK-R41G10-KAISU (1 IDV)  =   ZERO)
                   COMPUTE RECE041-ZAIKAISU    =
                           RECE041-ZAIKAISU    -   RECE041-KAISU (1 IDV)
                   MOVE    ZERO                TO  RECE041-KAISU (1 IDV)
               END-IF
             END-IF
             IF    WRK-R41G11-SRYCDTOTAL  NOT =  ZERO
               IF     (RECE041-KAISU (1 IDV)     >   ZERO)   AND
                      (WRK-R41G11-KAISU (1 IDV)  >   ZERO)
                   COMPUTE RECE041-ZAIKAISU    =
                           RECE041-ZAIKAISU    -   RECE041-KAISU (1 IDV)
                   MOVE    ZERO                TO  RECE041-KAISU (1 IDV)
               END-IF
             END-IF
           END-PERFORM
           .  
       89014-YKZ-HENSYU-EXT.
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
      *????
      *     IF      FLG-HKNCOMBI        =   1
      *         DISPLAY "HKNCOMBI INV " COMB-HOSPNUM " " COMB-PTID " "
      *                                 COMB-HKNCOMBINUM " "    
      *     END-IF
      *???? 
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ検索処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO         TO FLG-HKNCOMBI
           ELSE
               MOVE    1            TO FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスタ読込処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-FETCH-SEC     SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO        TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC TO  HKNCOMBI-REC
           ELSE
               MOVE    1           TO  FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
           END-IF
      *
           .
       910-HKNCOMBI-KEY2-FETCH-EXT.
           EXIT.
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
      *    システム管理マスタ読込
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
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    RECE02-SRYYM        TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOEDYMD
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
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
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE02-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1            
      *
               READ    RECE02-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                       MOVE    HIGH-VALUE  TO  KEY-NEW
                   NOT AT  END
                       IF      STS-RECE02          =   "00"
                           CONTINUE
                       ELSE
                           MOVE    SPACE           TO  WRK-ERR-AREA
                           INITIALIZE                  WRK-ERR-AREA
                           MOVE    2               TO  WRK-ERR-FLG
                           MOVE    RECE02PARA      TO  WRK-ERR-FILE-NM
                           MOVE    STS-RECE02      TO  WRK-ERR-FILE-STS
                           PERFORM 500-FILE-ERR-ABORT-SEC
                       END-IF
      *
                       IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
                                                   OR  "2" )   AND
                             ( RECE02-TEISYUTUSAKI
                                                   =   "1"  )      )
                       OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
                                                   OR  "2"  )  AND
                             ( RECE02-TEISYUTUSAKI
                                                   =   "2"  )      )
                       OR  ( ( WRK-PARA-KOUIKIRECEKBN
                                                   =   ZERO
                                                   OR  "2"  )  AND
                             ( RECE02-TEISYUTUSAKI
                                                   =   "6"  )      )
                       OR  (   RECE02-PTNUM        =   SPACE       )
                       OR  (   RECE02-TSTPTNUMKBN  =   "1"         )
                       OR  (   RECE02-RECESYUBETU  =   ZERO   OR 
      *************************************************8888   OR 
                                                       6666        )
                       OR  (   RECE02-ERR-ERRKBN     >   ZERO      )
                           MOVE    2               TO  FLG-READ
                       ELSE
                         IF    ( RECE02-RECESYUBETU  =   8888 )
                         AND   ( WRK-PARA-PRTKBN NOT =   "9"  )
                             MOVE    2                   TO  FLG-READ
                         ELSE
      *                    主科設定のとき未設定なら更新しない
                           IF    ( SYS-2009-SYUKA-KBN  =   "1"  )
                           AND   ( RECE02-SYUKA        =   "XX" )
                             IF  (     ( RECE02-TEISYUTUSAKI   = "1" )
                                   AND ( SYS-2009-SYUKA-SYAHO  = "1" ) )
                             OR  (     ( RECE02-TEISYUTUSAKI   = "2" )
                                   AND ( SYS-2009-SYUKA-KOKUHO = "1" ) )
                             OR  (     ( RECE02-TEISYUTUSAKI   = "6" )
                                   AND ( SYS-2009-SYUKA-KOUIKI = "1" ) )
                               MOVE    2               TO  FLG-READ
                             END-IF
                           ELSE
                               IF    ( RECE02-TOTALTEN (1) =   ZERO    )
                               AND   ( RECE02-TOTALTEN (2) =   ZERO    )
                               AND   ( RECE02-TOTALTEN (3) =   ZERO    )
                               AND   ( RECE02-TOTALTEN (4) =   ZERO    )
                               AND   ( RECE02-TOTALTEN (5) =   ZERO    )
                               AND   ( RECE02-SKYZEROFLG   =   ZERO    )
                               AND   ( RECE02-RECESYUBETU  NOT =  8888
                                                              AND ZERO )
                               AND   ( WRK-PARA-PRTKBN     NOT =  "9"  )
                                   MOVE    2               TO  FLG-READ
                               END-IF
                           END-IF
                         END-IF
                       END-IF    
      *
                       IF      FLG-READ            =   ZERO
      *                    医療機関情報取得
                           IF      WRK-PARA-SRYYM  =   ZERO
                               MOVE  RECE02-SRYYM      TO
                                                   WRK-SYS-SRYYMD(1:6)
                               MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD(7:2)
                               PERFORM 900-SYSKANRI-HENSYU-SEC
                           END-IF
      *
      *                    山形地方公費対応（国保）
                           IF    ( SYS-1001-PREFNUM    =   "06"     )
                           AND   ( RECE02-TEISYUTUSAKI =   "2"      )
                           AND   ( RECE02-SRYYM        <=  "200512" ) 
                               IF    ( RECE02-KOHNUM (1) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (2) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (3) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (4) (1:1)
                                                       =   "1" OR  "2" )
                                   CONTINUE
                               ELSE
                                   MOVE    1               TO  FLG-READ
                               END-IF
                           ELSE
      *
      *                      福岡地方公費対応（国保）
                             IF    ( SYS-1001-PREFNUM    =   "40"   )
                             AND   ( RECE02-TEISYUTUSAKI =   "2"    ) 
                             AND   ( RECE02-SRYYM        <=  200609 ) 
                               IF    ( RECE02-KOHNUM (1) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (2) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (3) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (4) (1:1)
                                                       =   "1" OR  "2" )
                                   IF    ( RECE02-KOHNUM (1)   =
                                                       "141" OR "241" )
                                   OR    ( RECE02-KOHNUM (2)    =
                                                       "141" OR "241" )
                                   OR    ( RECE02-KOHNUM (3)    =
                                                       "141" OR "241" )
                                   OR    ( RECE02-KOHNUM (4)    =
                                                       "141" OR "241" )
                                       MOVE    1           TO  FLG-READ
                                   END-IF
                               ELSE
                                   MOVE    1           TO  FLG-READ
                               END-IF
                             ELSE
      *************************災害対応分は対象としない 
      *************************IF      RECE02-RJNGAIKOHFTN (1)
      *************************                            =   ZERO
                                   MOVE    1           TO  FLG-READ
      *************************END-IF
                             END-IF
                           END-IF
                       END-IF    
      *
                       IF      FLG-READ            =   1
                           ADD     1           TO  CNT-IN
                           MOVE    RECE02-RECEKA 
                                               TO  KEY-N-SRYKA
      *
                           DISPLAY "RECE02  PTNUM=" RECE02-PTNUM     
                                " RECESYUBETU=" RECE02-RECESYUBETU
                                " RJNGAIKOHFTN=" RECE02-RJNGAIKOHFTN(1)
                                " PREFKBN=" RECE02-PREFKBN
                                " PREFNUM=" RECE02-HKNJANUM-PREFNUM
                       ELSE
                           MOVE    ZERO        TO  FLG-READ     
                       END-IF
               END-READ
           END-PERFORM
           .
       910-RECE02-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE03-NEXT-SEC           SECTION.
      *
           READ    RECE03-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE03
               NOT AT  END
                   IF      STS-RECE03          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE03PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE03
           END-READ
      *
           .
       990-RECE03-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE041-NEXT-SEC           SECTION.
      *
           READ    RECE041-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE04
               NOT AT  END
                   IF      STS-RECE04          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE04PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE04          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE04
                   ADD     1         TO  CNT-RECE04
           END-READ
      *
      *
           .
       990-RECE041-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE05-NEXT-SEC           SECTION.
      *
           READ    RECE05-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE05
               NOT AT  END
                   IF      STS-RECE05          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE05PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE05          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE05
           END-READ
      *
           .
       990-RECE05-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０９データ読み
      ****************************************************************
       990-RECE09-NEXT-SEC           SECTION.
      *
           READ    RECE09-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE09
               NOT AT  END
                   IF      STS-RECE09          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE09PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE09          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE09
           END-READ
      *
           .
       990-RECE09-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト１１データ読み
      ****************************************************************
       990-RECE11-READ-SEC           SECTION.
      *
           READ    RECE11-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE11
               NOT INVALID
                   IF      STS-RECE11          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE11PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE11          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO        TO  FLG-RECE11
           END-READ
      *
           .
       990-RECE11-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み
      ****************************************************************
       900-RECE12-NEXT-SEC           SECTION.
      *
           READ    RECE12-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE12
               NOT AT  END
                   IF      STS-RECE12          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE12PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE12          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE12
           END-READ
      *
           .
       900-RECE12-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    関連コメントデータ読み込み
      ****************************************************************
       900-RECE055-NEXT-SEC           SECTION.
      *
           READ    RECE055-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE055
               NOT AT  END
                   IF      STS-RECE055         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE055PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE055         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE055
           END-READ
      *
           .
       900-RECE055-NEXT-EXT.
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ更新処理エラー　TABLE="
                                           DELIMITED  BY  SIZE
                       MCP-TABLE           DELIMITED  BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0300 insert err TABLE="
                                           DELIMITED   BY  SIZE
                      MCP-TABLE            DELIMITED   BY  SPACE
                      " PATHNAME="         DELIMITED   BY  SIZE
                      MCP-PATHNAME         DELIMITED   BY  SPACE
                      " RC="               DELIMITED   BY  SIZE
                      WRK-MCP-RC           DELIMITED   BY  SIZE
                      LOW-VALUE            DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBINSERT-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
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
               STRING  "ORCR0300 select err TABLE="
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
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.

#!/usr/bin/ruby

$:.unshift("/usr/lib/jma-receipt/scripts/daily")

require	"apslib.rb"

#################################################
#
#  システム管理マスタ「3004」のデータ削除（空白更新）
#
#  引数  　1=医療機関番号　2=管理コード　3=区分コード
#          4=開始年月日  5=終了年月日
#  戻り値　0=正常　0以外＝エラー
#
#################################################
def sys3004_update(hospnum,kanricd,kbncd,stymd,edymd)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")

  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.KBNCD"] = kbncd
  sys3004["tbl_syskanri.STYUKYMD"] = stymd
  sys3004["tbl_syskanri.EDYUKYMD"] = edymd
  sys3004["tbl_syskanri.KANRITBL"] = "                                                            FFFF"
  sys3004["tbl_syskanri.UPHMS"] = "     0"
  rc = sys3004.execFunction("DBUPDATE","key")
  return rc
end

#################################################
#
#  システム管理マスタ「3004」の削除対象区分コードの取得
#
#  引数  　1=医療機関番号　2=管理コード　3=データ  4=開始年月日  5=終了年月日
#  戻り値　KBNCD=正常　1=エラー
#
#################################################
def sys3004_del_check(hospnum,kanricd,line,stymd,edymd)
  rc = 1
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.STYUKYMD"] = stymd
  sys3004["tbl_syskanri.EDYUKYMD"] = edymd
  if sys3004.execFunction("DBSELECT","key2") == 0
    while  sys3004.execFunction("DBFETCH","key2") == 0
#  削除対象のshellidかどうか
      if  sys3004["tbl_syskanri.KANRITBL"][0,20] == line[0,20]
        rc = sys3004["tbl_syskanri.KBNCD"]
        $stderr.printf("get del-shellid [ %s ].\n",line[0.20])
        break
      end
    end
    sys3004.execFunction("DBCLOSECURSOR","key2")
  else
    rc = 1
    $stderr.printf("dbselect error .\n")
  end
  return rc
end

#################################################
#
#  システム管理マスタ「3004」の最新有効期限取得
#
#  引数　1=医療機関番号  2=管理コード
#  戻り値　1=有効開始年月日  2=有効終了年月日
#
#################################################
def kigen_get(hospnum,kanricd)
  stymd = 0
  edymd = 0
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.KBNCD"] = "0001"
  if sys3004.execFunction("DBSELECT","key8") == 0
    if  sys3004.execFunction("DBFETCH","key8") == 0
      stymd = sys3004["tbl_syskanri.STYUKYMD"]
      edymd = sys3004["tbl_syskanri.EDYUKYMD"]
    else
      $stderr.printf("dbfetch error .\n")
    end
    sys3004.execFunction("DBCLOSECURSOR","key8")
  else
    $stderr.printf("dbselect error .\n")
  end
  return stymd,edymd
end

#################################################
#
#  システム管理マスタ「3004」へデータ削除
#
#  引数  　1=医療機関番号　2=管理コード
#
#################################################
def sys3004_del_main(hospnum,kanricd)
  stymd = 0
  edymd = 0
# 最新の有効期限取得
  stymd,edymd = kigen_get(hospnum,kanricd)

  f = open("sys3004.data")
  while line = f.gets
#   sys3004.dataのshellidのチェック（空白ではないか）
    if line[0,20] != "                    "
#   削除対象shellidのチェック（該当区分コード取得）
      kbncd = sys3004_del_check(hospnum,kanricd,line,stymd,edymd)
      if kbncd != 1
#   該当区分コードの削除（空白更新）
        result = sys3004_update(hospnum,kanricd,kbncd,stymd,edymd)

        if result == 0
          $stderr.printf("sys3004 del succeeded.\n")
        else
          $stderr.printf("sys3004 del failed.\n")
        end
      else
        $stderr.printf("sys3004 empty shellid = [ %s ].\n",line[0,20])
      end
    else
        $stderr.printf("sys3004 del check end.\n")
        break
    end
  end

  f.close
end

#################################################
#
# <  Main  >
#
# 引数　1=医療機関番号
#
#################################################
hospnum = ARGV[0]

@db = PandaDB.new
@db.execFunction("DBOPEN")
@db.execFunction("DBSTART")

$stderr.printf("\nSys3004del start\n\n")

#  シス管「3004」の削除
sys3004_del_main(hospnum,"3004")

@db.execFunction("DBCOMMIT")
@db.execFunction("DBDISCONNECT")

$stderr.printf("\nSys3004del end\n")

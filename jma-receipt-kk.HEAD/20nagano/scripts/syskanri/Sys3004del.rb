#!/usr/bin/ruby

$:.unshift("/usr/lib/jma-receipt/scripts/daily")

require	"apslib.rb"

#################################################
#
#  �����ƥ�����ޥ�����3004�פΥǡ�������ʶ��򹹿���
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=��ʬ������
#          4=����ǯ����  5=��λǯ����
#  ����͡�0=���0�ʳ��ᥨ�顼
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
#  �����ƥ�����ޥ�����3004�פκ���оݶ�ʬ�����ɤμ���
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=�ǡ���  4=����ǯ����  5=��λǯ����
#  ����͡�KBNCD=���1=���顼
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
#  ����оݤ�shellid���ɤ���
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
#  �����ƥ�����ޥ�����3004�פκǿ�ͭ�����¼���
#
#  ������1=���ŵ����ֹ�  2=����������
#  ����͡�1=ͭ������ǯ����  2=ͭ����λǯ����
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
#  �����ƥ�����ޥ�����3004�פإǡ������
#
#  ����  ��1=���ŵ����ֹ桡2=����������
#
#################################################
def sys3004_del_main(hospnum,kanricd)
  stymd = 0
  edymd = 0
# �ǿ���ͭ�����¼���
  stymd,edymd = kigen_get(hospnum,kanricd)

  f = open("sys3004.data")
  while line = f.gets
#   sys3004.data��shellid�Υ����å��ʶ���ǤϤʤ�����
    if line[0,20] != "                    "
#   ����о�shellid�Υ����å��ʳ�����ʬ�����ɼ�����
      kbncd = sys3004_del_check(hospnum,kanricd,line,stymd,edymd)
      if kbncd != 1
#   ������ʬ�����ɤκ���ʶ��򹹿���
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
# ������1=���ŵ����ֹ�
#
#################################################
hospnum = ARGV[0]

@db = PandaDB.new
@db.execFunction("DBOPEN")
@db.execFunction("DBSTART")

$stderr.printf("\nSys3004del start\n\n")

#  �����ɡ�3004�פκ��
sys3004_del_main(hospnum,"3004")

@db.execFunction("DBCOMMIT")
@db.execFunction("DBDISCONNECT")

$stderr.printf("\nSys3004del end\n")

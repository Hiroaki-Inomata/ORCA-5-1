      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *    �������������     COPY (CPKOHSKY.INCή��)            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/05/14  �õ������ɲ�
      *  01.00.02    ����    2014/09/12  Ĺ���ե饰�ɲ�
      *  01.00.03    ���    2018/05/10  �ݸ���ݰ�����ô�⵭���ִ��б�
      *  01.00.04    ���    2019/01/10  �������򥳥��Ⱥ��
      ************************************************************
      * 01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  MF100-SRYYM           PIC  9(06).
      *������ʧ����
               05  MF100-TEISYUTUSAKI    PIC  9(01).
      *���Ͻ�
               05  MF100-DSPSEQ          PIC  9(03).
      *�ݸ����ֹ�
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *���ʻ�̾(��������)
               05  MF100-KANANAME-KEY    PIC  X(50).
      *�����ֹ�(��������)
               05  MF100-PTNUM-KEY       PIC  X(20).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  MF100-PTID            PIC  9(10).
      *�ݸ����ֹ棲
               05  MF100-HKNJANUM2       PIC  X(02).
      *�쥻�ץȲ�
               05  MF100-RECEKA          PIC  X(02).
      *�쥻�ץȼ���
               05  MF100-RECESYUBETU     PIC  X(04).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  MF100-HOJOKBN-KEY     PIC  X(01).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD        PIC  X(08).
      *��ô���ֹ�(KEY)
               05  MF100-FTNJANUM-KEY    PIC  X(20).
      *������ֹ�
               05  MF100-JKYSNUM-KEY     PIC  X(20).
      *����ID
               05  MF100-KOHID-KEY       PIC  9(10).
      *�����ֹ�
           03  MF100-PTNUM               PIC  X(20).
      *���ʻ�̾
           03  MF100-KANANAME            PIC  X(50).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *����
           03  MF100-SEX                 PIC  X(01).
      *��ǯ����
           03  MF100-BIRTHDAY            PIC  9(08).
      *ǯ��
           03  MF100-AGE                 PIC  9(03).
      *��˴��ʬ
           03  MF100-DEATHKBN            PIC  X(01).
      *���ݸ�����
           03  MF100-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF100-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF100-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF100-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF100-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF100-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF100-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF100-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF100-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF100-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF100-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF100-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  MF100-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF100-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF100-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF100-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF100-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF100-RJNJKYSNUM      PIC  X(07).
      *�������
           03  MF100-KOHINF.
      *������ݸ��ɣ�
               05  MF100-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF100-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF100-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF100-KOHFTNJANUM     PIC  X(20).
      *����ݼ�����ֹ�
               05  MF100-KOHJKYSNUM      PIC  X(20).
      *�ݸ��ȹ礻�ֹ����
           03  MF100-HKNCOMBIINF         OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
               05  MF100-HKNCOMBI        PIC  9(04).
      *���ݸ��ֹ�
               05  MF100-HKNNUMHC        PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
               05  MF100-KOHNUMHCINF     OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                   07  MF100-KOHNUMHC    PIC  X(03).
      *���Ŷ�ʬ����������
           03  MF100-SRYKBNINF           OCCURS  50.
      *��������
               05  MF100-KHNTEN          PIC  9(08).
      *���
               05  MF100-KAISU           PIC  9(05).
      *������������
               05  MF100-TENSUINF        OCCURS  5.
      *��������
                   07  MF100-TENSU       PIC  9(08).
      *����������
           03  MF100-JNISSUINF.
      *������
               05  MF100-JNISSU          PIC  9(02).
      *�����������
           03  MF100-TOTALTENINF.
      *�������
               05  MF100-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF100-FTNMONEYINF.
      *��ô���
               05  MF100-FTNMONEY        PIC  9(09).
      *���ް�����ô�����
           03  MF100-YKZFTNINF.
      *���ް�����ô��
               05  MF100-YKZFTN          PIC  9(05).
      *�������������
           03  MF100-SHOKUJIINF.
      *��������
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *����������
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *ɸ����ô��ʬ
           03  MF100-SHOKUJIFTNKBN       PIC  X(01).
      *��ǻ��ֶ�ʬ
           03  MF100-SHOSHINTIME         PIC  X(01).
      *�������硦���������ե饰
           03  MF100-ZAITAKU             PIC  X(01).
      *�������硦��������������ô��Ʒ�
           03  MF100-ZAITAKU-SAIKEI      PIC  9(09).
      *���ճ�����
           03  MF100-KYUFUGAITEN         PIC  9(09).
      *�õ�����
           03  MF100-TOKKI               PIC  X(02)  OCCURS 4.
      *Ĺ���ե饰
           03  MF100-TYOKI               PIC  9(01).
      *���ݸ���ô�⡧���ִ����
           03  MF100-OKIKAE-HKN          PIC 9(07).
      *�֡ʺƷǲ��ˡ����ִ����
           03  MF100-OKIKAE-SAIKEI1      PIC 9(07).
      *�֡ʺƷǾ�ˡ����ִ����
           03  MF100-OKIKAE-SAIKEI2      PIC 9(07).
      *�֡ʺƷǣ��ˡ����ִ����
           03  MF100-OKIKAE-SAIKEI3      PIC 9(07).
      *�֡ʺƷǣ��ˡ����ִ����
           03  MF100-OKIKAE-SAIKEI4      PIC 9(07).

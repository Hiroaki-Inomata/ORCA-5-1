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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.00     ͭ¼       03/08/27  ��Į¼�ֹ�򣵢����Х��Ȥ��ѹ�
      *                                   ������ɲ� 
      * 01.00.01     ͭ¼       05/02/02  SORT1,2,3��SRYDAY��°����9�����פ��ѹ�
      * 01.00.02     ����       11/07/13  ����̾�ɲ�
      *                                   ������ô���ɲ�
      *                                   �����о������ɲ�
      * 01.00.03     ����       11/09/22  ���ͣ��ɲ�(��������������ͳ)
      * 01.00.04     ����       12/07/05  ����̾�Х��ȿ�����
      * 01.00.05     ����       14/02/04  ʣ�������ౡ�������б�
      * 01.00.06     ����       15/01/07  ʿ������ǯ��������б�
      * 01.00.07     ����       15/12/10  ���ͣ��ɲáʣ��л�����б���
      * 01.00.08     ����       16/04/11  ���ͣ��ɲ�
      * 01.00.09     ���       18/09/05  ���ץ����Ȥ���
      *                                   ������ε��ܽ���
      * 01.00.10     ����       19/06/10  ������¶�ʬ�ι��ܤ��ɲ�
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *����ɽ��ʬ
               05  MF100-TENHYOKBN       PIC  X(01).
      *������KEY
               05  MF100-SORT1           PIC  9(02).
               05  MF100-SORT2           PIC  9(01).
      *
      *��Į¼�ֹ�
               05  MF100-CITYNUM         PIC  X(08).
      *�������ô���ֹ�
               05  MF100-FTNJANUM        PIC  X(08).
      *�����ֹ�
               05  MF100-PTNUM           PIC  X(20).
      *�ݸ����ֹ�        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD        PIC  X(08). 
      *���Ų�
               05  MF100-SRYKA           PIC  X(02).
      *����ǯ��
               05  MF100-SRYYM           PIC  X(08).
      *���ԣɣ�
           03  MF100-PTID                PIC  9(10).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *��ǯ����
           03  MF100-BIRTHDAY            PIC  9(08).
      *ǯ��
           03  MF100-AGE                 PIC  9(03).
      *����
           03  MF100-SEX                 PIC  X(01).
      *�߰����ե饰
           03  MF100-ZAITAKU             PIC  X(01).
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
      *�������������
           03  MF100-SHOKUJIINF.
      *��������
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *����������
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *���ް�����ô�����
           03  MF100-YKZFTNINF.
      *���ް�����ô��
               05  MF100-YKZFTN         PIC  9(05).
      *Ĺ���ե饰
           03  MF100-TYOKI              PIC  9(02).
           03  MF100-TYOKI2             PIC  9(02).
      *������ô���
           03  MF100-JIKOFTNGK          PIC  9(08).
      *�����
           03  MF100-SEIKYUGK           PIC  9(09).
      *������
           03  MF100-NYUINYMD  OCCURS 5.
               05  MF100-NYUINYY         PIC  9(04).
               05  MF100-NYUINMM         PIC  9(02).
               05  MF100-NYUINDD         PIC  9(02).
      *�ౡ��
           03  MF100-TAIINYMD  OCCURS 5.
               05  MF100-TAIINYY         PIC  9(04).
               05  MF100-TAIINMM         PIC  9(02).
               05  MF100-TAIINDD         PIC  9(02).
      *�����̸����ʬ
           03  MF100-KOHKBN-TBL.
               05  MF100-KOHKBN   PIC 9(01) OCCURS 6.
      *�������������
           03  MF100-SRYDAY-TBL.
               05  MF100-SRYDAY OCCURS 31 PIC 9(01).
      *����
           03  MF100-BIKO               PIC  X(20).
      *������ô��
           03  MF100-KOHFTNGK           PIC  9(08).
      *����̾
           03  MF100-BYOMEI             PIC  X(160).
      *����̾����ǯ����
           03  MF100-BYOYMD             PIC  X(08).
      *�����о�����
           03  MF100-TAHOTEN            PIC  9(08).
      *���ͣ�
           03  MF100-BIKO2              PIC  X(40).
      *���ͣ�
           03  MF100-BIKO3              PIC  X(208).
      *���ͣ�
           03  MF100-BIKO4              PIC  X(60).
      *���ͣ�
           03  MF100-BIKO5              PIC  X(90).

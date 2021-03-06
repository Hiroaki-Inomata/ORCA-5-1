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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����      ����
      *  01.00.01    ����    09/02/06  ���߼�̾�Х��ȿ�����
      ********************************************************************
       01  SC11.
      *����ǯ����
           03  SC11-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC11-HOSPCDN           PIC  X(20).
      *����ɽ��ʬ
           03  SC11-TENSUHYO          PIC  X(02) OCCURS 4.
      *ͭ������
           03  SC11-YUKOYM            PIC  X(16).
      *��Į¼̾
           03  SC11-CITYNAME          PIC  X(30).
      *���ŵ��ؽ����
           03  SC11-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC11-HOSPNAME          PIC  X(100).
      *���߼�̾
      *=== �Х��ȿ����� by Yoshikawa(2009/02/06) start
      *     03  SC11-KAISETU           PIC  X(40).
           03  SC11-KAISETU           PIC  X(60).
      *=== �Х��ȿ����� by Yoshikawa(2009/02/06) end
      *�����ֹ�
           03  SC11-TEL               PIC  X(15).
      *����ǯ��
           03  SC11-SRYYM             PIC  X(16).
      *������ֹ�
           03  SC11-JKYSNUM1          PIC  X(12).
           03  SC11-JKYSNUM2          PIC  X(14).
      *���桦�ֹ�
           03  SC11-KIGO              PIC  X(08).
           03  SC11-BANGO             PIC  X(16).
      *��̾
           03  SC11-NAME              PIC  X(26).
      *����
           03  SC11-SEX               PIC  X(02) OCCURS 2.
      *������
           03  SC11-TANJOYMD          PIC  X(22).
      *�ݸ���ʬ
           03  SC11-HKNKBN            PIC  X(02) OCCURS 7.
      *�ݸ����ֹ�
           03  SC11-HKNJANUM          PIC  X(16).
      *�ܿ͡���²��ʬ
           03  SC11-HONKZKKBN         PIC  X(01).
      *�ݸ���������
           03  SC11-HKNTEN            PIC  X(07).
      *�ݸ�������
           03  SC11-HKNSEIKYUGK       PIC  X(07).
      *��ˡ���������
           03  SC11-KESETEN           PIC  X(07).
      *ˬ��Ǹ���
           03  SC11-HOUMONKINGK       PIC  X(07).
      *ʻ�ѡ������ʬ
           03  SC11-TOKUTEI           PIC  X(02).
           03  SC11-TYOKI             PIC  X(02).
           03  SC11-ZAITAKU           PIC  X(02).
      *������ʬ
           03  SC11-SHOTOKU           PIC  X(02)  OCCURS 4.
      *��ô���
           03  SC11-FTNRATE           PIC  X(02)  OCCURS 3.
      *������ô�����
           03  SC11-FTNKINGK          PIC  X(07).
      *������ʬ
           03  SC11-NYUGAIKBN         PIC  X(02) OCCURS 3.
      *������ż�����
           03  SC11-NISSU             PIC  X(02).
      *�������߲��
           03  SC11-KAISU500          PIC  X(01).
      *������ô��
           03  SC11-JIKOFTN           PIC  X(05).
      *������ټ���������
           03  SC11-KOREITEN          PIC  X(07).
      *������ټ�������
           03  SC11-KOREIKINGK        PIC  X(07).
      *Ĺ����������������
           03  SC11-GAITOU            PIC  X(02) OCCURS 2.
      *��������
           03  SC11-N-NISSU           PIC  X(02).
      *����������
           03  SC11-RYOYOHI           PIC  X(07).
      *��������
           03  SC11-SHOKUJINISSU      PIC  X(02).
      *����ɸ����ô��
           03  SC11-SHOKUJIFTNGK      PIC  X(07). 

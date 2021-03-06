      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ���    2018/10/12  �������򥳥��Ⱥ��
      *  01.00.02    ���    2019/04/25  �����б�
      *                                  ����̾�󳫻����θ�������б�
      ********************************************************************
       01  SC06.
      *��������
           03  SC06-PRTYM              PIC  X(16).
      *���ŵ��إ�����
           03  SC06-HOSPCDN            PIC  X(20).
      *�ݸ���ʬ
           03  SC06-HKNKBN-TBL OCCURS 4.
               05  SC06-HKNKBN         PIC  X(02).
      *ñ��ʻ�Ѷ�ʬ
           03  SC06-HEIKBN-TBL OCCURS 2.
               05  SC06-HEIKBN         PIC  X(02).
      *�ܿͲ�²��ʬ
           03  SC06-HONKZKKBN-TBL OCCURS 3.
               05  SC06-HONKZKKBN      PIC  X(02).
      *�����ʬ
           03  SC06-KOREIKBN-TBL OCCURS 2.
               05  SC06-KOREIKBN       PIC  X(02).
      *Ϸ�ͻ�Į¼�ֹ�
           03  SC06-RJNFTNJANUM        PIC  X(16).
      *������ô���ֹ�
           03  SC06-FTNJANUM1          PIC  X(16).
           03  SC06-FTNJANUM2          PIC  X(16).
      *Ϸ�ͼ�����ֹ�
           03  SC06-RJNJKYSNUM         PIC  X(14).
      *���������ֹ�
           03  SC06-JKYSNUM1           PIC  X(14).
           03  SC06-JKYSNUM2           PIC  X(14).
      *�ݸ����ֹ�
           03  SC06-HKNJANUM           PIC  X(16).
      *���ճ��(NEW)
           03  SC06-KYURATE10          PIC  X(02).
           03  SC06-KYURATE9           PIC  X(02).
           03  SC06-KYURATE8           PIC  X(02).
           03  SC06-KYURATE7           PIC  X(02).
      *���ݸ��Ծڵ����ֹ�
           03  SC06-KIGO               PIC  X(34).
           03  SC06-BANGO              PIC  X(34).
      *�����ֹ�
           03  SC06-PTNUM              PIC  X(20).
      *����Ի�̾
           03  SC06-KANANAME           PIC  X(40).
           03  SC06-NAME               PIC  X(30).
      *��ǯ����
           03  SC06-BIRTHDAY           PIC  X(08).
      *����
           03  SC06-SEX-TBL OCCURS 2.
               05  SC06-SEX            PIC  X(02).
      *��ǯ������ǯ��
           03  SC06-NENGO-TBL OCCURS 4.
               05  SC06-NENGO          PIC  X(02).
      *�õ�����
           03  SC06-TOKKI-TBL OCCURS 3.
               05  SC06-TOKKI              PIC  X(08).
           03  SC06-ZAISOU             PIC  X(04).
           03  SC06-ZAIMARU            PIC  X(02).
      *���ŵ��ؽ����
           03  SC06-ADRS               PIC  X(100).
      *���ŵ���̾��
           03  SC06-HOSPNAME           PIC  X(100).
      *���߼Ի�̾
           03  SC06-KAISETUNAME          PIC  X(40).
      *����̾
           03  SC06-BYOMEI-TBL  OCCURS  5.
               05  SC06-BYOMEI         PIC  X(54).
      *���ų�����
           03  SC06-SRY-TBL  OCCURS  5.
               05  SC06-SRYYMD           PIC  X(26).
      *���ż�����
           03  SC06-SNISSU1            PIC  X(04).
           03  SC06-SNISSU2            PIC  X(04).
           03  SC06-SNISSU3            PIC  X(04).
      *��Į¼������
           03  SC06-CITYCD             PIC  X(16).
      *���ž��ֹ�
           03  SC06-IRYONUM            PIC  X(20).
      *ͭ
           03  SC06-ARI                PIC  X(02).
      *̵
           03  SC06-NASHI              PIC  X(02).
      *������ô��
           03  SC06-ITBFTNARI          PIC  X(07).
      *��������
           03  SC06-STENSU1            PIC  X(07).
           03  SC06-STENSU2            PIC  X(07).
           03  SC06-STENSU3            PIC  X(07).
      *���ް�����ô��
           03  SC06-YKZFTN1            PIC  X(07).
           03  SC06-YKZFTN2            PIC  X(07).
           03  SC06-YKZFTN3            PIC  X(07).
      *������ô��
           03  SC06-ITBFTN1            PIC  X(07).
           03  SC06-ITBFTN2            PIC  X(07).
           03  SC06-ITBFTN3            PIC  X(07).
      *���ǥ�����
           03  SC06-KENSIN             PIC  X(60).
      *��ǯ����2
           03  SC06-BIRTHDAY2          PIC  X(22).

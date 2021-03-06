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
      * Maj/Min/Rev  ������   ����        ����
      *  01.00.01    ����     18/12/12   �������򥳥��Ⱥ��
      *  01.00.02    ���     19/04/09   �����б�
      *  01.00.03    ����     19/04/25   ����̾�󳫻����θ�������б�
      ************************************************************
       01  SC03.
      *��������
           03  SC03-PRTYM              PIC  X(16).
      *���ŵ��إ�����
           03  SC03-HOSPCDN            PIC  X(20).
      *�ݸ���ʬ
           03  SC03-HKNKBN-TBL OCCURS 4.
               05  SC03-HKNKBN         PIC  X(02).
      *ñ��ʻ�Ѷ�ʬ
           03  SC03-HEIKBN-TBL OCCURS 3.
               05  SC03-HEIKBN         PIC  X(02).
      *�ܿͲ�²��ʬ
           03  SC03-HONKZKKBN-TBL OCCURS 3.
               05  SC03-HONKZKKBN      PIC  X(02).
      *�����ʬ
           03  SC03-KOREIKBN-TBL OCCURS 2.
               05  SC03-KOREIKBN       PIC  X(02).
      *Ϸ�ͻ�Į¼�ֹ�
           03  SC03-RJNFTNJANUM        PIC  X(16).
      *������ô���ֹ�
           03  SC03-FTNJANUM1          PIC  X(16).
           03  SC03-FTNJANUM2          PIC  X(16).
      *Ϸ�ͼ�����ֹ�
           03  SC03-RJNJKYSNUM         PIC  X(14).
      *���������ֹ�
           03  SC03-JKYSNUM1           PIC  X(14).
           03  SC03-JKYSNUM2           PIC  X(14).
      *�ݸ����ֹ�
           03  SC03-HKNJANUM           PIC  X(16).
      *���ճ��(NEW)
           03  SC03-KYURATE10          PIC  X(02).
           03  SC03-KYURATE9           PIC  X(02).
           03  SC03-KYURATE8           PIC  X(02).
           03  SC03-KYURATE7           PIC  X(02).
      *���ݸ��Ծڵ����ֹ�
           03  SC03-KIGO               PIC  X(34).
           03  SC03-BANGO              PIC  X(34).
      *�����ֹ�
           03  SC03-PTNUM              PIC  X(20).
      *����Ի�̾
           03  SC03-NAME               PIC  X(30).
      *��ǯ����
           03  SC03-BIRTHDAY           PIC  X(08).
      *����
           03  SC03-SEX-TBL OCCURS 2.
               05  SC03-SEX            PIC  X(02).
      *��ǯ������ǯ��
           03  SC03-NENGO-TBL OCCURS 4.
               05  SC03-NENGO          PIC  X(02).
      *�õ�����
           03  SC03-TOKKI-TBL OCCURS 3.
               05  SC03-TOKKI          PIC  X(08).
           03  SC03-ZAISOU             PIC  X(04).
           03  SC03-ZAIMARU            PIC  X(02).
      *���ŵ��ؽ����
           03  SC03-ADRS               PIC  X(100).
      *���ŵ���̾��
           03  SC03-HOSPNAME           PIC  X(100).
      *������̾
           03  SC03-KAISETUNAME        PIC  X(40).
      *����̾
           03  SC03-BYOMEI-TBL  OCCURS  5.
               05  SC03-BYOMEI         PIC  X(54).
      *���ų�����
           03  SC03-SRYMD-TBL  OCCURS  5.
               05  SC03-SRYYMD         PIC  X(26).
      *���ż�����
           03  SC03-SNISSU1            PIC  X(04).
           03  SC03-SNISSU2            PIC  X(04).
           03  SC03-SNISSU3            PIC  X(04).
      *��Į¼������
           03  SC03-CITYCD             PIC  X(03).
      *�����ֹ�
           03  SC03-JIGYONUM           PIC  X(02).
      *������ֹ�(����)
           03  SC03-JKYSNUM            PIC  X(10).
      *��������
           03  SC03-STENSU1            PIC  X(07).
           03  SC03-STENSU2            PIC  X(07).
           03  SC03-STENSU3            PIC  X(07).
      *���ް�����ô��
           03  SC03-YKZFTN1            PIC  X(07).
           03  SC03-YKZFTN2            PIC  X(07).
           03  SC03-YKZFTN3            PIC  X(07).
      *������ô��
           03  SC03-ITBFTN1            PIC  X(07).
           03  SC03-ITBFTN2            PIC  X(07).
           03  SC03-ITBFTN3            PIC  X(07).
      *OCR���ꥢ
           03  SC03-OCR1               PIC  X(67).
           03  SC03-OCR2               PIC  X(67).
           03  SC03-OCR3               PIC  X(67).
      *��ǯ����
           03  SC03-BIRTHDAY2          PIC  X(22).

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
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/12/03  Ģɼ�ͼ����ѹ���ȼ������
      ********************************************************************
       01  SC09.
      *���ŵ��إ�����
           03  SC09-HOSPCD            PIC  X(14).
      *��������
           03  SC09-PRTYY             PIC  X(02).
           03  SC09-PRTMM             PIC  X(02).
      *��������
           03  SC09-SEIYY             PIC  X(02).
           03  SC09-SEIMM             PIC  X(02).
           03  SC09-SEIDD             PIC  X(02).
           03  SC09-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC09-HOSPNAME          PIC  X(100).
      *���ŵ��������ֹ�
           03  SC09-TEL               PIC  X(15).
      *���߼�̾
           03  SC09-KAISETUNAME       PIC  X(40).
      *��Į¼̾��
           03  SC09-CITYNAME          PIC  X(20).
      *����ɽ��ʬ
           03  SC09-TENSUKBN-TBL OCCURS 4.
               05  SC09-TENSUKBN      PIC  X(02).
      *�õ��ʬ
           03  SC09-TOKKYUKBN         PIC  X(03).
      *
           03  SC09-MEISAI-TBL OCCURS 10.
      *������ʬ��
               05  SC09-NYUGAIKBN1     PIC  X(02).
      *������ʬ��
               05  SC09-NYUGAIKBN2     PIC  X(04).
      *��ô���ֹ�
               05  SC09-FTNJANUM       PIC  X(16).
      *������ֹ�
               05  SC09-JKYSNUM        PIC  X(14).
      *�ݸ����ֹ�
               05  SC09-HKNJANUM       PIC  X(08).
      *����Ի�̾
               05  SC09-NAME           PIC  X(40).
      *����ǯ��
               05  SC09-TANJOGYM       PIC  X(08).
      *����
               05  SC09-SEX            PIC  X(02).
      *��������
               05  SC09-SRYNISSU       PIC  X(03).
      *��������
               05  SC09-SEIKYUTEN      PIC  X(11).
      *�����
               05  SC09-SEIKYUGAKU     PIC  X(10).
      *��ô���
               05  SC09-FTNMONEY       PIC  X(07).
      *�����ݸ������
               05  SC09-SHOKUJIMONEY   PIC  X(11).
      *����ɸ����ô��
               05  SC09-SHOKUJIFTN     PIC  X(10).
      *����ǯ����
               05  SC09-NYUINYMD       PIC  X(09).
      *����ˡ���ֹ�
               05  SC09-KOHNUM         PIC  X(02).
      *=== Ģɼ�ͼ����ѹ� by Yoshikawa(2010/12/03) start
      *�õ�����
               05  SC09-TOKKI          PIC  X(02).
      *=== Ģɼ�ͼ����ѹ� by Yoshikawa(2010/12/03) end
      *���
          03  SC09-GOKEI-TBL.
      *��׷��
               05  SC09-G-KENSU        PIC  X(03).
      *�����������
               05  SC09-G-SEIKYUTEN    PIC  X(11).
      *��׿����ݸ������
               05  SC09-G-SHOKUJIMONEY PIC  X(11).
      *��������
               05  SC09-G-SEIKYUGAKU   PIC  X(10).
      *��׿���ɸ����ô��
               05  SC09-G-SHOKUJIFTN   PIC  X(10).
      *�����ô���
               05  SC09-G-FTNMONEY     PIC  X(07).


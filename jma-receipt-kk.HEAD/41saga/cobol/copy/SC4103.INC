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
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         11/04/15  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ����         19/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SC03.
      *��������
           03  SC03-PRTYM             PIC  X(16).
      *���ŵ��ؽ����
           03  SC03-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC03-HOSPNAME          PIC  X(100).
      *�����ֹ�
           03  SC03-TEL               PIC  X(15).
      *���߼�̾
           03  SC03-KAISETUNAME       PIC  X(30).
      *���ŵ��إ�����
           03  SC03-HOSPCDN           PIC  X(20).
      *����ơ��֥�
           03  SC03-SEIKYU-TBL   OCCURS  16.
      *�ݸ���̾
               05  SC03-HKNJANAME1         PIC  X(20).
      *�����ơ��֥�ʹ��ݡ�
               05  SC03-NYUGAI-TBL1      OCCURS   2.
      *���
                   07  SC03-KENSU1         PIC  X(05).
      *����
                   07  SC03-NISSU1         PIC  X(05).
      *����
                   07  SC03-TENSU1         PIC  X(10).
      *������ô��
                   07  SC03-ITBFTN1        PIC  X(09).
      *��Į¼̾
               05  SC03-HKNJANAME2         PIC  X(20).
      *�����ơ��֥�ʼ��ݡ�
               05  SC03-NYUGAI-TBL2      OCCURS   2.
      *���
                   07  SC03-KENSU2         PIC  X(05).
      *����
                   07  SC03-NISSU2         PIC  X(05).
      *����
                   07  SC03-TENSU2         PIC  X(10).
      *������ô��
                   07  SC03-ITBFTN2        PIC  X(09).
      *��ץơ��֥�
           03  SC03-GOKEI-TBL   OCCURS  2.
      *��סݷ���ʹ��ݡ�
               05  SC03-GKENSU1        PIC  X(06).
      *��ס������ʹ��ݡ�
               05  SC03-GNISSU1        PIC  X(06).
      *��ס������ʹ��ݡ�
               05  SC03-GTENSU1        PIC  X(11).
      *��סݴ�����ô�ۡʹ��ݡ�
               05  SC03-GITBFTN1       PIC  X(11).
      *��סݷ���ʼ��ݡ�
               05  SC03-GKENSU2        PIC  X(06).
      *��ס������ʼ��ݡ�
               05  SC03-GNISSU2        PIC  X(06).
      *��ס������ʼ��ݡ�
               05  SC03-GTENSU2        PIC  X(11).
      *��סݴ�����ô�ۡʼ��ݡ�
               05  SC03-GITBFTN2       PIC  X(11).
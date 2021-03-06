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
      *    ����������������ɽ �ʻ�������     COPY (SKT0605.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/05/08    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/03/12  ����ɽ��ʬ�ɲ�
      *  01.00.02    ����    2012/05/10  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/10/12  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0605.
      *    ����ǯ��
           03  SKT0605-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT0605-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT0605-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT0605-KAISETUNAME         PIC X(40).
      *    ���ŵ��إ�����
           03  SKT0605-HOSPCD              PIC X(14).
      *    ����ɽ��ʬ
           03  SKT0605-TENHYOKBN           PIC X(02) OCCURS 4.
      *    �ǡ����ơ��֥�
           03  SKT0605-DATA-TBL   OCCURS 27.
      *    �ݸ���̾
               05  SKT0605-HKNJANAME       PIC X(20).
      *    �����ʬ
               05  SKT0605-KAMIBUN         PIC X(12).
      *    �����ơ��֥�
               05  SKT0605-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT0605-KENSU       PIC X(07).
      *    ����
                   07  SKT0605-NISSU       PIC X(07).
      *    ����
                   07  SKT0605-TENSU       PIC X(14).

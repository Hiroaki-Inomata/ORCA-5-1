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
      *    ���Ź԰����������û������å�
      *                               COPY (CPORCSCNKSNCHK.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCNKSNCHKAREA.
      *���Ź԰٥�����
           03  ORCSCNKSN-SRYCD             PIC  X(09).
      *����ǯ��
           03  ORCSCNKSN-SRYYM             PIC  X(06).
      *���������ʻ�������1�����
           03  ORCSCNKSN-DAY-G.
               05  ORCSCNKSN-DAY           PIC 9(01)
                                           OCCURS   31.
      *���顼������
      *  �����ѥ�᥿ϳ��
      *  �������Ծ��󸡺����顼
      *  ������������ݸ��ȹ礻̵��
      *  �����������������顼
      *  �����������Ȥ�ʻ���ꥨ�顼�����û���
      *  �����������Ȥ�ʻ���ꥨ�顼���������������û���
      *  �����������˲û�����
      *  ����������פǲû�����Ѥ�
      *  ����������פǲû��κޤ�¸�ߤ���
      *
           03  ORCSCNKSN-RC                PIC S9(9) BINARY.
      *���顼��å�����
           03  ORCSCNKSN-ERRMSG            PIC X(80).
      *

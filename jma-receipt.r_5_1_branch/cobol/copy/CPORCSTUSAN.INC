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
      *    �̻�������������          COPY (CPORCSTUSAN.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTUSANAREA.
      *  ���ŵ��إ�����
           03  STUSAN-HOSPNUM              PIC 9(2).
      *  ���ԣɣ�
           03  STUSAN-PTID                 PIC  9(10).
      *    �����
           03  STUSAN-KJNYMD               PIC X(08).
      *    ���������������ɡ������Ǥ�ա�
           03  STUSAN-TOKUTEI-SRYCD        PIC X(09)
                                           OCCURS 10.
      *    �����ֹ������ʬ�������Ǥ�ա�
           03  STUSAN-RRKNUMKBN            PIC X(01).
      *    �����ֹ�������ֹ������ʬ��"1"�ξ��������
           03  STUSAN-RRKNUM               PIC 9(03).
      *    ������׸�����ά
           03  STUSAN-NOT-SELECT-NACCT     PIC X(01).
      *
      *  �����ΰ�
      *    �̻�����
           03  STUSAN-OUT-AREA.
               05  STUSAN-NISSU-VAL.
      *            ���������������������������ޤǤ��̻�����
                   07  STUSAN-NISSU01      PIC 9(08).
      *            ������������������������ޤǤ��̻�������¾��ʬ�ޤ��
                   07  STUSAN-NISSU02      PIC 9(08).
      *            ������������������������ޤǤ��̻�������¾��ʬ�ޤ᤺��
                   07  STUSAN-NISSU03      PIC 9(08).
      *            ������������������������ޤǤ��̻�����
      *            �ʰ������������������Ƥ���������¾��ʬ�ޤ�ˡ�
                   07  STUSAN-NISSU04      PIC 9(08).
      *            ������������������������ޤǤ��̻�����
      *            �ʰ������������������Ƥ���������¾��ʬ�ޤ᤺�ˡ�
                   07  STUSAN-NISSU05      PIC 9(08).
      *            �����������������������������ޤǤ������������̻�����
                   07  STUSAN-NISSU06      PIC 9(08).
      *            �����������������������������ޤǤΰ����ǻ��ꤷ��
      *            �����������Ǥ��̻�����
                   07  STUSAN-NISSU07      PIC 9(08).
      *            ����������������������������ޤǤ������������̻�����
      *            �ʼ�����ʬ�ޤࡦ¾����ʬ�ޤ᤺��
                   07  STUSAN-NISSU08      PIC 9(08).
      *            ����������������������������ޤǤΰ����ǻ��ꤷ��
      *            �����������Ǥ��̻�����
                   07  STUSAN-NISSU09      PIC 9(08).
               05  STUSAN-TUSAN-R  REDEFINES   STUSAN-NISSU-VAL.
                   07  STUSAN-TUSAN-OCC    OCCURS  9.
                       09  STUSAN-NISSU    PIC 9(08).
      *            ����������å��ե饰����0:�оݳ���1:�оݡ�
      *            �ʴ���������������̻��������оݤȤʤ뤫�����å���
               05  STUSAN-KJNYMD-CHK-FLG   PIC 9(01).
      *        ���̤ʴط��ˤ�����ŵ��ؤ��������Ƥ�������
               05  STUSAN-TAINREL-NISSU    PIC 9(08).
               05  STUSAN-RRK-VAL.
      *            �ơ��֥륪���С��ե饰
                   07  STUSAN-RRK-FLG-OVER         PIC 9(01).
                   07  STUSAN-RRK-CNT              PIC 9(05).
                   07  STUSAN-RRK-OCC              OCCURS  50.
                       09  STUSAN-RRK-JTIKBN       PIC X(01).
                       09  STUSAN-RRK-TAINRELKBN   PIC X(01).
                       09  STUSAN-RRK-NYUINYMD     PIC X(08).
                       09  STUSAN-RRK-TAIINYMD     PIC X(08).
                       09  STUSAN-RRK-DJKBN        PIC X(01).
      *                �������������������Ƥ�������
      *                ��������ǡ����ξ����о�������
                       09  STUSAN-RRK-NISSU        PIC 9(08).
      *  �꥿���󥳡���
           03  STUSAN-RC               PIC 9(02).